use crate::data::Value;
use crate::state::Runtime;
use crate::state::NotifierList;
use crate::util;
use crate::util::{Cell,spawn_noerr};
use dbus::arg::Variant;
use dbus::channel::{BusType,Channel,MatchingReceiver};
use dbus::message::{MatchRule,Message,MessageType};
use dbus::nonblock::{LocalConnection,Process,NonblockReply};
use dbus::nonblock::stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged;
use futures_util::future::Either;
use futures_util::future::select;
use futures_util::pin_mut;
use log::{info,warn,error};
use once_cell::unsync::OnceCell;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::rc::Rc;
use std::ptr::NonNull;
use std::time::Duration;
use tokio::io::unix::AsyncFd;
use tokio::sync::Notify;

thread_local! {
    static SOCK : Rc<OnceCell<SessionDBus>> = Default::default();
}

struct SigWatcherNZ<F : ?Sized>(F);
struct SigWatcherZST<F : ?Sized>(u8, F);

trait SigWatcherCall {
    fn call(&mut self, m: &Message, s: &SessionDBus);
    fn get_sw_ptr(&self) -> Option<NonNull<()>>;
}

impl<F: FnMut(&Message, &SessionDBus)> SigWatcherCall for SigWatcherNZ<F> {
    fn call(&mut self, m: &Message, s: &SessionDBus) {
        (self.0)(m, s)
    }
    fn get_sw_ptr(&self) -> Option<NonNull<()>> {
        NonNull::new(self as *const SigWatcherNZ<F> as *mut ())
    }
}

impl<F: FnMut(&Message, &SessionDBus)> SigWatcherCall for SigWatcherZST<F> {
    fn call(&mut self, m: &Message, s: &SessionDBus) {
        (self.1)(m, s)
    }
    fn get_sw_ptr(&self) -> Option<NonNull<()>> {
        NonNull::new(self as *const SigWatcherZST<F> as *mut ())
    }
}

#[derive(Debug,Default)]
pub struct SigWatcherToken(Option<NonNull<()>>);

impl SigWatcherToken {
    pub fn is_active(&self) -> bool {
        self.0.is_some()
    }
}

pub struct SessionDBus {
    pub local : LocalConnection,
    prop_watchers : util::Cell<Vec<Box<dyn FnMut(&Message, &PropertiesPropertiesChanged, &SessionDBus)>>>,
    name_watchers : util::Cell<Vec<Box<dyn FnMut(&str, &str, &str, &SessionDBus)>>>,
    sig_watchers : util::Cell<Vec<Box<dyn SigWatcherCall>>>,
    api : util::Cell<HashMap<Cow<'static,str>, Box<dyn FnMut(Message, &SessionDBus)>>>,
    wake : Notify,
}

pub fn init() -> Result<(), Box<dyn Error>> {
    let rc = SOCK.with(|cell| cell.clone());

    let mut channel = Channel::get_private(BusType::Session)?;
    channel.set_watch_enabled(true);
    let watch = channel.watch();
    let afd = AsyncFd::new(util::Fd(watch.fd))?;

    let mut local = LocalConnection::from(channel);
    let wake = Notify::new();

    local.set_waker(Some(Box::new(|| {
        let rc = SOCK.with(|cell| cell.clone());
        match rc.get() {
            Some(conn) => conn.wake.notify_one(),
            None => error!("Ignoring dbus wake on wrong thread"),
        }
        Ok(())
    })));

    let mut rule = MatchRule::new();
    rule.msg_type = Some(MessageType::Signal);
    local.start_receive(rule, Box::new(move |msg, _local| {
        let this = SessionDBus::get();
        let mut watchers = this.sig_watchers.replace(Vec::new());
        for watcher in &mut watchers {
            watcher.call(&msg, &this);
        }
        this.sig_watchers.take_in(|w| {
            if w.is_empty() {
                *w = watchers;
            } else {
                w.extend(watchers);
            }
        });
        true
    }));

    rule = MatchRule::new_method_call();
    local.start_receive(rule, Box::new(move |msg, _local| {
        let this = SessionDBus::get();
        let mut objects = this.api.replace(HashMap::new());
        if let Some(object) = msg.path().and_then(|p| objects.get_mut(&*p)) {
            (object)(msg, &this);
        } else {
            use dbus::channel::Sender;
            let mut rsp = dbus::channel::default_reply(&msg);
            if msg.path().as_deref() == Some("/") &&
                msg.interface().as_deref() == Some("org.freedesktop.DBus.Introspectable") &&
                msg.member().as_deref() == Some("Introspect")
            {
                let mut rv = String::new();
                rv.push_str(r#"<!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd"><node>"#);
                for node in objects.keys() {
                    use std::fmt::Write;
                    write!(rv, r#"<node name="{}"/>"#, &node[1..]).unwrap();
                }
                rv.push_str(r#"</node>"#);
                rsp = Some(msg.return_with_args((rv,)));
            }
            let _ = rsp.map(|rsp| this.local.send(rsp));
        }
        this.api.take_in(|api| {
            if api.is_empty() {
                *api = objects;
            } else {
                api.extend(objects);
            }
        });
        true
    }));

    let dbus = SessionDBus {
        local,
        wake,
        prop_watchers : Default::default(),
        name_watchers : Default::default(),
        sig_watchers : Default::default(),
        api : Default::default(),
    };

    dbus.add_signal_watcher(move |msg, this| {
        if msg.interface().as_deref() != Some("org.freedesktop.DBus.Properties") ||
            msg.member().as_deref() != Some("PropertiesChanged")
        {
            return;
        }
        if let Ok(p) = msg.read_all::<PropertiesPropertiesChanged>() {
            let mut watchers = this.prop_watchers.replace(Vec::new());
            for watcher in &mut watchers {
                (*watcher)(&msg, &p, &this);
            }
            this.prop_watchers.take_in(|w| {
                if w.is_empty() {
                    *w = watchers;
                } else {
                    w.extend(watchers);
                }
            });
        } else {
            warn!("Could not parse PropertiesPropertiesChanged message: {:?}", msg);
        }
    });

    dbus.add_signal_watcher(move |msg, this| {
        if msg.interface().as_deref() != Some("org.freedesktop.DBus") ||
            msg.member().as_deref() != Some("NameOwnerChanged")
        {
            return;
        }
        if let (Some(name), Some(old), Some(new)) = msg.get3::<String, String, String>() {
            let mut watchers = this.name_watchers.replace(Vec::new());
            for watcher in &mut watchers {
                (*watcher)(&name, &old, &new, &this);
            }
            this.name_watchers.take_in(|w| {
                if w.is_empty() {
                    *w = watchers;
                } else {
                    w.extend(watchers);
                }
            });
        }
    });

    rc.set(dbus).ok().expect("Called init twice");

    util::spawn("D-Bus I/O loop", async move {
        let conn = rc.get().unwrap();
        let channel : &Channel = conn.local.as_ref();
        loop {
            let msg_in = afd.readable();
            let msg_out = conn.wake.notified();
            pin_mut!(msg_in, msg_out);
            let why = select(msg_in, msg_out).await;
            channel.read_write(Some(Default::default())).map_err(|()| io::Error::last_os_error())?;
            match why {
                Either::Left((rh, _)) => {
                    // if we woke due to readable, check to see if we are done reading and clear
                    // the ready status if so.
                    //
                    // https://github.com/diwic/dbus-rs/issues/254
                    let mut rh = rh?;
                    let mut buf = [0u8;1];
                    let rc = unsafe {
                        libc::recv(watch.fd, buf.as_mut_ptr() as *mut _, 1, libc::MSG_DONTWAIT | libc::MSG_PEEK)
                    };
                    if rc != 1 {
                        rh.clear_ready();
                    }
                }
                Either::Right(((), _)) => {}
            }
            conn.local.process_all();

            // clear out the send buffer.  This should only happen when a write was already blocked.
            if channel.has_messages_to_send() {
                loop {
                    let mut wh = afd.writable().await?;
                    channel.read_write(Some(Default::default())).map_err(|()| io::Error::last_os_error())?;
                    if channel.has_messages_to_send() {
                        wh.clear_ready();
                    } else {
                        break;
                    }
                }
            }
        }
    });

    Ok(())
}

impl SessionDBus {
    pub fn get() -> impl std::ops::Deref<Target=Self> {
        struct V(Rc<OnceCell<SessionDBus>>);

        impl std::ops::Deref for V {
            type Target = SessionDBus;
            fn deref(&self) -> &SessionDBus {
                self.0.get().expect("Must call dbus::init before dbus::get")
            }
        }
        V(SOCK.with(|cell| cell.clone()))
    }

    pub fn add_signal_watcher<F>(&self, f : F)
        -> SigWatcherToken
        where F : FnMut(&Message, &Self) + 'static
    {
        if std::mem::size_of::<F>() != 0 {
            self.do_add_signal_watcher(Box::new(SigWatcherNZ(f)))
        } else {
            self.do_add_signal_watcher(Box::new(SigWatcherZST(0, f)))
        }
    }

    fn do_add_signal_watcher(&self, b : Box<dyn SigWatcherCall>) -> SigWatcherToken
    {
        let rv = SigWatcherToken(b.get_sw_ptr());
        self.sig_watchers.take_in(|w| w.push(b));
        rv
    }

    pub fn stop_signal_watcher(&self, t : &mut SigWatcherToken) {
        if let Some(stop_ptr) = t.0.take() {
            self.sig_watchers.take_in(|w| {
                w.retain(|w| {
                    let ptr = w.get_sw_ptr();
                    ptr != Some(stop_ptr)
                });
            });
        }
    }

    pub async fn add_property_change_watcher<F>(&self, f : F)
        where F : FnMut(&Message, &PropertiesPropertiesChanged, &Self) + 'static
    {
        if self.prop_watchers.take_in(|w| {
            w.push(Box::new(f));
            w.len() == 1
        }) {
            let prop_rule = MatchRule::new_signal("org.freedesktop.DBus.Properties", "PropertiesChanged");
            let rule_str = prop_rule.match_str();
            match self.local.add_match_no_cb(&rule_str).await {
                Ok(()) => {}
                Err(e) => warn!("Could not register for PropertyChange messages: {}", e),
            }
        }
    }

    pub async fn add_name_watcher<F>(&self, f : F)
        where F : FnMut(&str, &str, &str, &Self) + 'static
    {
        if self.name_watchers.take_in(|w| {
            w.push(Box::new(f));
            w.len() == 1
        }) {
            let na_rule = MatchRule::new_signal("org.freedesktop.DBus", "NameOwnerChanged");
            let rule_str = na_rule.match_str();
            match self.local.add_match_no_cb(&rule_str).await {
                Ok(()) => {}
                Err(e) => warn!("Could not register for NameAcquired messages: {}", e),
            }
        }
    }

    pub fn add_api(&self, name : impl Into<Cow<'static, str>>, imp : Box<dyn FnMut(Message, &SessionDBus)>) {
        if self.api.take_in(|api| api.insert(name.into(), imp)).is_some() {
            panic!("Object already exists");
        }
    }
}

pub fn get() -> impl std::ops::Deref<Target=SessionDBus> {
    SessionDBus::get()
}

pub fn read_hash_map(value : &impl dbus::arg::RefArg) -> Option<HashMap<String, Variant<Box<dyn dbus::arg::RefArg>>>> {
    if let Some(iter) = value.as_iter() {
        let mut map = HashMap::new();
        let mut k = None;
        for i in iter {
            match k.take() {
                None => {
                    k = i.as_str().map(String::from);
                    if k.is_none() {
                        return None;
                    }
                }
                Some(k) => {
                    map.insert(k, Variant(i.box_clone()));
                }
            }
        }
        if k.is_none() {
            return Some(map);
        }
    }
    None
}

/// The "dbus" block
#[derive(Debug)]
pub struct DbusValue {
    bus_name : Box<str>,
    path : Box<str>,
    interface : Box<str>,
    member : Box<str>,
    args : Box<[toml::Value]>,
    sig : Cell<Option<Box<str>>>,
    value : RefCell<Option<Box<[Box<dyn dbus::arg::RefArg>]>>>,
    interested : Cell<NotifierList>,
    watch : Cell<SigWatcherToken>,
}

impl Drop for DbusValue {
    fn drop(&mut self) {
        let mut w = self.watch.take();
        if w.is_active() {
            let bus = SessionDBus::get();
            bus.stop_signal_watcher(&mut w);
        }
    }
}

impl DbusValue {
    pub fn from_toml(value : &toml::Value) -> Option<Rc<Self>> {
        let bus_name = value.get("owner").and_then(|v| v.as_str()).unwrap_or("").into();
        let path = value.get("path").and_then(|v| v.as_str()).unwrap_or("").into();
        let method = value.get("method").and_then(|v| v.as_str());
        let property = value.get("property").and_then(|v| v.as_str());
        let (interface, member, args);
        match (method.map(|s| s.rsplit_once(".")) , property.map(|s| s.rsplit_once("."))) {
            (Some(_), Some(_)) => {
                error!("dbus cannot query both a property and a method");
                return None;
            }
            (Some(Some((i,m))), None) => {
                interface = i.into();
                member = m.into();
                args = value.get("args")
                    .and_then(|v| v.as_array())
                    .cloned()
                    .unwrap_or_default()
                    .into_boxed_slice();
            }
            (None, Some(Some((i,p)))) => {
                interface = "org.freedesktop.DBus.Properties".into();
                member = "Get".into();
                args = Box::new([i.into(), p.into()]);
            }
            _ => {
                error!("dbus requires a member or property to query");
                return None;
            }
        }

        let rc = Rc::new(DbusValue {
            bus_name, path, interface, member, args,
            value : RefCell::new(None),
            sig : Default::default(),
            interested : Default::default(),
            watch : Default::default(),
        });

        let watch_path = value.get("watch-path").and_then(|v| v.as_str());
        let watch_method = value.get("watch-method").and_then(|v| v.as_str());
        match watch_method.map(|s| s.rsplit_once(".")) {
            Some(Some((i,m))) => {
                let mut rule = dbus::message::MatchRule::new_signal(i, m);
                rule.path = watch_path.as_deref().map(Into::into);
                let expr = rule.match_str();
                spawn_noerr(async move {
                    let bus = SessionDBus::get();
                    match bus.local.add_match_no_cb(&expr).await {
                        Ok(()) => {}
                        Err(e) => warn!("Could not add watch on {}: {}", expr, e)
                    }
                });
                let weak = Rc::downgrade(&rc);
                let rule = rule.static_clone();
                let bus = SessionDBus::get();
                let watch = bus.add_signal_watcher(move |msg, _bus| {
                    if rule.matches(msg) {
                        if let Some(rc) = weak.upgrade() {
                            rc.call_now();
                        }
                    }
                });
                rc.watch.set(watch);
            }
            Some(None) if watch_method == Some("") => {}
            Some(None) => error!("Invalid dbus watch expression, ignoring"),
            None if property.is_some() => {
                let prop = property.unwrap_or_default().to_owned();
                let weak = Rc::downgrade(&rc);
                spawn_noerr(async move {
                    let bus = SessionDBus::get();
                    bus.add_property_change_watcher(move |msg, ppc, _bus| {
                        use dbus::arg::RefArg;
                        if let (Some(rc), Some((iface, prop))) = (weak.upgrade(), prop.rsplit_once(".")) {
                            if ppc.interface_name != iface || msg.path().as_deref() != Some(&*rc.path) {
                                return;
                            }
                            if let Some(value) = ppc.changed_properties.get(prop) {
                                *rc.value.borrow_mut() = Some(Box::new([value.box_clone()]));
                            } else if ppc.invalidated_properties.iter().any(|p| p == prop) {
                                *rc.value.borrow_mut() = None;
                            }
                        }
                    }).await;
                });
            }
            None => {}
        }
        Some(rc)
    }

    fn call_now(self : Rc<Self>) {
        spawn_noerr(async move {
            self.do_call().await;
        });
    }

    pub async fn do_call(self : Rc<Self>) {
        use toml::value::Value;
        let bus = SessionDBus::get();
        let proxy = dbus::nonblock::Proxy::new(&*self.bus_name, &*self.path, Duration::from_secs(30), &bus.local);
        let mut api = None;
        if !self.args.iter().all(Value::is_str) {
            api = self.sig.take_in(|s| s.clone());
            if api.is_none() {
                use dbus::nonblock::stdintf::org_freedesktop_dbus::Introspectable;
                match proxy.introspect().await {
                    Ok(xml) => {
                        let mut reader = xml::EventReader::from_str(&xml);
                        let mut sig = String::new();
                        let mut in_iface = false;
                        let mut in_method = false;
                        loop {
                            use xml::reader::XmlEvent;
                            match reader.next() {
                                Ok(XmlEvent::StartElement { name, attributes, .. })
                                    if name.local_name == "interface" =>
                                {
                                    in_iface = attributes.iter().any(|attr| {
                                        attr.name.local_name == "name" &&
                                        attr.value == &*self.interface
                                    });
                                }
                                Ok(XmlEvent::EndElement { name })
                                    if name.local_name == "interface" =>
                                {
                                    in_iface = false;
                                }
                                Ok(XmlEvent::StartElement { name, attributes, .. })
                                    if name.local_name == "method" =>
                                {
                                    in_method = attributes.iter().any(|attr| {
                                        attr.name.local_name == "name" &&
                                        attr.value == &*self.member
                                    });
                                }
                                Ok(XmlEvent::EndElement { name })
                                    if name.local_name == "interface" =>
                                {
                                    in_method = false;
                                }
                                Ok(XmlEvent::StartElement { name, attributes, .. })
                                    if in_iface && in_method &&
                                        name.local_name == "arg" &&
                                        attributes.iter().any(|attr| {
                                            attr.name.local_name == "direction" &&
                                            attr.value == "in"
                                        }) =>
                                {
                                    for attr in attributes {
                                        if attr.name.local_name == "type" {
                                            sig.push_str(&attr.value);
                                            sig.push_str(",");
                                        }
                                    }
                                }
                                Ok(XmlEvent::EndDocument) => break,
                                Ok(_) => {}
                                Err(e) => {
                                    info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                                    *self.value.borrow_mut() = None;
                                    return;
                                }
                            }
                        }
                        sig.pop();
                        api = Some(sig.into());
                        self.sig.set(api.clone());
                    }
                    Err(e) => {
                        info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                        *self.value.borrow_mut() = None;
                        return;
                    }
                };
            }
        }

        struct A<'a>(&'a [toml::Value], Option<&'a str>);
        impl dbus::arg::AppendAll for A<'_> {
            fn append(&self, a: &mut dbus::arg::IterAppend<'_>) {
                use dbus::arg::Append;
                if let Some(sig) = self.1 {
                    for (ty,arg) in sig.split(",").zip(self.0) {
                        match ty {
                            "s" => { arg.as_str().map(|s| s.append(a)); }
                            "d" => {
                                arg.as_float()
                                    .or_else(|| arg.as_integer().map(|i| i as f64))
                                    .map(|f| f.append(a));
                            }
                            "y" => { arg.as_integer().map(|i| i as u8 ).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "n" => { arg.as_integer().map(|i| i as i16).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "q" => { arg.as_integer().map(|i| i as u16).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "i" => { arg.as_integer().map(|i| i as i32).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "u" => { arg.as_integer().map(|i| i as u32).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "x" => { arg.as_integer().map(|i| i as i64).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "t" => { arg.as_integer().map(|i| i as u64).or_else(|| arg.as_float().map(|f| f as _)).map(|i| i.append(a)); }
                            "b" => {
                                arg.as_bool()
                                    .or_else(|| arg.as_integer().map(|i| i != 0))
                                    .or_else(|| arg.as_float().map(|i| i != 0.0))
                                    .map(|b| b.append(a));
                            }
                            _ => {
                                info!("Unsupported dbus argument type '{}'", ty);
                            }
                        }
                    }
                } else {
                    for arg in self.0.iter() {
                        match arg {
                            Value::String(s) => s.append(a),
                            _ => {
                                info!("Invalid dbus argument '{}'", arg);
                            }
                        }
                    }
                }
            }
        }
        struct R(Box<[Box<dyn dbus::arg::RefArg>]>);
        impl dbus::arg::ReadAll for R {
            fn read(i: &mut dbus::arg::Iter<'_>) -> Result<Self, dbus::arg::TypeMismatchError> {
                Ok(R(i.collect()))
            }
        }
        match proxy.method_call(&*self.interface, &*self.member, A(&self.args, api.as_deref())).await {
            Ok(R(rv)) => {
                *self.value.borrow_mut() = Some(rv);
            }
            Err(e) => {
                info!("Error calling {}.{}: {}", self.interface, self.member, e);
                *self.value.borrow_mut() = None;
            }
        }
        self.interested.take().notify_data("dbus-read");
    }

    pub fn read_in<F : FnOnce(Value) -> R, R>(&self, key : &str, rt : &Runtime, f : F) -> R {
        self.interested.take_in(|i| i.add(rt));
        let value = self.value.borrow();
        let mut keys = key.split(".");
        let key0 = keys.next().unwrap_or("");
        let vi = match (key0.parse::<usize>(), value.as_ref()) {
            (Ok(i), Some(v)) => v.get(i),
            (Err(_), Some(v)) => v.get(0),
            _ => None
        };
        let mut vi = vi.map(|v| &**v);
        while let Some(v) = vi {
            // debug!("type: {:?}", v.arg_type());
            if let Some(v) = v.as_str() {
                return f(Value::Borrow(v))
            }
            if let Some(v) = v.as_f64() {
                return f(Value::Float(v))
            }
            if let Some(v) = v.as_u64() {
                return f(Value::Float(v as _))
            }
            if let Some(v) = v.as_i64() {
                return f(Value::Float(v as _))
            }
            if let Some(mut iter) = v.as_iter() {
                // TODO handle dict keys?
                let n = keys.next().and_then(|k| k.parse().ok()).unwrap_or(0);
                vi = iter.nth(n);
            }
        }
        f(Value::Null)
    }
}
