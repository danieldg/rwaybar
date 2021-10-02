use crate::data::Value;
use crate::state::Runtime;
use crate::state::NotifierList;
use crate::util;
use crate::util::{Cell,spawn_noerr};
use futures_channel::mpsc::{self,UnboundedSender};
use futures_util::future::RemoteHandle;
use futures_util::StreamExt;
use log::{info,warn,error};
use once_cell::unsync::OnceCell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::os::unix::prelude::RawFd;
use std::pin::Pin;
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::Arc;
use std::task;
use tokio::net::UnixStream;
use zbus::{Connection,ConnectionBuilder,MessageStream};
use zbus::names::BusName;
use zbus::zvariant;
use zvariant::OwnedValue;
use zvariant::Value as Variant;

pub struct DBus {
    send : UnboundedSender<zbus::Message>,

    bus: util::Cell<Result<Connection, Vec<task::Waker>>>,

    sig_watchers : util::Cell<Vec<Box<dyn SignalWatcherCall>>>,
    prop_watchers : util::Cell<Vec<Box<dyn FnMut(&zbus::MessageHeader, &str, &HashMap<&str, OwnedValue>, &[&str])>>>,
    name_watchers : util::Cell<Vec<Box<dyn FnMut(&BusName, &str, &str)>>>,
}

impl fmt::Debug for DBus {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "DBus")
    }
}

thread_local! {
    static SESSION : OnceCell<Rc<DBus>> = Default::default();
    static SYSTEM : OnceCell<Rc<DBus>> = Default::default();
}

impl DBus {
    pub fn get_session() -> Rc<Self> {
        SESSION.with(|s| {
            s.get_or_init(|| {
                Self::new(true)
            }).clone()
        })
    }

    pub fn get_system() -> Rc<Self> {
        SYSTEM.with(|s| {
            s.get_or_init(|| {
                Self::new(false)
            }).clone()
        })
    }

    pub async fn connection(&self) -> Connection {
        futures_util::future::poll_fn(|ctx| {
            match self.bus.replace(Err(Vec::new())) {
                Err(mut wakers) => {
                    let me = ctx.waker();
                    if !wakers.iter().any(|w| w.will_wake(me)) {
                        wakers.push(me.clone());
                    }
                    self.bus.set(Err(wakers));
                    task::Poll::Pending
                }
                Ok(conn) => {
                    let rv = conn.clone();
                    self.bus.set(Ok(conn));
                    task::Poll::Ready(rv)
                }
            }
        }).await
    }

    fn new(is_session : bool) -> Rc<Self> {
        let (send, mut recv) = mpsc::unbounded();
        let tb = Rc::new(DBus {
            send,
            bus : Cell::new(Err(Vec::new())),
            sig_watchers : Default::default(),
            prop_watchers : Default::default(),
            name_watchers : Default::default(),
        });

        // Note: reference cycles don't matter, the DBus object is not freeable
        let this = tb.clone();
        tb.add_signal_watcher(move |_path, iface, memb, msg| {
            match (|| -> zbus::Result<()> {
                if iface != "org.freedesktop.DBus.Properties" ||
                    memb != "PropertiesChanged"
                {
                    return Ok(());
                }
                let hdr = msg.header()?;
                let (iface, changed, inval) : (&str, HashMap<&str, OwnedValue>, Vec<&str>) = msg.body()?;
                let mut watchers = this.prop_watchers.replace(Vec::new());
                for watcher in &mut watchers {
                    (*watcher)(&hdr, iface, &changed, &inval);
                }
                this.prop_watchers.take_in(|w| {
                    if w.is_empty() {
                        *w = watchers;
                    } else {
                        w.extend(watchers);
                    }
                });
                Ok(())
            })() {
                Ok(()) => {}
                Err(e) => warn!("Could not parse PropertiesPropertiesChanged message: {}", e),
            }
        });

        let this = tb.clone();
        tb.add_signal_watcher(move |_path, iface, memb, msg| {
            match (|| -> zbus::Result<()> {
                if iface != "org.freedesktop.DBus" || memb != "NameOwnerChanged" {
                    return Ok(());
                }
                let (name, old, new) = msg.body()?;
                let mut watchers = this.name_watchers.replace(Vec::new());
                for watcher in &mut watchers {
                    (*watcher)(&name, old, new);
                }
                this.name_watchers.take_in(|w| {
                    if w.is_empty() {
                        *w = watchers;
                    } else {
                        w.extend(watchers);
                    }
                });
                Ok(())
            })() {
                Ok(()) => {}
                Err(e) => warn!("Could not parse: {}", e),
            }
        });

        let this = tb.clone();
        util::spawn("DBus Sender", async move {
            use zbus::Address;
            let addr = if is_session {
                Address::session()?
            } else {
                Address::system()?
            };

            let zbus = match addr {
                Address::Unix(s) => {
                    let stream = AsSocket(UnixStream::connect(s).await?);
                    ConnectionBuilder::socket(stream)
                        .internal_executor(false)
                        .build().await?
                }
            };
            match this.bus.replace(Ok(zbus.clone())) {
                Ok(_) => unreachable!(),
                Err(wakers) => {
                    for waker in wakers {
                        waker.wake();
                    }
                }
            }
            util::spawn("Incoming DBus Events", this.clone().dispatcher((&zbus).into()));

            let rv = zbus.executor().run(async {
                while let Some(msg) = recv.next().await {
                    zbus.send_message(msg).await?;
                }
                Ok(())
            }).await;
            error!("Unexpected termination of D-Bus output stream");
            rv
        });

        tb
    }

    pub fn send(&self, msg : zbus::Message) {
        let _ = self.send.unbounded_send(msg);
    }

    pub fn add_signal_watcher<F>(&self, f : F)
        where F : FnMut(&zvariant::ObjectPath, &str, &str, &zbus::Message) + 'static
    {
        if std::mem::size_of::<F>() != 0 {
            self.do_add_signal_watcher(Box::new(SignalWatcherNZ(f)))
        } else {
            self.do_add_signal_watcher(Box::new(SignalWatcherZST(0, f)))
        }
    }

    fn do_add_signal_watcher(&self, b : Box<dyn SignalWatcherCall>)
    {
        self.sig_watchers.take_in(|w| w.push(b));
    }

    pub fn add_property_change_watcher<F>(&self, f : F)
        where F : FnMut(&zbus::MessageHeader, &str, &HashMap<&str, OwnedValue>, &[&str]) + 'static
    {
        if self.prop_watchers.take_in(|w| {
            w.push(Box::new(f));
            w.len() == 1
        }) {
            self.send(zbus::Message::method(
                None::<&str>,
                Some("org.freedesktop.DBus"),
                "/org/freedesktop/DBus",
                Some("org.freedesktop.DBus"),
                "AddMatch",
                &("type='signal',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'")
            ).unwrap());
        }
    }

    pub fn add_name_watcher<F>(&self, f : F)
        where F : FnMut(&BusName, &str, &str) + 'static
    {
        if self.name_watchers.take_in(|w| {
            w.push(Box::new(f));
            w.len() == 1
        }) {
            self.send(zbus::Message::method(
                None::<&str>,
                Some("org.freedesktop.DBus"),
                "/org/freedesktop/DBus",
                Some("org.freedesktop.DBus"),
                "AddMatch",
                &("type='signal',interface='org.freedesktop.DBus',member='NameOwnerChanged'")
            ).unwrap());
        }
    }

    async fn dispatcher(self : Rc<Self>, mut zbus : MessageStream) -> Result<(), Box<dyn std::error::Error>>  {
        while let Some(msg) = zbus.next().await {
            self.dispatch(msg?)?;
        }
        error!("Unexpected end of D-Bus message stream");
        Ok(())
    }

    fn dispatch(&self, msg : Arc<zbus::Message>) -> zbus::Result<()> {
        use zbus::MessageType;
        match msg.message_type() {
            MessageType::Signal => {
                let mut watchers = self.sig_watchers.replace(Vec::new());
                match (msg.path()?, msg.interface()?, msg.member()?) {
                    (Some(path), Some(iface), Some(memb)) => {
                        for watcher in &mut watchers {
                            watcher.call(&path, &iface, &memb, &msg);
                        }
                    }
                    _ => {
                        log::debug!("Ignoring invalid dbus signal");
                    }
                }
                self.sig_watchers.take_in(|w| {
                    if w.is_empty() {
                        *w = watchers;
                    } else {
                        w.extend(watchers);
                    }
                });
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug)]
struct AsSocket(UnixStream);

impl zbus::Socket for AsSocket {
    fn poll_recvmsg(&mut self, cx: &mut task::Context<'_>, buf: &mut [u8]) -> task::Poll<io::Result<(usize, Vec<zvariant::OwnedFd>)>> {
        use tokio::io::AsyncRead;
        //use futures::ready;
        //ready!(self.0.poll_read_ready(cx)?);
        let mut buf = tokio::io::ReadBuf::new(buf);
        let rv = Pin::new(&mut self.0).poll_read(cx, &mut buf);
        rv.map_ok(|()| (buf.filled().len(), vec![]))
    }
    fn poll_sendmsg(&mut self, cx: &mut task::Context<'_>, buffer: &[u8], fds: &[RawFd]) -> task::Poll<io::Result<usize>> {
        use tokio::io::AsyncWrite;
        if !fds.is_empty() {
            // I don't use fds.  Makes this impl much easier.
            Err(io::ErrorKind::InvalidData)?;
        }
        Pin::new(&mut self.0).poll_write(cx, buffer)
    }
    fn close(&self) -> io::Result<()>{ Ok(()) }
    fn as_raw_fd(&self) -> RawFd {
        std::os::unix::io::AsRawFd::as_raw_fd(&self.0)
    }
}

struct SignalWatcherNZ<F : ?Sized>(F);
struct SignalWatcherZST<F : ?Sized>(u8, F);

trait SignalWatcherCall {
    fn call(&mut self, path : &zvariant::ObjectPath, iface : &str, memb: &str, msg: &zbus::Message);
    fn get_sw_ptr(&self) -> Option<NonNull<()>>;
}

impl<F> SignalWatcherCall for SignalWatcherNZ<F>
    where F : FnMut(&zvariant::ObjectPath, &str, &str, &zbus::Message) + 'static
{
    fn call(&mut self, path : &zvariant::ObjectPath, iface : &str, memb: &str, msg: &zbus::Message) {
        (self.0)(path, iface, memb, msg)
    }
    fn get_sw_ptr(&self) -> Option<NonNull<()>> {
        NonNull::new(self as *const SignalWatcherNZ<F> as *mut ())
    }
}

impl<F> SignalWatcherCall for SignalWatcherZST<F>
    where F : FnMut(&zvariant::ObjectPath, &str, &str, &zbus::Message) + 'static
{
    fn call(&mut self, path : &zvariant::ObjectPath, iface : &str, memb: &str, msg: &zbus::Message) {
        (self.1)(path, iface, memb, msg)
    }
    fn get_sw_ptr(&self) -> Option<NonNull<()>> {
        NonNull::new(self as *const SignalWatcherZST<F> as *mut ())
    }
}

/// The "dbus" block
#[derive(Debug)]
pub struct DbusValue {
    bus : Rc<DBus>,
    bus_name : Box<str>,
    path : Box<str>,
    interface : Box<str>,
    member : Box<str>,
    args : Box<[toml::Value]>,
    sig : Cell<Option<Rc<str>>>,
    value : RefCell<Option<OwnedValue>>,
    interested : Cell<NotifierList>,
    watch : Cell<Option<RemoteHandle<()>>>,
}

impl DbusValue {
    pub fn from_toml(value : &toml::Value) -> Option<Rc<Self>> {
        let dbus = match value.get("bus").and_then(|v| v.as_str()) {
            None | Some("session") => DBus::get_session(),
            Some("system") => DBus::get_system(),
            _ => {
                error!("bus must be either 'session' or 'system'");
                return None;
            }
        };
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
            bus: dbus.clone(),
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
                let mut expr = format!("type='signal',interface='{}',member='{}'", i, m);
                if let Some(path) = &watch_path {
                    expr.push_str(",path='");
                    expr.push_str(path);
                    expr.push_str("'");
                }
                dbus.send(zbus::Message::method(
                    None::<&str>,
                    Some("org.freedesktop.DBus"),
                    "/org/freedesktop/DBus",
                    Some("org.freedesktop.DBus"),
                    "AddMatch",
                    &expr,
                ).unwrap());

                let watch_path : Option<Box<str>> = watch_path.map(Into::into);
                let watch_method = watch_method.unwrap().to_owned();
                let weak = Rc::downgrade(&rc);
                let h = util::spawn_handle("DBus value watcher", async move {
                    let zbus = dbus.connection().await;
                    let mut stream = zbus::MessageStream::from(zbus);
                    while let Some(Ok(msg)) = stream.next().await {
                        if let Some(rc) = weak.upgrade() {
                            let (i,m) = watch_method.rsplit_once(".").unwrap();
                            if msg.interface()?.as_deref() != Some(i) ||
                                msg.member()?.as_deref() != Some(m)
                            {
                                continue;
                            }
                            if let Some(path) = &watch_path {
                                if msg.path()?.as_deref() != Some(&*path) {
                                    continue;
                                }
                            }
                            rc.call_now();
                        }
                    }
                    Ok(())
                });
                rc.watch.set(Some(h));
            }
            Some(None) if watch_method == Some("") => {}
            Some(None) => error!("Invalid dbus watch expression, ignoring"),
            None if property.is_some() => {
                let prop = property.unwrap_or_default().to_owned();
                let weak = Rc::downgrade(&rc);
                rc.bus.add_property_change_watcher(move |hdr, iface, change, invalid| {
                    if let (Some(rc), Some((target_iface, prop))) = (weak.upgrade(), prop.rsplit_once(".")) {
                        if iface != target_iface || hdr.path().ok().flatten().map(|p| p.as_str()) != Some(&*rc.path) {
                            return;
                        }
                        if let Some(value) = change.get(prop) {
                            // match the Get return type
                            let v = zvariant::StructureBuilder::new()
                                .add_field(value.clone())
                                .build()
                                .into();
                            *rc.value.borrow_mut() = Some(v);
                        } else if invalid.iter().any(|p| p == &prop) {
                            *rc.value.borrow_mut() = None;
                        }
                    }
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
        match self.try_call().await {
            Ok(()) => (),
            Err(e) => log::debug!("DBus error: {}", e),
        }
    }

    async fn try_call(self : Rc<Self>) -> zbus::Result<()> {
        use toml::value::Value;
        let dbus = &*self.bus;
        let zbus = dbus.connection().await;

        let mut api = self.sig.take_in(|s| s.clone());

        if api.is_none() {
            let msg = zbus.call_method(
                Some(&*self.bus_name),
                &*self.path,
                Some("org.freedesktop.DBus.Introspectable"),
                "Introspect",
                &(),
            ).await;
            match msg.as_ref().map(|m| m.body()) {
                Ok(Ok(xml)) => {
                    let mut reader = xml::EventReader::from_str(xml);
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
                            Ok(XmlEvent::EndDocument) => {
                                sig.pop();
                                api = Some(sig.into());
                                self.sig.set(api.clone());
                                break;
                            }
                            Ok(_) => {}
                            Err(e) => {
                                info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                                break;
                            }
                        }
                    }
                }
                Ok(Err(e)) => {
                    info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                }
                Err(e) => {
                    info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                }
            }
        }

        let mut args = zvariant::StructureBuilder::new();
        if let Some(sig) = api {
            for (ty,arg) in sig.split(",").zip(self.args.iter()) {
                match match ty {
                    "s" => { arg.as_str().map(|s| args.push_field(s)) }
                    "d" => {
                        arg.as_float()
                            .or_else(|| arg.as_integer().map(|i| i as f64))
                            .map(|f| args.push_field(f))
                    }
                    "y" => { arg.as_integer().map(|i| i as u8 ).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "n" => { arg.as_integer().map(|i| i as i16).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "q" => { arg.as_integer().map(|i| i as u16).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "i" => { arg.as_integer().map(|i| i as i32).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "u" => { arg.as_integer().map(|i| i as u32).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "x" => { arg.as_integer().map(|i| i as i64).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "t" => { arg.as_integer().map(|i| i as u64).or_else(|| arg.as_float().map(|f| f as _)).map(|i| args.push_field(i)) }
                    "b" => {
                        arg.as_bool()
                            .or_else(|| arg.as_integer().map(|i| i != 0))
                            .or_else(|| arg.as_float().map(|i| i != 0.0))
                            .map(|b| args.push_field(b))
                    }
                    "v" => match arg {
                        // best guess as to the type of a variant parameter
                        Value::String(s) => Some(args.push_field(Variant::from(s))),
                        Value::Integer(i) => Some(args.push_field(Variant::from(i))),
                        Value::Float(f) => Some(args.push_field(Variant::from(f))),
                        Value::Boolean(b) => Some(args.push_field(Variant::from(b))),
                        _ => None,
                    }
                    _ => None,
                } {
                    Some(()) => {}
                    None => {
                        info!("Unsupported or mismatching dbus argument: expected type '{}' got '{}'", ty, arg);
                    }
                }
            }
        } else {
            // no introspection, we just have to guess the argument types
            for arg in self.args.iter() {
                match arg {
                    Value::String(s) => args.push_field(s),
                    Value::Integer(i) => args.push_field(i),
                    Value::Float(f) => args.push_field(f),
                    Value::Boolean(b) => args.push_field(b),
                    _ => {
                        info!("Invalid dbus argument '{}'", arg);
                    }
                }
            }
        }
        let args = args.build();
        let reply = zbus.call_method(
            Some(&*self.bus_name),
            &*self.path,
            Some(&*self.interface),
            &*self.member,
            &args,
        ).await?;

        *self.value.borrow_mut() = Some(Variant::Structure(reply.body()?).into());

        self.interested.take().notify_data("dbus-read");
        Ok(())
    }

    fn read_variant<'a, F : FnOnce(Value) -> R, R>(value : &Variant, mut keys : impl Iterator<Item=&'a str>, rt : &Runtime, f : F) -> R {
        match value {
            Variant::Bool(b) => f(Value::Bool(*b)),
            Variant::U8(v) => f(Value::Float(*v as _)),
            Variant::I16(v) => f(Value::Float(*v as _)),
            Variant::U16(v) => f(Value::Float(*v as _)),
            Variant::I32(v) => f(Value::Float(*v as _)),
            Variant::U32(v) => f(Value::Float(*v as _)),
            Variant::I64(v) => f(Value::Float(*v as _)),
            Variant::U64(v) => f(Value::Float(*v as _)),
            Variant::F64(v) => f(Value::Float(*v)),
            Variant::Str(s) => f(Value::Borrow(s)),
            Variant::Signature(s) => f(Value::Borrow(s)),
            Variant::ObjectPath(s) => f(Value::Borrow(s)),
            Variant::Value(v) => Self::read_variant(v, keys, rt, f),
            Variant::Array(a) => {
                match keys.next()
                    .unwrap_or("0")
                    .parse::<usize>()
                    .ok()
                    .and_then(|i| a.get().get(i))
                {
                    Some(v) => Self::read_variant(v, keys, rt, f),
                    None => f(Value::Null),
                }
            }
            Variant::Dict(d) => {
                let key = match keys.next() {
                    Some(k) => k,
                    None => return f(Value::Null),
                };
                // sig is "a{sv}" or "a{oa...}"
                let sig = d.full_signature().as_bytes();
                let v = match sig.get(2) {
                    Some(b's') => d.get(key),
                    Some(b'o') => d.get(&zvariant::ObjectPath::from_str_unchecked(key)),
                    Some(b'g') => d.get(&zvariant::Signature::from_str_unchecked(key)),
                    _ => {
                        info!("Unsupported dict key in type: '{}'", d.full_signature().as_str());
                        return f(Value::Null);
                    }
                };
                match v.ok().flatten() {
                    Some(v) => Self::read_variant(v, keys, rt, f),
                    None => f(Value::Null),
                }
            }
            Variant::Structure(s) => {
                let i = keys.next()
                    .unwrap_or("0")
                    .parse::<usize>()
                    .ok()
                    .unwrap_or(0);
                match s.fields().get(i) {
                    Some(v) => Self::read_variant(v, keys, rt, f),
                    None => f(Value::Null),
                }
            }
            Variant::Fd(_) => f(Value::Null),
        }
    }

    pub fn read_in<F : FnOnce(Value) -> R, R>(&self, key : &str, rt : &Runtime, f : F) -> R {
        self.interested.take_in(|i| i.add(rt));
        let value = self.value.borrow();
        match value.as_deref() {
            Some(value) => Self::read_variant(value, key.split("."), rt, f),
            None => f(Value::Null),
        }
    }
}
