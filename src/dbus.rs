use crate::util;
use dbus::arg::{RefArg,Variant};
use dbus::channel::{BusType,Channel,MatchingReceiver};
use dbus::message::{MatchRule,Message,MessageType};
use dbus::nonblock::{LocalConnection,Process,NonblockReply};
use dbus::nonblock::stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged;
use futures_util::future::Either;
use futures_util::future::select;
use futures_util::pin_mut;
use log::{warn,error};
use once_cell::unsync::OnceCell;
use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::rc::Rc;
use std::ptr::NonNull;
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

pub fn read_hash_map(value : &impl RefArg) -> Option<HashMap<String, Variant<Box<dyn RefArg>>>> {
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
