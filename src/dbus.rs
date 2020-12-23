use crate::util;
use dbus::arg::{RefArg,Variant};
use dbus::channel::{BusType,Channel,MatchingReceiver};
use dbus::message::{MatchRule,Message};
use dbus::nonblock::{LocalConnection,Process,NonblockReply};
use dbus::nonblock::stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged;
use futures_util::future::Either;
use futures_util::future::select;
use futures_util::pin_mut;
use log::{warn,error};
use once_cell::unsync::OnceCell;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::rc::Rc;
use tokio::io::unix::AsyncFd;
use tokio::sync::Notify;

thread_local! {
    static SOCK : Rc<OnceCell<SessionDBus>> = Default::default();
}

pub struct SessionDBus {
    pub local : LocalConnection,
    prop_watchers : util::Cell<Vec<Box<dyn FnMut(&Message, &PropertiesPropertiesChanged, &SessionDBus)>>>,
    name_watchers : util::Cell<Vec<Box<dyn FnMut(&str, &str, &str, &SessionDBus)>>>,
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

    rc.set(SessionDBus {
        local,
        wake,
        prop_watchers : Default::default(),
        name_watchers : Default::default(),
    }).ok().expect("Called init twice");

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

    pub async fn add_property_change_watcher<F>(&self, f : F)
        where F : FnMut(&Message, &PropertiesPropertiesChanged, &Self) + 'static
    {
        if self.prop_watchers.take_in(|w| {
            w.push(Box::new(f));
            w.len() == 1
        }) {
            let prop_rule = MatchRule::new_signal("org.freedesktop.DBus.Properties", "PropertiesChanged");
            let rule_str = prop_rule.match_str();
            self.local.start_receive(prop_rule, Box::new(move |msg, _local| {
                let this = Self::get();
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
                true
            }));

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
            self.local.start_receive(na_rule, Box::new(move |msg, _local| {
                let this = Self::get();
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
                true
            }));
            match self.local.add_match_no_cb(&rule_str).await {
                Ok(()) => {}
                Err(e) => warn!("Could not register for NameAcquired messages: {}", e),
            }
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
