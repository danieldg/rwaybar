use bytes::{Buf,BytesMut};
use crate::Variable;
use crate::data::Cell;
use crate::state::Runtime;
use log::{warn,error};
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;
use tokio::net::UnixStream;
use tokio::sync::Notify;

thread_local! {
    static SOCK : RefCell<Option<SwaySocket>> = RefCell::new(None);
}

#[derive(Default,Debug)]
struct ListenerResult {
    consumed : bool,
    remove_callback : bool,
}

struct SwaySocket {
    wbuf : Vec<u8>,
    notify : Rc<Notify>,

    subscribed : Vec<&'static str>,
    listeners : Vec<(u32, Box<dyn FnMut(&[u8]) -> ListenerResult>)>,
}

impl SwaySocket {
    fn init() -> Self {
        let write_notify = Rc::new(Notify::new());
        let notify = write_notify.clone();

        tokio::task::spawn_local(async move {
            let (mut rh, mut wh) = match match std::env::var_os("SWAYSOCK") {
                Some(path) => UnixStream::connect(path).await,
                None => {
                    error!("Could not connect to sway: no SWAYSOCK defined");
                    return;
                }
            } {
                Ok(sock) => sock.into_split(),
                Err(e) => {
                    error!("Could not connect to sway: {}", e);
                    return;
                }
            };

            tokio::task::spawn_local(async move {
                let mut wbuf = Vec::new();
                loop {
                    debug_assert!(wbuf.is_empty());
                    SOCK.with(|cell| {
                        let mut borrow = cell.borrow_mut();
                        let sock = borrow.as_mut().unwrap();
                        std::mem::swap(&mut wbuf, &mut sock.wbuf);
                    });

                    if !wbuf.is_empty() {
                        match tokio::io::AsyncWriteExt::write_all(&mut wh, &wbuf).await {
                            Ok(()) => wbuf.clear(),
                            Err(e) => {
                                error!("Error writing to SWAYSOCK: {}", e);
                                return;
                            }
                        }
                    }
                    write_notify.notified().await;
                }
            });

            let mut rbuf = BytesMut::with_capacity(1000);
            let hdr_len = 6+4+4;
            'read : loop {
                match tokio::io::AsyncReadExt::read_buf(&mut rh, &mut rbuf).await {
                    Ok(0) => {
                        error!("End of file when reading from SWAYSOCK");
                        return;
                    }
                    Err(e) => {
                        error!("Error reading from SWAYSOCK: {}", e);
                        return;
                    }
                    Ok(_) => {}
                }
                
                loop {
                    if rbuf.len() < hdr_len {
                        continue 'read;
                    }
                    debug_assert_eq!(&rbuf[..6], b"i3-ipc");
                    let len = u32::from_ne_bytes(rbuf[6..10].try_into().unwrap()) as usize;
                    let ptype = u32::from_ne_bytes(rbuf[10..hdr_len].try_into().unwrap());
                    if rbuf.capacity() < len + hdr_len {
                        rbuf.reserve(len + hdr_len - rbuf.len());
                    }
                    if rbuf.len() < len + hdr_len {
                        continue 'read;
                    }
                    let msg = &rbuf[hdr_len..][..len];

                    SOCK.with(|cell| {
                        let mut borrow = cell.borrow_mut();
                        let sock = borrow.as_mut().unwrap();
                        sock.on_read(ptype, msg);
                    });

                    rbuf.advance(hdr_len + len);
                }
            }
        });

        Self {
            wbuf : Vec::with_capacity(100),
            notify,
            subscribed : Vec::new(),
            listeners : Vec::new(),
        }
    }

    fn do_send_msg<F : FnMut(&[u8]) + 'static>(&mut self, id : u32, msg : &[u8], mut on_reply : F) {
        self.wbuf.extend_from_slice(b"i3-ipc");
        self.wbuf.extend_from_slice(&(msg.len() as u32).to_ne_bytes());
        self.wbuf.extend_from_slice(&id.to_ne_bytes());
        self.wbuf.extend_from_slice(&msg);

        self.listeners.push((id, Box::new(move |buf| {
            on_reply(buf);
            ListenerResult { consumed : true, remove_callback : true }
        })));

        self.notify.notify_one();
    }

    fn on_read(&mut self, ptype : u32, msg : &[u8]) {
        // Note: drain_filter would be nice here
        let mut i = 0;
        while i < self.listeners.len() {
            if self.listeners[i].0 == ptype {
                let rv = (self.listeners[i].1)(msg);
                if rv.remove_callback {
                    drop(self.listeners.remove(i));
                } else {
                    i += 1;
                }
                if rv.consumed {
                    break;
                }
            } else {
                i += 1;
            }
        }
    }

    pub fn send<F : FnMut(&[u8]) + 'static>(id : u32, msg : &[u8], callback : F) {
        SOCK.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let sock = borrow.get_or_insert_with(SwaySocket::init);
            sock.do_send_msg(id, msg, callback);
        })
    }

    pub fn subscribe(name : &'static str, id : u32, callback : Box<dyn FnMut(&[u8]) -> ListenerResult>) {
        SOCK.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let sock = borrow.get_or_insert_with(SwaySocket::init);
            if sock.subscribed.iter().find(|e| **e == name).is_none() {
                sock.do_send_msg(2,
                    format!(r#"[ "{}" ]"#, name).as_bytes(),
                    move |buf| {
                        match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                            Ok(Ok(value)) if value["success"].as_bool() == Some(true) => {
                                // great
                            }
                            Ok(Ok(value)) => {
                                error!("Could not subscribe to {}: {}", name, value);
                            }
                            Ok(Err(e)) => {
                                error!("Could not subscribe to {}: {}", name, e);
                            }
                            Err(e) => {
                                error!("Could not subscribe to {}: {}", name, e);
                            }
                        }
                    });
                sock.subscribed.push(name);
            }
            sock.listeners.push((id, callback));
        })
    }
}

#[derive(Debug,Default)]
pub struct Mode {
    value : Rc<Cell<String>>,
}

impl Variable for Mode {
    fn from_json(_config : &json::JsonValue) -> Option<Self> {
        Some(Mode::default())
    }

    fn init(&self, _name : &str, rt : &Runtime) {
        let value = self.value.clone();
        let notify = rt.notify.unbound();
        SwaySocket::subscribe("mode", 0x80000002, Box::new(move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    msg["change"].as_str().map(|mode| value.set(mode.to_owned()));
                    notify.notify_data();
                }
                _ => warn!("Ignoring invalid mode change message")
            }
            ListenerResult {
                remove_callback : Rc::strong_count(&value) == 1,
                consumed : false,
            }
        }));
        let value = self.value.clone();
        let notify = rt.notify.unbound();
        SwaySocket::send(12, b"", move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    msg["name"].as_str().map(|mode| value.set(mode.to_owned()));
                    notify.notify_data();
                }
                _ => warn!("Ignoring invalid get_binding_state reply")
            }
        });
    }

    fn read_in<F : FnOnce(&str) -> R,R>(&self, _name : &str, key : &str, _rt : &Runtime, f : F) -> R {
        self.value.take_in(|s| {
            match key {
                "" if s == "default" => f(""),
                "" => f(s),
                "raw" => f(s),
                _ => {
                    warn!("Unknown key in sway-mode");
                    f(s)
                }
            }
        })
    }
}

#[derive(Debug,Default)]
struct WorkspaceData {
    focus : String,
    list : Vec<String>,
}

#[derive(Debug,Default)]
pub struct Workspace {
    // TODO configuration to track all outputs vs one output
    value : Rc<Cell<Option<WorkspaceData>>>,
}

impl Workspace {
    pub fn read_focus_list<F : FnMut(&str, bool)>(&self, mut f : F) {
        self.value.take_in_some(|data| {
            for item in &data.list {
                let focus = *item == data.focus;
                f(&item, focus);
            }
        });
    }
}

impl Variable for Workspace {
    fn from_json(_config : &json::JsonValue) -> Option<Self> {
        Some(Workspace::default())
    }

    fn init(&self, _name : &str, rt : &Runtime) {
        let value = self.value.clone();
        let notify = rt.notify.unbound();
        SwaySocket::subscribe("workspace", 0x80000000, Box::new(move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    value.take_in_some(|data| {
                        match msg["change"].as_str() {
                            Some("focus") => {
                                data.focus = msg["current"]["name"].as_str().unwrap_or("").to_owned();
                            }
                            Some("init") => {
                                msg["current"]["name"].as_str().map(|v| data.list.push(v.to_owned()));
                            }
                            Some("empty") => {
                                msg["current"]["name"].as_str().map(|gone| {
                                    data.list.retain(|n| n != gone)
                                });
                            }
                            Some("rename") => {
                                let old = msg["old"]["name"].as_str();
                                let new = msg["current"]["name"].as_str();
                                if let (Some(old), Some(new)) = (old,new) {
                                    for name in &mut data.list {
                                        if *name == old {
                                            *name = new.to_owned();
                                        }
                                    }
                                }
                            }
                            // TODO move, if we track outputs
                            _ => {}
                        }
                    });
                    notify.notify_data();
                }
                _ => warn!("Ignoring invalid workspace change message")
            }
            ListenerResult {
                remove_callback : Rc::strong_count(&value) == 1,
                consumed : false,
            }
        }));

        let value = self.value.clone();
        let notify = rt.notify.unbound();
        SwaySocket::send(1, b"", move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    let mut data = WorkspaceData::default();
                    for workspace in msg.members() {
                        workspace["name"].as_str().map(|name| {
                            data.list.push(name.to_owned());
                            if workspace["focused"].as_bool() == Some(true) {
                                data.focus = name.to_owned();
                            }
                        });
                    }
                    value.set(Some(data));
                    notify.notify_data();
                }
                _ => warn!("Ignoring invalid get_workspaces reply")
            }
        });
    }

    fn read_in<F : FnOnce(&str) -> R,R>(&self, _name : &str, key : &str, _rt : &Runtime, f : F) -> R {
        self.value.take_in(|v| {
            if let Some(data) = v {
                match key {
                    "" | "focus" => f(&data.focus),
                    "list" => {
                        let w = data.list.join(" ");
                        f(&w)
                    }
                    _ => {
                        warn!("Unknown key in sway-workspace");
                        f("")
                    }
                }
            } else {
                f("")
            }
        })
    }

    fn write(&self, name : &str, key : &str, value : String, _rt : &Runtime) {
        match key {
            "switch" => SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, value).as_bytes(), |_| ()),
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }
}
