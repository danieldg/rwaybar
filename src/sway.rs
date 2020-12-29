use bytes::{Buf,BytesMut};
use crate::data::IterationItem;
use crate::state::Runtime;
use crate::state::NotifierList;
use crate::util::{Cell,spawn_noerr};
use log::{warn,error};
use std::cell::RefCell;
use std::cmp::Ordering;
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

        spawn_noerr(async move {
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

            spawn_noerr(async move {
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
    value : Rc<ModeInner>,
}

#[derive(Debug,Default)]
struct ModeInner {
    mode : Cell<String>,
    interested : Cell<NotifierList>,
}

impl Mode {
    pub fn from_toml(_config : &toml::Value) -> Option<Self> {
        Some(Mode::default())
    }

    pub fn init(&self, _name : &str, _rt : &Runtime) {
        let weak = Rc::downgrade(&self.value);
        SwaySocket::subscribe("mode", 0x80000002, Box::new(move |buf| {
            let remove_callback;
            if let Some(mi) = weak.upgrade() {
                match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        msg["change"].as_str().map(|mode| mi.mode.set(mode.to_owned()));
                        mi.interested.take().notify_data();
                    }
                    _ => warn!("Ignoring invalid mode change message")
                }
                remove_callback = false;
            } else {
                remove_callback = true;
            }
            ListenerResult {
                remove_callback,
                consumed : false,
            }
        }));
        let value = self.value.clone();
        SwaySocket::send(12, b"", move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    msg["name"].as_str().map(|mode| value.mode.set(mode.to_owned()));
                    value.interested.take().notify_data();
                }
                _ => warn!("Ignoring invalid get_binding_state reply")
            }
        });
    }

    pub fn read_in<F : FnOnce(&str) -> R,R>(&self, _name : &str, key : &str, rt : &Runtime, f : F) -> R {
        self.value.interested.take_in(|i| i.add(rt));
        self.value.mode.take_in(|s| {
            match key {
                "" | "text" if s == "default" => f(""),
                "" | "text" => f(s),
                "raw" => f(s),
                "tooltip" => f(""),
                _ => {
                    warn!("Unknown key in sway-mode");
                    f(s)
                }
            }
        })
    }
}

#[derive(Debug,Clone)]
pub struct WorkspaceData {
    name : String,
    output : String,
    repr : String,
}

impl WorkspaceData {
    pub fn read_in<F : FnOnce(&str) -> R,R>(&self, key : &str, _rt : &Runtime, f : F) -> R {
        match key {
            "name" | "text" | "" => {
                f(&self.name)
            }
            "output" => {
                f(&self.output)
            }
            "repr" | "tooltip" => {
                f(&self.repr)
            }
            _ => f("")
        }
    }

    pub fn write(&self, key : &str, value : String, _rt : &Runtime) {
        match key {
            "switch" => SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, value).as_bytes(), |_| ()),
            "" if value == "switch" => {
                SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, self.name).as_bytes(), |_| ());
            }
            _ => {
                error!("Ignoring write to item.{}", key);
            }
        }
    }
}

fn sway_sort_fn(a : &WorkspaceData, b : &WorkspaceData) -> Ordering {
    let mut a = a.name.as_str();
    let mut b = b.name.as_str();
    let a1 = a.chars().next().as_ref().map(char::is_ascii_digit);
    let b1 = b.chars().next().as_ref().map(char::is_ascii_digit);
    match (a1, b1) {
        (Some(true), Some(true)) => {
            if let Some(p) = a.find(|c : char| !c.is_ascii_digit()) {
                a = &a[..p];
            }
            if let Some(p) = b.find(|c : char| !c.is_ascii_digit()) {
                b = &b[..p];
            }
            let ai = a.parse::<u64>().ok();
            let bi = b.parse::<u64>().ok();
            ai.cmp(&bi)
        }
        (Some(true), _) => Ordering::Less,
        (_, Some(true)) => Ordering::Greater,
        _ => Ordering::Equal,
    }
}

#[derive(Debug,Default)]
struct WorkspacesData {
    focus : Cell<String>,
    list : Cell<Vec<WorkspaceData>>,
    interested : Cell<NotifierList>,
}

#[derive(Debug,Default)]
pub struct Workspace {
    value : Rc<WorkspacesData>,
}

impl WorkspacesData {
    fn parse_update(&self, msg : json::JsonValue) {
        match msg["change"].as_str() {
            Some("focus") => {
                if let Some(name) = msg["current"]["name"].as_str() {
                    self.focus.set(name.to_owned());
                    if let Some(repr) = msg["current"]["representation"].as_str() {
                        self.list.take_in(|list| {
                            for wks in list {
                                if wks.name == name {
                                    wks.repr = repr.to_owned();
                                    return;
                                }
                            }
                        });
                    }
                }
            }
            Some("init") => {
                let new = WorkspaceData {
                    name : match msg["current"]["name"].as_str() {
                        Some(n) => n.to_owned(),
                        None => return,
                    },
                    output : msg["current"]["output"].as_str().unwrap_or("").to_owned(),
                    repr : msg["current"]["representation"].as_str().unwrap_or("").to_owned(),
                };
                // Note: sway will sometimes send duplicate "init" messages
                self.list.take_in(|list| {
                    for wks in &mut *list {
                        if wks.name == new.name {
                            *wks = new;
                            return;
                        }
                    }
                    list.push(new);
                    list.sort_by(sway_sort_fn);
                });
            }
            Some("empty") => {
                msg["current"]["name"].as_str().map(|gone| {
                    self.list.take_in(|list| list.retain(|wks| wks.name != gone))
                });
            }
            Some("rename") => {
                let old = msg["old"]["name"].as_str();
                let new = msg["current"]["name"].as_str();
                if let (Some(old), Some(new)) = (old,new) {
                    self.list.take_in(|list| {
                        for wks in &mut *list {
                            if wks.name == old {
                                wks.name = new.to_owned();
                            }
                        }
                        list.sort_by(sway_sort_fn);
                    })
                }
            }
            Some("move") => {
                let name = msg["current"]["name"].as_str();
                let output = msg["current"]["output"].as_str();
                if let (Some(name), Some(output)) = (name,output) {
                    self.list.take_in(|list| {
                        for wks in list {
                            if wks.name == name {
                                wks.output = output.to_owned();
                            }
                        }
                    });
                }
            }
            _ => {}
        }
    }
}

impl Workspace {
    pub fn from_toml(_config : &toml::Value) -> Option<Self> {
        Some(Workspace::default())
    }

    pub fn init(&self, _name : &str, _rt : &Runtime) {
        let weak = Rc::downgrade(&self.value);
        SwaySocket::subscribe("workspace", 0x80000000, Box::new(move |buf| {
            let remove_callback;
            if let Some(value) = weak.upgrade() {
                match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        value.parse_update(msg);
                        value.interested.take().notify_data();
                    }
                    _ => warn!("Ignoring invalid workspace change message")
                }
                remove_callback = false;
            } else {
                remove_callback = true;
            }
            ListenerResult {
                remove_callback,
                consumed : false,
            }
        }));

        let value = self.value.clone();
        SwaySocket::send(1, b"", move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    let mut list = Vec::new();
                    for workspace in msg.members() {
                        let new = WorkspaceData {
                            name : workspace["name"].as_str().unwrap_or("").to_owned(),
                            output : workspace["output"].as_str().unwrap_or("").to_owned(),
                            repr : workspace["representation"].as_str().unwrap_or("").to_owned(),
                        };
                        if workspace["focused"].as_bool() == Some(true) {
                            value.focus.set(new.name.clone());
                        }
                        list.push(new);
                    }
                    value.list.set(list);
                    value.interested.take().notify_data();
                }
                _ => warn!("Ignoring invalid get_workspaces reply")
            }
        });
    }

    pub fn read_in<F : FnOnce(&str) -> R,R>(&self, _name : &str, key : &str, rt : &Runtime, f : F) -> R {
        self.value.interested.take_in(|i| i.add(rt));
        match key {
            "text" | "focus" => self.value.focus.take_in(|focus| f(&focus)),
            "tooltip" => f(""),
            _ => {
                warn!("Unknown key in sway-workspace");
                f("")
            }
        }
    }

    pub fn read_focus_list<F : FnMut(bool, Rc<IterationItem>)>(&self, rt : &Runtime, mut f : F) {
        self.value.interested.take_in(|i| i.add(rt));
        let focus = self.value.focus.take_in(|f| f.clone());
        self.value.list.take_in(|list| {
            for item in &*list {
                let focus = item.name == focus;
                f(focus, Rc::new(IterationItem::SwayWorkspace(item.clone())));
            }
        });
    }

    pub fn write(&self, name : &str, key : &str, value : String, _rt : &Runtime) {
        match key {
            "switch" => SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, value).as_bytes(), |_| ()),
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }
}
