use crate::data::Cell;
use crate::Variable;
use crate::state::{State,Runtime};
use std::convert::TryInto;
use std::cell::RefCell;
use std::io::{self,Read,Write};
use std::os::unix::net::UnixStream;
use std::os::unix::io::AsRawFd;
use std::rc::Rc;
use log::{warn,error};

thread_local! {
    static SOCK : RefCell<Option<SwaySocket>> = RefCell::new(None);
}

#[derive(Default,Debug)]
struct ListenerResult {
    consumed : bool,
    remove_callback : bool,
}

struct SwaySocket {
    sock : Option<UnixStream>,
    rbuf : Vec<u8>,
    wbuf : Vec<u8>,

    subscribed : Vec<&'static str>,
    listeners : Vec<(u32, Box<dyn FnMut(&mut State, &[u8]) -> ListenerResult>)>,
    src_handle : Option<calloop::Source<calloop::generic::Generic<calloop::generic::Fd>>>,
}

impl SwaySocket {
    fn init(rt : &Runtime) -> Self {
        let sock = std::env::var_os("SWAYSOCK").and_then(|path| UnixStream::connect(path).ok());
        let mut src_handle = None;
        if let Some(sock) = sock.as_ref() {
            let fd = sock.as_raw_fd();
            sock.set_nonblocking(true).unwrap();
            let src = calloop::generic::Generic::from_fd(fd, calloop::Interest::Both, calloop::Mode::Edge);
            src_handle = rt.eloop.insert_source(src, move |how, _fd, state| {
                SOCK.with(|cell| {
                    let mut borrow = cell.borrow_mut();
                    let sock = borrow.as_mut().unwrap();
                    if how.readable {
                        sock.do_read(state);
                    }
                    if how.writable {
                        sock.do_write();
                    }
                    if how.error {
                        sock.sock.take();
                        sock.src_handle.take().map(|v| state.runtime.eloop.kill(v));
                    }
                });
                Ok(())
            }).ok();
        } else {
            error!("Could not connect to sway, check SWAYSOCK");
        }
        Self {
            sock,
            src_handle,
            rbuf : Vec::with_capacity(1000),
            wbuf : Vec::with_capacity(100),
            subscribed : Vec::new(),
            listeners : Vec::new(),
        }
    }

    fn do_send_msg<F : FnMut(&mut State, &[u8]) + 'static>(&mut self, id : u32, msg : &[u8], mut on_reply : F) {
        self.wbuf.extend_from_slice(b"i3-ipc");
        self.wbuf.extend_from_slice(&(msg.len() as u32).to_ne_bytes());
        self.wbuf.extend_from_slice(&id.to_ne_bytes());
        self.wbuf.extend_from_slice(&msg);

        self.listeners.push((id, Box::new(move |state, buf| {
            on_reply(state, buf);
            ListenerResult { consumed : true, remove_callback : true }
        })));

        self.do_write();
    }

    fn do_read(&mut self, state : &mut State) {
        let hdr_len = 6+4+4;
        debug_assert!(self.rbuf.capacity() > hdr_len);
        while let Some(sock) = &mut self.sock {
            while self.rbuf.len() >= hdr_len {
                debug_assert_eq!(&self.rbuf[..6], b"i3-ipc");
                let len = u32::from_ne_bytes(self.rbuf[6..10].try_into().unwrap()) as usize;
                if self.rbuf.capacity() < len + hdr_len {
                    self.rbuf.reserve(len + hdr_len - self.rbuf.len());
                }
                if self.rbuf.len() < len + hdr_len {
                    break;
                }
                let ptype = u32::from_ne_bytes(self.rbuf[10..hdr_len].try_into().unwrap());
                let msg = &self.rbuf[hdr_len..][..len];
                // Note: drain_filter would be nice here
                let mut i = 0;
                while i < self.listeners.len() {
                    if self.listeners[i].0 == ptype {
                        let rv = (self.listeners[i].1)(state, msg);
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
                self.rbuf.drain(..(len + hdr_len));
            }
            unsafe {
                let pos = self.rbuf.len();
                self.rbuf.set_len(self.rbuf.capacity());
                match sock.read(&mut self.rbuf[pos..]) {
                    Ok(0) => {
                        self.rbuf.set_len(pos);
                        error!("Sway socket end-of-file");
                        self.sock = None;
                        return;
                    }
                    Ok(n) => {
                        self.rbuf.set_len(pos + n);
                    }
                    Err(e) => {
                        self.rbuf.set_len(pos);
                        match e.kind() {
                            io::ErrorKind::WouldBlock => return,
                            io::ErrorKind::Interrupted => continue,
                            _ => {
                                error!("Sway socket read failed: {}", e);
                                self.sock = None;
                                return;
                            }
                        }
                    }
                }
            }
        }
    }

    fn do_write(&mut self) {
        let wbuf = &mut self.wbuf;
        while !wbuf.is_empty() {
            match self.sock.as_mut().map(|s| s.write(&wbuf)) {
                None => return,
                Some(Ok(n)) => { wbuf.drain(..n); }
                Some(Err(e)) if e.kind() == io::ErrorKind::WouldBlock => return,
                Some(Err(e)) if e.kind() == io::ErrorKind::Interrupted => continue,
                Some(Err(e)) => {
                    error!("Sway socket write failed: {}", e);
                    self.sock.take();
                    return;
                }
            }
        }
    }

    pub fn send<F : FnMut(&mut State, &[u8]) + 'static>(rt : &Runtime, id : u32, msg : &[u8], callback : F) {
        SOCK.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let sock = borrow.get_or_insert_with(|| SwaySocket::init(rt));
            sock.do_send_msg(id, msg, callback);
        })
    }

    pub fn subscribe(rt : &Runtime, name : &'static str, id : u32, callback : Box<dyn FnMut(&mut State, &[u8]) -> ListenerResult>) {
        SOCK.with(|cell| {
            let mut borrow = cell.borrow_mut();
            let sock = borrow.get_or_insert_with(|| SwaySocket::init(rt));
            if sock.subscribed.iter().find(|e| **e == name).is_none() {
                sock.do_send_msg(2,
                    format!(r#"[ "{}" ]"#, name).as_bytes(),
                    move |_, buf| {
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

#[derive(Debug)]
pub struct Mode {
    value : Rc<Cell<String>>,
}

impl Variable for Mode {
    fn from_json(_config : &json::JsonValue) -> Option<Self> {
        Some(Mode { value : Rc::new(Cell::new(String::new())) })
    }

    fn init(&self, _name : &str, rt : &Runtime) {
        let value = self.value.clone();
        SwaySocket::subscribe(rt, "mode", 0x80000002, Box::new(move |state, buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    msg["change"].as_str().map(|mode| value.set(mode.to_owned()));
                    state.request_draw();
                }
                _ => warn!("Ignoring invalid mode change message")
            }
            ListenerResult {
                remove_callback : Rc::strong_count(&value) == 1,
                consumed : false,
            }
        }));
        let value = self.value.clone();
        SwaySocket::send(rt, 12, b"", move |state, buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    msg["name"].as_str().map(|mode| value.set(mode.to_owned()));
                    state.request_draw();
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

#[derive(Debug)]
pub struct Workspace {
    // TODO configuration to track all outputs vs one output
    value : Rc<Cell<Option<WorkspaceData>>>,
}

impl Variable for Workspace {
    fn from_json(_config : &json::JsonValue) -> Option<Self> {
        Some(Workspace { value : Rc::new(Cell::new(None)) })
    }

    fn init(&self, _name : &str, rt : &Runtime) {
        let value = self.value.clone();
        SwaySocket::subscribe(rt, "workspace", 0x80000000, Box::new(move |state, buf| {
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
                    state.request_draw();
                }
                _ => warn!("Ignoring invalid workspace change message")
            }
            ListenerResult {
                remove_callback : Rc::strong_count(&value) == 1,
                consumed : false,
            }
        }));
        let value = self.value.clone();
        SwaySocket::send(rt, 1, b"", move |state, buf| {
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
                    state.request_draw();
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
}
