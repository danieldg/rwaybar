use bytes::{Buf,BytesMut};
use crate::item::{Item,Render,EventSink};
use crate::data::{IterationItem,Value};
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
    running : Cell<bool>,
    interested : Cell<NotifierList>,
}

impl Mode {
    pub fn from_toml(_config : &toml::Value) -> Option<Self> {
        Some(Mode::default())
    }

    fn interest(&self, rt : &Runtime) {
        self.value.interested.take_in(|i| i.add(rt));
        if self.value.running.replace(true) {
            return;
        }

        let weak = Rc::downgrade(&self.value);
        SwaySocket::subscribe("mode", 0x80000002, Box::new(move |buf| {
            let remove_callback;
            if let Some(mi) = weak.upgrade() {
                match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        msg["change"].as_str().map(|mode| mi.mode.set(mode.to_owned()));
                        mi.interested.take().notify_data("sway:mode");
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
                    value.interested.take().notify_data("sway:mode");
                }
                _ => warn!("Ignoring invalid get_binding_state reply")
            }
        });
    }

    pub fn read_in<F : FnOnce(Value) -> R,R>(&self, _name : &str, key : &str, rt : &Runtime, f : F) -> R {
        self.interest(rt);
        self.value.mode.take_in(|s| {
            match key {
                "" | "text" if s == "default" => f(Value::Null),
                "" | "text" => f(Value::Borrow(s)),
                "raw" => f(Value::Borrow(s)),
                "tooltip" => f(Value::Null),
                _ => {
                    warn!("Unknown key in sway-mode");
                    f(Value::Borrow(s))
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
    pub fn read_in<F : FnOnce(Value) -> R,R>(&self, key : &str, _rt : &Runtime, f : F) -> R {
        match key {
            "name" | "text" | "" => {
                f(Value::Borrow(&self.name))
            }
            "output" | "tooltip" => {
                f(Value::Borrow(&self.output))
            }
            "repr" => {
                f(Value::Borrow(&self.repr))
            }
            _ => f(Value::Null)
        }
    }

    pub fn write(&self, key : &str, value : Value, _rt : &Runtime) {
        match key {
            "switch" => SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, value).as_bytes(), |_| ()),
            "" if value.into_text() == "switch" => {
                SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, self.name).as_bytes(), |_| ());
            }
            _ => {
                error!("Ignoring write to item.{}", key);
            }
        }
    }
}

fn sway_sort_fn(a : &Rc<WorkspaceData>, b : &Rc<WorkspaceData>) -> Ordering {
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
    list : Cell<Vec<Rc<WorkspaceData>>>,
    running : Cell<bool>,
    interested : Cell<NotifierList>,
}

#[derive(Debug)]
pub struct Workspace {
    output : Option<Box<str>>,
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
                                    let mut new = (**wks).clone();
                                    new.repr = repr.to_owned();
                                    *wks = Rc::new(new);
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
                            *wks = Rc::new(new);
                            return;
                        }
                    }
                    list.push(Rc::new(new));
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
                                let mut w = (**wks).clone();
                                w.name = new.to_owned();
                                *wks = Rc::new(w);
                                break;
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
                                let mut new = (**wks).clone();
                                new.output = output.to_owned();
                                *wks = Rc::new(new);
                                return;
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
    pub fn from_toml(config : &toml::Value) -> Option<Self> {
        let output = config.get("output").and_then(|v| v.as_str()).map(Into::into);
        Some(Workspace {
            output,
            value : Default::default(),
        })
    }

    fn interest(&self, rt : &Runtime) {
        self.value.interested.take_in(|i| i.add(rt));
        if self.value.running.replace(true) {
            return;
        }

        let weak = Rc::downgrade(&self.value);
        SwaySocket::subscribe("workspace", 0x80000000, Box::new(move |buf| {
            let remove_callback;
            if let Some(value) = weak.upgrade() {
                match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        value.parse_update(msg);
                        value.interested.take().notify_data("sway:workspace");
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
                        list.push(Rc::new(new));
                    }
                    value.list.set(list);
                    value.interested.take().notify_data("sway:workspace");
                }
                _ => warn!("Ignoring invalid get_workspaces reply")
            }
        });
    }

    pub fn read_in<F : FnOnce(Value) -> R,R>(&self, _name : &str, key : &str, rt : &Runtime, f : F) -> R {
        self.interest(rt);
        match key {
            "text" | "focus" => self.value.focus.take_in(|focus| f(Value::Borrow(&focus))),
            "tooltip" => f(Value::Null),
            _ => {
                warn!("Unknown key in sway-workspace");
                f(Value::Null)
            }
        }
    }

    pub fn read_focus_list<F : FnMut(bool, IterationItem)>(&self, rt : &Runtime, mut f : F) {
        self.interest(rt);
        let output = self.output.as_ref()
            .map(|v| rt.format_or(&v, "sway-workspace").into_text())
            .unwrap_or_default();
        let focus = self.value.focus.take_in(|f| f.clone());
        self.value.list.take_in(|list| {
            for item in &*list {
                let focus = item.name == focus;
                if !output.is_empty() && item.output != output {
                    continue;
                }
                f(focus, IterationItem::SwayWorkspace(item.clone()));
            }
        });
    }

    pub fn write(&self, name : &str, key : &str, value : Value, _rt : &Runtime) {
        match key {
            "switch" => SwaySocket::send(0, format!(r#"workspace --no-auto-back-and-forth "{}""#, value).as_bytes(), |_| ()),
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }
}

#[derive(Debug,Copy,Clone)]
enum Layout {
    Horiz,
    Vert,
    Tabbed,
    Stacked,
}

#[derive(Debug)]
enum NodeType {
    Container {
        layout : Layout,
        children : Vec<Rc<Node>>,
    },
    Window {
        title : Cell<Box<str>>,
        appid : Box<str>, // or Class if null
    },
}

#[derive(Debug)]
pub struct Node {
    id : u32,
    focus : bool,
    marks : String, // "" or "1" or "mark-name, another-mark"
    contents : NodeType,
}

impl Node {
    fn parse(value : &mut json::JsonValue) -> Node {
        let mut marks = String::new();
        for (i, mark) in value["marks"].members().enumerate() {
            if i != 0 {
                marks.push_str(", ");
            }
            marks.push_str(mark.as_str().unwrap_or(""));
        }

        let contents = loop {
            let layout = match value["layout"].as_str() {
                Some("splith") => Layout::Horiz,
                Some("splitv") => Layout::Vert,
                Some("tabbed") => Layout::Tabbed,
                Some("stacked") => Layout::Stacked,
                _ => {
                    break NodeType::Window {
                        title : Cell::new(value["name"].take_string().unwrap_or_default().into()),
                        appid : value["app_id"].take_string()
                            .or_else(|| value["window_properties"]["class"].take_string())
                            .unwrap_or_default().into(),
                    };
                }
            };
            break NodeType::Container {
                layout,
                children : value["nodes"].members_mut().map(Node::parse).map(Rc::new).collect(),
            };
        };
        Node {
            id : value["id"].as_u32().unwrap_or(!0),
            focus : value["focused"].as_bool().unwrap_or(false),
            marks,
            contents,
        }
    }

    fn render(self : &Rc<Self>, items : &TreeItems, ctx : &Render, ev : &mut EventSink) {
        let ii = Rc::new(IterationItem::SwayTreeItem(self.clone()));
        match &self.contents {
            NodeType::Container { children, .. } => {
                if let Some(item) = &items.pre_node {
                    item.render_clamped_item(ctx, ev, &ii);
                }
                for child in children {
                    child.render(items, ctx, ev);
                }
                if let Some(item) = &items.post_node {
                    item.render_clamped_item(ctx, ev, &ii);
                }
            }
            NodeType::Window { .. } => {
                if let Some(item) = &items.window {
                    item.render_clamped_item(ctx, ev, &ii);
                }
            }
        }
    }

    pub fn read_in<F : FnOnce(Value) -> R,R>(&self, key : &str, _rt : &Runtime, f : F) -> R {
        match (key, &self.contents) {
            ("id", _) => {
                f(Value::Float(self.id as f64))
            }
            ("marks", _) => {
                f(Value::Borrow(&self.marks))
            }
            ("focus", _) => {
                f(Value::Bool(self.focus))
            }
            ("appid", NodeType::Window { appid, .. }) => {
                f(Value::Borrow(appid))
            }
            ("icon", NodeType::Window { appid, .. }) => {
                if appid.starts_with("org.kde.") {
                    f(Value::Borrow(&appid[8..]))
                } else {
                    f(Value::Borrow(appid))
                }
            }
            ("title", NodeType::Window { title, .. }) => {
                f(Value::Owned(title.take_in(|t| String::from(&**t))))
            }
            ("layout", NodeType::Container { layout, ..}) => {
                f(match layout {
                    Layout::Horiz => "H",
                    Layout::Vert => "V",
                    Layout::Tabbed => "T",
                    Layout::Stacked => "S",
                }.into())
            }
            _ => f(Value::Null)
        }
    }

    pub fn write(&self, _key : &str, value : Value, _rt : &Runtime) {
        SwaySocket::send(0, format!("[con_id={}] {}", self.id, value).as_bytes(), |_| ());
    }

    pub fn find_node<'a>(self : &'a Rc<Self>, id : u32) -> Option<&'a Rc<Self>> {
        if self.id == id {
            return Some(self);
        }
        match &self.contents {
            NodeType::Container { children, .. } => {
                for child in children {
                    match child.find_node(id) {
                        rv @ Some(_) => return rv,
                        None => {}
                    }
                }
            }
            _ => {}
        }
        None
    }
}

#[derive(Debug,Clone)]
struct WorkspaceNode {
    name : String,
    output : String,
    repr : Rc<Node>,
    floating : Vec<Rc<Node>>,
}

impl WorkspaceNode {
    fn parse_tree(mut value : json::JsonValue) -> Vec<WorkspaceNode> {
        let mut rv = Vec::new();
        for output in value["nodes"].members_mut() {
            let output_name = output["name"].as_str().unwrap_or_default().to_owned();
            for workspace in output["nodes"].members_mut() {
                let repr = Rc::new(Node::parse(workspace));
                rv.push(WorkspaceNode {
                    output : output_name.clone(),
                    name : workspace["name"].take_string().unwrap_or_default(),
                    repr,
                    floating : workspace["floating_nodes"].members_mut().map(Node::parse).map(Rc::new).collect(),
                });
            }
        }
        rv
    }
}

#[derive(Debug)]
pub struct Tree {
    value : Rc<TreeInner>,
    items : Box<TreeItems>,
    output : Option<Box<str>>,
}

#[derive(Debug)]
struct TreeItems {
    pre_workspace : Option<Rc<Item>>,
    pre_node : Option<Rc<Item>>,
    window : Option<Rc<Item>>,
    post_node : Option<Rc<Item>>,
    pre_float : Option<Rc<Item>>,
    post_float : Option<Rc<Item>>,
    post_workspace : Option<Rc<Item>>,
}

#[derive(Debug,Default)]
struct TreeInner {
    workspaces : Cell<Option<Vec<WorkspaceNode>>>,
    running : Cell<bool>,
    interested : Cell<NotifierList>,
}

impl TreeInner {
    fn refresh(value : Rc<Self>) {
        SwaySocket::send(4, b"", move |buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    value.workspaces.set(Some(WorkspaceNode::parse_tree(msg)));
                    value.interested.take().notify_data("sway:tree");
                }
                _ => warn!("Ignoring invalid get_binding_state reply")
            }
        });
    }

    fn find_node(&self, id : u32) -> Option<Rc<Node>> {
        self.workspaces.take_in_some(|ws| {
            for wks in ws {
                match wks.repr.find_node(id) {
                    rv @ Some(_) => return rv.cloned(),
                    None => {}
                }
                for node in &wks.floating {
                    match node.find_node(id) {
                        rv @ Some(_) => return rv.cloned(),
                        None => {}
                    }
                }
            }
            None
        }).flatten()
    }
}

impl Tree {
    pub fn from_toml(config : &toml::Value) -> Option<Self> {
        let items = TreeItems {
            pre_workspace : config.get("pre-workspace").map(Item::from_toml_ref).map(Rc::new),
            pre_node : config.get("pre-node").map(Item::from_toml_ref).map(Rc::new),
            window : config.get("window").map(Item::from_toml_ref).map(Rc::new),
            post_node : config.get("post-node").map(Item::from_toml_ref).map(Rc::new),
            pre_float : config.get("pre-float").map(Item::from_toml_ref).map(Rc::new),
            post_float : config.get("post-float").map(Item::from_toml_ref).map(Rc::new),
            post_workspace : config.get("post-workspace").map(Item::from_toml_ref).map(Rc::new),
        };
        let output = config.get("output").and_then(|v| v.as_str()).map(Into::into);
        Some(Tree {
            value : Default::default(),
            items : Box::new(items),
            output,
        })
    }

    fn interest(&self, rt : &Runtime) {
        self.value.interested.take_in(|i| i.add(rt));
        if self.value.running.replace(true) {
            return;
        }

        let weak = Rc::downgrade(&self.value);
        SwaySocket::subscribe("window", 0x80000003, Box::new(move |buf| {
            let remove_callback;
            if let Some(mi) = weak.upgrade() {
                match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        if msg["change"].as_str() == Some("title") {
                            mi.interested.take().notify_data("sway:title");
                            let id = msg["container"]["id"].as_u32().unwrap_or(!0);
                            if let Some(new_title) = msg["container"]["name"].as_str() {
                                if let Some(Node { contents : NodeType::Window { title, .. }, .. }) =
                                    mi.find_node(id).as_deref()
                                {
                                    title.set(new_title.into());
                                }
                            }
                        } else {
                            // Other update messages don't have enough information to determine the
                            // new layout, so we need to rerun get_tree.  This needs to be done
                            // outside the callback to avoid a RefCell reborrow.
                            spawn_noerr(async move {
                                TreeInner::refresh(mi);
                            });
                        }
                    }
                    _ => warn!("Ignoring invalid window change message")
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
        TreeInner::refresh(self.value.clone());
    }

    pub fn read_in<F : FnOnce(Value) -> R,R>(&self, _name : &str, _key : &str, _rt : &Runtime, f : F) -> R {
        f(Value::Null)
    }

    pub fn render(&self, ctx : &Render, ev : &mut EventSink) {
        let items = &self.items;
        let output = self.output.as_ref()
            .map(|v| ctx.runtime.format_or(&v, ctx.err_name).into_text())
            .unwrap_or_default();
        self.interest(ctx.runtime);
        self.value.workspaces.take_in_some(|workspaces| {
            for workspace in workspaces {
                if !output.is_empty() && workspace.output != output {
                    continue;
                }
                let ii = IterationItem::SwayWorkspace(Rc::new(WorkspaceData {
                    name : workspace.name.clone(),
                    output : workspace.output.clone(),
                    repr : String::new(), // TODO
                }));
                if let Some(item) = &items.pre_workspace {
                    item.render_clamped_item(ctx, ev, &ii);
                }
                workspace.repr.render(items, ctx, ev);
                for float in &workspace.floating {
                    if let Some(item) = &items.pre_float {
                        item.render_clamped_item(ctx, ev, &ii);
                    }
                    float.render(items, ctx, ev);
                    if let Some(item) = &items.post_float {
                        item.render_clamped_item(ctx, ev, &ii);
                    }
                }
                if let Some(item) = &items.post_workspace {
                    item.render_clamped_item(ctx, ev, &ii);
                }
            }
        });
    }
}

pub fn write(value : Value, _rt : &Runtime) {
    SwaySocket::send(0, format!("{}", value).as_bytes(), |_| ());
}
