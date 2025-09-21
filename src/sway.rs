use crate::{
    data::IterationItem,
    event::EventSink,
    item::{Item, ModuleContext},
    render::{Group, Render},
    state::{NotifierList, Runtime},
    util::{spawn_noerr, Cell},
    value::Value,
};
use bytes::{Buf, BytesMut};
use log::{error, warn};
use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fs,
    io::{BufRead, BufReader},
    rc::{Rc, Weak},
    sync::OnceLock,
};
use tokio::{net::UnixStream, sync::Notify};

pub fn appid_to_icon<'a>(rt: &Runtime, appid: &'a str) -> Value<'a> {
    static APPID_ICON_TABLE: OnceLock<HashMap<Box<str>, Box<str>>> = OnceLock::new();
    let table = APPID_ICON_TABLE.get_or_init(|| {
        use std::os::unix::ffi::OsStringExt;
        let mut map = HashMap::new();
        for dir in rt.xdg.find_data_files("applications") {
            for ent in fs::read_dir(dir).into_iter().flatten() {
                let Ok(ent) = ent else { continue };
                let filename = ent.file_name().into_vec();
                let Some(basename) = filename.strip_suffix(b".desktop") else {
                    continue;
                };
                let Ok(file) = fs::File::open(ent.path()) else {
                    continue;
                };
                let mut first = std::str::from_utf8(basename).ok();
                let mut icon = None::<Box<str>>;
                let mut id = None::<Box<str>>;
                for line in BufReader::new(file).lines() {
                    let Ok(line) = line else { continue };
                    if let Some((name, value)) = line.split_once('=') {
                        if name.trim() == "Icon" {
                            icon = Some(value.trim().into());
                        }
                        if name.trim() == "StartupWMClass" {
                            id = Some(value.trim().into());
                        }
                    }
                    if icon.is_some() && first.is_some() {
                        map.insert(first.take().unwrap().into(), icon.clone().unwrap());
                    }
                    if icon.is_some() && id.is_some() {
                        map.insert(id.take().unwrap(), icon.take().unwrap());
                    } else if line.trim_start().starts_with('[') {
                        icon = None;
                        id = None;
                    }
                }
            }
        }
        map
    });
    if let Some(icon) = table.get(appid) {
        Value::Borrow(icon)
    } else {
        Value::Borrow(appid)
    }
}

#[derive(Debug)]
pub struct SwaySocket {
    inner: Cell<Weak<SocketInner>>,
}

#[derive(Default, Debug)]
struct SocketInner {
    wbuf: Cell<Vec<u8>>,
    notify: Notify,
    listeners: Cell<VecDeque<Box<dyn FnOnce(&SocketInner, &[u8])>>>,

    workspaces: WorkspacesData,
    tree: TreeData,
    mode: ModeData,
}

impl SwaySocket {
    pub fn lazy() -> Self {
        Self {
            inner: Cell::new(Weak::new()),
        }
    }

    fn get(&self) -> Rc<SocketInner> {
        self.inner.upgrade_or_init(Self::init)
    }

    fn init() -> Rc<SocketInner> {
        let rv = Rc::new(SocketInner::default());
        spawn_noerr(rv.clone().init_task());

        rv
    }
}

impl SocketInner {
    /// Send a RUN_COMMMAND IPC message
    pub fn send_cmd(&self, msg: &str) {
        self.send(0, msg.as_bytes(), |_, _| ())
    }

    /// Subscribe to the provided message type.
    ///
    /// Events are handled in on_read, not via a provided callback
    fn subscribe(&self, name: &'static str) {
        self.send(2, format!(r#"[ "{}" ]"#, name).as_bytes(), move |_, buf| {
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
    }

    async fn init_task(self: Rc<Self>) {
        let (mut rh, wh) = match match std::env::var_os("SWAYSOCK") {
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

        spawn_noerr(self.clone().write_task(wh));

        let mut rbuf = BytesMut::with_capacity(1000);
        let hdr_len = 6 + 4 + 4;
        'read: loop {
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

                self.on_read(ptype, msg);

                rbuf.advance(hdr_len + len);
            }
        }
    }

    async fn write_task(self: Rc<Self>, mut wh: impl tokio::io::AsyncWriteExt + Unpin) {
        let mut wbuf = Vec::new();
        loop {
            debug_assert!(wbuf.is_empty());
            wbuf = self.wbuf.replace(wbuf);

            if !wbuf.is_empty() {
                match wh.write_all(&wbuf).await {
                    Ok(()) => wbuf.clear(),
                    Err(e) => {
                        error!("Error writing to SWAYSOCK: {}", e);
                        return;
                    }
                }
            }
            self.notify.notified().await;
        }
    }

    pub fn send<F: FnOnce(&Self, &[u8]) + 'static>(&self, id: u32, msg: &[u8], on_reply: F) {
        self.wbuf.take_in(|buf| {
            buf.extend_from_slice(b"i3-ipc");
            buf.extend_from_slice(&(msg.len() as u32).to_ne_bytes());
            buf.extend_from_slice(&id.to_ne_bytes());
            buf.extend_from_slice(&msg);
        });

        self.listeners
            .take_in(|list| list.push_back(Box::new(on_reply)));

        self.notify.notify_one();
    }

    fn on_read(&self, ptype: u32, msg: &[u8]) {
        match ptype {
            0x80000000 => {
                // WORKSPACE
                match std::str::from_utf8(msg).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        self.workspaces.parse_update(msg);
                        self.workspaces.interested.notify_data("sway:workspace");
                    }
                    _ => warn!("Ignoring invalid workspace change message"),
                }
            }
            0x80000001 => {} // OUTPUT
            0x80000002 => {
                // MODE
                match std::str::from_utf8(msg).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        msg["change"]
                            .as_str()
                            .map(|mode| self.mode.mode.set(mode.to_owned()));
                        self.mode.interested.notify_data("sway:mode");
                    }
                    _ => warn!("Ignoring invalid mode change message"),
                }
            }
            0x80000003 => {
                // WINDOW (tree update)
                match std::str::from_utf8(msg).map(|buf| json::parse(buf)) {
                    Ok(Ok(msg)) => {
                        if msg["change"].as_str() == Some("title") {
                            self.tree.interested.notify_data("sway:title");
                            let id = msg["container"]["id"].as_u32().unwrap_or(!0);
                            if let Some(new_title) = msg["container"]["name"].as_str() {
                                if let Some(Node {
                                    contents: NodeType::Window { title, .. },
                                    ..
                                }) = self.tree.find_node(id).as_deref()
                                {
                                    title.set(new_title.into());
                                }
                            }
                        } else {
                            // Other update messages don't have enough information to determine the
                            // new layout, so we need to rerun get_tree.
                            TreeData::refresh(&self);
                        }
                    }
                    _ => warn!("Ignoring invalid window change message"),
                }
            }
            // Others documented in man sway-ipc
            0x8000000.. => {}
            0..0x7FFF_FFFF => {
                // Handle request callbacks
                self.listeners.take_in(|list| {
                    if let Some(cb) = list.pop_front() {
                        cb(self, msg);
                    } else {
                        warn!("Unexpected sway reply of type {ptype}");
                        return;
                    }
                });
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Mode {}

#[derive(Debug, Default)]
struct ModeData {
    mode: Cell<String>,
    running: Cell<bool>,
    interested: NotifierList,
}

impl ModeData {
    fn interest(&self, rt: &Runtime) {
        self.interested.add(rt);
        if self.running.replace(true) {
            return;
        }

        let sway = rt.sway.get();
        sway.subscribe("mode");
        // GET_BINDING_STATE
        sway.send(12, b"", move |inner, buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    msg["name"]
                        .as_str()
                        .map(|mode| inner.mode.mode.set(mode.to_owned()));
                    inner.mode.interested.notify_data("sway:mode");
                }
                _ => warn!("Ignoring invalid get_binding_state reply"),
            }
        });
    }
}

impl Mode {
    pub fn from_toml(_config: &toml::Value) -> Self {
        Mode {}
    }

    pub fn read_in<F: FnOnce(Value) -> R, R>(
        &self,
        _name: &str,
        key: &str,
        rt: &Runtime,
        f: F,
    ) -> R {
        let sway = rt.sway.get();
        sway.mode.interest(rt);
        sway.mode.mode.take_in(|s| match key {
            "" | "text" if s == "default" => f(Value::Empty),
            "" | "text" => f(Value::Borrow(s)),
            "raw" => f(Value::Borrow(s)),
            "tooltip" => f(Value::Empty),
            _ => {
                warn!("Unknown key in sway-mode");
                f(Value::Borrow(s))
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct WorkspaceData {
    name: String,
    output: String,
    repr: String,
}

impl WorkspaceData {
    pub fn read_in<F: FnOnce(Value) -> R, R>(&self, key: &str, _rt: &Runtime, f: F) -> R {
        match key {
            "name" | "text" | "" => f(Value::Borrow(&self.name)),
            "output" | "tooltip" => f(Value::Borrow(&self.output)),
            "repr" => f(Value::Borrow(&self.repr)),
            _ => f(Value::Error),
        }
    }

    pub fn write(&self, key: &str, value: Value, rt: &Runtime) {
        let sway = rt.sway.get();
        match key {
            "switch" => sway.send_cmd(&format!(r#"workspace --no-auto-back-and-forth "{value}""#)),
            "" if value.into_text() == "switch" => {
                sway.send_cmd(&format!(
                    r#"workspace --no-auto-back-and-forth "{}""#,
                    self.name
                ));
            }
            _ => {
                error!("Ignoring write to item.{}", key);
            }
        }
    }
}

fn sway_sort_fn(a: &Rc<WorkspaceData>, b: &Rc<WorkspaceData>) -> Ordering {
    let mut a = a.name.as_str();
    let mut b = b.name.as_str();
    let a1 = a.chars().next().as_ref().map(char::is_ascii_digit);
    let b1 = b.chars().next().as_ref().map(char::is_ascii_digit);
    match (a1, b1) {
        (Some(true), Some(true)) => {
            if let Some(p) = a.find(|c: char| !c.is_ascii_digit()) {
                a = &a[..p];
            }
            if let Some(p) = b.find(|c: char| !c.is_ascii_digit()) {
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

#[derive(Debug, Default)]
struct WorkspacesData {
    focus: Cell<String>,
    list: Cell<Vec<Rc<WorkspaceData>>>,
    running: Cell<bool>,
    interested: NotifierList,
}

#[derive(Debug)]
pub struct Workspace {
    output: Option<Box<str>>,
}

impl WorkspacesData {
    fn interest(&self, rt: &Runtime) {
        self.interested.add(rt);
        if self.running.replace(true) {
            return;
        }

        let sway = rt.sway.get();
        sway.subscribe("workspace");

        // GET_WORKSPACES
        sway.send(1, b"", move |inner, buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    let mut list = Vec::new();
                    for workspace in msg.members() {
                        let new = WorkspaceData {
                            name: workspace["name"].as_str().unwrap_or("").to_owned(),
                            output: workspace["output"].as_str().unwrap_or("").to_owned(),
                            repr: workspace["representation"]
                                .as_str()
                                .unwrap_or("")
                                .to_owned(),
                        };
                        if workspace["focused"].as_bool() == Some(true) {
                            inner.workspaces.focus.set(new.name.clone());
                        }
                        list.push(Rc::new(new));
                    }
                    list.sort_by(sway_sort_fn);
                    inner.workspaces.list.set(list);
                    inner.workspaces.interested.notify_data("sway:workspace");
                }
                _ => warn!("Ignoring invalid get_workspaces reply"),
            }
        });
    }

    fn parse_update(&self, msg: json::JsonValue) {
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
                    name: match msg["current"]["name"].as_str() {
                        Some(n) => n.to_owned(),
                        None => return,
                    },
                    output: msg["current"]["output"].as_str().unwrap_or("").to_owned(),
                    repr: msg["current"]["representation"]
                        .as_str()
                        .unwrap_or("")
                        .to_owned(),
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
                    self.list
                        .take_in(|list| list.retain(|wks| wks.name != gone))
                });
            }
            Some("rename") => {
                let old = msg["old"]["name"].as_str();
                let new = msg["current"]["name"].as_str();
                if let (Some(old), Some(new)) = (old, new) {
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
                if let (Some(name), Some(output)) = (name, output) {
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
    pub fn from_toml(config: &toml::Value) -> Self {
        let output = config
            .get("output")
            .and_then(|v| v.as_str())
            .map(Into::into);
        Workspace { output }
    }

    pub fn read_in<F: FnOnce(Value) -> R, R>(
        &self,
        _name: &str,
        key: &str,
        rt: &Runtime,
        f: F,
    ) -> R {
        let sway = rt.sway.get();
        sway.workspaces.interest(rt);
        match key {
            "text" | "focus" => sway
                .workspaces
                .focus
                .take_in(|focus| f(Value::Borrow(&focus))),
            "tooltip" => f(Value::Empty),
            _ => {
                warn!("Unknown key in sway-workspace");
                f(Value::Error)
            }
        }
    }

    pub fn read_focus_list<F: FnMut(bool, IterationItem)>(&self, rt: &Runtime, mut f: F) {
        let sway = rt.sway.get();
        sway.workspaces.interest(rt);
        let output = self
            .output
            .as_ref()
            .map(|v| rt.format_or(&v, "sway-workspace").into_text())
            .unwrap_or_default();
        let focus = sway.workspaces.focus.take_in(|f| f.clone());
        sway.workspaces.list.take_in(|list| {
            for item in &*list {
                let focus = item.name == focus;
                if !output.is_empty() && item.output != output {
                    continue;
                }
                f(focus, IterationItem::SwayWorkspace(item.clone()));
            }
        });
    }

    pub fn write(&self, name: &str, key: &str, value: Value, rt: &Runtime) {
        let sway = rt.sway.get();
        match key {
            "switch" => sway.send_cmd(&format!(r#"workspace --no-auto-back-and-forth "{value}""#,)),
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Layout {
    Horiz,
    Vert,
    Tabbed,
    Stacked,
}

#[derive(Debug)]
enum NodeType {
    Container {
        layout: Layout,
        children: Vec<Rc<Node>>,
    },
    Window {
        title: Cell<Box<str>>,
        appid: Box<str>, // or Class if null
    },
}

#[derive(Debug)]
pub struct Node {
    id: u32,
    focus: bool,
    marks: String, // "" or "1" or "mark-name, another-mark"
    contents: NodeType,
}

impl Node {
    fn parse(value: &mut json::JsonValue) -> Node {
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
                        title: Cell::new(value["name"].take_string().unwrap_or_default().into()),
                        appid: value["app_id"]
                            .take_string()
                            .or_else(|| value["window_properties"]["class"].take_string())
                            .unwrap_or_default()
                            .into(),
                    };
                }
            };
            break NodeType::Container {
                layout,
                children: value["nodes"]
                    .members_mut()
                    .map(Node::parse)
                    .map(Rc::new)
                    .collect(),
            };
        };
        Node {
            id: value["id"].as_u32().unwrap_or(!0),
            focus: value["focused"].as_bool().unwrap_or(false),
            marks,
            contents,
        }
    }

    fn render(
        self: &Rc<Self>,
        items: &TreeItems,
        ctx: &mut Render,
        group: &mut Group,
        ev: &mut EventSink,
    ) {
        let ii = Rc::new(IterationItem::SwayTreeItem(self.clone()));
        match &self.contents {
            NodeType::Container { children, .. } => {
                if let Some(item) = &items.pre_node {
                    item.render_clamped_item(ctx, ev, &ii);
                    group.next_h(ctx);
                }
                for child in children {
                    child.render(items, ctx, group, ev);
                }
                if let Some(item) = &items.post_node {
                    item.render_clamped_item(ctx, ev, &ii);
                    group.next_h(ctx);
                }
            }
            NodeType::Window { .. } => {
                if let Some(item) = &items.window {
                    item.render_clamped_item(ctx, ev, &ii);
                    group.next_h(ctx);
                }
            }
        }
    }

    pub fn read_in<F: FnOnce(Value) -> R, R>(&self, key: &str, rt: &Runtime, f: F) -> R {
        match (key, &self.contents) {
            ("id", _) => f(Value::Float(self.id as f64)),
            ("marks", _) => f(Value::Borrow(&self.marks)),
            ("focus", _) => f(Value::Bool(self.focus)),
            ("appid", NodeType::Window { appid, .. }) => f(Value::Borrow(appid)),
            ("icon", NodeType::Window { appid, .. }) => f(appid_to_icon(rt, appid)),
            ("title", NodeType::Window { title, .. }) => {
                f(Value::Owned(title.take_in(|t| String::from(&**t))))
            }
            ("layout", NodeType::Container { layout, .. }) => f(match layout {
                Layout::Horiz => "H",
                Layout::Vert => "V",
                Layout::Tabbed => "T",
                Layout::Stacked => "S",
            }
            .into()),
            _ => f(Value::Error),
        }
    }

    pub fn write(&self, _key: &str, value: Value, rt: &Runtime) {
        let sway = rt.sway.get();
        sway.send_cmd(&format!("[con_id={}] {}", self.id, value));
    }

    pub fn find_node<'a>(self: &'a Rc<Self>, id: u32) -> Option<&'a Rc<Self>> {
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

#[derive(Debug, Clone)]
struct WorkspaceNode {
    name: String,
    output: String,
    repr: Rc<Node>,
    floating: Vec<Rc<Node>>,
}

impl WorkspaceNode {
    fn parse_tree(mut value: json::JsonValue) -> Vec<WorkspaceNode> {
        let mut rv = Vec::new();
        for output in value["nodes"].members_mut() {
            let output_name = output["name"].as_str().unwrap_or_default().to_owned();
            for workspace in output["nodes"].members_mut() {
                let repr = Rc::new(Node::parse(workspace));
                rv.push(WorkspaceNode {
                    output: output_name.clone(),
                    name: workspace["name"].take_string().unwrap_or_default(),
                    repr,
                    floating: workspace["floating_nodes"]
                        .members_mut()
                        .map(Node::parse)
                        .map(Rc::new)
                        .collect(),
                });
            }
        }
        rv
    }
}

#[derive(Debug)]
pub struct Tree {
    items: Box<TreeItems>,
    output: Option<Box<str>>,
    workspace: Option<Box<str>>,
}

#[derive(Debug)]
struct TreeItems {
    pre_workspace: Option<Rc<Item>>,
    pre_node: Option<Rc<Item>>,
    window: Option<Rc<Item>>,
    post_node: Option<Rc<Item>>,
    pre_floats: Option<Rc<Item>>,
    pre_float: Option<Rc<Item>>,
    post_float: Option<Rc<Item>>,
    post_workspace: Option<Rc<Item>>,
}

#[derive(Debug, Default)]
struct TreeData {
    workspaces: Cell<Option<Vec<WorkspaceNode>>>,
    /// True once a window subscribe and initial get_tree request was made
    running: Cell<bool>,
    interested: NotifierList,
}

impl TreeData {
    fn interest(&self, rt: &Runtime) {
        self.interested.add(rt);
        if self.running.replace(true) {
            return;
        }

        let sway = rt.sway.get();
        sway.subscribe("window");
        TreeData::refresh(&sway);
    }

    fn refresh(sway: &SocketInner) {
        // GET_TREE
        sway.send(4, b"", move |inner, buf| {
            match std::str::from_utf8(buf).map(|buf| json::parse(buf)) {
                Ok(Ok(msg)) => {
                    inner
                        .tree
                        .workspaces
                        .set(Some(WorkspaceNode::parse_tree(msg)));
                    inner.tree.interested.notify_data("sway:tree");
                }
                _ => warn!("Ignoring invalid get_tree reply"),
            }
        });
    }

    fn find_node(&self, id: u32) -> Option<Rc<Node>> {
        self.workspaces
            .take_in_some(|ws| {
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
            })
            .flatten()
    }
}

impl Tree {
    pub fn from_toml(config: &toml::Value, ctx: ModuleContext) -> Self {
        let items = TreeItems {
            pre_workspace: ctx.opt_item_from_key(config, "pre-workspace").map(Rc::new),
            pre_node: ctx.opt_item_from_key(config, "pre-node").map(Rc::new),
            window: ctx.opt_item_from_key(config, "window").map(Rc::new),
            post_node: ctx.opt_item_from_key(config, "post-node").map(Rc::new),
            pre_floats: ctx.opt_item_from_key(config, "pre-floats").map(Rc::new),
            pre_float: ctx.opt_item_from_key(config, "pre-float").map(Rc::new),
            post_float: ctx.opt_item_from_key(config, "post-float").map(Rc::new),
            post_workspace: ctx.opt_item_from_key(config, "post-workspace").map(Rc::new),
        };
        let output = config
            .get("output")
            .and_then(|v| v.as_str())
            .map(Into::into);
        let workspace = config
            .get("workspace")
            .and_then(|v| v.as_str())
            .map(Into::into);
        Tree {
            items: Box::new(items),
            output,
            workspace,
        }
    }

    pub fn read_in<F: FnOnce(Value) -> R, R>(
        &self,
        _name: &str,
        _key: &str,
        _rt: &Runtime,
        f: F,
    ) -> R {
        f(Value::Error)
    }

    pub fn render(&self, ctx: &mut Render, ev: &mut EventSink) {
        let mut group = ctx.item_group();
        let items = &self.items;
        let output = self
            .output
            .as_ref()
            .map(|v| ctx.runtime.format_or(&v, ctx.err_name).into_text())
            .unwrap_or_default();
        let name_filter = self
            .workspace
            .as_ref()
            .map(|v| ctx.runtime.format_or(&v, ctx.err_name).into_text())
            .unwrap_or_default();
        let sway = ctx.runtime.sway.get();
        sway.tree.interest(ctx.runtime);
        sway.tree.workspaces.take_in_some(|workspaces| {
            for workspace in workspaces {
                if !output.is_empty() && workspace.output != output {
                    continue;
                }
                if !name_filter.is_empty() && workspace.name != name_filter {
                    continue;
                }
                let ii = IterationItem::SwayWorkspace(Rc::new(WorkspaceData {
                    name: workspace.name.clone(),
                    output: workspace.output.clone(),
                    repr: String::new(), // TODO
                }));
                if let Some(item) = &items.pre_workspace {
                    item.render_clamped_item(ctx, ev, &ii);
                    group.next_h(ctx);
                }
                workspace.repr.render(items, ctx, &mut group, ev);
                if !workspace.floating.is_empty() {
                    if let Some(item) = &items.pre_floats {
                        item.render_clamped_item(ctx, ev, &ii);
                        group.next_h(ctx);
                    }
                }
                for float in &workspace.floating {
                    if let Some(item) = &items.pre_float {
                        item.render_clamped_item(ctx, ev, &ii);
                        group.next_h(ctx);
                    }
                    float.render(items, ctx, &mut group, ev);
                    if let Some(item) = &items.post_float {
                        item.render_clamped_item(ctx, ev, &ii);
                        group.next_h(ctx);
                    }
                }
                if let Some(item) = &items.post_workspace {
                    item.render_clamped_item(ctx, ev, &ii);
                    group.next_h(ctx);
                }
            }
        });
        ctx.render_pos = group.bounds;
    }
}

pub fn write(value: Value, rt: &Runtime) {
    rt.sway.get().send_cmd(&format!("{value}"));
}
