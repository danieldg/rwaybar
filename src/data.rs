//! Text expansion and data sources
#[cfg(feature = "dbus")]
use crate::dbus::DbusValue;
#[cfg(feature = "dbus")]
use crate::mpris;
#[cfg(feature = "pulse")]
use crate::pulse;
#[cfg(feature = "dbus")]
use crate::tray;
use crate::{
    item::{Item, ItemFormat, ModuleContext},
    pipewire,
    state::{NotifierList, Runtime},
    sway,
    util::{glob_expand, spawn_handle, spawn_noerr, toml_to_f64, toml_to_string, Cell, Fd},
    wlr::ClipboardData,
};
use evalexpr::Node as EvalExpr;
use futures_util::future::RemoteHandle;
use json::JsonValue;
use libc;
use log::{debug, error, info, warn};
use std::{
    borrow::Cow,
    fmt, fs,
    future::Future,
    io,
    io::Write,
    os::unix::io::{AsRawFd, IntoRawFd},
    process::{ChildStdin, Command, Stdio},
    rc::{Rc, Weak},
    time::{Duration, Instant},
};
use tokio::io::unix::AsyncFd;

/// The result of a data source or text expansion
#[derive(Debug)]
pub enum Value<'a> {
    Borrow(&'a str),
    Owned(String),
    Float(f64),
    Bool(bool),
    Null,
}

#[cfg_attr(not(feature = "pulse"), allow(unused))]
impl<'a> Value<'a> {
    pub fn as_ref(&self) -> Value {
        match self {
            Value::Borrow(v) => Value::Borrow(v),
            Value::Owned(v) => Value::Borrow(&v[..]),
            Value::Float(f) => Value::Float(*f),
            Value::Bool(b) => Value::Bool(*b),
            Value::Null => Value::Null,
        }
    }

    pub fn as_str_fast(&self) -> &str {
        match self {
            Value::Borrow(v) => v,
            Value::Owned(v) => v,
            _ => "",
        }
    }

    pub fn into_owned(self) -> Value<'static> {
        match self {
            Value::Borrow(v) => Value::Owned(v.into()),
            Value::Owned(v) => Value::Owned(v),
            Value::Float(f) => Value::Float(f),
            Value::Bool(b) => Value::Bool(b),
            Value::Null => Value::Null,
        }
    }

    pub fn into_text(self) -> Cow<'a, str> {
        match self {
            Value::Borrow(v) => Cow::Borrowed(v),
            Value::Owned(v) => Cow::Owned(v),
            Value::Float(f) => format!("{}", f).into(),
            Value::Bool(true) => "1".into(),
            Value::Bool(false) => "0".into(),
            Value::Null => "".into(),
        }
    }

    pub fn parse_f32(&self) -> Option<f32> {
        match self {
            Value::Borrow(v) => v.parse().ok(),
            Value::Owned(v) => v.parse().ok(),
            Value::Float(f) => Some(*f as f32),
            Value::Bool(true) => Some(1.0),
            Value::Bool(false) => Some(0.0),
            Value::Null => None,
        }
    }

    pub fn parse_f64(&self) -> Option<f64> {
        match self {
            Value::Borrow(v) => v.parse().ok(),
            Value::Owned(v) => v.parse().ok(),
            Value::Float(f) => Some(*f),
            Value::Bool(true) => Some(1.0),
            Value::Bool(false) => Some(0.0),
            Value::Null => None,
        }
    }

    pub fn parse_bool(&self) -> Option<bool> {
        match self.as_ref() {
            Value::Borrow(v) if v == "1" => Some(true),
            Value::Borrow(v) if v == "0" => Some(false),
            Value::Borrow(v) if v == "true" => Some(true),
            Value::Borrow(v) if v == "false" => Some(false),
            Value::Borrow(_) => None,
            Value::Owned(_) => None,
            Value::Float(f) if f == 0.0 => Some(false),
            Value::Float(f) if f == 1.0 => Some(true),
            Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Borrow(v) => !v.is_empty(),
            Value::Owned(v) => !v.is_empty(),
            Value::Float(f) => *f != 0.0,
            Value::Bool(b) => *b,
            Value::Null => false,
        }
    }
}

impl<'a> Default for Value<'a> {
    fn default() -> Self {
        Value::Null
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Borrow(v) => v.fmt(fmt),
            Value::Owned(v) => v.fmt(fmt),
            Value::Float(f) => f.fmt(fmt),
            Value::Bool(true) => "1".fmt(fmt),
            Value::Bool(false) => "0".fmt(fmt),
            Value::Null => Ok(()),
        }
    }
}

impl<'a> Into<JsonValue> for Value<'a> {
    fn into(self) -> JsonValue {
        match self {
            Value::Borrow(v) => v.into(),
            Value::Owned(v) => v.into(),
            Value::Float(f) => f.into(),
            Value::Bool(b) => b.into(),
            Value::Null => JsonValue::Null,
        }
    }
}

impl<'a> From<Value<'a>> for evalexpr::Value {
    fn from(v: Value) -> Self {
        match v {
            Value::Owned(s) => evalexpr::Value::String(s),
            Value::Borrow(s) => evalexpr::Value::String(s.into()),
            Value::Float(f) => evalexpr::Value::Float(f),
            Value::Bool(f) => evalexpr::Value::Boolean(f),
            Value::Null => evalexpr::Value::Empty,
        }
    }
}

impl<'a> From<evalexpr::Value> for Value<'a> {
    fn from(v: evalexpr::Value) -> Self {
        match v {
            evalexpr::Value::String(s) => Value::Owned(s),
            evalexpr::Value::Float(n) => Value::Float(n),
            evalexpr::Value::Int(n) => Value::Float(n as _),
            evalexpr::Value::Boolean(b) => Value::Bool(b),
            evalexpr::Value::Empty => Value::Null,
            _ => {
                warn!("Ignoring invalid return type from eval");
                Value::Null
            }
        }
    }
}

impl<'a> From<&'a JsonValue> for Value<'a> {
    fn from(v: &'a JsonValue) -> Self {
        match v {
            JsonValue::String(s) => Value::Borrow(s),
            JsonValue::Short(s) => Value::Borrow(s),
            &JsonValue::Number(n) => Value::Float(n.into()),
            &JsonValue::Boolean(b) => Value::Bool(b),
            JsonValue::Object(_) | JsonValue::Array(_) => Value::Null,
            JsonValue::Null => Value::Null,
        }
    }
}

impl<'a> From<toml::Value> for Value<'a> {
    fn from(v: toml::Value) -> Self {
        match v {
            toml::Value::String(s) => Value::Owned(s),
            toml::Value::Integer(n) => Value::Float(n as f64),
            toml::Value::Float(n) => Value::Float(n),
            toml::Value::Boolean(b) => Value::Bool(b),
            _ => Value::Null,
        }
    }
}

impl<'a> From<Cow<'a, str>> for Value<'a> {
    fn from(v: Cow<'a, str>) -> Self {
        match v {
            Cow::Borrowed(v) => Value::Borrow(v),
            Cow::Owned(v) => Value::Owned(v),
        }
    }
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(v: &'a str) -> Self {
        Value::Borrow(v)
    }
}

impl<'a> From<String> for Value<'a> {
    fn from(v: String) -> Self {
        Value::Owned(v)
    }
}

#[derive(Debug, Clone)]
pub enum ItemReference {
    New(Rc<str>),
    Looped,
    Found(Weak<Item>),
    NotFound(Rc<Item>),
}

impl ItemReference {
    pub fn with<F: FnOnce(Option<&Rc<Item>>) -> R, R>(this: &Cell<Self>, rt: &Runtime, f: F) -> R {
        let mut me = this.replace(ItemReference::Looped);
        let rv = match me {
            ItemReference::New(name) => match rt.items.get(&*name) {
                Some(item) => {
                    me = ItemReference::Found(Rc::downgrade(item));
                    f(Some(item))
                }
                None => {
                    debug!("Unresolved reference to item '{name}', interpreting as text");
                    let item = Item::new_unresolved(&name);
                    let rv = f(Some(&item));
                    me = ItemReference::NotFound(item);
                    rv
                }
            },
            ItemReference::Found(ref v) => f(v.upgrade().as_ref()),
            ItemReference::NotFound(ref v) => f(Some(v)),
            ItemReference::Looped => {
                error!("Loop found when resolving reference");
                f(None)
            }
        };
        this.set(me);
        rv
    }
}

/// Helper for items that are updated based on a polling timer
#[derive(Debug)]
pub struct Periodic<T> {
    period: f64,
    shared: Rc<PeriodicInner<T>>,
}

#[derive(Debug)]
struct PeriodicInner<T> {
    interested: NotifierList,
    was_read: Cell<bool>,
    last_update: Cell<Option<Instant>>,
    timer: Cell<Option<RemoteHandle<()>>>,
    data: T,
}

impl<T: 'static> Periodic<T> {
    pub fn new(period: f64, data: T) -> Self {
        Periodic {
            period,
            shared: Rc::new(PeriodicInner {
                interested: Default::default(),
                was_read: Cell::default(),
                last_update: Cell::default(),
                timer: Cell::default(),
                data,
            }),
        }
    }

    pub fn data(&self) -> &T {
        self.shared.was_read.set(true);
        &self.shared.data
    }

    /// Read periodically using the given closure.
    ///
    /// If the closure returns `Some(reason)`, an update will happen; otherwise, the closure will
    /// continue to be polled at the specified period.
    pub fn read_refresh<F>(&self, rt: &Runtime, mut do_read: F)
    where
        F: FnMut(&T) -> Option<&str> + 'static,
    {
        self._read_refresh(rt, move |n, t| {
            match (n, do_read(t)) {
                (Some(notify), Some(reason)) => {
                    notify.notify_data(reason);
                }
                _ => {}
            }
            None::<std::future::Ready<_>>
        })
    }

    /// Read periodically using the given async closure, spawning it off if in sync context.
    ///
    /// The closure is responsible for change notification.
    pub fn read_refresh_async<F, Fut>(&self, rt: &Runtime, mut do_read: F)
    where
        F: FnMut(&T) -> Fut + 'static,
        Fut: Future<Output = ()> + 'static,
    {
        self._read_refresh(rt, move |_n, t| Some(do_read(t)));
    }

    fn _read_refresh<F, Fut>(&self, rt: &Runtime, mut do_read: F)
    where
        F: FnMut(Option<&NotifierList>, &T) -> Option<Fut> + 'static,
        Fut: Future<Output = ()> + 'static,
    {
        let last_update = self.shared.last_update.get();
        if self.period <= 0.0 && last_update.is_some() {
            // the one-shot read is already done
            return;
        }

        let now = Instant::now();
        self.shared.interested.add(rt);
        if let Some(last_update) = last_update {
            // Read a new values if we are currently redrawing and it's at least 90% of the
            // deadline.  This avoids waking up several times in a row to update each of a
            // group of items that have almost the same deadline.
            let early = last_update + Duration::from_secs_f64(self.period * 0.9);
            if early > now {
                // just keep the timer active
                return;
            }
        }
        // last_update was too long ago, update now

        self.shared.last_update.set(Some(now));
        let fut = do_read(None, &self.shared.data);
        if self.period > 0.0 {
            let weak = Rc::downgrade(&self.shared);
            let period = self.period;

            let rh = spawn_handle("Periodic", async move {
                match fut {
                    Some(fut) => fut.await,
                    None => {}
                }
                loop {
                    tokio::time::sleep(Duration::from_secs_f64(period)).await;
                    let shared = match weak.upgrade() {
                        Some(v) => v,
                        None => return Ok(()),
                    };

                    // Try to avoid reading if nobody is listening.
                    if !shared.was_read.replace(false) {
                        shared.timer.set(None);
                        return Ok(());
                    }

                    shared.last_update.set(Some(Instant::now()));
                    let fut = do_read(Some(&shared.interested), &shared.data);
                    match fut {
                        Some(fut) => fut.await,
                        None => {}
                    }
                }
            });
            // ensure we only have one task working to update the value
            self.shared.timer.set(Some(rh));
        } else if let Some(fut) = fut {
            spawn_noerr(fut);
        }
    }
}

/// Type-specific part of an [Item]
#[derive(Debug)]
pub enum Module {
    Bar {
        left: Rc<Item>,
        center: Rc<Item>,
        right: Rc<Item>,
        data: Box<BarData>,
    },
    Calendar {
        day_fmt: Box<str>,
        today_fmt: Box<str>,
        other_fmt: Box<str>,
        zone: Box<str>,
        monday: bool,
    },
    Calendar2 {
        day_fmt: Box<str>,
        today_fmt: Box<str>,
        other_fmt: Box<str>,
        zone: Box<str>,
        past: u8,
        future: u8,
        monday: bool,
    },
    Clipboard {
        state: Rc<ClipboardData>,
    },
    Clock {
        format: Box<str>,
        zone: Box<str>,
        state: Rc<ClockState>,
    },
    #[cfg(feature = "dbus")]
    DbusCall {
        poll: Periodic<Rc<DbusValue>>,
    },
    Disk {
        poll: Periodic<(Box<str>, Cell<libc::statvfs>)>,
    },
    Eval {
        expr: EvalExpr,
        vars: Vec<(Box<str>, Module)>,
    },
    ExecJson {
        command: Box<str>,
        stdin: Cell<Option<ChildStdin>>,
        value: Cell<Option<Rc<(Cell<JsonValue>, NotifierList)>>>,
        handle: Cell<Option<RemoteHandle<()>>>,
    },
    Fade {
        items: Vec<Rc<Item>>,
        tooltip: Option<Rc<Item>>,
        value: Box<Module>,
        dir: u8,
    },
    FocusList {
        source: Box<Module>,
        others: Rc<Item>,
        focused: Rc<Item>,
        spacing: Box<str>,
    },
    FontTest {
        offset: Cell<u16>,
        interested: NotifierList,
    },
    Formatted {
        format: Box<str>,
        tooltip: Option<Rc<Item>>,
    },
    Group {
        condition: Option<Box<str>>,
        items: Vec<Rc<Item>>,
        tooltip: Option<Rc<Item>>,
        spacing: Box<str>,
        vertical: bool,
        // TODO crop ordering: allow specific items to be cropped first
        // TODO use min-width to force earlier cropping
    },
    Icon {
        name: Box<str>,
        fallback: Box<str>,
        tooltip: Box<str>,
    },
    Item {
        // unique variant for the reserved "item" item
        value: Cell<Option<IterationItem>>,
    },
    ItemReference {
        value: Cell<ItemReference>,
    },
    List {
        value: Cell<usize>,
        choices: Box<[Box<str>]>,
        interested: NotifierList,
        wrap: bool,
    },
    #[cfg(feature = "dbus")]
    MediaPlayer2 {
        target: Box<str>,
    },
    Meter {
        min: Box<str>,
        max: Box<str>,
        src: Box<Module>,
        values: Box<[Box<str>]>,
    },
    ParseError {
        msg: Cell<Option<Cow<'static, str>>>,
    },
    Pipewire {
        target: Box<str>,
    },
    #[cfg(feature = "pulse")]
    Pulse {
        target: Box<str>,
    },
    ReadFile {
        on_err: Box<str>,
        poll: Periodic<(Box<str>, Cell<Option<String>>)>,
    },
    Regex {
        regex: regex::Regex,
        text: Box<str>,
        replace: Box<str>,
    },
    SwayMode(sway::Mode),
    SwayTree(sway::Tree),
    SwayWorkspace(sway::Workspace),
    Switch {
        format: Box<Module>,
        cases: toml::value::Table,
        default: Box<str>,
    },
    Thermal {
        poll: Periodic<(Box<str>, Cell<u32>)>,
        label: Option<Box<str>>,
    },
    Tray {
        passive: Rc<Item>,
        active: Rc<Item>,
        urgent: Rc<Item>,
    },
    Value {
        value: Cell<Value<'static>>,
        interested: NotifierList,
    },
}

#[derive(Debug)]
pub struct BarData {
    pub tooltips: ItemFormat,
    pub config: toml::Value,
}

impl BarData {
    pub fn new(tooltips: ItemFormat, config: toml::Value) -> Box<Self> {
        Box::new(BarData { tooltips, config })
    }
}

/// Possible contents of the "item" block
#[derive(Debug, Clone)]
pub enum IterationItem {
    #[cfg(feature = "dbus")]
    MediaPlayer2 {
        target: Rc<str>,
    },
    Pipewire {
        target: Rc<str>,
    },
    #[cfg(feature = "pulse")]
    Pulse {
        target: Rc<str>,
    },
    SwayWorkspace(Rc<sway::WorkspaceData>),
    SwayTreeItem(Rc<sway::Node>),
    #[cfg(feature = "dbus")]
    Tray(Rc<tray::TrayItem>),
}

impl PartialEq for IterationItem {
    fn eq(&self, rhs: &Self) -> bool {
        use IterationItem::*;
        match (self, rhs) {
            #[cfg(feature = "dbus")]
            (MediaPlayer2 { target: a }, MediaPlayer2 { target: b }) => Rc::ptr_eq(a, b),
            (Pipewire { target: a }, Pipewire { target: b }) => Rc::ptr_eq(a, b),
            #[cfg(feature = "pulse")]
            (Pulse { target: a }, Pulse { target: b }) => Rc::ptr_eq(a, b),
            (SwayWorkspace(a), SwayWorkspace(b)) => Rc::ptr_eq(a, b),
            (SwayTreeItem(a), SwayTreeItem(b)) => Rc::ptr_eq(a, b),
            #[cfg(feature = "dbus")]
            (Tray(a), Tray(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Module {
    pub fn from_toml_in(value: &toml::Value, ctx: ModuleContext) -> Self {
        match value.get("type").and_then(|v| v.as_str()) {
            // keep values in alphabetical order
            Some("calendar") => {
                let day_fmt = value
                    .get("day-format")
                    .and_then(|v| v.as_str())
                    .unwrap_or(" %e")
                    .into();
                let today_fmt = value
                    .get("today-format")
                    .and_then(|v| v.as_str())
                    .unwrap_or(" <span color='green'><b>%e</b></span>")
                    .into();
                let other_fmt = value
                    .get("other-format")
                    .and_then(|v| v.as_str())
                    .unwrap_or(" <span color='gray'>%e</span>")
                    .into();
                let zone = value
                    .get("timezone")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .into();
                let monday = value
                    .get("start")
                    .and_then(|v| v.as_str())
                    .map_or(false, |v| v.eq_ignore_ascii_case("monday"));
                let past = match toml_to_f64(value.get("before")) {
                    None => {
                        return Module::Calendar {
                            day_fmt,
                            today_fmt,
                            other_fmt,
                            zone,
                            monday,
                        }
                    }
                    Some(past) => past as u8,
                };
                let future = toml_to_f64(value.get("after"))
                    .map(|f| f as u8)
                    .unwrap_or(past);
                Module::Calendar2 {
                    day_fmt,
                    today_fmt,
                    other_fmt,
                    zone,
                    past,
                    future,
                    monday,
                }
            }
            Some("clipboard") => {
                let seat = value.get("seat").and_then(|v| v.as_str()).map(Into::into);
                let selection = match value.get("selection").map(|v| v.as_bool()) {
                    None => false,
                    Some(Some(b)) => b,
                    Some(None) => {
                        return Module::parse_error("Invalid clipboard type");
                    }
                };
                let mime_list = value
                    .get("types")
                    .and_then(|v| v.as_array())
                    .map(|v| v.as_slice())
                    .unwrap_or_default()
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(Into::into)
                    .collect();
                Module::Clipboard {
                    state: Rc::new(ClipboardData {
                        seat,
                        mime_list,
                        selection,
                        interested: Default::default(),
                    }),
                }
            }
            Some("clock") => {
                let format = value
                    .get("format")
                    .and_then(|v| v.as_str())
                    .unwrap_or("%H:%M")
                    .into();
                let zone = value
                    .get("timezone")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .into();
                Module::Clock {
                    format,
                    zone,
                    state: Default::default(),
                }
            }
            #[cfg(feature = "dbus")]
            Some("dbus") => {
                let rc = match DbusValue::from_toml(value) {
                    Ok(rc) => rc,
                    Err(e) => return Module::parse_error(e),
                };
                let poll = Periodic::new(toml_to_f64(value.get("poll")).unwrap_or(0.0), rc);
                Module::DbusCall { poll }
            }
            Some("disk") => {
                let path = value
                    .get("path")
                    .and_then(|v| v.as_str())
                    .unwrap_or("/")
                    .into();
                let v: libc::statvfs = unsafe { std::mem::zeroed() };
                let poll = Periodic::new(
                    toml_to_f64(value.get("poll")).unwrap_or(60.0),
                    (path, Cell::new(v)),
                );
                Module::Disk { poll }
            }
            Some("eval") => Self::new_eval(value, ctx),
            Some("exec-json") => {
                let command = match value.get("command").and_then(|v| v.as_str()) {
                    Some(cmd) => cmd.into(),
                    None => {
                        return Module::parse_error("Comamnd to execute is required");
                    }
                };
                Module::ExecJson {
                    command,
                    stdin: Cell::new(None),
                    value: Cell::new(None),
                    handle: Cell::new(None),
                }
            }
            Some("fade") => {
                let items = [value.get("item"), value.get("items")]
                    .iter()
                    .filter_map(Option::as_deref)
                    .filter_map(|v| v.as_array())
                    .flatten()
                    .map(|v| ctx.item_from_value(v))
                    .map(Rc::new)
                    .collect::<Vec<_>>();
                if items.is_empty() {
                    return Module::parse_error("At least one item is required");
                }
                let tooltip = ctx
                    .with_text_preferred()
                    .opt_item_from_key(value, "tooltip")
                    .map(Rc::new);
                let dir = match value.get("dir").and_then(toml::Value::as_str) {
                    None | Some("right") => b'r',
                    Some("left") => b'l',
                    Some("up") => b'u',
                    Some("down") => b'd',
                    _ => return Module::parse_error("Invalid 'dir'"),
                };
                let value = if value.get("expr").is_some() {
                    Box::new(Self::new_eval(value, ctx))
                } else if let Some(item) = value.get("value") {
                    Box::new(Self::from_toml_in(item, ctx.with_ref_preferred()))
                } else {
                    return Module::parse_error("'value' or 'expr' is required");
                };
                Module::Fade {
                    items,
                    value,
                    dir,
                    tooltip,
                }
            }
            Some("focus-list") => {
                let source = match value.get("source") {
                    Some(s) => Box::new(Module::from_toml_in(s, ctx.with_ref_preferred())),
                    None => {
                        return Module::parse_error("A source is required for focus-list");
                    }
                };
                let spacing = toml_to_string(value.get("spacing"))
                    .unwrap_or_default()
                    .into();
                let others = Rc::new(ctx.item_from_key(value, "item"));
                let focused = ctx
                    .opt_item_from_key(value, "focused-item")
                    .map(Rc::new)
                    .unwrap_or_else(|| others.clone());

                Module::FocusList {
                    source,
                    others,
                    focused,
                    spacing,
                }
            }
            Some("font-test") => Module::FontTest {
                offset: Cell::new(0),
                interested: Default::default(),
            },
            Some("formatted") | Some("text") => {
                let format = value
                    .get("format")
                    .and_then(|v| v.as_str())
                    .unwrap_or_else(|| {
                        error!("Formatted variables require a format: {}", value);
                        ""
                    })
                    .into();
                let tooltip = ctx
                    .with_text_preferred()
                    .opt_item_from_key(value, "tooltip")
                    .map(Rc::new);

                Module::Formatted { format, tooltip }
            }
            Some("group") => {
                let spacing = toml_to_string(value.get("spacing"))
                    .unwrap_or_default()
                    .into();
                let tooltip = ctx
                    .with_text_preferred()
                    .opt_item_from_key(value, "tooltip")
                    .map(Rc::new);

                let condition = toml_to_string(value.get("condition")).map(Into::into);
                let vertical = match value.get("orientation").and_then(|v| v.as_str()) {
                    Some("vertical") | Some("v") => true,
                    None | Some("horizontal") | Some("h") => false,
                    Some(x) => {
                        error!("Invalid orientation: '{}'", x);
                        false
                    }
                };
                let items = [value.get("item"), value.get("items")]
                    .iter()
                    .filter_map(Option::as_deref)
                    .filter_map(|v| v.as_array())
                    .flatten()
                    .enumerate()
                    .map(|(i, v)| {
                        ModuleContext {
                            parent: &format!("{}[{i}]", ctx.parent).into(),
                            prefer_ref: true,
                        }
                        .item_from_value(v)
                    })
                    .map(Rc::new)
                    .collect();

                Module::Group {
                    condition,
                    items,
                    tooltip,
                    spacing,
                    vertical,
                }
            }
            Some("icon") => {
                let name = value
                    .get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or_else(|| {
                        error!("Icon requires a name expression");
                        ""
                    })
                    .into();
                let fallback = toml_to_string(value.get("fallback"))
                    .unwrap_or_default()
                    .into();
                let tooltip = value
                    .get("tooltip")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .into();
                Module::Icon {
                    name,
                    fallback,
                    tooltip,
                }
            }
            Some("list") => {
                let wrap = value
                    .get("wrap")
                    .and_then(toml::Value::as_bool)
                    .unwrap_or(false);
                if let Some(choices) = value
                    .get("values")
                    .and_then(|v| v.as_array())
                    .map(|v| v.iter().map(toml::Value::as_str))
                    .into_iter()
                    .flatten()
                    .map(|v| v.map(Box::from))
                    .collect::<Option<Box<[_]>>>()
                {
                    if choices.is_empty() {
                        return Module::parse_error("A list item cannot be empty");
                    }
                    let def = match value.get("default").map(toml::Value::as_integer) {
                        Some(Some(i)) if i >= 1 && i <= choices.len() as _ => i as usize - 1,
                        None => 0,
                        _ => {
                            return Module::parse_error("Invalid default value");
                        }
                    };
                    Module::List {
                        value: Cell::new(def),
                        choices,
                        interested: Default::default(),
                        wrap,
                    }
                } else {
                    Module::parse_error("A list of values is required")
                }
            }
            Some("meter") => {
                let min = toml_to_string(value.get("min")).unwrap_or_default().into();
                let max = toml_to_string(value.get("max")).unwrap_or_default().into();
                let src = match value.get("src").or_else(|| value.get("source")) {
                    Some(item) => Box::new(Module::from_toml_in(item, ctx.with_ref_preferred())),
                    None => {
                        return Module::parse_error("Meter requires a source expression");
                    }
                };
                let mut values = match Some(Some(""))
                    .into_iter()
                    .chain(
                        value
                            .get("values")
                            .and_then(|v| v.as_array())
                            .map(|v| v.iter().map(toml::Value::as_str))
                            .into_iter()
                            .flatten(),
                    )
                    .chain(Some(Some("")))
                    .map(|v| v.map(Box::from))
                    .collect::<Option<Box<[_]>>>()
                {
                    Some(v) if v.len() > 2 => v,
                    _ => {
                        return Module::parse_error("Meter requires an array of string values");
                    }
                };
                let e = values.len() - 1;
                values[0] = value
                    .get("below")
                    .and_then(|v| v.as_str())
                    .unwrap_or(&values[1])
                    .into();
                values[e] = value
                    .get("above")
                    .and_then(|v| v.as_str())
                    .unwrap_or(&values[e - 1])
                    .into();
                Module::Meter {
                    min,
                    max,
                    src,
                    values,
                }
            }
            #[cfg(feature = "dbus")]
            Some("mpris") => {
                let target = toml_to_string(value.get("name")).unwrap_or_default().into();
                Module::MediaPlayer2 { target }
            }
            Some("pipewire") => {
                let target = toml_to_string(value.get("target"))
                    .unwrap_or_default()
                    .into();
                Module::Pipewire { target }
            }
            #[cfg(feature = "pulse")]
            Some("pulse") => {
                let target = toml_to_string(value.get("target"))
                    .unwrap_or_default()
                    .into();
                Module::Pulse { target }
            }
            Some("regex") => {
                let text = value
                    .get("text")
                    .and_then(|v| v.as_str())
                    .unwrap_or_else(|| {
                        error!("Regex requires a text expression");
                        ""
                    })
                    .into();
                let replace = value
                    .get("replace")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .into();
                let regex = value
                    .get("regex")
                    .and_then(|v| v.as_str())
                    .unwrap_or_else(|| {
                        error!("Regex requires a regex expression");
                        ""
                    });
                match regex::RegexBuilder::new(&regex)
                    .dot_matches_new_line(true)
                    .build()
                {
                    Ok(regex) => Module::Regex {
                        regex,
                        text,
                        replace,
                    },
                    Err(e) => Module::parse_error(format!("Error compiling regex '{regex}': {e}")),
                }
            }
            Some("read-file") => {
                let name;
                if let Some(file) = toml_to_string(value.get("file")) {
                    name = file.into_boxed_str();
                } else if let Some(file) = toml_to_string(value.get("path")) {
                    if let Some((p, extra)) = glob_expand(&file) {
                        name = p.into_owned().into_boxed_str();
                        if extra {
                            warn!("Multiple matches found for glob '{file}', using '{name}'");
                        }
                    } else {
                        return Module::parse_error(format!("No matches found for glob '{file}'"));
                    }
                } else {
                    return Module::parse_error(format!("Read-file requires a file name: {value}"));
                };
                let on_err = value
                    .get("on-err")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default()
                    .into();
                let poll = Periodic::new(
                    toml_to_f64(value.get("poll")).unwrap_or(60.0),
                    (name, Cell::new(None)),
                );
                Module::ReadFile { on_err, poll }
            }
            Some("sway-mode") => Module::SwayMode(sway::Mode::from_toml(value)),
            Some("sway-tree") => Module::SwayTree(sway::Tree::from_toml(value, ctx)),
            Some("sway-workspace") => Module::SwayWorkspace(sway::Workspace::from_toml(value)),
            Some("switch") => {
                let format = if let Some(item) = value.get("format") {
                    Box::new(Module::from_toml_in(item, ctx.with_text_preferred()))
                } else if let Some(item) = value.get("source") {
                    Box::new(Module::from_toml_in(item, ctx.with_ref_preferred()))
                } else {
                    return Module::parse_error("'switch' requires a 'format' or 'source' item");
                };
                let cases = match value.get("cases") {
                    Some(toml::Value::Table(cases)) => cases.clone(),
                    _ => {
                        return Module::parse_error("'cases' must be a table in the 'switch' type");
                    }
                };
                let default = toml_to_string(value.get("default"))
                    .unwrap_or_default()
                    .into();
                Module::Switch {
                    format,
                    cases,
                    default,
                }
            }
            // "text" is an alias for "formatted"
            Some("thermal") => {
                let label;
                let path;
                if let Some(file) = toml_to_string(value.get("file")) {
                    path = file.into_boxed_str();
                    label = None;
                } else if let Some(file) = toml_to_string(value.get("path")) {
                    if let Some((p, extra)) = glob_expand(&file) {
                        path = p.into_owned().into_boxed_str();
                        if extra {
                            warn!("Multiple matches found for glob '{file}', using '{path}'");
                        }
                    } else {
                        return Module::parse_error(format!("No matches found for glob '{file}'"));
                    }
                    label = None;
                } else if let Some(name) = toml_to_string(value.get("name")) {
                    use once_cell::sync::OnceCell;
                    static TEMP_NAMES: OnceCell<Vec<(Box<str>, Box<str>)>> = OnceCell::new();
                    path = match TEMP_NAMES
                        .get_or_init(|| {
                            fs::read_dir("/sys/class/hwmon")
                                .into_iter()
                                .flatten()
                                .filter_map(Result::ok)
                                .filter(|e| match e.file_name().to_str() {
                                    Some(n) => n.starts_with("hwmon"),
                                    None => false,
                                })
                                .filter_map(|e| fs::read_dir(e.path()).ok())
                                .flatten()
                                .filter_map(Result::ok)
                                .filter(|e| match e.file_name().to_str() {
                                    Some(n) => n.starts_with("temp") && n.ends_with("_label"),
                                    None => false,
                                })
                                .filter_map(|e| {
                                    let path = e.path();
                                    let mut name = fs::read_to_string(&path).ok()?;
                                    name.pop();
                                    let path = path.into_os_string().into_string().ok()?;
                                    let path = format!("{}_input", path.strip_suffix("_label")?);
                                    debug!("{path}: {name}");
                                    Some((name.into_boxed_str(), path.into_boxed_str()))
                                })
                                .collect()
                        })
                        .iter()
                        .find(|(n, _)| **n == name)
                    {
                        Some((_, path)) => path.clone(),
                        None => {
                            return Module::parse_error(format!("Sensor '{name}' not found"));
                        }
                    };
                    label = Some(name.into());
                } else {
                    return Module::parse_error("'thermal' requires a 'file' or 'name'");
                };

                let poll = Periodic::new(
                    toml_to_f64(value.get("poll")).unwrap_or(60.0),
                    (path, Cell::new(0)),
                );

                Module::Thermal { poll, label }
            }
            Some("tray") => {
                let active = Rc::new(ctx.opt_item_from_key(value, "item").unwrap_or_else(|| {
                    ctx.item_from(Module::Icon {
                        name: "{item.icon}".into(),
                        fallback: "{item.title}".into(),
                        tooltip: "".into(),
                    })
                }));
                let passive = Rc::new(ctx.item_from_key(value, "passive"));
                let urgent = ctx
                    .opt_item_from_key(value, "urgent")
                    .map(Rc::new)
                    .unwrap_or_else(|| active.clone());
                Module::Tray {
                    passive,
                    active,
                    urgent,
                }
            }
            Some("value") => {
                Module::new_value(value.get("value").cloned().map_or(Value::Null, Into::into))
            }
            Some(t) => Module::parse_error(format!("Unknown module type '{t}'")),
            None => {
                if let Some(value) = value.as_str() {
                    if ctx.prefer_ref && value.contains('{') {
                        Module::Formatted {
                            format: value.into(),
                            tooltip: None,
                        }
                    } else if ctx.prefer_ref {
                        Module::ItemReference {
                            value: Cell::new(ItemReference::New(value.into())),
                        }
                    } else {
                        Module::Formatted {
                            format: value.into(),
                            tooltip: None,
                        }
                    }
                } else if let Some(format) = value.get("format").and_then(|v| v.as_str()) {
                    let tooltip = ctx
                        .with_text_preferred()
                        .opt_item_from_key(value, "tooltip")
                        .map(Rc::new);

                    Module::Formatted {
                        format: format.into(),
                        tooltip,
                    }
                } else if let Some(value) = value.get("value") {
                    Module::new_value(value.clone())
                } else {
                    Module::parse_error("The 'type' value is required")
                }
            }
        }
    }

    pub fn new_value<T: Into<Value<'static>>>(t: T) -> Self {
        Module::Value {
            value: Cell::new(t.into()),
            interested: Default::default(),
        }
    }

    pub fn new_format<T: Into<String>>(t: T) -> Self {
        Module::Formatted {
            format: t.into().into(),
            tooltip: None,
        }
    }

    pub fn new_eval(value: &toml::Value, ctx: ModuleContext) -> Self {
        match value
            .get("expr")
            .and_then(|v| v.as_str())
            .map(|expr| evalexpr::build_operator_tree(&expr))
        {
            Some(Ok(expr)) => {
                let mut vars = Vec::new();
                for ident in expr.iter_variable_identifiers() {
                    if vars.iter().find(|(k, _)| k as &str == ident).is_some() {
                        continue;
                    }
                    match value.get(ident) {
                        Some(value) => {
                            let value = Module::from_toml_in(value, ctx.with_ref_preferred());
                            vars.push((ident.into(), value));
                        }
                        None => {
                            return Module::parse_error(format!(
                                "Undefined variable '{ident}' in expression"
                            ));
                        }
                    }
                }
                Module::Eval { expr, vars }
            }
            Some(Err(e)) => Module::parse_error(format!("Could not parse expression: {e}")),
            None => Module::parse_error("Eval blocks require an expression"),
        }
    }

    /// One-time setup, if needed, and migration from before a reload
    pub fn init(&self, name: &str, _rt: &Runtime, from: Option<&Self>) {
        match (self, from) {
            (
                Module::ExecJson {
                    command,
                    stdin,
                    value,
                    handle,
                },
                Some(Module::ExecJson {
                    command: old_cmd,
                    stdin: old_stdin,
                    value: old_value,
                    handle: old_handle,
                }),
            ) if *command == *old_cmd => {
                stdin.set(old_stdin.take());
                value.set(old_value.take());
                handle.set(old_handle.take());
            }

            (
                Module::ExecJson {
                    command,
                    stdin,
                    value,
                    handle,
                },
                _,
            ) => {
                match Command::new("/bin/sh")
                    .arg("-c")
                    .arg(&**command)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Err(e) => error!("Could not execute {}: {}", command, e),
                    Ok(mut child) => {
                        let rc = Rc::new((Cell::new(JsonValue::Null), Default::default()));
                        let pipe_in = child.stdin.take().unwrap();
                        let fd = child.stdout.take().unwrap().into_raw_fd();
                        unsafe {
                            libc::fcntl(pipe_in.as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK);
                        }
                        unsafe {
                            libc::fcntl(fd, libc::F_SETFL, libc::O_NONBLOCK);
                        }
                        stdin.set(Some(pipe_in));
                        value.set(Some(rc.clone()));
                        handle.set(Some(spawn_handle(
                            "ExecJson",
                            do_exec_json(fd, name.to_owned(), rc),
                        )));
                    }
                }
            }
            (Module::Value { value, .. }, Some(Module::Value { value: cur, .. })) => {
                value.set(cur.take());
            }
            (Module::List { value, choices, .. }, Some(Module::List { value: cur, .. })) => {
                let i = cur.get();
                if i < choices.len() {
                    value.set(i);
                }
            }
            _ => {}
        }
    }

    /// Read the value of a variable
    ///
    /// This is identical to read_in, but returns a Value instead of using a callback closure.
    pub fn read_to_owned(&self, name: &str, key: &str, rt: &Runtime) -> Value<'static> {
        self.read_in(name, key, rt, |s| s.into_owned())
    }

    /// Read the value of a variable
    ///
    /// The provided closure should be passed the value of the variable.  This is done instead of
    /// returning the value to avoid unneeded string copies.
    ///
    /// Note: The name is a hint and should not be assumed to uniquely identify this module.
    pub fn read_in<F: FnOnce(Value) -> R, R>(
        &self,
        name: &str,
        key: &str,
        rt: &Runtime,
        f: F,
    ) -> R {
        let _handle = match rt.get_recursion_handle() {
            Some(r) => r,
            None => {
                error!("Loop found when evaluating '{}.{}'", name, key);
                return f(Value::Null);
            }
        };

        match self {
            Module::Group { .. }
            | Module::Fade { .. }
            | Module::FocusList { .. }
            | Module::FontTest { .. }
            | Module::Tray { .. } => {
                error!("Cannot use '{}' in a text expansion", name);
                f(Value::Null)
            }

            Module::Bar { data, .. } => match toml_to_string(data.config.get(key)) {
                Some(value) => f(Value::Owned(value)),
                None => f(Value::Null),
            },
            Module::Calendar {
                day_fmt,
                today_fmt,
                other_fmt,
                zone,
                monday,
            } => {
                use chrono::{Datelike, Duration};
                use std::fmt::Write;
                let real_zone = rt.format_or(&zone, &name).into_text();
                let now = match real_zone.parse::<chrono_tz::Tz>() {
                    Ok(tz) => chrono::Utc::now().with_timezone(&tz).date_naive(),
                    Err(_) => chrono::Local::now().date_naive(),
                };
                let day1 = now - Duration::days(now.day0() as i64);
                let mut pre_offset = if *monday {
                    day1.weekday().num_days_from_monday()
                } else {
                    day1.weekday().num_days_from_sunday()
                } as i64;
                if pre_offset < 3 && now.day() < 10 {
                    pre_offset += 7;
                }
                let mut date = day1 - Duration::days(pre_offset);
                let mut rv = String::with_capacity(3 * 7 * 6 + 6);
                for _week in 0..6 {
                    for _day in 0..7 {
                        if date.month() != now.month() {
                            write!(rv, "{}", date.format(&other_fmt)).unwrap();
                        } else if date.day() != now.day() {
                            write!(rv, "{}", date.format(&day_fmt)).unwrap();
                        } else {
                            write!(rv, "{}", date.format(&today_fmt)).unwrap();
                        }
                        date = date + Duration::days(1);
                    }
                    rv.push('\n');
                }
                rv.pop();
                f(Value::Owned(rv))
            }
            Module::Calendar2 {
                day_fmt,
                today_fmt,
                other_fmt,
                zone,
                past,
                future,
                monday,
            } => {
                use chrono::{Datelike, Duration};
                use std::fmt::Write;
                let real_zone = rt.format_or(&zone, &name).into_text();
                let now = match real_zone.parse::<chrono_tz::Tz>() {
                    Ok(tz) => chrono::Utc::now().with_timezone(&tz).date_naive(),
                    Err(_) => chrono::Local::now().date_naive(),
                };
                let day1 = now - Duration::days(*past as i64 * 7);
                let pre_offset = if *monday {
                    day1.weekday().num_days_from_monday()
                } else {
                    day1.weekday().num_days_from_sunday()
                } as i64;
                let mut date = day1 - Duration::days(pre_offset);
                let mut rv = String::with_capacity(3 * 7 * 6 + 6);
                let weeks = past + future;
                for _week in 0..=weeks {
                    for _day in 0..7 {
                        if date.month() != now.month() {
                            write!(rv, "{}", date.format(&other_fmt)).unwrap();
                        } else if date.day() != now.day() {
                            write!(rv, "{}", date.format(&day_fmt)).unwrap();
                        } else {
                            write!(rv, "{}", date.format(&today_fmt)).unwrap();
                        }
                        date = date + Duration::days(1);
                    }
                    rv.push('\n');
                }
                rv.pop();
                f(Value::Owned(rv))
            }
            Module::Clipboard { state } => state.read_in(name, key, rt, f),
            Module::Clock {
                format,
                zone,
                state,
            } => {
                let real_format = rt.format_or(&format, &name).into_text();
                let real_zone = rt.format_or(&zone, &name).into_text();

                // offset by 5ms as a (low) estimate of the frame rate
                // this means we might rendering about 4ms prior to the actual tick
                let now = chrono::Utc::now() + chrono::Duration::milliseconds(5);
                let inow = Instant::now();
                let subsec = chrono::Timelike::nanosecond(&now) as u64;
                let next_sec = now + chrono::Duration::seconds(1);
                let (value, nv);
                if real_zone.is_empty() {
                    value = format!("{}", now.with_timezone(&chrono::Local).format(&real_format));
                    nv = format!(
                        "{}",
                        next_sec.with_timezone(&chrono::Local).format(&real_format)
                    );
                } else {
                    match real_zone.parse::<chrono_tz::Tz>() {
                        Ok(tz) => {
                            value = format!("{}", now.with_timezone(&tz).format(&real_format));
                            nv = format!("{}", next_sec.with_timezone(&tz).format(&real_format));
                        }
                        Err(e) => {
                            warn!("Could not find timezone '{}': {}", real_zone, e);
                            return f(Value::Null);
                        }
                    }
                }

                // Set a timer to expire when the subsecond offset will be zero
                // add another 1ms delay because epoll only gets 1ms granularity
                let delay;
                if value != nv {
                    // we need to tick every second
                    delay = 1_000_999_999u64.checked_sub(subsec);
                } else {
                    // the displayed text in one second is the same; tick every minute
                    // (really, we could continue on, but that's unlikely to help)
                    let sec = chrono::Timelike::second(&now) as u64;
                    delay = (1_000_000_000 * (60 - sec) + 999_999).checked_sub(subsec);
                }
                let wake = inow + delay.map_or(Duration::from_secs(1), Duration::from_nanos);
                state.wake_at(wake, rt);

                f(Value::Owned(value))
            }
            #[cfg(feature = "dbus")]
            Module::DbusCall { poll } => {
                poll.read_refresh_async(rt, move |rc| rc.clone().do_call());
                poll.data().read_in(key, rt, f)
            }
            Module::Disk { poll } => {
                poll.read_refresh(rt, |(path, contents)| {
                    let cstr = std::ffi::CString::new(path.as_bytes()).unwrap();
                    let rv = unsafe { libc::statvfs(cstr.as_ptr(), contents.as_ptr()) };
                    if rv != 0 {
                        warn!(
                            "Could not read disk at '{}': {}",
                            path,
                            std::io::Error::last_os_error()
                        );
                    }
                    Some(path)
                });
                let vfs = poll.data().1.get();
                match key {
                    "size" => f(Value::Float((vfs.f_frsize * vfs.f_blocks) as f64)),
                    "free" => f(Value::Float((vfs.f_bsize * vfs.f_bfree) as f64)),
                    "avail" => f(Value::Float((vfs.f_bsize * vfs.f_bavail) as f64)),

                    "size-mb" => f(Value::Float(
                        (vfs.f_frsize * vfs.f_blocks) as f64 / 1_000_000.0,
                    )),
                    "free-mb" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bfree) as f64 / 1_000_000.0,
                    )),
                    "avail-mb" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bavail) as f64 / 1_000_000.0,
                    )),

                    "size-gb" => f(Value::Float(
                        (vfs.f_frsize * vfs.f_blocks) as f64 / 1_000_000_000.0,
                    )),
                    "free-gb" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bfree) as f64 / 1_000_000_000.0,
                    )),
                    "avail-gb" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bavail) as f64 / 1_000_000_000.0,
                    )),

                    "size-tb" => f(Value::Float(
                        (vfs.f_frsize * vfs.f_blocks) as f64 / 1_000_000_000_000.0,
                    )),
                    "free-tb" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bfree) as f64 / 1_000_000_000_000.0,
                    )),
                    "avail-tb" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bavail) as f64 / 1_000_000_000_000.0,
                    )),

                    "size-mib" => f(Value::Float(
                        (vfs.f_frsize * vfs.f_blocks) as f64 / 1048576.0,
                    )),
                    "free-mib" => f(Value::Float((vfs.f_bsize * vfs.f_bfree) as f64 / 1048576.0)),
                    "avail-mib" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bavail) as f64 / 1048576.0,
                    )),

                    "size-gib" => f(Value::Float(
                        (vfs.f_frsize * vfs.f_blocks) as f64 / 1073741824.0,
                    )),
                    "free-gib" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bfree) as f64 / 1073741824.0,
                    )),
                    "avail-gib" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bavail) as f64 / 1073741824.0,
                    )),

                    "size-tib" => f(Value::Float(
                        (vfs.f_frsize * vfs.f_blocks) as f64 / 1099511627776.0,
                    )),
                    "free-tib" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bfree) as f64 / 1099511627776.0,
                    )),
                    "avail-tib" => f(Value::Float(
                        (vfs.f_bsize * vfs.f_bavail) as f64 / 1099511627776.0,
                    )),

                    "" | "text" | "percent-used" if vfs.f_frsize * vfs.f_blocks != 0 => {
                        let size = (vfs.f_frsize * vfs.f_blocks) as f64;
                        let free = (vfs.f_bsize * vfs.f_bfree) as f64;
                        let pct = 100.0 - 100.0 * free / size;
                        if key == "" || key == "text" {
                            f(Value::Owned(format!("{:.0}%", pct)))
                        } else {
                            f(Value::Float(pct))
                        }
                    }
                    _ => f(Value::Null),
                }
            }
            Module::Eval { expr, vars } => {
                let ctx = EvalContext {
                    rt: &rt,
                    vars: vars
                        .iter()
                        .map(|(k, v)| (&k[..], v.read_to_owned(k, "", rt).into()))
                        .collect(),
                };
                match expr.eval_with_context(&ctx) {
                    Ok(e) => f(e.into()),
                    Err(e) => {
                        warn!("Eval error in {}: {}", name, e);
                        f(Value::Null)
                    }
                }
            }
            Module::ExecJson { command, value, .. } => {
                let value = value.take_in_some(|v| v.clone()).unwrap();
                let v = value.0.replace(JsonValue::Null);
                let rv = match &v {
                    JsonValue::Object(obj) => match obj.get(key) {
                        Some(v) => f(v.into()),
                        None => {
                            debug!(
                                "Could not find {}.{} in the output of {}",
                                name, key, command
                            );
                            f(Value::Null)
                        }
                    },
                    _ => f(Value::Null),
                };
                value.0.set(v);
                value.1.add(rt);
                rv
            }
            Module::Formatted { format, tooltip } => match key {
                "tooltip" => match tooltip {
                    Some(tt) => tt.data.read_in(name, "text", rt, f),
                    None => f(Value::Null),
                },
                _ => f(rt.format_or(&format, &name)),
            },
            Module::Icon { tooltip, .. } => match key {
                "tooltip" => f(rt.format_or(&tooltip, &name)),
                _ => f(Value::Null),
            },
            Module::Item { value } => value.take_in(|item| match item.as_ref() {
                #[cfg(feature = "dbus")]
                Some(IterationItem::MediaPlayer2 { target }) => {
                    mpris::read_in(name, target, key, rt, f)
                }
                Some(IterationItem::Pipewire { target }) => {
                    pipewire::read_in(name, target, key, rt, f)
                }
                #[cfg(feature = "pulse")]
                Some(IterationItem::Pulse { target }) => pulse::read_in(name, target, key, rt, f),
                Some(IterationItem::SwayWorkspace(data)) => data.read_in(key, rt, f),
                Some(IterationItem::SwayTreeItem(node)) => node.read_in(key, rt, f),
                #[cfg(feature = "dbus")]
                Some(IterationItem::Tray(item)) => tray::read_in(name, item, key, rt, f),
                None => f(Value::Null),
            }),
            Module::ItemReference { value } => ItemReference::with(value, rt, |item| match item {
                Some(item) => item.data.read_in(name, key, rt, f),
                None => f(Value::Null),
            }),
            Module::List {
                value,
                choices,
                interested,
                ..
            } => {
                interested.add(rt);
                let i = value.get();
                let v = choices.get(i).unwrap_or(&choices[0]);
                f(Value::Borrow(v))
            }
            #[cfg(feature = "dbus")]
            Module::MediaPlayer2 { target } => mpris::read_in(name, target, key, rt, f),
            Module::Meter {
                min,
                max,
                src,
                values,
            } => {
                let value = src.read_to_owned(&name, "", rt).parse_f64().unwrap_or(0.0);
                let min = rt.format_or(&min, &name).parse_f64().unwrap_or(0.0);
                let max = rt.format_or(&max, &name).parse_f64().unwrap_or(100.0);
                let steps = values.len() - 2;
                let step = (max - min) / (steps as f64);
                let expr = if value < min {
                    &values[0]
                } else if value > max {
                    &values[values.len() - 1]
                } else {
                    let i = (value - min) / step;
                    &values[i as usize + 1]
                };
                f(rt.format_or(&expr, &name))
            }
            Module::ParseError { msg } => {
                if let Some(msg) = msg.take() {
                    log::debug!("{msg}");
                }
                f(Value::Null)
            }
            Module::Pipewire { target } => pipewire::read_in(name, target, key, rt, f),
            #[cfg(feature = "pulse")]
            Module::Pulse { target } => pulse::read_in(name, target, key, rt, f),
            Module::ReadFile { on_err, poll } => {
                use std::io::Read;
                poll.read_refresh(rt, move |(name, contents)| {
                    let mut v = String::with_capacity(4096);
                    match std::fs::File::open(&**name).and_then(|mut f| f.read_to_string(&mut v)) {
                        Ok(_len) => {
                            if contents.take_in(|prev| Some(&v) == prev.as_ref()) {
                                None
                            } else {
                                contents.set(Some(v));
                                Some(name)
                            }
                        }
                        Err(e) => {
                            debug!("Could not read {}: {}", name, e);
                            if contents.take().is_some() {
                                Some(name)
                            } else {
                                None
                            }
                        }
                    }
                });
                let (_, contents) = poll.data();
                if key == "raw" {
                    contents.take_in(|s| f(s.as_deref().map_or(Value::Null, Value::Borrow)))
                } else {
                    contents.take_in(|s| match s {
                        Some(s) => f(Value::Borrow(s.trim())),
                        None => f(rt.format_or(&on_err, &name)),
                    })
                }
            }
            Module::Regex {
                regex,
                text,
                replace,
            } => {
                let text = rt.format_or(&text, &name).into_text();
                if key == "" || key == "text" {
                    let output = regex.replace_all(&text, &**replace);
                    f(output.into())
                } else {
                    if let Some(cap) = regex.captures(&text) {
                        if let Ok(i) = key.parse() {
                            f(Value::Borrow(cap.get(i).map_or("", |m| m.as_str())))
                        } else {
                            f(Value::Borrow(cap.name(key).map_or("", |m| m.as_str())))
                        }
                    } else {
                        f(Value::Null)
                    }
                }
            }
            Module::SwayMode(mode) => mode.read_in(name, key, rt, f),
            Module::SwayTree(tree) => tree.read_in(name, key, rt, f),
            Module::SwayWorkspace(ws) => ws.read_in(name, key, rt, f),
            Module::Switch {
                format,
                cases,
                default,
            } => {
                let text = format.read_to_owned(&name, "", rt).into_text();
                let case = toml_to_string(cases.get(&text[..]));
                let case = case.as_deref().unwrap_or(default);
                let res = rt.format_or(case, &name);
                f(res)
            }
            Module::Thermal { poll, label } => {
                match key {
                    "label" => return f(label.as_deref().map_or(Value::Null, Value::Borrow)),
                    _ => {}
                }
                poll.read_refresh(rt, move |(name, value)| match fs::read_to_string(&**name) {
                    Ok(mut s) => {
                        s.pop();
                        match s.parse() {
                            Ok(v) => value.set(v),
                            _ => {
                                debug!("Invalid value '{}' read from {}", s, name);
                                value.set(0);
                            }
                        }
                        Some(name)
                    }
                    Err(e) => {
                        debug!("Could not read {}: {}", name, e);
                        value.set(0);
                        Some(name)
                    }
                });
                let (_, value) = poll.data();
                f(Value::Float(value.get() as f64 / 1000.0))
            }
            Module::Value { value, interested } => {
                interested.add(rt);
                value.take_in(|s| f(s.as_ref()))
            }
        }
    }

    /// Handle a write or send to the variable
    pub fn write(&self, name: &str, key: &str, value: Value, rt: &Runtime) {
        debug!("Writing {} to {}.{}", value, name, key);
        match self {
            Module::ExecJson { stdin, .. } => {
                let w = match stdin.take() {
                    None => {
                        warn!("Not writing to closed exec-json stream");
                        return;
                    }
                    Some(w) => w,
                };
                let mut json = JsonValue::new_object();
                json.insert(key, value).unwrap();
                let mut line: Vec<u8> = Vec::new();
                json.write(&mut line).unwrap();
                line.push(b'\n');
                // Note: this will return WouldBlock instead of blocking, in which case we stop
                // sending instead of blocking the GUI or buffering messages.
                //
                // This could be changed to write via the event loop if needed.
                match (&w).write_all(&line) {
                    Ok(()) => {
                        stdin.set(Some(w));
                    }
                    Err(e) => {
                        warn!("Could not write to '{}' : {}", name, e);
                    }
                }
            }
            Module::FontTest { offset, interested } => {
                let value = value.into_text();
                let off = if value.len() == 1 {
                    Some(16)
                } else {
                    value.get(1..).and_then(|v| v.parse::<u16>().ok())
                };
                match (value.chars().next(), off) {
                    (Some('-'), Some(off)) => {
                        offset.set(offset.get().saturating_sub(off));
                    }
                    (Some('+'), Some(off)) => {
                        offset.set(offset.get().saturating_add(off));
                    }
                    _ => {
                        warn!("Could not parse offset {value}");
                        return;
                    }
                }
                interested.notify_data("value");
            }
            Module::Item { value: v } => v.take_in(|item| match item.as_ref() {
                #[cfg(feature = "dbus")]
                Some(IterationItem::MediaPlayer2 { target }) => {
                    mpris::write(name, target, key, value, rt)
                }
                Some(IterationItem::Pipewire { target }) => {
                    pipewire::do_write(name, target, key, value, rt)
                }
                #[cfg(feature = "pulse")]
                Some(IterationItem::Pulse { target }) => {
                    pulse::do_write(name, target, key, value, rt)
                }
                Some(IterationItem::SwayWorkspace(data)) => data.write(key, value, rt),
                Some(IterationItem::SwayTreeItem(node)) => node.write(key, value, rt),
                #[cfg(feature = "dbus")]
                Some(IterationItem::Tray(item)) => tray::write(name, item, key, value, rt),
                None => {}
            }),
            Module::List {
                value: v,
                choices,
                interested,
                wrap,
            } => {
                let max = choices.len();
                let prev = v.get();
                let value = value.into_text();
                let off = if value.len() == 1 {
                    Some(1)
                } else {
                    value.get(1..).and_then(|v| v.parse::<usize>().ok())
                };
                match (value.chars().next(), off) {
                    (Some('-'), Some(off)) => {
                        if *wrap {
                            let off = max - (off % max);
                            v.set((prev + off) % max);
                        } else {
                            v.set(prev.saturating_sub(off));
                        }
                    }
                    (Some('+'), Some(off)) => {
                        if *wrap {
                            v.set((prev + off) % max);
                        } else {
                            v.set(max.min(prev + off));
                        }
                    }
                    (Some('='), Some(value)) => {
                        v.set(value.saturating_sub(1) % max);
                    }
                    _ => {
                        warn!("Could not parse offset {value}");
                        return;
                    }
                }
                interested.notify_data("value");
            }
            #[cfg(feature = "dbus")]
            Module::MediaPlayer2 { target } => mpris::write(name, target, key, value, rt),
            Module::Pipewire { target } => pipewire::do_write(name, target, key, value, rt),
            #[cfg(feature = "pulse")]
            Module::Pulse { target } => pulse::do_write(name, target, key, value, rt),
            Module::SwayMode(_) => sway::write(value, rt),
            Module::SwayTree(_) => sway::write(value, rt),
            Module::SwayWorkspace(ws) => ws.write(name, key, value, rt),
            Module::Value {
                value: v,
                interested,
            } if key == "" => {
                interested.notify_data("value");
                v.set(value.into_owned());
            }
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }

    pub fn parse_error(msg: impl Into<Cow<'static, str>>) -> Self {
        Module::ParseError {
            msg: Cell::new(Some(msg.into())),
        }
    }

    pub fn new_current_item() -> Self {
        Module::Item {
            value: Cell::new(None),
        }
    }

    /// Use this module as the source of a focus-list item
    pub fn read_focus_list<F: FnMut(bool, IterationItem)>(&self, rt: &Runtime, f: F) {
        match self {
            #[cfg(feature = "dbus")]
            Module::MediaPlayer2 { .. } => mpris::read_focus_list(rt, f),
            Module::SwayWorkspace(ws) => ws.read_focus_list(rt, f),
            Module::Pipewire { target } => pipewire::read_focus_list(rt, target, f),
            #[cfg(feature = "pulse")]
            Module::Pulse { target } => pulse::read_focus_list(rt, target, f),
            Module::ItemReference { value } => {
                ItemReference::with(value, rt, |v| match v {
                    Some(item) => {
                        item.data.read_focus_list(rt, f);
                    }
                    None => {}
                });
            }
            _ => (),
        }
    }
}

#[derive(Debug, Default)]
pub struct ClockState {
    interested: NotifierList,
    wake: Cell<Option<Instant>>,
    task: Cell<Option<RemoteHandle<()>>>,
}

impl ClockState {
    fn wake_at(self: &Rc<Self>, wake: Instant, rt: &Runtime) {
        self.interested.add(rt);
        if matches!(self.wake.get(), Some(time) if time <= wake) {
            return;
        }

        let this = self.clone();
        self.wake.set(Some(wake));
        self.task.set(Some(spawn_handle("Clock tick", async move {
            tokio::time::sleep_until(wake.into()).await;
            this.interested.notify_data("clock");
            this.wake.set(None);
            this.task.set(None);
            Ok(())
        })));
    }
}

pub struct EvalContext<'a> {
    pub rt: &'a Runtime,
    pub vars: Vec<(&'a str, evalexpr::Value)>,
}

impl<'a> evalexpr::Context for EvalContext<'a> {
    fn get_value(&self, name: &str) -> Option<&evalexpr::Value> {
        self.vars
            .iter()
            .filter(|&&(k, _)| k == name)
            .next()
            .map(|(_, v)| v)
    }
    fn call_function(
        &self,
        name: &str,
        arg: &evalexpr::Value,
    ) -> evalexpr::EvalexprResult<evalexpr::Value> {
        match name {
            "float" => {
                if arg.is_float() {
                    return Ok(arg.clone());
                }
                let rv = arg.as_string()?;
                match rv.trim().parse() {
                    Ok(v) => Ok(evalexpr::Value::Float(v)),
                    Err(_) => Err(evalexpr::error::EvalexprError::ExpectedFloat {
                        actual: evalexpr::Value::String(rv),
                    }),
                }
            }
            "int" => {
                if let Ok(f) = arg.as_float() {
                    return Ok(evalexpr::Value::Int(f as _));
                }
                let rv = arg.as_string()?;
                match rv.trim().parse() {
                    Ok(v) => Ok(evalexpr::Value::Int(v)),
                    Err(_) => Err(evalexpr::error::EvalexprError::ExpectedInt {
                        actual: evalexpr::Value::String(rv),
                    }),
                }
            }
            "get" => {
                let key = arg.as_string()?;
                let (name, key) = match key.find('.') {
                    Some(p) => (&key[..p], &key[p + 1..]),
                    None => (&key[..], ""),
                };
                if let Some(item) = self.rt.items.get(name) {
                    Ok(item.data.read_to_owned(name, key, self.rt).into())
                } else {
                    Ok(evalexpr::Value::Empty)
                }
            }
            _ => Err(evalexpr::error::EvalexprError::FunctionIdentifierNotFound(
                name.into(),
            )),
        }
    }
    fn are_builtin_functions_disabled(&self) -> bool {
        false
    }
    fn set_builtin_functions_disabled(&mut self, _: bool) -> evalexpr::EvalexprResult<()> {
        Err(evalexpr::error::EvalexprError::BuiltinFunctionsCannotBeDisabled)
    }
}

use std::error::Error;
async fn do_exec_json(
    fd: i32,
    name: String,
    value: Rc<(Cell<JsonValue>, NotifierList)>,
) -> Result<(), Box<dyn Error>> {
    let afd = AsyncFd::new(Fd(fd)).expect("Invalid FD from ChildStdin");
    let mut buffer: Vec<u8> = Vec::with_capacity(1024);

    'waiting: loop {
        let mut rh = match afd.readable().await {
            Ok(h) => h,
            Err(e) => {
                warn!("Unable to wait for child read: {}", e);
                return Ok(());
            }
        };
        'reading: loop {
            if buffer.len() == buffer.capacity() {
                buffer.reserve(2048);
            }
            unsafe {
                // child pipe read into vec spare capacity
                let start = buffer.len();
                let max_len = buffer.capacity() - start;
                let rv = libc::read(
                    fd,
                    buffer.as_mut_ptr().offset(start as isize) as *mut _,
                    max_len,
                );
                match rv {
                    0 => {
                        libc::close(fd);
                        return Ok(());
                    }
                    len if rv > 0 && rv <= max_len as _ => {
                        buffer.set_len(start + len as usize);
                    }
                    _ => {
                        let e = io::Error::last_os_error();
                        match e.kind() {
                            io::ErrorKind::Interrupted => continue 'reading,
                            io::ErrorKind::WouldBlock => {
                                rh.clear_ready();
                                continue 'waiting;
                            }
                            _ => {
                                warn!("Got {} on child read; discontinuing", e);
                                return Ok(());
                            }
                        }
                    }
                }
            }
            while let Some(eol) = buffer.iter().position(|&c| c == b'\n') {
                let mut json = None;
                match std::str::from_utf8(&buffer[..eol]) {
                    Err(_) => info!("Ignoring bad UTF8 from '{}'", name),
                    Ok(v) => {
                        debug!("'{}': {}", name, v);
                        match json::parse(v) {
                            Ok(v) => {
                                json = Some(v);
                            }
                            Err(e) => info!("Ignoring bad JSON from '{}': {}", name, e),
                        }
                    }
                }
                // Note: this is optimized for the normal case where the script sends one line
                // at a time, so this drain would empty the buffer.
                buffer.drain(..eol + 1);
                if let Some(json) = json {
                    value.0.set(json);
                    value.1.notify_data("exec-json");
                }
            }
        }
    }
}
