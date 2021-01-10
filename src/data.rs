use crate::item::Item;
use crate::mpris;
use crate::pulse;
use crate::state::NotifierList;
use crate::state::Runtime;
use crate::sway;
use crate::tray;
use crate::util::{Cell,Fd,toml_to_string,toml_to_f64,spawn_noerr};
use evalexpr::Node as EvalExpr;
use futures_util::future::RemoteHandle;
use futures_util::FutureExt;
use json::JsonValue;
use log::{debug,info,warn,error};
use std::io;
use std::io::Write;
use std::os::unix::io::{AsRawFd,IntoRawFd};
use std::process::{Command,Stdio,ChildStdin};
use std::rc::Rc;
use std::time::{Duration,Instant};
use tokio::io::unix::AsyncFd;
use libc;

/// Type-specific part of an [Item]
#[derive(Debug)]
pub enum Module {
    Bar {
        // always three items: left, center, right
        items : Box<[Item;3]>,
        config : toml::Value,
    },
    Clock {
        format : Box<str>,
        zone : Box<str>,
        time : Cell<String>,
    },
    Disk {
        path : Box<str>,
        poll : f64,
        last_read : Cell<Option<Instant>>,
        contents : Cell<libc::statvfs>,
    },
    Eval {
        expr : EvalExpr,
        vars : Vec<(Box<str>, Box<str>)>,
        looped : Cell<bool>,
    },
    ExecJson {
        command : Box<str>,
        stdin : Cell<Option<ChildStdin>>,
        value : Cell<Option<Rc<(Cell<JsonValue>, Cell<NotifierList>)>>>,
        handle : Cell<Option<RemoteHandle<()>>>,
    },
    FocusList {
        source : Box<str>,
        // always two items: non-focused, focused
        items : Box<(Item, Option<Item>)>,
        spacing : Box<str>,
    },
    Formatted {
        format : Box<str>,
        tooltip : Box<str>,
        looped : Cell<bool>,
    },
    Group {
        items : Vec<Item>,
        spacing : Box<str>,
        // TODO crop ordering: allow specific items to be cropped first
        // TODO use min-width to force earlier cropping
    },
    Icon {
        name : Box<str>,
        fallback : Box<str>,
        tooltip : Box<str>,
    },
    Item { // unique variant for the reserved "item" item
        value : Cell<Option<IterationItem>>,
    },
    ItemReference { name : Box<str> },
    MediaPlayer2 { target : Box<str> },
    Meter {
        min : Box<str>,
        max : Box<str>,
        src : Box<str>,
        values : Box<[Box<str>]>,
        looped : Cell<bool>,
    },
    None,
    Pulse {
        target : Box<str>,
    },
    ReadFile {
        name : Box<str>,
        poll : f64,
        last_read : Cell<Option<Instant>>,
        contents : Cell<String>,
    },
    Regex {
        regex : regex::Regex,
        text : Box<str>,
        replace : Box<str>,
        looped : Cell<bool>,
    },
    SwayMode(sway::Mode),
    SwayTree(sway::Tree),
    SwayWorkspace(sway::Workspace),
    Switch {
        format : Box<str>,
        cases : toml::value::Table,
        default : Box<str>,
        looped : Cell<bool>,
    },
    Tray {
        spacing : Box<str>,
    },
    Value {
        value : Cell<String>,
        interested : Cell<NotifierList>,
    },
}

#[derive(Debug,Clone)]
pub enum IterationItem {
    MediaPlayer2 { target : Rc<str> },
    Pulse { target : Rc<str> },
    SwayWorkspace(Rc<sway::WorkspaceData>),
    SwayTreeItem(Rc<sway::Node>),
}

impl Module {
    pub fn from_toml(value : &toml::Value) -> Self {
        match value.get("type").and_then(|v| v.as_str()) {
            // keep values in alphabetical order
            Some("clock") => {
                let format = value.get("format").and_then(|v| v.as_str()).unwrap_or("%H:%M").into();
                let zone = value.get("timezone").and_then(|v| v.as_str()).unwrap_or("").into();
                Module::Clock { format, zone, time : Cell::new(String::new()) }
            }
            Some("disk") => {
                let path = value.get("path").and_then(|v| v.as_str()).unwrap_or("/").into();
                let poll = toml_to_f64(value.get("poll")).unwrap_or(60.0);
                let v : libc::statvfs = unsafe { std::mem::zeroed() };
                Module::Disk { path, poll, last_read: Cell::default(), contents : Cell::new(v) }
            }
            Some("eval") => {
                match value.get("expr")
                    .and_then(|v| v.as_str())
                    .map(|expr| evalexpr::build_operator_tree(&expr))
                {
                    Some(Ok(expr)) => {
                        let mut vars = Vec::new();
                        for ident in expr.iter_variable_identifiers() {
                            if vars.iter().find(|(k, _)| k as &str == ident).is_some() {
                                continue;
                            }
                            match toml_to_string(value.get(ident)) {
                                Some(value) => vars.push((ident.into(), value.into())),
                                None => {
                                    error!("Undefined variable '{}' in expression", ident);
                                    return Module::None;
                                }
                            }
                        }
                        Module::Eval { expr, vars, looped : Cell::new(false) }
                    }
                    Some(Err(e)) => {
                        error!("Could not parse expression: {}", e);
                        Module::None
                    }
                    None => {
                        error!("Eval blocks require an expression");
                        Module::None
                    }
                }
            }
            Some("exec-json") => {
                let command = match value.get("command").and_then(|v| v.as_str()) {
                    Some(cmd) => cmd.into(),
                    None => {
                        error!("Comamnd to execute is required: {}", value);
                        return Module::None;
                    }
                };
                Module::ExecJson {
                    command,
                    stdin : Cell::new(None),
                    value : Cell::new(None),
                    handle : Cell::new(None),
                }
            }
            Some("focus-list") => {
                let source = match value.get("source").and_then(|v| v.as_str()) {
                    Some(s) => s.into(),
                    None => {
                        error!("A source is required for focus-list");
                        return Module::None.into();
                    }
                };
                let spacing = toml_to_string(value.get("spacing")).unwrap_or_default().into();
                let item = value.get("item").map_or_else(Item::none, Item::from_toml_ref);
                let fitem = value.get("focused-item").map(Item::from_toml_ref);

                let items = Box::new((item, fitem));
                Module::FocusList {
                    source,
                    items,
                    spacing,
                }
            }
            Some("formatted") | Some("text") => {
                let format = value.get("format").and_then(|v| v.as_str()).unwrap_or_else(|| {
                    error!("Formatted variables require a format: {}", value);
                    ""
                }).into();
                let tooltip = value.get("tooltip").and_then(|v| v.as_str()).unwrap_or("").into();
                Module::Formatted { format, tooltip, looped : Cell::new(false) }
            }
            Some("group") => {
                let spacing = toml_to_string(value.get("spacing")).unwrap_or_default().into();
                let items = value.get("items")
                    .and_then(|v| v.as_array())
                    .map(|a| a.iter().map(Item::from_toml_ref).collect())
                    .unwrap_or_default();

                Module::Group {
                    items,
                    spacing,
                }
            }
            Some("icon") => {
                let name = value.get("name").and_then(|v| v.as_str()).unwrap_or_else(|| {
                    error!("Icon requires a name expression");
                    ""
                }).into();
                let fallback = toml_to_string(value.get("fallback")).unwrap_or_default().into();
                let tooltip = value.get("tooltip").and_then(|v| v.as_str()).unwrap_or("").into();
                Module::Icon { name, fallback, tooltip }
            }
            Some("meter") => {
                let min = toml_to_string(value.get("min")).unwrap_or_default().into();
                let max = toml_to_string(value.get("max")).unwrap_or_default().into();
                let src = value.get("src").and_then(|v| v.as_str()).unwrap_or_else(|| {
                    error!("Meter requires a src expression");
                    ""
                }).into();
                let mut values = match Some(Some("")).into_iter()
                        .chain(value.get("values").and_then(|v| v.as_array()).map(|v| v.iter().map(toml::Value::as_str)).into_iter().flatten())
                        .chain(Some(Some("")))
                        .map(|v| v.map(Box::from))
                        .collect::<Option<Box<[_]>>>()
                    {
                        Some(v) if v.len() > 2 => v,
                        _ => {
                            error!("Meter requires an array of string values");
                            return Module::None;
                        }
                    };
                let e = values.len() - 1;
                values[0] = value.get("below").and_then(|v| v.as_str()).unwrap_or(&values[1]).into();
                values[e] = value.get("above").and_then(|v| v.as_str()).unwrap_or(&values[e - 1]).into();
                Module::Meter { min, max, src, values, looped : Cell::new(false) }
            }
            Some("mpris") => {
                let target = toml_to_string(value.get("name")).unwrap_or_default().into();
                Module::MediaPlayer2 { target }
            }
            Some("pulse") => {
                let target = toml_to_string(value.get("target")).unwrap_or_default().into();
                Module::Pulse { target }
            }
            Some("regex") => {
                let text = value.get("text").and_then(|v| v.as_str()).unwrap_or_else(|| {
                    error!("Regex requires a text expression");
                    ""
                }).into();
                let replace = value.get("replace").and_then(|v| v.as_str()).unwrap_or("").into();
                let regex = value.get("regex").and_then(|v| v.as_str()).unwrap_or_else(|| {
                    error!("Regex requires a regex expression");
                    ""
                });
                match regex::RegexBuilder::new(&regex)
                    .dot_matches_new_line(true)
                    .build()
                {
                    Ok(regex) => Module::Regex { regex, text, replace, looped : Cell::new(false) },
                    Err(e) => {
                        error!("Error compiling regex '{}': {}", regex, e);
                        Module::None
                    }
                }
            }
            Some("read-file") => {
                let name = match value.get("file").and_then(|v| v.as_str()) {
                    Some(name) => name.into(),
                    None => {
                        error!("Read-file requires a file name: {}", value);
                        return Module::None;
                    }
                };
                let poll = toml_to_f64(value.get("poll")).unwrap_or(60.0);
                Module::ReadFile {
                    name, poll, last_read: Cell::default(), contents : Cell::default()
                }
            }
            Some("sway-mode") => {
                sway::Mode::from_toml(value).map_or(Module::None, Module::SwayMode)
            }
            Some("sway-tree") => {
                sway::Tree::from_toml(value).map_or(Module::None, Module::SwayTree)
            }
            Some("sway-workspace") => {
                sway::Workspace::from_toml(value).map_or(Module::None, Module::SwayWorkspace)
            }
            Some("switch") => {
                let format = toml_to_string(value.get("format")).unwrap_or_default().into();
                let cases = match value.get("cases") {
                    Some(toml::Value::Table(cases)) => cases.clone(),
                    _ => {
                        error!("'cases' must be a table in the 'switch' type");
                        return Module::None;
                    }
                };
                let default = toml_to_string(value.get("default")).unwrap_or_default().into();
                Module::Switch { format, cases, default, looped : Cell::new(false) }
            }
            // "text" is an alias for "formatted"
            Some("tray") => {
                let spacing = toml_to_string(value.get("spacing")).unwrap_or_default().into();
                Module::Tray {
                    spacing,
                }
            }
            Some("value") => {
                Module::new_value(toml_to_string(value.get("value")).unwrap_or_default())
            }
            Some(_) => {
                Module::None
            }
            None => {
                if let Some(value) = value.as_str() {
                    Module::new_value(value)
                } else if let Some(format) = value.get("format").and_then(|v| v.as_str()) {
                    let tooltip = value.get("tooltip").and_then(|v| v.as_str()).unwrap_or("").into();
                    Module::Formatted { format : format.into(), tooltip, looped : Cell::new(false) }
                } else if let Some(value) = toml_to_string(value.get("value")) {
                    Module::new_value(value)
                } else {
                    Module::None
                }
            }
        }
    }

    pub fn new_value<T : Into<String>>(t : T) -> Self {
        Module::Value {
            value : Cell::new(t.into()),
            interested : Default::default(),
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Module::None)
    }

    /// One-time setup, if needed
    pub fn init(&self, name : &str, rt : &Runtime, from : Option<Self>) {
        match (self, from) {
            (Module::ExecJson { command, stdin, value, handle },
                Some(Module::ExecJson {
                    command : old_cmd,
                    stdin : old_stdin,
                    value : old_value,
                    handle : old_handle,
                }))
                if *command == old_cmd =>
            {
                stdin.set(old_stdin.into_inner());
                value.set(old_value.into_inner());
                handle.set(old_handle.into_inner());
            }

            (Module::ExecJson { command, stdin, value, handle }, _) => {
                match Command::new("/bin/sh")
                    .arg("-c").arg(&**command)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Err(e) => error!("Could not execute {}: {}", command, e),
                    Ok(mut child) => {
                        let rc = Rc::new((Cell::new(JsonValue::Null), Default::default()));
                        let pipe_in = child.stdin.take().unwrap();
                        let fd = child.stdout.take().unwrap().into_raw_fd();
                        unsafe { libc::fcntl(pipe_in.as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK); }
                        unsafe { libc::fcntl(fd, libc::F_SETFL, libc::O_NONBLOCK); }
                        stdin.set(Some(pipe_in));
                        value.set(Some(rc.clone()));
                        handle.set(Some(do_exec_json(fd, name.to_owned(), rc)));
                    }
                }
            }
            (Module::SwayMode(mode), _) => mode.init(name, rt),
            (Module::SwayTree(tree), _) => tree.init(name, rt),
            (Module::SwayWorkspace(ws), _) => ws.init(name, rt),
            _ => {}
        }
    }

    fn should_read_now(poll : f64, last_read : &Cell<Option<Instant>>, rt : &Runtime, who : &'static str) -> bool {
        let now = Instant::now();
        if poll > 0.0 {
            let last = last_read.get();
            if let Some(last) = last {
                let next = last + Duration::from_secs_f64(poll);
                let early = last + Duration::from_secs_f64(poll * 0.9);
                if early > now {
                    rt.set_wake_at(next, who);
                    return false;
                }
            }
            rt.set_wake_at(now + Duration::from_secs_f64(poll), who);
        } else if last_read.get().is_some() {
            return false;
        }
        last_read.set(Some(now));
        true
    }

    /// Periodic update (triggered by timer)
    pub fn update(&self, name : &str, rt : &Runtime) {
        match self {
            Module::Clock { format, zone, time } => {
                let real_format = rt.format(&format).unwrap_or_else(|e| {
                    warn!("Error expanding '{}' format: {}", name, e);
                    String::new()
                });
                let real_zone = rt.format(&zone).unwrap_or_else(|e| {
                    warn!("Error expanding '{}' timezone format: {}", name, e);
                    String::new()
                });

                let now = chrono::Utc::now();
                let subsec = chrono::Timelike::nanosecond(&now) as u64;
                let wake;
                if real_format.contains("%S") || real_format.contains("%T") || real_format.contains("%X") {
                    let delay = 1_000_999_999u64.checked_sub(subsec);
                    wake = Instant::now() + delay.map_or(Duration::from_secs(1), Duration::from_nanos);
                } else {
                    let sec = chrono::Timelike::second(&now) as u64;
                    let delay = (1_000_000_000 * (60 - sec) + 999_999).checked_sub(subsec);
                    wake = Instant::now() + delay.map_or(Duration::from_secs(1), Duration::from_nanos);
                }

                // Set a timer to expire when the subsecond offset will be zero
                // add another 1ms delay because epoll only gets 1ms granularity
                rt.set_wake_at(wake, "clock");

                if real_zone.is_empty() {
                    time.set(format!("{}", now.with_timezone(&chrono::Local).format(&real_format)));
                } else {
                    match real_zone.parse::<chrono_tz::Tz>() {
                        Ok(tz) => {
                            time.set(format!("{}", now.with_timezone(&tz).format(&real_format)));
                        }
                        Err(e) => {
                            warn!("Could not find timezone '{}': {}", real_zone, e);
                        }
                    }
                }
            }
            Module::Disk { path, poll, last_read, contents } => {
                if !Self::should_read_now(*poll, last_read, rt, "disk") {
                    return;
                }
                let cstr = std::ffi::CString::new(path.as_bytes()).unwrap();
                let rv = unsafe { libc::statvfs(cstr.as_ptr(), contents.as_ptr()) };
                if rv != 0 {
                    warn!("Could not read disk at '{}': {}", path, std::io::Error::last_os_error());
                }

            }
            Module::ReadFile { name, poll, last_read, contents } => {
                use std::io::Read;
                if !Self::should_read_now(*poll, last_read, rt, "read-file") {
                    return;
                }
                let mut v = String::with_capacity(4096);
                match std::fs::File::open(&**name).and_then(|mut f| f.read_to_string(&mut v)) {
                    Ok(_len) => {
                        contents.set(v);
                    }
                    Err(e) => {
                        warn!("Could not read {}: {}", name, e);
                    }
                }
            }
            _ => {}
        }
    }

    /// Read the value of a variable
    ///
    /// This is identical to read_in, but returns a String instead of using a callback closure.
    pub fn read_to_string(&self, name : &str, key : &str, rt : &Runtime) -> String {
        self.read_in(name, key, rt, |s| s.into())
    }

    /// Read the value of a variable
    ///
    /// The provided closure should be passed the value of the variable.  This is done instead of
    /// returning the value to avoid unneeded string copies.
    ///
    /// Note: The name is a hint and should not be assumed to uniquely identify this module.
    pub fn read_in<F : FnOnce(&str) -> R, R>(&self, name : &str, key : &str, rt : &Runtime, f : F) -> R {
        match self {
            Module::ItemReference { .. } |
            Module::Group { .. } |
            Module::Icon { .. } |
            Module::FocusList { .. } |
            Module::Tray { .. } => {
                error!("Cannot use '{}' in a text expansion", name);
                f("")
            }

            Module::Bar { config, .. } => {
                match toml_to_string(config.get(key)) {
                    Some(value) => f(&value),
                    None => f(""),
                }
            }
            Module::Clock { time, .. } => time.take_in(|s| f(s)),
            Module::Disk { contents, .. } => {
                let vfs = contents.get();
                match key {
                    "size" => f(&format!("{}", vfs.f_frsize * vfs.f_blocks)),
                    "free" => f(&format!("{}", vfs.f_bsize * vfs.f_bfree)),
                    "avail" => f(&format!("{}", vfs.f_bsize * vfs.f_bavail)),
                    "" | "percent-used" if vfs.f_frsize * vfs.f_blocks != 0 => {
                        let size = (vfs.f_frsize * vfs.f_blocks) as f64;
                        let free = (vfs.f_bsize * vfs.f_bfree) as f64;
                        if key == "" {
                            f(&format!("{:.0}%", 100.0 - 100.0 * free / size))
                        } else {
                            f(&format!("{:.0}", 100.0 - 100.0 * free / size))
                        }
                    }
                    _ => f("")
                }
            }
            Module::Eval { expr, vars, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                struct Context<'a> {
                    float : evalexpr::Function,
                    int : evalexpr::Function,
                    vars : Vec<(&'a str, evalexpr::Value)>,
                }
                impl<'a> evalexpr::Context for Context<'a> {
                    fn get_value(&self, name : &str) -> Option<&evalexpr::Value> {
                        self.vars.iter()
                            .filter(|&&(k,_)| k == name)
                            .next()
                            .map(|(_,v)| v)
                    }
                    fn get_function(&self, name : &str) -> Option<&evalexpr::Function> {
                        match name {
                            "float" => Some(&self.float),
                            "int" => Some(&self.int),
                            _ => None,
                        }
                    }
                }
                looped.set(true);
                let ctx = Context {
                    int : evalexpr::Function::new(Box::new(move |arg| {
                        let rv = arg.as_string()?;
                        match rv.trim().parse() {
                            Ok(v) => Ok(evalexpr::Value::Int(v)),
                            Err(_) => Err(evalexpr::error::EvalexprError::ExpectedInt {
                                actual : evalexpr::Value::String(rv),
                            })
                        }
                    })),
                    float : evalexpr::Function::new(Box::new(move |arg| {
                        let rv = arg.as_string()?;
                        match rv.trim().parse() {
                            Ok(v) => Ok(evalexpr::Value::Float(v)),
                            Err(_) => Err(evalexpr::error::EvalexprError::ExpectedFloat {
                                actual : evalexpr::Value::String(rv),
                            })
                        }
                    })),
                    vars : vars.iter()
                        .map(|(k,v)| {
                            (&k[..], evalexpr::Value::String(rt.format_or(v, k)))
                        })
                        .collect(),
                };
                looped.set(false);
                match expr.eval_with_context(&ctx) {
                    Ok(evalexpr::Value::String(s)) => f(&s),
                    Ok(evalexpr::Value::Float(n)) => f(&format!("{}", n)),
                    Ok(evalexpr::Value::Int(n)) => f(&format!("{}", n)),
                    Ok(evalexpr::Value::Boolean(b)) => f(if b { "1" } else { "0" }),
                    Ok(evalexpr::Value::Empty) => f(""),
                    Ok(_) => {
                        warn!("Ignoring invalid return type from eval");
                        f("")
                    }
                    Err(e) => {
                        warn!("Eval error in {}: {}", name, e);
                        f("")
                    }
                }
            }
            Module::ExecJson { command, value, .. } => {
                let value = value.take_in_some(|v| v.clone()).unwrap();
                let v = value.0.replace(JsonValue::Null);
                let rv = f(v[key].as_str().unwrap_or_else(|| {
                    debug!("Could not find {}.{} in the output of {}", name, key, command);
                    ""
                }));
                value.0.set(v);
                value.1.take_in(|i| i.add(rt));
                rv
            }
            Module::Formatted { format, tooltip, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let value = match key {
                    "tooltip" => rt.format_or(&tooltip, &name),
                    _ => rt.format_or(&format, &name),
                };
                looped.set(false);
                f(&value)
            }
            Module::Item { value } => value.take_in(|item| {
                match item.as_ref() {
                    Some(IterationItem::MediaPlayer2 { target }) => mpris::read_in(name, target, key, rt, f),
                    Some(IterationItem::Pulse { target }) => pulse::read_in(name, target, key, rt, f),
                    Some(IterationItem::SwayWorkspace(data)) => data.read_in(key, rt, f),
                    Some(IterationItem::SwayTreeItem(node)) => node.read_in(key, rt, f),
                    None => f(""),
                }
            }),
            Module::MediaPlayer2 { target } => mpris::read_in(name, target, key, rt, f),
            Module::Meter { min, max, src, values, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let value = rt.format_or(&src, &name);
                let min = rt.format_or(&min, &name);
                let max = rt.format_or(&max, &name);
                let value = value.parse::<f64>().unwrap_or(0.0);
                let min = min.parse::<f64>().unwrap_or(0.0);
                let max = max.parse::<f64>().unwrap_or(100.0);
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
                let res = rt.format_or(&expr, &name);
                looped.set(false);
                f(&res)
            }
            Module::None => f(""),
            Module::Pulse { target } => pulse::read_in(name, target, key, rt, f),
            Module::ReadFile { contents, .. } if key == "raw" => contents.take_in(|s| f(s)),
            Module::ReadFile { contents, .. } => contents.take_in(|s| f(s.trim())),
            Module::Regex { regex, text, replace, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let text = rt.format_or(&text, &name);
                looped.set(false);
                if key == "" || key == "text" {
                    let output = regex.replace_all(&text, &**replace);
                    f(&output)
                } else {
                    if let Some(cap) = regex.captures(&text) {
                        if let Ok(i) = key.parse() {
                            f(cap.get(i).map_or("", |m| m.as_str()))
                        } else {
                            f(cap.name(key).map_or("", |m| m.as_str()))
                        }
                    } else {
                        f("")
                    }
                }
            }
            Module::SwayMode(mode) => mode.read_in(name, key, rt, f),
            Module::SwayTree(tree) => tree.read_in(name, key, rt, f),
            Module::SwayWorkspace(ws) => ws.read_in(name, key, rt, f),
            Module::Switch { format, cases, default, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let text = rt.format_or(&format, &name);
                let case = toml_to_string(cases.get(&text));
                let case = case.as_deref().unwrap_or(default);
                let text = rt.format_or(case, &name);
                looped.set(false);
                f(&text)
            }
            Module::Value { value, interested } => {
                interested.take_in(|i| i.add(rt));
                value.take_in(|s| f(s))
            }
        }
    }

    /// Handle a write or send to the variable
    pub fn write(&self, name : &str, key : &str, value : String, rt : &Runtime) {
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
                let mut line : Vec<u8> = Vec::new();
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
            Module::Item { value : v } => v.take_in(|item| {
                match item.as_ref() {
                    Some(IterationItem::MediaPlayer2 { target }) => mpris::write(name, target, key, value, rt),
                    Some(IterationItem::Pulse { target }) => pulse::do_write(name, target, key, value, rt),
                    Some(IterationItem::SwayWorkspace(data)) => data.write(key, value, rt),
                    Some(IterationItem::SwayTreeItem(node)) => node.write(key, value, rt),
                    None => {}
                }
            }),
            Module::MediaPlayer2 { target } => mpris::write(name, target, key, value, rt),
            Module::Pulse { target } => pulse::do_write(name, target, key, value, rt),
            Module::SwayWorkspace(ws) => ws.write(name, key, value, rt),
            Module::Value { value : v, interested } if key == "" => {
                interested.take().notify_data("value");
                v.set(value);
            }
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }

    pub fn none() -> Self {
        Module::None
    }

    pub fn new_current_item() -> Self {
        Module::Item { value : Cell::new(None) }
    }

    pub fn read_focus_list<F : FnMut(bool, IterationItem)>(&self, rt : &Runtime, f : F) {
        match self {
            Module::MediaPlayer2 { .. } => mpris::read_focus_list(rt, f),
            Module::SwayWorkspace(ws) => ws.read_focus_list(rt, f),
            Module::Pulse { target } => pulse::read_focus_list(rt, target, f),
            _ => ()
        }
    }
}

/// Handler invoked by a click or touch event
#[derive(Debug,Clone)]
pub enum Action {
    Exec { format : String },
    Write { target : String, format : String },
    List(Vec<Action>),
    Tray { owner : Rc<str>, path : Rc<str> },
    None,
}

impl Action {
    pub fn from_toml(value : &toml::Value) -> Self {
        if let Some(array) = value.as_array() {
            return Action::List(array.iter().map(Action::from_toml).collect());
        }
        if let Some(dest) = value.get("write").and_then(|v| v.as_str()).or_else(|| value.get("send").and_then(|v| v.as_str())) {
            let format = value.get("format").and_then(|v| v.as_str())
                .or_else(|| value.get("msg").and_then(|v| v.as_str()))
                .unwrap_or("").to_owned();
            return Action::Write { target : dest.into(), format };
        }
        if let Some(cmd) = value.get("exec").and_then(|v| v.as_str()) {
            return Action::Exec { format : cmd.into() };
        }
        error!("Unknown action: {}", value);
        Action::None
    }

    pub fn from_tray(owner : Rc<str>, path : Rc<str>) -> Self {
        Action::Tray { owner, path }
    }

    pub fn invoke(&self, runtime : &Runtime, how : u32) {
        match self {
            Action::List(actions) => {
                for action in actions {
                    action.invoke(runtime, how);
                }
            }
            Action::Write { target, format } => {
                let value = match runtime.format(&format) {
                    Ok(value) => value,
                    Err(e) => {
                        error!("Error expanding format for command: {}", e);
                        return;
                    }
                };

                let (name, key) = match target.find('.') {
                    Some(p) => (&target[..p], &target[p + 1..]),
                    None => (&target[..], ""),
                };

                match runtime.items.get(name) {
                    Some(item) => {
                        item.data.write(name, key, value, &runtime);
                    }
                    None => error!("Could not find variable {}", target),
                }
            }
            Action::Exec { format } => {
                match runtime.format(&format) {
                    Ok(cmd) => {
                        info!("Executing '{}'", cmd);
                        match Command::new("/bin/sh").arg("-c").arg(&cmd).spawn() {
                            Ok(child) => drop(child),
                            Err(e) => error!("Could not execute {}: {}", cmd, e),
                        }
                    }
                    Err(e) => {
                        error!("Error expanding format for command: {}", e);
                    }
                }
            }
            Action::Tray { owner, path } => {
                tray::do_click(owner, path, how);
            }
            Action::None => { info!("Invoked a no-op"); }
        }
    }
}

fn do_exec_json(fd : i32, name : String, value : Rc<(Cell<JsonValue>, Cell<NotifierList>)>) -> RemoteHandle<()> {
    let (task, rh) = async move {
        let afd = AsyncFd::new(Fd(fd)).expect("Invalid FD from ChildStdin");
        let mut buffer : Vec<u8> = Vec::with_capacity(1024);

        'waiting : loop {
            let mut rh = match afd.readable().await {
                Ok(h) => h,
                Err(e) => {
                    warn!("Unable to wait for child read: {}", e);
                    return;
                }
            };
            'reading : loop {
                if buffer.len() == buffer.capacity() {
                    buffer.reserve(2048);
                }
                unsafe { // child pipe read into vec spare capacity
                    let start = buffer.len();
                    let max_len = buffer.capacity() - start;
                    let rv = libc::read(fd, buffer.as_mut_ptr().offset(start as isize) as *mut _, max_len);
                    match rv {
                        0 => {
                            libc::close(fd);
                            return;
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
                                    return;
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
                                Ok(v) => { json = Some(v); }
                                Err(e) => info!("Ignoring bad JSON from '{}': {}", name, e),
                            }
                        }
                    }
                    // Note: this is optimized for the normal case where the script sends one line
                    // at a time, so this drain would empty the buffer.
                    buffer.drain(..eol + 1);
                    if let Some(json) = json {
                        value.0.set(json);
                        value.1.take().notify_data("exec-json");
                    }
                }
            }
        }
    }.remote_handle();
    spawn_noerr(task);
    rh
}
