use calloop::LoopHandle;
use crate::state::{State,Runtime};
use json::JsonValue;
use log::{debug,info,warn,error};
use std::fmt;
use std::io;
use std::io::Write;
use std::os::unix::io::{AsRawFd,IntoRawFd};
use std::process::{Command,Stdio,ChildStdin};
use std::rc::Rc;
use std::time::{Duration,Instant};
use libc;

/// Wrapper around [std::cell::Cell] that implements [fmt::Debug].
#[derive(Default)]
struct Cell<T>(std::cell::Cell<T>);

impl<T> Cell<T> {
    fn new(t : T) -> Self {
        Cell(std::cell::Cell::new(t))
    }
}

impl<T : Default> Cell<T> {
    fn take_in<F : FnOnce(&mut T) -> R, R>(&self, f : F) -> R {
        let mut t = self.0.take();
        let rv = f(&mut t);
        self.0.set(t);
        rv
    }
}

impl<T> std::ops::Deref for Cell<T> {
    type Target = std::cell::Cell<T>;
    fn deref(&self) -> &std::cell::Cell<T> {
        &self.0
    }
}

impl<T> fmt::Debug for Cell<T> {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Cell")
    }
}

/// Type-specific part of a [Variable]
#[derive(Debug)]
enum Module {
    Clock {
        format : String,
        zone : String,
        time : Cell<String>,
    },
    ReadFile {
        name : String,
        poll : f64,
        last_read : Cell<Option<Instant>>,
        contents : Cell<String>,
    },
    ExecJson {
        command : String,
        stdin : Cell<Option<ChildStdin>>,
        value : Rc<Cell<JsonValue>>,
    },
    Value {
        value : Cell<String>,
    },
    Formatted {
        format : String,
        looped : Cell<bool>,
    },
    Regex {
        regex : regex::Regex,
        text : String,
        replace : String,
        looped : Cell<bool>,
    },
    Meter {
        min : f64,
        max : f64,
        src : String,
        values : Box<[String]>,
        looped : Cell<bool>,
    },
    None,
}

impl Module {
    fn from_json(value : &JsonValue) -> Self {
        match value["module"].as_str() {
            Some("clock") => {
                let format = value["format"].as_str().unwrap_or("%H:%M").to_owned();
                let zone = value["timezone"].as_str().unwrap_or("").to_owned();
                Module::Clock { format, zone, time : Cell::new(String::new()) }
            }
            Some("formatted") => {
                let format = value["format"].as_str().unwrap_or_else(|| {
                    error!("Formatted variables require a format: {}", value);
                    ""
                }).to_owned();
                Module::Formatted { format, looped : Cell::new(false) }
            }
            Some("regex") => {
                let text = value["text"].as_str().unwrap_or_else(|| {
                    error!("Regex requires a text expression");
                    ""
                }).to_owned();
                let replace = value["replace"].as_str().unwrap_or_else(|| {
                    error!("Regex requires an replace expression");
                    ""
                }).to_owned();
                let regex = value["regex"].as_str().unwrap_or_else(|| {
                    error!("Regex requires a regex expression");
                    ""
                }).to_owned();
                match regex::Regex::new(&regex) {
                    Ok(regex) => Module::Regex { regex, text, replace, looped : Cell::new(false) },
                    Err(e) => {
                        error!("Error compiling regex '{}': {}", regex, e);
                        Module::None
                    }
                }
            }
            Some("read-file") => {
                let name = match value["file"].as_str() {
                    Some(name) => name.to_owned(),
                    None => {
                        error!("Formatted variables require a format: {}", value);
                        return Module::None;
                    }
                };
                let poll = value["poll"].as_f64().unwrap_or(60.0);
                Module::ReadFile {
                    name, poll, last_read: Cell::default(), contents : Cell::default()
                }
            }
            Some("exec-json") => {
                let command = match value["command"].as_str() {
                    Some(cmd) => cmd.to_owned(),
                    None => {
                        error!("Comamnd to execute is required: {}", value);
                        return Module::None;
                    }
                };
                Module::ExecJson {
                    command,
                    stdin : Cell::new(None),
                    value : Rc::new(Cell::new(JsonValue::Null)),
                }
            }
            Some("meter") => {
                let min = value["min"].as_f64().unwrap_or(0.0);
                let max = value["max"].as_f64().unwrap_or(100.0);
                let src = value["src"].as_str().unwrap_or_else(|| {
                    error!("Meter requires a src expression");
                    ""
                }).to_owned();
                let mut values = match Some(Some("")).into_iter()
                        .chain(value["values"].members().map(JsonValue::as_str))
                        .chain(Some(Some("")))
                        .map(|v| v.map(String::from))
                        .collect::<Option<Box<[_]>>>()
                    {
                        Some(v) if v.len() > 2 => v,
                        _ => {
                            error!("Meter requires an array of string values");
                            return Module::None;
                        }
                    };
                let e = values.len() - 1;
                values[0] = value["below"].as_str().unwrap_or(&values[1]).to_owned();
                values[e] = value["above"].as_str().unwrap_or(&values[e - 1]).to_owned();
                Module::Meter { min, max, src, values, looped : Cell::new(false) }
            }
            None => {
                if let Some(value) = value.as_str().map(String::from) {
                    Module::Value { value : Cell::new(value) }
                } else {
                    error!("Invalid module definition: {}", value);
                    Module::None
                }
            }
            Some(m) => {
                error!("Unknown module '{}' in variable definition", m);
                Module::None
            }
        }
    }
}

/// Handler invoked by a click or touch event
#[derive(Debug,Clone)]
pub enum Action {
    Exec { format : String },
    Write { target : String, format : String },
    List(Vec<Action>),
    None,
}

impl Action {
    pub fn from_json(value : &JsonValue) -> Self {
        if value.is_array() {
            return Action::List(value.members().map(Action::from_json).collect());
        }
        if let Some(dest) = value["write"].as_str().or_else(|| value["send"].as_str()) {
            let format = value["format"].as_str()
                .or_else(|| value["msg"].as_str())
                .unwrap_or("").to_owned();
            return Action::Write { target : dest.into(), format };
        }
        if let Some(cmd) = value["exec"].as_str() {
            return Action::Exec { format : cmd.into() };
        }
        error!("Unknown action: {}", value);
        Action::None
    }

    pub fn invoke(&self, runtime : &Runtime) {
        match self {
            Action::List(actions) => {
                for action in actions {
                    action.invoke(runtime);
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

                match runtime.vars.get(name) {
                    Some(var) => {
                        var.write(name, key, value, &runtime);
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
            Action::None => { info!("Invoked a no-op"); }
        }
    }
}

/// A value or set of values usable in string expansions
///
/// The value of a variable may be the contents of a file or the output of a command; it may change
/// while the bar is running.
///
/// Some types of variables allow an [Action] to write to the variable in response to clicks.
#[derive(Debug)]
pub struct Variable {
    module : Module,
}

fn do_exec_json(eloop : &LoopHandle<State>, fd : i32, name : String, value : Rc<Cell<JsonValue>>) {
    let src = calloop::generic::Generic::from_fd(fd, calloop::Interest::Readable, calloop::Mode::Level);
    let mut buffer : Vec<u8> = Vec::with_capacity(1024);

    eloop.insert_source(src, move |how, fd, state| {
        let fd = fd.as_raw_fd();
        if how.error {
            unsafe { libc::close(fd); }
        } else if how.readable {
            loop {
                if buffer.len() == buffer.capacity() {
                    buffer.reserve(2048);
                }
                unsafe {
                    let start = buffer.len();
                    let max_len = buffer.capacity() - start;
                    let rv = libc::read(fd, buffer.as_mut_ptr().offset(start as isize) as *mut _, max_len);
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
                                io::ErrorKind::Interrupted => continue,
                                io::ErrorKind::WouldBlock => return Ok(()),
                                _ => return Err(e),
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
                    buffer.drain(..eol + 1);
                    let json = match json { Some(json) => json, None => continue };
                    value.set(json);
                    state.draw();
                }
            }
        }
        Ok(())
    }).expect("Error adding FD");
}

impl Variable {
    pub fn new((key, value) : (&str, &JsonValue)) -> (String, Self) {
        let name = key.into();
        if key.contains('.') {
            error!("Variable name '{}' contains a '.', cannot be read", key);
        }
        let module = Module::from_json(value);
        (name, Variable { module })
    }

    pub fn init(&self, name : &str, rt : &Runtime) {
        match &self.module {
            Module::ExecJson { command, stdin, value } => {
                match Command::new("/bin/sh")
                    .arg("-c").arg(&command)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Err(e) => error!("Could not execute {}: {}", command, e),
                    Ok(mut child) => {
                        let pipe_in = child.stdin.take().unwrap();
                        let fd = child.stdout.take().unwrap().into_raw_fd();
                        unsafe {
                            libc::fcntl(pipe_in.as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK);
                            libc::fcntl(fd, libc::F_SETFL, libc::O_NONBLOCK);
                        }
                        stdin.set(Some(pipe_in));
                        value.set(JsonValue::Null);

                        do_exec_json(&rt.eloop, fd, name.to_owned(), value.clone());
                    }
                }
            }
            _ => {}
        }
    }

    pub fn update(&self, _name : &str, rt : &Runtime) {
        match &self.module {
            Module::Clock { format, zone, time } => {
                let now = chrono::Utc::now();

                // Set a timer to expire when the subsecond offset will be zero
                let subsec = chrono::Timelike::nanosecond(&now) as u64;
                // add another 1ms delay because epoll only gets 1ms granularity
                let delay = 1_000_999_999u64.checked_sub(subsec);
                let wake = Instant::now() + delay.map_or(Duration::from_secs(1), Duration::from_nanos);
                rt.set_wake_at(wake);

                let real_format = rt.format(&format).unwrap_or_else(|e| {
                    warn!("Error expanding clock format: {}", e);
                    String::new()
                });
                let real_zone = rt.format(&zone).unwrap_or_else(|e| {
                    warn!("Error expanding clock timezone format: {}", e);
                    String::new()
                });
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
            Module::ReadFile { name, poll, last_read, contents } => {
                let now = Instant::now();
                if *poll > 0.0 {
                    let next = last_read.get().map_or(now, |v| v + Duration::from_secs_f64(*poll));
                    if next > now {
                        rt.set_wake_at(next);
                        return;
                    } else {
                        rt.set_wake_at(now + Duration::from_secs_f64(*poll));
                    }
                } else if last_read.get().is_some() {
                    return;
                }
                last_read.set(Some(now));
                match std::fs::read_to_string(&name) {
                    Ok(mut v) => {
                        if v.ends_with('\n') {
                            v.pop();
                        }
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

    pub fn read_in<F : FnOnce(&str) -> R, R>(&self, name : &str, key : &str, rt : &Runtime, f : F) -> R {
        match &self.module {
            Module::None => f(""),
            Module::Clock { time, .. } => time.take_in(|s| f(s)),
            Module::Value { value } => value.take_in(|s| f(s)),
            Module::ReadFile { contents, .. } => contents.take_in(|s| f(s)),
            Module::Formatted { format, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let value = rt.format_or(&format, &name);
                looped.set(false);
                f(&value)
            }
            Module::Regex { regex, text, replace, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let text = rt.format_or(&text, &name);
                looped.set(false);
                let output = regex.replace_all(&text, replace.as_str());
                f(&output)
            }
            Module::ExecJson { command, value, .. } => {
                let v = value.replace(JsonValue::Null);
                let rv = f(v[key].as_str().unwrap_or_else(|| {
                    debug!("Could not find {}.{} in the output of {}", name, key, command);
                    ""
                }));
                value.set(v);
                rv
            }
            Module::Meter { min, max, src, values, looped } => {
                if looped.get() {
                    error!("Recursion detected when expanding {}", name);
                    return f("");
                }
                looped.set(true);
                let value = rt.format_or(&src, &name);
                let value = value.parse::<f64>().unwrap_or(0.0);
                let steps = values.len() - 2;
                let step = (*max - *min) / (steps as f64);
                let expr = if value < *min {
                    &values[0]
                } else if value > *max {
                    &values[values.len() - 1]
                } else {
                    let i = (value - min) / step;
                    &values[i as usize + 1]
                };
                let res = rt.format_or(&expr, &name);
                looped.set(false);
                f(&res)
            }
        }
    }

    pub fn write(&self, name : &str, key : &str, value : String, _rt : &Runtime) {
        match &self.module {
            Module::Value { value : v } if key == "" => {
                v.set(value);
            }
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
            _ => {
                error!("Ignoring write to {}.{}", name, key);
            }
        }
    }
}
