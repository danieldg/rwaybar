use calloop::LoopHandle;
use crate::state::{State,Runtime};
use json::JsonValue;
use log::{debug,info,warn,error};
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::process::{Command,Stdio,ChildStdin};
use std::os::unix::io::{AsRawFd,IntoRawFd};
use std::time::{Duration,Instant};
use libc;

#[derive(Debug)]
enum Module {
    Clock {
        format : String,
        zone : String,
    },
    ReadFile {
        name : String,
        poll : f64,
        last_read : Option<Instant>,
    },
    ExecJson {
        command : String,
        stdin : Option<ChildStdin>,
    },
    Value {
        value : String,
    },
    Formatted {
        format : String,
    },
    None,
}

impl Module {
    fn from_json(value : &JsonValue) -> Self {
        match value["module"].as_str() {
            Some("clock") => {
                let format = value["format"].as_str().unwrap_or("%H:%M").to_owned();
                let zone = value["timezone"].as_str().unwrap_or("").to_owned();
                Module::Clock { format, zone }
            }
            Some("formatted") => {
                let format = value["format"].as_str().unwrap_or_else(|| {
                    error!("Formatted variables require a format: {}", value);
                    ""
                }).to_owned();
                Module::Formatted { format }
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
                    name, poll, last_read: None
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
                    stdin : None
                }
            }
            None => {
                if let Some(value) = value.as_str().map(String::from) {
                    Module::Value { value }
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

    pub fn invoke(&self, runtime : &mut Runtime) {
        match self {
            Action::List(actions) => {
                for action in actions {
                    action.invoke(runtime);
                }
            }
            Action::Write { target, format } => {
                let value = match strfmt::strfmt(&format, &runtime.vars) {
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

                match runtime.sources.iter_mut().find(|v| &v.name == name) {
                    Some(var) => {
                        var.write(key, value, &mut runtime.vars);
                    }
                    None => error!("Could not find variable {}", target),
                }
            }
            Action::Exec { format } => {
                match strfmt::strfmt(&format, &runtime.vars) {
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

#[derive(Debug)]
pub struct Variable {
    name : String,
    module : Module,
}

fn set_wake_at(timer : &mut Option<Instant>, wake : Instant) {
    match timer {
        Some(then) if wake > *then => {}
        _ => { *timer = Some(wake) }
    }
}

fn do_exec_json(eloop : &LoopHandle<State>, fd : i32, name : String) {
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
                    for (key, value) in json.entries() {
                        let key = format!("{}.{}", name, key);
                        if let Some(value) = value.as_str() {
                            state.runtime.vars.insert(key, value.to_owned());
                        } else {
                            state.runtime.vars.insert(key, value.dump());
                        }
                    }
                    state.draw();
                }
            }
        }
        Ok(())
    }).expect("Error adding FD");
}

impl Variable {
    pub fn new((key, value) : (&str, &JsonValue)) -> Self {
        let name = key.into();
        if key.contains('.') {
            warn!("Variable name '{}' contains a '.', may produce unexpected behavior", key);
        }
        let module = Module::from_json(value);
        Variable {
            name,
            module
        }
    }

    pub fn init(&mut self, eloop : &LoopHandle<State>, vars : &mut HashMap<String, String>) {
        match &mut self.module {
            Module::Value { value } => {
                vars.insert(self.name.clone(), value.clone());
            }
            Module::ExecJson { command, stdin } => {
                match Command::new("/bin/sh")
                    .arg("-c").arg(&command)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Err(e) => error!("Could not execute {}: {}", command, e),
                    Ok(mut child) => {
                        *stdin = child.stdin.take();
                        let fd = child.stdout.take().unwrap().into_raw_fd();
                        unsafe {
                            libc::fcntl(stdin.as_ref().unwrap().as_raw_fd(), libc::F_SETFL, libc::O_NONBLOCK);
                            libc::fcntl(fd, libc::F_SETFL, libc::O_NONBLOCK);
                        }

                        do_exec_json(eloop, fd, self.name.clone());
                    }
                }
            }
            _ => {}
        }
    }

    pub fn update(&mut self, timer : &mut Option<Instant>, vars : &mut HashMap<String, String>) {
        match &mut self.module {
            Module::Clock { format, zone } => {
                let now = chrono::Utc::now();

                // Set a timer to expire when the subsecond offset will be zero
                let subsec = chrono::Timelike::nanosecond(&now) as u64;
                // add another 1ms delay because epoll only gets 1ms granularity
                let delay = 1_000_999_999u64.checked_sub(subsec);
                let wake = Instant::now() + delay.map_or(Duration::from_secs(1), Duration::from_nanos);
                set_wake_at(timer, wake);

                let real_format = strfmt::strfmt(&format, vars).unwrap_or_else(|e| {
                    warn!("Error expanding clock format: {}", e);
                    String::new()
                });
                let real_zone = strfmt::strfmt(&zone, vars).unwrap_or_else(|e| {
                    warn!("Error expanding clock timezone format: {}", e);
                    String::new()
                });
                let value = if real_zone.is_empty() {
                    format!("{}", now.with_timezone(&chrono::Local).format(&real_format))
                } else {
                    match real_zone.parse::<chrono_tz::Tz>() {
                        Ok(tz) => {
                            format!("{}", now.with_timezone(&tz).format(&real_format))
                        }
                        Err(e) => {
                            warn!("Could not find timezone '{}': {}", real_zone, e);
                            String::new()
                        }
                    }
                };

                vars.insert(self.name.clone(), value);
            }
            Module::ReadFile { name, poll, last_read } => {
                let now = Instant::now();
                if *poll > 0.0 {
                    match *last_read {
                        Some(then) if then + Duration::from_secs_f64(*poll) > now => return,
                        _ => ()
                    }

                    set_wake_at(timer, now + Duration::from_secs_f64(*poll));
                } else if last_read.is_some() {
                    return;
                }
                *last_read = Some(now);
                match std::fs::read_to_string(&name) {
                    Ok(mut v) => {
                        if v.ends_with('\n') {
                            v.pop();
                        }
                        vars.insert(self.name.clone(), v);
                    }
                    Err(e) => {
                        warn!("Could not read {}: {}", name, e);
                        vars.insert(self.name.clone(), String::new());
                    }
                }
            }
            Module::Formatted { format } => {
                match strfmt::strfmt(&format, vars) {
                    Ok(value) => {
                        vars.insert(self.name.clone(), value);
                    }
                    Err(e) => {
                        warn!("Error expanding format for '{}': {}", self.name, e);
                    }
                }
            }
            Module::None |
            Module::ExecJson { .. } |
            Module::Value { .. } => {} // only assigns at init
        }
    }

    pub fn write(&mut self, key : &str, value : String, vars : &mut HashMap<String, String>) {
        match &mut self.module {
            Module::Value { .. } if key == "" => {
                vars.get_mut(&self.name).map(|v| *v = value);
            }
            Module::ExecJson { stdin, .. } => {
                if stdin.is_none() {
                    warn!("Not writing to closed exec-json stream");
                    return;
                }
                let mut json = JsonValue::new_object();
                json.insert(key, value).unwrap();
                let mut line : Vec<u8> = Vec::new();
                json.write(&mut line).unwrap();
                line.push(b'\n');
                // Note: this will return WouldBlock instead of blocking, in which case we stop
                // sending instead of blocking the GUI or buffering messages.
                //
                // This could be changed to write via the event loop if needed.
                match stdin.as_mut().unwrap().write_all(&line) {
                    Ok(()) => {}
                    Err(e) => {
                        warn!("Could not write to '{}' : {}", self.name, e);
                        stdin.take();
                    }
                }
            }
            _ => {
                error!("Ignoring write to {}.{}", self.name, key);
            }
        }
    }
}
