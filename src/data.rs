use json::JsonValue;
use std::collections::HashMap;
use std::time::{Duration,Instant};
use log::{info,warn,error};
use crate::state::Runtime;

#[derive(Debug)]
enum Module {
    Clock {
        format : String,
    },
    Value {
        value : String,
    },
    Formatted {
        format : String,
    },
}

impl Module {
    fn from_json(value : &JsonValue) -> Self {
        match value["module"].as_str() {
            Some("clock") => {
                let format = value["format"].as_str().unwrap_or("%H:%M").to_owned();
                Module::Clock { format }
            }
            Some("formatted") => {
                let format = value["format"].as_str().unwrap_or_else(|| {
                    error!("Formatted variables require a format: {}", value);
                    ""
                }).to_owned();
                Module::Formatted { format }
            }
            None => {
                if let Some(value) = value.as_str().map(String::from) {
                    Module::Value { value }
                } else {
                    error!("Invalid module definition: {}", value);
                    Module::Value { value : String::new() }
                }
            }
            Some(m) => {
                error!("Unknown module '{}' in variable definition", m);
                Module::Value { value : String::new() }
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
                        let rv = std::process::Command::new("/bin/sh")
                            .arg("-c").arg(cmd)
                            .spawn();
                        drop(rv);
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

    pub fn init(&self, vars : &mut HashMap<String, String>) {
        match &self.module {
            Module::Value { value } => {
                vars.insert(self.name.clone(), value.clone());
            }
            _ => {}
        }
    }

    pub fn update(&mut self, timer : &mut Option<Instant>, vars : &mut HashMap<String, String>) {
        match &self.module {
            Module::Clock { format } => {
                let now = chrono::Local::now();

                // Set a timer to expire when the subsecond offset will be zero
                let subsec = chrono::Timelike::nanosecond(&now) as u64;
                // add another 1ms delay because epoll only gets 1ms granularity
                let delay = 1_000_999_999u64.checked_sub(subsec);
                let wake = Instant::now() + delay.map_or(Duration::from_secs(1), Duration::from_nanos);
                match timer {
                    Some(then) if wake > *then => {}
                    _ => { *timer = Some(wake) }
                }

                let real_format = strfmt::strfmt(&format, vars).unwrap();

                vars.insert(self.name.clone(), format!("{}", now.format(&real_format)));
            }
            Module::Formatted { format } => {
                vars.insert(self.name.clone(), strfmt::strfmt(&format, vars).unwrap());
            }
            Module::Value { .. } => {} // only assigns at init
        }
    }

    pub fn write(&mut self, key : &str, value : String, vars : &mut HashMap<String, String>) {
        match &self.module {
            Module::Value { .. } if key == "" => {
                vars.get_mut(&self.name).map(|v| *v = value);
            }
            _ => {
                error!("Ignoring write to {}.{}", self.name, key);
            }
        }
    }
}
