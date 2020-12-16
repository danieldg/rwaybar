use json::JsonValue;
use std::collections::HashMap;
use std::time::{Duration,Instant};

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
                let format = value["format"].as_str().unwrap().to_owned();
                Module::Formatted { format }
            }
            None if value.is_string() => {
                Module::Value { value : value.as_str().unwrap().to_owned() }
            }
            _ => panic!("Unknown module in source: {}", value),
        }
    }
}

pub struct DataSource {
    name : String,
    module : Module,
}

impl DataSource {
    pub fn new((key, value) : (&str, &JsonValue)) -> Self {
        let name = key.into();
        let module = Module::from_json(value);
        DataSource {
            name,
            module
        }
    }

    pub fn init(&self, data : &mut HashMap<String, String>) {
        match &self.module {
            Module::Value { value } => {
                data.insert(self.name.clone(), value.clone());
            }
            _ => {}
        }
    }

    pub fn update(&mut self, timer : &mut Option<Instant>, data : &mut HashMap<String, String>) {
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

                let real_format = strfmt::strfmt(&format, data).unwrap();

                data.insert(self.name.clone(), format!("{}", now.format(&real_format)));
            }
            Module::Formatted { format } => {
                data.insert(self.name.clone(), strfmt::strfmt(&format, data).unwrap());
            }
            Module::Value { .. } => {} // only assigns at init
        }
    }
}
