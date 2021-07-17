//! Event handling (click, scroll)
use crate::data::IterationItem;
use crate::item::PopupDesc;
use crate::state::Runtime;
#[cfg(feature="dbus")]
use crate::tray;
use log::{info,error};
use std::rc::Rc;
use std::process::Command;

/// A single click action associated with the area that activates it
#[derive(Debug,Clone)]
struct EventListener {
    x_min : f32,
    x_max : f32,
    buttons : u32,
    item : Option<IterationItem>,
    target : Action,
}

/// A list of [EventListener]s
#[derive(Debug,Default,Clone)]
pub struct EventSink {
    handlers : Vec<EventListener>,
    hovers : Vec<(f32, f32, PopupDesc)>,
}

impl EventSink {
    pub fn from_toml(value : &toml::Value) -> Self {
        let mut sink = EventSink::default();
        sink.add_click(value.get("on-click"), 1 << 0);
        sink.add_click(value.get("on-click-right"), 1 << 1);
        sink.add_click(value.get("on-click-middle"), 1 << 2);
        sink.add_click(value.get("on-click-back"), 1 << 3);
        sink.add_click(value.get("on-click-backward"), 1 << 3);
        sink.add_click(value.get("on-click-forward"), 1 << 4);
        sink.add_click(value.get("on-scroll-up"), 1 << 5);
        sink.add_click(value.get("on-scroll-down"), 1 << 6);
        sink.add_click(value.get("on-vscroll"), 3 << 5);
        sink.add_click(value.get("on-scroll-left"), 1 << 7);
        sink.add_click(value.get("on-scroll-right"), 1 << 8);
        sink.add_click(value.get("on-hscroll"), 3 << 7);
        sink.add_click(value.get("on-scroll"), 15 << 5);
        sink
    }

    fn add_click(&mut self, value : Option<&toml::Value>, buttons : u32) {
        if let Some(value) = value {
            self.handlers.push(EventListener {
                x_min : 0.0,
                x_max : 1e20,
                buttons,
                item : None,
                target : Action::from_toml(value)
            })
        }
    }

    #[cfg(feature="dbus")]
    pub fn from_tray(owner : Rc<str>, path : Rc<str>) -> Self {
        let mut sink = EventSink::default();
        sink.handlers.push(EventListener {
            x_min : -1e20,
            x_max : 1e20,
            buttons : 7 | (15 << 5),
            item : None,
            target : Action::from_tray(owner, path),
        });
        sink
    }

    pub fn add_tooltip(&mut self, desc : PopupDesc) {
        self.hovers.push((0.0, 1e20, desc));
    }

    pub fn set_item(&mut self, item : &IterationItem) {
        for h in &mut self.handlers {
            h.item.get_or_insert_with(|| item.clone());
        }
    }

    pub fn merge(&mut self, sink : Self) {
        self.handlers.extend(sink.handlers);
        self.hovers.extend(sink.hovers);
    }

    pub fn offset_clamp(&mut self, offset : f32, min : f32, max : f32) {
        for h in &mut self.handlers {
            h.x_min += offset;
            h.x_max += offset;
            if h.x_min < min {
                h.x_min = min;
            } else if h.x_min > max {
                h.x_min = max;
            }
            if h.x_max < min {
                h.x_max = min;
            } else if h.x_max > max {
                h.x_max = max;
            }
        }
        for (x_min, x_max, _) in &mut self.hovers {
            *x_min += offset;
            *x_max += offset;
            if *x_min < min {
                *x_min = min;
            } else if *x_min > max {
                *x_min = max;
            }
            if *x_max < min {
                *x_max = min;
            } else if *x_max > max {
                *x_max = max;
            }
        }
    }

    pub fn button(&self, x : f32, y : f32, button : u32, runtime : &mut Runtime) {
        let _ = y;
        for h in &self.handlers {
            if x < h.x_min || x > h.x_max {
                continue;
            }
            if (h.buttons & (1 << button)) == 0 {
                continue;
            }
            if h.item.is_none() {
                h.target.invoke(runtime, button);
            } else {
                let item_var = runtime.get_item_var();
                item_var.set(h.item.clone());
                h.target.invoke(runtime, button);
                item_var.set(None);
            }
        }
    }

    #[cfg_attr(not(feature="dbus"), allow(unused))]
    pub fn add_hover(&mut self, min : f32, max : f32, desc : PopupDesc) {
        self.hovers.push((min, max, desc));
    }

    pub fn get_hover(&mut self, x : f32, y : f32) -> Option<(f32, f32, &mut PopupDesc)> {
        let _ = y;
        for &mut (min, max, ref mut text) in &mut self.hovers {
            if x >= min && x < max {
                return Some((min, max, text));
            }
        }
        None
    }

    pub fn for_active_regions(&self, mut f : impl FnMut(f32, f32)) {
        let mut ha = self.handlers.iter().peekable();
        let mut ho = self.hovers.iter().peekable();
        loop {
            let a_min = ha.peek().map_or(f32::NAN, |e| e.x_min);
            let o_min = ho.peek().map_or(f32::NAN, |e| e.0);
            let min = f32::min(a_min, o_min);
            let mut max;
            if min == a_min {
                max = ha.next().unwrap().x_max;
            } else if min == o_min {
                max = ho.next().unwrap().1;
            } else {
                // NaN
                return;
            }
            loop {
                if ha.peek().map_or(f32::NAN, |e| e.x_min) <= max + 1.0 {
                    max = f32::max(max, ha.next().map_or(f32::NAN, |e| e.x_max));
                } else if ho.peek().map_or(f32::NAN, |e| e.0) <= max + 1.0 {
                    max = f32::max(max, ho.next().map_or(f32::NAN, |e| e.1));
                } else {
                    break;
                }
            }
            f(min, max);
        }
    }
}

/// Handler invoked by a click or touch event
#[derive(Debug,Clone)]
pub enum Action {
    Exec { format : String },
    Write { target : String, format : String },
    List(Vec<Action>),
    #[cfg(feature="dbus")]
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

    #[cfg(feature="dbus")]
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
                        let cmd = cmd.into_text();
                        info!("Executing '{}'", cmd);
                        match Command::new("/bin/sh").arg("-c").arg(&cmd[..]).spawn() {
                            Ok(child) => drop(child),
                            Err(e) => error!("Could not execute {}: {}", cmd, e),
                        }
                    }
                    Err(e) => {
                        error!("Error expanding format for command: {}", e);
                    }
                }
            }
            #[cfg(feature="dbus")]
            Action::Tray { owner, path } => {
                tray::do_click(owner, path, how);
            }
            Action::None => { info!("Invoked a no-op"); }
        }
    }
}
