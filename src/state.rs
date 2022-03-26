use log::{debug,info,warn,error};
use futures_util::future::poll_fn;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::time::Instant;
use std::rc::{self,Rc};
use std::task;
use wayland_client::Connection;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_callback;

use crate::bar::Bar;
use crate::data::{Module,IterationItem,Value};
use crate::font::FontMapped;
use crate::item::*;
use crate::render::{Renderer,RenderCache};
use crate::util::{Cell,spawn,spawn_noerr};
use crate::wayland::{SurfaceData,WaylandClient};

#[derive(Debug,Clone)]
struct Notifier {
    inner : Rc<NotifierInner>,
}

#[derive(Debug,Copy,Clone,PartialEq,Eq)]
enum NotifyState {
    Idle,
    DrawOnly,
    NewData,
}

#[derive(Debug)]
struct NotifierInner {
    waker : Cell<Option<task::Waker>>,
    state : Cell<NotifyState>,
    data_update_seq : Cell<u64>,
}

impl Notifier {
    pub fn notify_data(&self, who : &str) {
        debug!("{} triggered refresh", who);
        self.inner.state.set(NotifyState::NewData);
        self.inner.waker.take().map(|w| w.wake());
    }

    pub fn notify_draw_only(&self) {
        if self.inner.state.get() == NotifyState::Idle {
            self.inner.state.set(NotifyState::DrawOnly);
            self.inner.waker.take().map(|w| w.wake());
        }
    }
}

#[derive(Debug,Default,Clone)]
pub struct NotifierList(Option<Notifier>);

impl NotifierList {
    pub fn active(rt : &Runtime) -> Self {
        NotifierList(Some(Notifier { inner : rt.notify.inner.clone() }))
    }

    pub fn data_update_seq(&self) -> u64 {
        self.0.as_ref().map(|n| n.inner.data_update_seq.get()).unwrap_or_default()
    }

    /// Add the currently-rendering bar to this list
    ///
    /// The next call to notify_data will redraw the bar that was rendering when this was called.
    pub fn add(&mut self, rt : &Runtime) {
        self.0 = Some(Notifier { inner : rt.notify.inner.clone() });
    }

    pub fn merge(&mut self, other: &Self) {
        if self.0.is_none() {
            self.clone_from(other);
        }
    }

    /// Notify the bars in the list, and then remove them until they  Future calls to notify_data
    /// will do nothing until you add() bars again.
    pub fn notify_data(&mut self, who : &str) {
        self.0.take().map(|n| n.notify_data(who));
    }
}

/// Common state available during rendering operations
#[derive(Debug)]
pub struct Runtime {
    pub xdg : xdg::BaseDirectories,
    pub fonts : Vec<FontMapped>,
    pub items : HashMap<String, Rc<Item>>,
    pub cache: RenderCache,
    pub wayland : WaylandClient,
    item_var : Rc<Item>,
    notify : Notifier,
    read_depth : Cell<u8>,
}

impl Runtime {
    pub fn get_recursion_handle(&self) -> Option<impl Sized + '_> {
        let depth = self.read_depth.get();
        if depth > 80 {
            None
        } else {
            self.read_depth.set(depth + 1);
            struct LoopRef<'a>(&'a Runtime);
            impl<'a> Drop for LoopRef<'a> {
                fn drop(&mut self) {
                    self.0.read_depth.set(self.0.read_depth.get() - 1)
                }
            }
            Some(LoopRef(self))
        }
    }

    pub fn format<'a>(&'a self, fmt : &'a str) -> Result<Value<'a>, strfmt::FmtError> {
        if !fmt.contains("{") {
            return Ok(Value::Borrow(fmt));
        }
        if fmt.starts_with("{") && fmt.ends_with("}") && !fmt[1..fmt.len() - 1].contains(&['{', ':'] as &[char]) {
            let q = &fmt[1..fmt.len() - 1];
            let (name, key) = match q.find('.') {
                Some(p) => (&q[..p], &q[p + 1..]),
                None => (&q[..], ""),
            };
            if let Some(item) = self.items.get(name) {
                return Ok(item.data.read_to_owned(name, key, self));
            } else {
                return Err(strfmt::FmtError::KeyError(name.to_string()));
            }
        }
        strfmt::strfmt_map(fmt, &|mut q| {
            let (name, key) = match q.key.find('.') {
                Some(p) => (&q.key[..p], &q.key[p + 1..]),
                None => (&q.key[..], ""),
            };
            match self.items.get(name) {
                Some(item) => {
                    item.data.read_in(name, key, self, |s| match s {
                        Value::Borrow(s) => q.str(s),
                        Value::Owned(s) => q.str(&s),
                        Value::Float(f) => q.f64(f),
                        Value::Bool(true) => q.str("1"),
                        Value::Bool(false) => q.str("0"),
                        Value::Null => q.str(""),
                    })
                }
                None => Err(strfmt::FmtError::KeyError(name.to_string()))
            }
        }).map(Value::Owned)
    }

    pub fn format_or<'a>(&'a self, fmt : &'a str, context : &str) -> Value<'a> {
        match self.format(fmt) {
            Ok(v) => v,
            Err(e) => {
                warn!("Error formatting '{}': {}", context, e);
                Value::Null
            }
        }
    }

    pub fn copy_item_var(&self) -> Option<IterationItem> {
        self.get_item_var().take_in_some(|v| v.clone())
    }

    pub fn get_item_var(&self) -> &Cell<Option<IterationItem>> {
        match &*self.item_var {
            &Item { data : Module::Item { ref value }, .. } => value,
            _ => {
                panic!("The 'item' variable was not assignable");
            }
        }
    }
}

/// The singleton global state object
#[derive(Debug)]
pub struct State {
    pub bars : Vec<Bar>,
    bar_config : Vec<toml::Value>,
    pub renderer : Renderer,
    pub runtime : Runtime,
    this : rc::Weak<RefCell<State>>,
}

impl State {
    pub fn new(wayland : WaylandClient) -> Result<Rc<RefCell<Self>>, Box<dyn Error>> {
        let notify_inner = Rc::new(NotifierInner {
            waker : Cell::new(None),
            state : Cell::new(NotifyState::NewData),
            data_update_seq : Cell::new(1),
        });
        log::debug!("State::new");

        let mut state = Self {
            bars : Vec::new(),
            bar_config : Vec::new(),
            renderer: Renderer::new(),
            runtime : Runtime {
                xdg : xdg::BaseDirectories::new()?,
                fonts : Vec::new(),
                cache: RenderCache::new(),
                items : Default::default(),
                item_var : Rc::new(Module::new_current_item().into()),
                notify : Notifier { inner : notify_inner.clone() },
                read_depth : Cell::new(0),
                wayland,
            },
            this : rc::Weak::new(),
        };

        state.load_config(false)?;

        let rv = Rc::new(RefCell::new(state));
        rv.borrow_mut().this = Rc::downgrade(&rv);

        let state = rv.clone();
        spawn_noerr(async move {
            loop {
                poll_fn(|ctx| {
                    notify_inner.waker.set(Some(ctx.waker().clone()));
                    match notify_inner.state.get() {
                        NotifyState::Idle => task::Poll::Pending,
                        _ => task::Poll::Ready(()),
                    }
                }).await;
                let mut state = state.borrow_mut();
                state.draw_now();
            }
        });

        let state = rv.clone();
        spawn("Config reload", async move {
            let mut hups = tokio::signal::unix::signal(tokio::signal::unix::SignalKind::hangup())?;
            while let Some(()) = hups.recv().await {
                match state.borrow_mut().load_config(true) {
                    Ok(()) => (),
                    Err(e) => error!("Config reload failed: {}", e),
                }
            }
            Ok(())
        });

        Ok(rv)
    }

    /// Note: always call from a task, not drectly from dispatch
    fn load_config(&mut self, reload : bool) -> Result<(), Box<dyn Error>> {
        let mut bar_config = Vec::new();
        let mut font_list = Vec::new();

        let config_path = self.runtime.xdg.find_config_file("rwaybar.toml")
            .ok_or("Could not find configuration: create ~/.config/rwaybar.toml")?;

        let cfg = std::fs::read_to_string(config_path)?;
        let config : toml::Value = toml::from_str(&cfg)?;

        let cfg = config.as_table().unwrap();

        let new_items = cfg.iter().filter_map(|(key, value)| {
            match key.as_str() {
                "bar" => {
                    if let Some(bars) = value.as_array() {
                        bar_config.extend(bars.iter().cloned());
                    } else {
                        bar_config.push(value.clone());
                    }
                    None
                }
                "fonts" => {
                    if let Some(list) = value.as_table() {
                        font_list = list.iter().collect();
                    }
                    None
                }
                _ => {
                    let key = key.to_owned();
                    let value = Rc::new(Item::from_item_list(&key, value));
                    Some((key, value))
                }
            }
        }).collect();

        if bar_config.is_empty() {
            Err("At least one [[bar]] section is required")?;
        }

        let mut fonts = Vec::with_capacity(font_list.len());
        for (name, path) in font_list {
            match FontMapped::new(name.clone(), path.as_str().unwrap_or("").to_owned().into()) {
                Ok(font) => fonts.push(font),
                Err(e) => {
                    error!("Could not load font '{name}' from {path}: {e}");
                }
            }
        }

        if fonts.is_empty() {
            Err("At least one valid font is required in the [fonts] section")?;
        }

        debug!("Loading configuration");

        let mut old_items = std::mem::replace(&mut self.runtime.items, new_items);
        self.bar_config = bar_config;
        self.runtime.fonts = fonts;

        self.runtime.items.insert("item".into(), self.runtime.item_var.clone());

        for (k,v) in &self.runtime.items {
            if let Some(item) = old_items.remove(k) {
                v.data.init(k, &self.runtime, Some(&item.data));
            } else {
                v.data.init(k, &self.runtime, None);
            }
        }
        self.runtime.notify.inner.state.set(NotifyState::NewData);

        self.bars.clear();
        for output in self.runtime.wayland.output.outputs() {
            self.output_ready(&output);
        }
        if reload {
            if self.bars.is_empty() {
                error!("No bars matched this outptut configuration.  Available outputs:");
                for output in self.runtime.wayland.output.outputs() {
                    if let Some(oi) = self.runtime.wayland.output.info(&output) {
                        error!(" name='{}' description='{}' make='{}' model='{}'",
                            oi.name.as_deref().unwrap_or_default(),
                            oi.description.as_deref().unwrap_or_default(),
                            oi.make, oi.model);
                    }
                }
            }
            self.runtime.notify.notify_data("reload");
        } else {
            self.set_data();
        }

        Ok(())
    }

    pub fn request_draw(&mut self) {
        self.runtime.notify.notify_draw_only();
    }

    fn set_data(&mut self) {
        // Propagate new_data notifications to all bar dirty fields
        match self.runtime.notify.inner.state.replace(NotifyState::Idle) {
            NotifyState::Idle => return,
            NotifyState::DrawOnly => return,
            NotifyState::NewData => {}
        }

        let seq = self.runtime.notify.inner.data_update_seq.get();
        self.runtime.notify.inner.data_update_seq.set(seq + 1);

        // A smarter notify could only damage some of the surfaces
        for bar in &self.bars {
            SurfaceData::from_wl(bar.ls.wl_surface()).damage_full();
            if let Some(popup) = &bar.popup {
                SurfaceData::from_wl(&popup.wl.surf).damage_full();
            }
        }
    }

    pub fn draw_now(&mut self) {
        self.set_data();

        let begin = Instant::now();
        for bar in &mut self.bars {
            bar.render_with(&mut self.runtime, &mut self.renderer);
        }
        self.runtime.cache.prune(begin);
        self.runtime.wayland.flush();
        let render_time = begin.elapsed().as_nanos();
        log::debug!("Frame took {}.{:06} ms", render_time / 1_000_000, render_time % 1_000_000);
    }

    pub fn output_ready(&mut self, output : &WlOutput) {
        let data = match self.runtime.wayland.output.info(&output) {
            Some(info) => info,
            None => return,
        };
        info!("Output name='{}' description='{}' make='{}' model='{}'",
            data.name.as_deref().unwrap_or_default(),
            data.description.as_deref().unwrap_or_default(),
            data.make, data.model);
        for (i, cfg) in self.bar_config.iter().enumerate() {
            if let Some(name) = cfg.get("name").and_then(|v| v.as_str()) {
                if Some(name) != data.name.as_deref() {
                    continue;
                }
            }
            if let Some(make) = cfg.get("make").and_then(|v| v.as_str()) {
                match regex::Regex::new(make) {
                    Ok(re) => {
                        if !re.is_match(&data.make) {
                            continue;
                        }
                    }
                    Err(e) => {
                        error!("Ignoring invalid regex in bar.make: {}", e);
                    }
                }
            }
            if let Some(model) = cfg.get("model").and_then(|v| v.as_str()) {
                match regex::Regex::new(model) {
                    Ok(re) => {
                        if !re.is_match(&data.model) {
                            continue;
                        }
                    }
                    Err(e) => {
                        error!("Ignoring invalid regex in bar.model: {}", e);
                    }
                }
            }
            if let Some(description) = cfg.get("description").and_then(|v| v.as_str()) {
                match regex::Regex::new(description) {
                    Ok(re) => {
                        if !re.is_match(data.description.as_deref().unwrap_or_default()) {
                            continue;
                        }
                    }
                    Err(e) => {
                        error!("Ignoring invalid regex in bar.description: {}", e);
                    }
                }
            }
            let mut cfg = cfg.clone();
            let name = data.name.clone().unwrap_or_default();
            self.bars.retain(|bar| {
                bar.cfg_index != i || &*bar.name != name
            });
            if let Some(table) = cfg.as_table_mut() {
                table.insert("name".into(), name.into());
            }

            let bar = Bar::new(&mut self.runtime.wayland, &output, &data, cfg, i);
            self.bars.push(bar);
            self.runtime.wayland.flush();
        }
    }
}

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
pub enum Callbacks {
    Init2,
}

impl wayland_client::Dispatch<wl_callback::WlCallback, Callbacks> for State {
    fn event(&mut self, _: &wl_callback::WlCallback, _: wl_callback::Event, why: &Callbacks,
        _: &Connection, _: &wayland_client::QueueHandle<Self>)
    {
        match why {
            Callbacks::Init2 => {
                debug!("Done with initial events; checking if config is empty.");
                if self.bars.is_empty() {
                    error!("No bars matched this outptut configuration.  Available outputs:");
                    for output in self.runtime.wayland.output.outputs() {
                        if let Some(oi) = self.runtime.wayland.output.info(&output) {
                            error!(" name='{}' description='{}' make='{}' model='{}'",
                                oi.name.as_deref().unwrap_or_default(),
                                oi.description.as_deref().unwrap_or_default(),
                                oi.make, oi.model);
                        }
                    }
                }
            }
        }
    }
}
