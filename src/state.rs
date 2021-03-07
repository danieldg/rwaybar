use futures_util::future::select;
use futures_util::pin_mut;
use log::{debug,info,warn,error};
use smithay_client_toolkit::output::with_output_info;
use smithay_client_toolkit::output::OutputInfo;
use smithay_client_toolkit::output::OutputStatusListener;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::time::Instant;
use std::rc::{self,Rc};
use wayland_client::protocol::wl_output::WlOutput;

use crate::bar::Bar;
use crate::data::{Module,IterationItem,Value};
use crate::item::*;
use crate::render::RenderTarget;
use crate::util::{Cell,spawn,spawn_noerr};
use crate::wayland::WaylandClient;

#[derive(Debug)]
struct Notifier {
    inner : Rc<NotifierInner>,
}

#[derive(Debug)]
struct NotifierInner {
    notify : tokio::sync::Notify,
    data_update : Cell<bool>,
}

impl Notifier {
    pub fn notify_data(&self, who : &str) {
        debug!("{} triggered refresh", who);
        self.inner.data_update.set(true);
        self.inner.notify.notify_one();
    }

    pub fn notify_draw_only(&self) {
        self.inner.notify.notify_one();
    }
}

#[derive(Debug,Default)]
pub struct NotifierList(Option<Notifier>);

impl NotifierList {
    /// Add the currently-rendering bar to this list
    ///
    /// The next call to notify_data will redraw the bar that was rendering when this was called.
    pub fn add(&mut self, rt : &Runtime) {
        self.0 = Some(Notifier { inner : rt.notify.inner.clone() });
    }

    /// Notify the bars in the list, and then remove them until they  Future calls to notify_data
    /// will do nothing until you add() bars again.
    pub fn notify_data(&mut self, who : &str) {
        self.0.take().map(|n| n.notify_data(who));
    }
}

/// Common state available during rendering operations
pub struct Runtime {
    pub xdg : xdg::BaseDirectories,
    pub items : HashMap<String, Rc<Item>>,
    item_var : Rc<Item>,
    notify : Notifier,
    refresh : Rc<RefreshState>,
}

#[derive(Default)]
struct RefreshState {
    time : Cell<Option<Instant>>,
    cause : Cell<&'static str>,
    notify : tokio::sync::Notify,
}

impl Runtime {
    pub fn set_wake_at(&self, wake : Instant, who : &'static str) {
        match self.refresh.time.get() {
            Some(t) if t < wake => return,
            _ => ()
        }

        self.refresh.time.set(Some(wake));
        self.refresh.cause.set(who);
        self.refresh.notify.notify_one();
    }

    pub fn format<'a>(&'a self, fmt : &'a str) -> Result<Value<'a>, strfmt::FmtError> {
        if !fmt.contains("{") {
            return Ok(Value::Borrow(fmt));
        }
        if fmt.starts_with("{") && fmt.ends_with("}") && !fmt[1..fmt.len() - 1].contains("{") {
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
pub struct State {
    pub wayland : WaylandClient,
    pub bars : Vec<Bar>,
    bar_config : Vec<toml::Value>,
    pub runtime : Runtime,
    this : rc::Weak<RefCell<State>>,
    #[allow(unused)] // need to hold this handle for the callback to remain alive
    output_status_listener : OutputStatusListener,
}

impl State {
    pub fn new(mut wayland : WaylandClient) -> Result<Rc<RefCell<Self>>, Box<dyn Error>> {
        let notify_inner = Rc::new(NotifierInner {
            notify : tokio::sync::Notify::new(),
            data_update : Cell::new(true),
        });

        let output_status_listener = wayland.add_output_listener(move |output, _oi, mut data| {
            let state : &mut State = data.get().unwrap();
            let state = state.this.upgrade().unwrap();
            spawn_noerr(async move {
                let mut state = state.borrow_mut();
                with_output_info(&output, |oi| {
                    state.output_ready(&output, oi);
                });
            });
        });

        let mut state = Self {
            wayland,
            bars : Vec::new(),
            bar_config : Vec::new(),
            runtime : Runtime {
                xdg : xdg::BaseDirectories::new()?,
                items : Default::default(),
                item_var : Rc::new(Module::new_current_item().into()),
                refresh : Default::default(),
                notify : Notifier { inner : notify_inner.clone() },
            },
            this : rc::Weak::new(),
            output_status_listener,
        };

        state.load_config(false)?;

        let notify = Notifier { inner : state.runtime.notify.inner.clone() };
        let refresh = state.runtime.refresh.clone();
        spawn_noerr(async move {
            loop {
                use futures_util::future::Either;
                if let Some(deadline) = refresh.time.get() {
                    let sleep = tokio::time::sleep_until(deadline.into());
                    let wake = refresh.notify.notified();
                    pin_mut!(sleep, wake);
                    match select(sleep, wake).await {
                        Either::Left(_) => {
                            refresh.time.set(None);
                            notify.notify_data(refresh.cause.get());
                        }
                        _ => {}
                    }
                } else {
                    refresh.notify.notified().await;
                }
            }
        });

        let sync_cb = state.wayland.wl_display.sync();
        sync_cb.quick_assign(move |_sync, _event, mut data| {
            let state : &mut State = data.get().unwrap();
            if !state.bars.is_empty() {
                return;
            }
            error!("No bars matched this outptut configuration.  Available outputs:");
            for output in state.wayland.env.get_all_outputs() {
                with_output_info(&output, |oi| {
                    error!(" name='{}' description='{}' make='{}' model='{}'",
                        oi.name, oi.description, oi.make, oi.model);
                });
            }
        });

        let rv = Rc::new(RefCell::new(state));
        rv.borrow_mut().this = Rc::downgrade(&rv);

        let state = rv.clone();
        spawn_noerr(async move {
            loop {
                notify_inner.notify.notified().await;
                let mut state = state.borrow_mut();
                if state.wayland.renderer.request_draw() {
                    state.draw_now();
                }
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

    fn load_config(&mut self, reload : bool) -> Result<(), Box<dyn Error>> {
        let mut bar_config = Vec::new();

        let config_path = self.runtime.xdg.find_config_file("rwaybar.toml")
            .ok_or("Could not find configuration: create ~/.config/rwaybar.toml")?;

        let cfg = std::fs::read_to_string(config_path)?;
        let config : toml::Value = toml::from_str(&cfg)?;

        let cfg = config.as_table().unwrap();

        let new_items = cfg.iter().filter_map(|(key, value)| {
            if key == "bar" {
                if let Some(bars) = value.as_array() {
                    bar_config.extend(bars.iter().cloned());
                } else {
                    bar_config.push(value.clone());
                }
                None
            } else {
                let key = key.to_owned();
                let value = Rc::new(Item::from_item_list(&key, value));
                Some((key, value))
            }
        }).collect();

        if bar_config.is_empty() {
            Err("At least one [[bar]] section is required")?;
        }

        debug!("Loading configuration");

        let mut old_items = std::mem::replace(&mut self.runtime.items, new_items);
        self.bar_config = bar_config;

        self.runtime.items.insert("item".into(), self.runtime.item_var.clone());

        for (k,v) in &self.runtime.items {
            if let Some(item) = old_items.remove(k) {
                v.data.init(k, &self.runtime, Some(&item.data));
            } else {
                v.data.init(k, &self.runtime, None);
            }
        }
        self.runtime.notify.inner.data_update.set(true);

        self.bars.clear();
        for output in self.wayland.env.get_all_outputs() {
            with_output_info(&output, |oi| {
                self.output_ready(&output, oi);
            });
        }
        if reload {
            if self.bars.is_empty() {
                error!("No bars matched this outptut configuration.  Available outputs:");
                for output in self.wayland.env.get_all_outputs() {
                    with_output_info(&output, |oi| {
                        error!(" name='{}' description='{}' make='{}' model='{}'",
                            oi.name, oi.description, oi.make, oi.model);
                    });
                }
            }
            self.request_draw();
        } else {
            self.set_data();
        }

        Ok(())
    }

    pub fn request_draw(&mut self) {
        self.runtime.notify.notify_draw_only();
    }

    fn set_data(&mut self) {
        // Propagate the data_update field to all bar dirty fields
        if !self.runtime.notify.inner.data_update.get() {
            return;
        }

        for bar in &mut self.bars {
            bar.dirty = true;
        }
        self.runtime.notify.inner.data_update.set(false);
    }

    pub fn draw_now(&mut self) {
        self.set_data();

        let mut shm_size = 0;
        let begin = Instant::now();
        for bar in &self.bars {
            shm_size += bar.get_render_size();
        }

        if shm_size == 0 {
            return;
        }

        let mut target = RenderTarget::new(&mut self.wayland, shm_size);

        for bar in &mut self.bars {
            bar.render_with(&mut self.runtime, &mut target);
        }
        self.wayland.flush();
        let render_time = begin.elapsed().as_nanos();
        log::debug!("Frame took {}.{:06} ms", render_time / 1_000_000, render_time % 1_000_000);
    }

    pub fn output_ready(&mut self, output : &WlOutput, data : &OutputInfo) {
        if data.obsolete {
            return;
        }
        info!("Output name='{}' description='{}' make='{}' model='{}'",
            data.name, data.description, data.make, data.model);
        for (i, cfg) in self.bar_config.iter().enumerate() {
            if let Some(name) = cfg.get("name").and_then(|v| v.as_str()) {
                if name != &data.name {
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
                        if !re.is_match(&data.description) {
                            continue;
                        }
                    }
                    Err(e) => {
                        error!("Ignoring invalid regex in bar.description: {}", e);
                    }
                }
            }
            let mut cfg = cfg.clone();
            if let Some(table) = cfg.as_table_mut() {
                table.insert("name".into(), data.name.clone().into());
            }

            let bar = Bar::new(&self.wayland, &output, data.scale_factor, cfg, i);
            self.bars.retain(|bar| {
                bar.cfg_index != i ||
                    bar.item.config
                        .as_ref()
                        .and_then(|c| c.get("name"))
                        .and_then(|n| n.as_str())
                    != Some(&data.name)
            });
            self.bars.push(bar);
            self.wayland.flush();
        }
    }

}
