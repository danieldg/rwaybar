use futures_util::future::poll_fn;
use log::{debug, error, info, warn};
use smithay_client_toolkit::shell::WaylandSurface;
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    iter,
    rc::{self, Rc},
    task,
    time::Instant,
};
use wayland_client::{
    protocol::{wl_callback, wl_output::WlOutput},
    Connection,
};

use crate::{
    bar::Bar,
    data::{EvalContext, IterationItem, Module, Value},
    font::FontMapped,
    item::*,
    render::Renderer,
    util::{spawn, spawn_noerr, Cell},
    wayland::{SurfaceData, WaylandClient},
};

#[derive(Default, Debug, Copy, Clone, Eq, PartialEq)]
pub struct InterestMask(u64);

impl InterestMask {
    pub fn bar_region(&self, region: u64) -> Self {
        InterestMask(self.0 * region)
    }
}

thread_local! {
    static NOTIFY: NotifierInner = NotifierInner {
        waker: Cell::new(None),
        state: Cell::new(NotifyState::Idle),
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum NotifyState {
    Idle,
    DrawOnly,
    NewData(InterestMask),
}

#[derive(Debug)]
struct NotifierInner {
    waker: Cell<Option<task::Waker>>,
    state: Cell<NotifyState>,
}

impl NotifierInner {
    pub fn notify_draw_only(&self) {
        if self.state.get() == NotifyState::Idle {
            self.state.set(NotifyState::DrawOnly);
            self.waker.take().map(|w| w.wake());
        }
    }

    fn full_redraw(&self) {
        self.waker.take().map(|w| w.wake());
        self.state.set(NotifyState::NewData(InterestMask(!0)));
    }
}

#[derive(Debug, Clone)]
pub struct DrawNotifyHandle {}

impl DrawNotifyHandle {
    pub fn new(_: &Runtime) -> Self {
        Self {}
    }

    pub fn notify_draw_only(&self) {
        NOTIFY.with(|notify| notify.notify_draw_only());
    }
}

#[derive(Debug, Default)]
pub struct NotifierList {
    interest: Cell<InterestMask>,
}

impl NotifierList {
    /// Add the currently-rendering bar to this list
    ///
    /// The next call to notify_data will redraw the bar that was rendering when this was called.
    pub fn add(&self, rt: &Runtime) {
        self.interest
            .set(InterestMask(self.interest.get().0 | rt.interest.get().0));
    }

    /// Add all the items in `other` to this notifier, so they will also be marked dirty when this
    /// notifier is used.
    pub fn merge(&self, other: &Self) {
        self.interest
            .set(InterestMask(self.interest.get().0 | other.interest.get().0));
    }

    /// Mark all items in this notifier list as dirty.
    ///
    /// Future calls to notify_data will do nothing until you add() bars again.
    pub fn notify_data(&self, who: &str) {
        let mut interest = self.interest.take();
        if interest.0 == 0 {
            return;
        }
        debug!(
            "{} triggered refresh on {}",
            who,
            (0..32)
                .filter_map(|i| {
                    let v = (interest.0 >> (2 * i)) & 3;
                    if v != 0 {
                        use std::fmt::Write;
                        let mut r = String::new();
                        for (i, c) in b"BP".iter().enumerate() {
                            if v & (1 << i) != 0 {
                                r.push(*c as char)
                            }
                        }
                        write!(r, "{i}").unwrap();
                        Some(r)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
                .join(", ")
        );
        NOTIFY.with(|notify| {
            match notify.state.get() {
                NotifyState::Idle => {
                    notify.waker.take().map(|w| w.wake());
                }
                NotifyState::DrawOnly => {
                    // already woken, and no added items
                }
                NotifyState::NewData(mask) => {
                    interest.0 |= mask.0;
                }
            }
            notify.state.set(NotifyState::NewData(interest))
        });
    }
}

/// Common state available during rendering operations
#[derive(Debug)]
pub struct Runtime {
    pub xdg: xdg::BaseDirectories,
    pub fonts: Vec<FontMapped>,
    pub items: HashMap<Rc<str>, Rc<Item>>,
    pub wayland: WaylandClient,
    item_var: Rc<Item>,
    interest: Cell<InterestMask>,
    read_depth: Cell<u8>,
}

impl Runtime {
    pub fn set_interest_mask(&self, mask: InterestMask) {
        self.interest.set(mask);
    }

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

    pub fn eval(&self, expr: &str) -> Result<Value<'static>, evalexpr::EvalexprError> {
        let expr = evalexpr::build_operator_tree(expr)?;
        let mut vars = Vec::new();
        for ident in expr.iter_variable_identifiers() {
            if let Some(item) = self.items.get(ident) {
                let value = item.data.read_to_owned(ident, "", self);
                vars.push((ident, value.into()));
            } else {
                return Err(evalexpr::EvalexprError::CustomMessage(format!(
                    "Value {ident} not found"
                )));
            }
        }
        let ctx = EvalContext { rt: &self, vars };
        expr.eval_with_context(&ctx).map(Into::into)
    }

    pub fn format<'a>(&'a self, fmt: &'a str) -> Result<Value<'a>, strfmt::FmtError> {
        if !fmt.contains("{") {
            return Ok(Value::Borrow(fmt));
        }
        if fmt.starts_with("{")
            && fmt.ends_with("}")
            && !fmt[1..fmt.len() - 1].contains(&['{', ':'] as &[char])
        {
            let q = &fmt[1..fmt.len() - 1];
            if q.starts_with("=") {
                return self
                    .eval(&q[1..])
                    .map_err(|e| strfmt::FmtError::KeyError(e.to_string()));
            }
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

        strfmt::strfmt_map(fmt, |mut q| {
            if q.key.starts_with("=") {
                match self.eval(&q.key[1..]) {
                    Ok(s) => {
                        return match Value::from(s) {
                            Value::Borrow(s) => q.str(s),
                            Value::Owned(s) => q.str(&s),
                            Value::Float(f) => q.f64(f),
                            Value::Bool(true) => q.str("1"),
                            Value::Bool(false) => q.str("0"),
                            Value::Null => q.str(""),
                        }
                    }
                    Err(e) => {
                        return Err(strfmt::FmtError::KeyError(e.to_string()));
                    }
                }
            }
            let (name, key) = match q.key.find('.') {
                Some(p) => (&q.key[..p], &q.key[p + 1..]),
                None => (&q.key[..], ""),
            };
            match self.items.get(name) {
                Some(item) => item.data.read_in(name, key, self, |s| match s {
                    Value::Borrow(s) => q.str(s),
                    Value::Owned(s) => q.str(&s),
                    Value::Float(f) => q.f64(f),
                    Value::Bool(true) => q.str("1"),
                    Value::Bool(false) => q.str("0"),
                    Value::Null => q.str(""),
                }),
                None => Err(strfmt::FmtError::KeyError(name.to_string())),
            }
        })
        .map(Value::Owned)
    }

    pub fn format_or<'a>(&'a self, fmt: &'a str, context: &str) -> Value<'a> {
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
            &Item {
                data: Module::Item { ref value },
                ..
            } => value,
            _ => {
                panic!("The 'item' variable was not assignable");
            }
        }
    }
}

/// The singleton global state object
#[derive(Debug)]
pub struct State {
    pub bars: Vec<Bar>,
    bar_config: Vec<toml::Value>,
    pub renderer: Renderer,
    pub runtime: Runtime,
    this: rc::Weak<RefCell<State>>,
}

impl State {
    pub fn new(wayland: WaylandClient) -> Result<Rc<RefCell<Self>>, Box<dyn Error>> {
        log::debug!("State::new");

        let mut state = Self {
            bars: Vec::new(),
            bar_config: Vec::new(),
            renderer: Renderer::new(),
            runtime: Runtime {
                xdg: xdg::BaseDirectories::new()?,
                fonts: Vec::new(),
                items: Default::default(),
                item_var: Item::new_current_item(),
                interest: Cell::new(InterestMask(0)),
                read_depth: Cell::new(0),
                wayland,
            },
            this: rc::Weak::new(),
        };

        state.load_config(false)?;

        let rv = Rc::new(RefCell::new(state));
        rv.borrow_mut().this = Rc::downgrade(&rv);

        let state = rv.clone();
        spawn_noerr(async move {
            loop {
                poll_fn(|ctx| {
                    NOTIFY.with(|notify| {
                        if notify.state.get() == NotifyState::Idle {
                            notify.waker.set(Some(ctx.waker().clone()));
                            task::Poll::Pending
                        } else {
                            task::Poll::Ready(())
                        }
                    })
                })
                .await;
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
    fn load_config(&mut self, reload: bool) -> Result<(), Box<dyn Error>> {
        let mut bar_config = Vec::new();
        let mut font_list = Vec::new();

        let config_path = self
            .runtime
            .xdg
            .find_config_file("rwaybar.toml")
            .ok_or("Could not find configuration: create ~/.config/rwaybar.toml")?;

        let cfg = std::fs::read_to_string(config_path)?;
        let config: toml::Value = toml::from_str(&cfg)?;

        let cfg = config.as_table().unwrap();

        let new_items = cfg
            .iter()
            .filter_map(|(key, value)| match key.as_str() {
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
                key => {
                    let key = key.into();
                    let value = Rc::new(Item::from_item_list(&key, value));
                    Some((key, value))
                }
            })
            .collect();

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

        self.runtime
            .items
            .insert("item".into(), self.runtime.item_var.clone());

        for (k, v) in &self.runtime.items {
            if let Some(item) = old_items.remove(k) {
                v.data.init(k, &self.runtime, Some(&item.data));
            } else {
                v.data.init(k, &self.runtime, None);
            }
        }
        NOTIFY.with(|notify| notify.full_redraw());

        self.bars.clear();
        for output in self.runtime.wayland.output.outputs() {
            self.output_ready(&output);
        }
        if reload {
            if self.bars.is_empty() {
                error!("No bars matched this outptut configuration.  Available outputs:");
                for output in self.runtime.wayland.output.outputs() {
                    if let Some(oi) = self.runtime.wayland.output.info(&output) {
                        error!(
                            " name='{}' description='{}' make='{}' model='{}'",
                            oi.name.as_deref().unwrap_or_default(),
                            oi.description.as_deref().unwrap_or_default(),
                            oi.make,
                            oi.model
                        );
                    }
                }
            }
        } else {
            self.set_data();
        }

        Ok(())
    }

    /// Request a redraw of all surfaces that have been damaged and whose rendering is not
    /// throttled.  This should be called after damaging a surface in some way unrelated to the
    /// items on the surface, such as by receiving a configure or scale event from the compositor.
    pub fn request_draw(&mut self) {
        NOTIFY.with(|notify| notify.notify_draw_only());
    }

    fn set_data(&mut self) {
        // Propagate new_data notifications to all bar dirty fields
        let dirty_mask = match NOTIFY.with(|notify| notify.state.replace(NotifyState::Idle)) {
            NotifyState::Idle => return,
            NotifyState::DrawOnly => return,
            NotifyState::NewData(d) => d.0,
        };

        for (i, bar) in (0..31).chain(iter::repeat(31)).zip(&mut self.bars) {
            let mask = (dirty_mask >> (2 * i)) & 3;
            if mask & 1 != 0 {
                SurfaceData::from_wl(bar.ls.wl_surface()).damage_full();
            }
            if mask & 2 != 0 {
                if let Some(popup) = &bar.popup {
                    SurfaceData::from_wl(&popup.wl.surf).damage_full();
                }
            }
        }
    }

    pub fn draw_now(&mut self) {
        self.set_data();

        let begin = Instant::now();
        for (i, bar) in (0..31).chain(iter::repeat(31)).zip(&mut self.bars) {
            let mask = InterestMask(1 << (2 * i));
            bar.render_with(mask, &mut self.runtime, &mut self.renderer);
        }
        self.runtime.set_interest_mask(InterestMask(0));
        self.renderer.cache.prune(begin);
        self.runtime.wayland.flush();
        let render_time = begin.elapsed().as_nanos();
        log::debug!(
            "Frame took {}.{:06} ms",
            render_time / 1_000_000,
            render_time % 1_000_000
        );
    }

    pub fn output_ready(&mut self, output: &WlOutput) {
        let data = match self.runtime.wayland.output.info(&output) {
            Some(info) => info,
            None => return,
        };
        info!(
            "Output name='{}' description='{}' make='{}' model='{}'",
            data.name.as_deref().unwrap_or_default(),
            data.description.as_deref().unwrap_or_default(),
            data.make,
            data.model
        );
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
            self.bars
                .retain(|bar| bar.cfg_index != i || &*bar.name != name);
            if let Some(table) = cfg.as_table_mut() {
                table.insert("name".into(), name.into());
            }

            let bar = Bar::new(&mut self.runtime.wayland, &output, &data, cfg, i);
            self.bars.push(bar);
            self.runtime.wayland.flush();
        }
    }
}

pub struct OutputsReadyCallback;

impl wayland_client::Dispatch<wl_callback::WlCallback, OutputsReadyCallback> for State {
    fn event(
        state: &mut State,
        _: &wl_callback::WlCallback,
        _: wl_callback::Event,
        _: &OutputsReadyCallback,
        _: &Connection,
        _: &wayland_client::QueueHandle<Self>,
    ) {
        debug!("Done with initial events; checking if config is empty.");
        if state.bars.is_empty() {
            error!("No bars matched this outptut configuration.  Available outputs:");
            for output in state.runtime.wayland.output.outputs() {
                if let Some(oi) = state.runtime.wayland.output.info(&output) {
                    error!(
                        " name='{}' description='{}' make='{}' model='{}'",
                        oi.name.as_deref().unwrap_or_default(),
                        oi.description.as_deref().unwrap_or_default(),
                        oi.make,
                        oi.model
                    );
                }
            }
        }
    }
}
