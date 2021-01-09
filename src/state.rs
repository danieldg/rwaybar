use futures_util::future::select;
use futures_util::pin_mut;
use log::{debug,info,warn,error};
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use std::time::Instant;
use std::rc::Rc;
use wayland_client::Attached;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_client::protocol::wl_callback::WlCallback;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;

use layer_shell::zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer};
use layer_shell::zwlr_layer_surface_v1::{ZwlrLayerSurfaceV1, Anchor};

use crate::item::*;
use crate::data::{Module,IterationItem};
use crate::util::{Cell,spawn,spawn_noerr};
use crate::wayland::{OutputData,Popup,WaylandClient};

pub struct BarPopup {
    pub wl : Popup,
    vanish : Option<Instant>,
}

/// A single taskbar on a single output
pub struct Bar {
    pub surf : Attached<WlSurface>,
    pub ls_surf : Attached<ZwlrLayerSurfaceV1>,
    pub popup : Option<BarPopup>,
    pub sink : EventSink,
    pub anchor_top : bool,
    popup_x : f64,
    pub scale : i32,
    pixel_width : i32,
    pixel_height : i32,
    dirty : bool,
    throttle : Option<Attached<WlCallback>>,
    item : Item,
    cfg_index : usize,
}

impl Bar {
    fn get_render_size(&self) -> usize {
        let mut rv = 0;
        if self.dirty && self.throttle.is_none() {
            let stride = cairo::Format::ARgb32.stride_for_width(self.pixel_width as u32).unwrap();
            rv += (self.pixel_height as usize) * (stride as usize);
        }
        if let Some(popup) = &self.popup {
            if !popup.wl.waiting_on_configure {
                let stride = cairo::Format::ARgb32.stride_for_width((popup.wl.size.0 * popup.wl.scale) as u32).unwrap();
                rv += (popup.wl.size.1 * popup.wl.scale) as usize * (stride as usize);
            }
        }
        rv
    }

    fn render_with(&mut self, runtime : &mut Runtime, target : &mut RenderTarget) {
        if self.dirty && self.throttle.is_none() {
            let rt_item = runtime.items.entry("bar".into()).or_insert_with(Item::none);
            std::mem::swap(&mut self.item, rt_item);

            let surf = cairo::RecordingSurface::create(cairo::Content::ColorAlpha,
                cairo::Rectangle {
                    x : 0.0,
                    y : 0.0,
                    width : self.pixel_width as f64,
                    height : self.pixel_height as f64,
                }).expect("Error creating bar rendering surface");
            let ctx = cairo::Context::new(&surf);
            let mut scale_matrix = cairo::Matrix::identity();
            scale_matrix.scale(self.scale as f64, self.scale as f64);
            ctx.set_matrix(scale_matrix);
            let font = pango::FontDescription::new();
            ctx.set_operator(cairo::Operator::Clear);
            ctx.paint();
            ctx.set_operator(cairo::Operator::Over);
            let render_extents = ctx.clip_extents();
            let render_pos = Cell::new(0.0);

            let ctx = Render {
                cairo : &ctx,
                font : &font,
                align : Align::bar_default(),
                render_extents : &render_extents,
                render_pos : &render_pos,
                err_name: "bar",
                text_stroke : None,
                runtime,
            };
            self.sink = ctx.runtime.items["bar"].render(&ctx);

            target.render((self.pixel_width, self.pixel_height), &self.surf, &surf);
            std::mem::swap(&mut self.item, runtime.items.get_mut("bar").unwrap());

            let frame = self.surf.frame();
            let id = frame.as_ref().id();
            frame.quick_assign(move |_frame, _event, mut data| {
                let state : &mut State = data.get().unwrap();
                for bar in &mut state.bars {
                    let done = match bar.throttle.as_ref() {
                        Some(cb) if !cb.as_ref().is_alive() => true,
                        Some(cb) if cb.as_ref().id() == id => true,
                        _ => false,
                    };
                    if done {
                        bar.throttle.take();
                    }
                }
                state.request_draw();
            });
            self.surf.commit();
            self.throttle = Some(frame.into());
            self.dirty = false;
        }
        if let Some(popup) = &mut self.popup {
            if popup.vanish.map_or(false, |vanish| vanish < Instant::now()) {
                self.popup = None;
                return;
            }
            if popup.wl.waiting_on_configure {
                return;
            }
            if let Some((_,_,desc)) = self.sink.get_hover(self.popup_x, 0.0) {
                let scale = popup.wl.scale;
                let pixel_size = (popup.wl.size.0 * scale, popup.wl.size.1 * scale);
                let surf = cairo::RecordingSurface::create(cairo::Content::ColorAlpha,
                    cairo::Rectangle {
                        x : 0.0,
                        y : 0.0,
                        width : pixel_size.0 as f64,
                        height : pixel_size.1 as f64,
                    }).expect("Error creating popup rendering surface");
                let ctx = cairo::Context::new(&surf);
                ctx.set_operator(cairo::Operator::Source);
                ctx.paint();
                ctx.set_operator(cairo::Operator::Over);
                ctx.set_source_rgb(1.0, 1.0, 1.0);

                let mut scale_matrix = cairo::Matrix::identity();
                scale_matrix.scale(scale as f64, scale as f64);
                ctx.set_matrix(scale_matrix);
                let new_size = desc.render(&ctx, runtime);

                target.render(pixel_size, &popup.wl.surf, &surf);
                popup.wl.surf.commit();
                if new_size.0 > popup.wl.size.0 || new_size.1 > popup.wl.size.1 {
                    target.wayland.resize_popup(&self.ls_surf, &mut popup.wl, new_size, scale);
                }
            } else {
                // contents vanished, dismiss the popup
                self.popup = None;
            }
        }
    }

    pub fn hover(&mut self, x : f64, y : f64, wayland : &WaylandClient, _runtime : &Runtime) {
        self.popup_x = x;
        if let Some(popup) = &self.popup {
            if x > popup.wl.anchor.0 as f64 && x < (popup.wl.anchor.0 + popup.wl.anchor.2) as f64 {
                return;
            } else {
                self.popup = None;
            }
        }
        if let Some((min_x, max_x, desc)) = self.sink.get_hover(x, y) {
            let anchor = (min_x as i32, 0, (max_x - min_x) as i32, self.pixel_height / self.scale);
            let size = desc.get_size();

            let popup = BarPopup {
                wl : wayland.new_popup(self, anchor, size),
                vanish : None,
            };
            self.popup = Some(popup);
        }
    }

    pub fn no_hover(&mut self, runtime : &mut Runtime) {
        if let Some(popup) = &mut self.popup {
            let vanish = Instant::now() + std::time::Duration::from_millis(100);
            popup.vanish = Some(vanish);
            runtime.set_wake_at(vanish, "bar-hover");
        }
    }

    pub fn hover_popup(&mut self, x : f64, y : f64, _wayland : &WaylandClient, _runtime : &Runtime) {
        if let Some(popup) = &mut self.popup {
            popup.vanish = None;
            let _ = (x, y);
        }
    }

    pub fn popup_button(&mut self, x : f64, y : f64, button : u32, runtime : &mut Runtime) {
        if let Some((_,_,desc)) = self.sink.get_hover(self.popup_x, 0.0) {
            desc.button(x, y, button, runtime);
        }
    }
}

impl Drop for Bar {
    fn drop(&mut self) {
        self.ls_surf.destroy();
        self.surf.destroy();
    }
}

struct RenderTarget<'a> {
    wayland : &'a mut WaylandClient,
    pos : usize,
}
impl<'a> RenderTarget<'a> {
    fn new(wayland : &'a mut WaylandClient, len : usize) -> Self {
        wayland.shm.resize(len).expect("OOM");
        RenderTarget { wayland, pos : 0 }
    }

    fn render(&mut self, size : (i32, i32), target : &WlSurface, surf : &cairo::Surface) {
        let stride = cairo::Format::ARgb32.stride_for_width(size.0 as u32).unwrap();
        let len = (size.1 as usize) * (stride as usize);
        let buf : &mut [u8] = &mut self.wayland.shm.mmap().as_mut()[self.pos..][..len];

        unsafe {
            // cairo::ImageSurface::create_for_data requires a 'static type, so give it that.
            // This could be done safely by creating a type that takes ownership of the MemPool and
            // returns it on Drop, which would require starting with an Rc<State> handle.
            let buf : &'static mut [u8] = &mut *(buf as *mut [u8]);

            let is = cairo::ImageSurface::create_for_data(buf, cairo::Format::ARgb32, size.0, size.1, stride).unwrap();
            let ctx = cairo::Context::new(&is);
            ctx.set_source_surface(surf, 0.0, 0.0);
            ctx.set_operator(cairo::Operator::Source);
            ctx.paint();
            surf.finish();
            drop(surf);
        }

        let buf = self.wayland.shm.buffer(self.pos as i32, size.0, size.1, stride, smithay_client_toolkit::shm::Format::Argb8888);
        target.attach(Some(&buf), 0, 0);
        target.damage_buffer(0, 0, size.0, size.1);
        self.pos += len;
    }
}

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
    pub items : HashMap<String, Item>,
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

    pub fn format(&self, fmt : &str) -> Result<String, strfmt::FmtError> {
        strfmt::strfmt_map(fmt, &|mut q| {
            let (name, key) = match q.key.find('.') {
                Some(p) => (&q.key[..p], &q.key[p + 1..]),
                None => (&q.key[..], ""),
            };
            match self.items.get(name) {
                Some(item) => {
                    item.data.read_in(name, key, self, |s| q.str(s))
                }
                None => Err(strfmt::FmtError::KeyError(name.to_string()))
            }
        })
    }

    pub fn format_or(&self, fmt : &str, context : &str) -> String {
        match self.format(fmt) {
            Ok(v) => v,
            Err(e) => {
                warn!("Error formatting '{}': {}", context, e);
                String::new()
            }
        }
    }

    pub fn get_item_var(&self) -> &Cell<Option<IterationItem>> {
        match self.items.get("item") {
            Some(&Item { data : Module::Item { ref value }, .. }) => value,
            _ => {
                panic!("The 'item' variable was not assignable");
            }
        }
    }
}

/// The singleton global state object bound to the calloop runner
pub struct State {
    pub wayland : WaylandClient,
    pub bars : Vec<Bar>,
    bar_config : Vec<toml::Value>,
    pub runtime : Runtime,
    draw_waiting_on_shm : bool,
}

impl State {
    pub fn new(wayland : WaylandClient) -> Result<Rc<RefCell<Self>>, Box<dyn Error>> {
        let notify_inner = Rc::new(NotifierInner {
            notify : tokio::sync::Notify::new(),
            data_update : Cell::new(true),
        });

        let mut state = Self {
            wayland,
            bars : Vec::new(),
            bar_config : Vec::new(),
            runtime : Runtime {
                items : Default::default(),
                refresh : Default::default(),
                notify : Notifier { inner : notify_inner.clone() },
            },
            draw_waiting_on_shm : false,
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
            state.wayland.get_outputs().take_in(|outputs| {
                for data in outputs {
                    error!(" name='{}' description='{}' make='{}' model='{}' at {},{} {}x{}",
                        data.name, data.description, data.make, data.model, data.position.0, data.position.1, data.size.0, data.size.1);
                }
            });
        });

        let rv = Rc::new(RefCell::new(state));
        let state = rv.clone();
        spawn_noerr(async move {
            loop {
                notify_inner.notify.notified().await;
                state.borrow_mut().request_draw_internal();
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

        let mut config_path = std::env::var_os("XDG_CONFIG_HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                let mut home = std::env::var_os("HOME")
                    .map(PathBuf::from)
                    .unwrap_or_else(PathBuf::new);
                home.push(".config");
                home
            });
        config_path.push("rwaybar.toml");

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
                let value = Item::from_item_list(&key, value);
                Some((key, value))
            }
        }).collect();

        if bar_config.is_empty() {
            Err("At least one [[bar]] section is required")?;
        }

        debug!("Loading configuration");

        let mut old_items = std::mem::replace(&mut self.runtime.items, new_items);
        self.bar_config = bar_config;

        self.runtime.items.insert("item".into(), Module::new_current_item().into());

        for (k,v) in &self.runtime.items {
            if let Some(item) = old_items.remove(k) {
                v.data.init(k, &self.runtime, Some(item.data));
            } else {
                v.data.init(k, &self.runtime, None);
            }
        }
        self.runtime.notify.inner.data_update.set(true);

        if reload {
            self.bars.clear();
            self.wayland.get_outputs().take_in(|outputs| {
                for output in outputs {
                    self.output_ready(output);
                }
            });
            if self.bars.is_empty() {
                error!("No bars matched this outptut configuration.  Available outputs:");
                self.wayland.get_outputs().take_in(|outputs| {
                    for data in outputs {
                        error!(" name='{}' description='{}' make='{}' model='{}' at {},{} {}x{}",
                            data.name, data.description, data.make, data.model, data.position.0, data.position.1, data.size.0, data.size.1);
                    }
                });
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

    fn request_draw_internal(&mut self) {
        if self.wayland.shm.is_used() {
            self.draw_waiting_on_shm = true;
        } else {
            self.set_data();
            self.draw_now().expect("Render error");
        }
    }

    pub fn shm_ok_callback(&mut self) {
        if self.draw_waiting_on_shm {
            self.draw_waiting_on_shm = false;
            self.set_data();
            self.draw_now().expect("Render error");
        }
    }

    fn set_data(&mut self) {
        // Propagate the data_update field to all bar dirty fields
        if !self.runtime.notify.inner.data_update.get() {
            return;
        }
        for (k, v) in &self.runtime.items {
            v.data.update(k, &self.runtime);
        }

        for bar in &mut self.bars {
            bar.dirty = true;
        }
        self.runtime.notify.inner.data_update.set(false);
    }

    fn draw_now(&mut self) -> Result<(), Box<dyn Error>> {
        let mut shm_size = 0;
        let begin = Instant::now();
        for bar in &self.bars {
            shm_size += bar.get_render_size();
        }

        if shm_size == 0 {
            return Ok(());
        }

        let mut target = RenderTarget::new(&mut self.wayland, shm_size);

        for bar in &mut self.bars {
            bar.render_with(&mut self.runtime, &mut target);
        }
        self.wayland.flush();
        let render_time = begin.elapsed().as_nanos();
        log::debug!("Frame took {}.{:06} ms", render_time / 1_000_000, render_time % 1_000_000);
        Ok(())
    }

    pub fn output_ready(&mut self, data : &OutputData) {
        info!("Output name='{}' description='{}' make='{}' model='{}' at {},{} {}x{}",
            data.name, data.description, data.make, data.model, data.position.0, data.position.1, data.size.0, data.size.1);
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

            let bar = self.new_bar(&data.output, data.scale, cfg, i);
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

    fn new_bar(&self, output : &WlOutput, scale : i32, cfg : toml::Value, cfg_index : usize) -> Bar {
        let ls : Attached<ZwlrLayerShellV1> = self.wayland.env.require_global();
        let surf : Attached<_> = self.wayland.env.create_surface();
        let ls_surf = ls.get_layer_surface(&surf, Some(output), Layer::Top, "bar".to_owned());

        let size = cfg.get("size").and_then(|v| v.as_integer()).unwrap_or(20) as u32;
        let size_excl = cfg.get("size-exclusive")
            .and_then(|v| v.as_integer())
            .map(|v| v as u32)
            .unwrap_or(size);
        let anchor_top;

        match cfg.get("side").and_then(|v| v.as_str()) {
            Some("top") => {
                ls_surf.set_size(0, size);
                ls_surf.set_anchor(Anchor::Top | Anchor::Left | Anchor::Right);
                anchor_top = true;
            }
            None | Some("bottom") => {
                ls_surf.set_size(0, size);
                ls_surf.set_anchor(Anchor::Bottom | Anchor::Left | Anchor::Right);
                anchor_top = false;
            }
            Some(side) => {
                error!("Unknown side '{}', defaulting to bottom", side);
                ls_surf.set_size(0, size);
                ls_surf.set_anchor(Anchor::Bottom | Anchor::Left | Anchor::Right);
                anchor_top = false;
            }
        }
        ls_surf.set_exclusive_zone(size_excl as i32);
        ls_surf.quick_assign(move |ls_surf, event, mut data| {
            use layer_shell::zwlr_layer_surface_v1::Event;
            let state : &mut State = data.get().unwrap();
            match event {
                Event::Configure { serial, width, height } => {
                    for bar in &mut state.bars {
                        if bar.ls_surf != *ls_surf {
                            continue;
                        }

                        bar.pixel_width = width as i32 * bar.scale;
                        bar.pixel_height = height as i32 * bar.scale;

                        ls_surf.ack_configure(serial);
                        bar.dirty = true;
                    }
                    state.request_draw();
                }
                Event::Closed => {
                    state.bars.retain(|bar| {
                        if bar.ls_surf == *ls_surf {
                            bar.ls_surf.destroy();
                            bar.surf.destroy();
                            false
                        } else {
                            true
                        }
                    });
                },
                _ => ()
            }
        });
        surf.set_buffer_scale(scale);

        surf.commit();

        Bar {
            surf,
            ls_surf : ls_surf.into(),
            item : Item::new_bar(cfg),
            scale,
            pixel_width : 0,
            pixel_height : 0,
            popup_x : 0.0,
            anchor_top,
            sink : EventSink::default(),
            dirty : false,
            throttle : None,
            popup : None,
            cfg_index,
        }
    }
}
