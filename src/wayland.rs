use log::debug;
use std::cell::RefCell;
use std::convert::Infallible;
use std::error::Error;
use std::io;
use std::rc::Rc;
use std::sync::{Mutex, atomic::{AtomicU8,AtomicU32,Ordering}};
use std::task;
use smithay_client_toolkit::compositor::CompositorState;
use smithay_client_toolkit::registry::RegistryState;
use smithay_client_toolkit::seat::{SeatState, self};
use smithay_client_toolkit::output::OutputState;
use smithay_client_toolkit::seat::pointer::{PointerHandler, PointerEvent, PointerEventKind};
use smithay_client_toolkit::shell::layer::{LayerSurface, LayerState, self as sctk_layer};
use smithay_client_toolkit::shell::xdg::XdgShellState;
use smithay_client_toolkit::shell::xdg::popup;
use smithay_client_toolkit::shm::ShmState;
use tokio::io::unix::AsyncFd;
use wayland_client::{Connection,QueueHandle,Proxy};
use wayland_client::backend::WaylandError;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_pointer::WlPointer;
use wayland_client::protocol::wl_seat::WlSeat;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_client::protocol::wl_touch::WlTouch;
use wayland_protocols_wlr::data_control::v1::client::zwlr_data_control_manager_v1::ZwlrDataControlManagerV1;
use wayland_protocols_wlr::layer_shell::v1::client::zwlr_layer_surface_v1::ZwlrLayerSurfaceV1;
use wayland_protocols::xdg::shell::client::xdg_popup;
use wayland_protocols::xdg::shell::client::xdg_positioner;

use crate::state::{State,Runtime};
use crate::util;

#[repr(u32)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Button {
    // Note: the u32 repr must match the bitmasks in EventSink::from_toml
    Left,
    Right,
    Middle,
    Back,
    Forward,
    ScrollUp,
    ScrollDown,
    ScrollLeft,
    ScrollRight,
    Tap,
}

/// Structures related to the Wayland display
#[derive(Debug)]
pub struct WaylandClient {
    pub conn: wayland_client::Connection,
    pub queue: QueueHandle<State>,
    pub registry: RegistryState,

    pub compositor: CompositorState,
    pub layer: LayerState,
    pub output: OutputState,
    pub seat: SeatState,
    pub shm: ShmState,
    pub xdg: XdgShellState,
    pub wlr_dcm: Option<ZwlrDataControlManagerV1>,

    taps: Vec<TapState>,

    flush : Option<task::Waker>,
    need_flush : bool,
}

#[derive(Debug)]
struct OtherRegistries;

impl smithay_client_toolkit::registry::RegistryHandler<State> for OtherRegistries {
    fn ready(
        data: &mut State,
        conn: &Connection,
        qh: &QueueHandle<State>,
    ) {
        data.runtime.wayland.wlr_dcm = data.runtime.wayland.registry.bind_one(qh, 1..=2, ()).ok();
        debug!("Done enumerating globals; starting round-trip for initial events.");
        conn.display().sync(
            &qh,
            crate::state::Callbacks::Init2,
        ).unwrap();
        data.runtime.wayland.flush();
    }
}

wayland_client::delegate_dispatch!(State: [ WlSurface: SurfaceData, ] => CompositorState);
wayland_client::delegate_dispatch!(State: [ WlPointer: PointerData, ] => SeatState);
smithay_client_toolkit::delegate_compositor!(State);
smithay_client_toolkit::delegate_pointer!(State);
smithay_client_toolkit::delegate_layer!(State);
smithay_client_toolkit::delegate_output!(State);
smithay_client_toolkit::delegate_registry!(State);
smithay_client_toolkit::delegate_seat!(State);
smithay_client_toolkit::delegate_shm!(State);
smithay_client_toolkit::delegate_touch!(State);
smithay_client_toolkit::delegate_xdg_shell!(State);
smithay_client_toolkit::delegate_xdg_popup!(State);

#[derive(Default, Debug, Clone)]
struct PointerState {
    axis_h: f64,
    axis_v: f64,
    axis_ts: u32,
}

#[derive(Debug)]
struct TapState {
    touch: WlTouch,
    surface: WlSurface,
    id: i32,
    x: f64,
    y: f64,
}

impl smithay_client_toolkit::registry::ProvidesRegistryState for State {
    fn registry(&mut self) -> &mut RegistryState {
        &mut self.runtime.wayland.registry
    }

    smithay_client_toolkit::registry_handlers![
        CompositorState,
        LayerState,
        OutputState,
        SeatState,
        ShmState,
        XdgShellState,
        OtherRegistries,
    ];
}

#[derive(Debug)]
pub struct SurfaceData {
    sctk: smithay_client_toolkit::compositor::SurfaceData,
    width: AtomicU32,
    height: AtomicU32,

    state: AtomicU8,
}

impl smithay_client_toolkit::compositor::SurfaceDataExt for SurfaceData {
    fn surface_data(&self) -> &smithay_client_toolkit::compositor::SurfaceData {
        &self.sctk
    }
}

impl SurfaceData {
    const DAMAGED: u8 = 1;
    const THROTTLED: u8 = 2;
    const CONFIGURED: u8 = 4;
    const NEW: u8 = 0;
    const NEED_RENDER: u8 = SurfaceData::DAMAGED | SurfaceData::CONFIGURED;
    const POST_RENDER: u8 = SurfaceData::THROTTLED | SurfaceData::CONFIGURED;

    pub fn try_from_wl(wl: &WlSurface) -> Option<&Self> {
        wl.data()
    }

    pub fn from_wl(wl: &WlSurface) -> &Self {
        wl.data().unwrap()
    }

    pub fn start_render(&self) -> bool {
        self.state.compare_exchange(SurfaceData::NEED_RENDER, SurfaceData::POST_RENDER, Ordering::Relaxed, Ordering::Relaxed).is_ok()
    }

    /// Returns true if a render should be requested now
    pub fn damage_full(&self) -> bool {
        let prev = self.state.fetch_or(SurfaceData::DAMAGED, Ordering::Relaxed);
        prev == SurfaceData::NEED_RENDER & !SurfaceData::DAMAGED
    }

    pub fn scale_transform(&self) -> tiny_skia::Transform {
        let scale = self.scale_factor();
        tiny_skia::Transform::from_scale(scale as f32, scale as f32)
    }

    pub fn scale_factor(&self) -> i32 {
        self.sctk.scale_factor()
    }

    pub fn height(&self) -> u32 {
        self.height.load(Ordering::Relaxed) 
    }

    pub fn width(&self) -> u32 {
        self.width.load(Ordering::Relaxed) 
    }

    pub fn pixel_width(&self) -> i32 {
        self.width.load(Ordering::Relaxed) as i32 * self.scale_factor()
    }

    pub fn pixel_height(&self) -> i32 {
        self.height.load(Ordering::Relaxed) as i32 * self.scale_factor()
    }
}

impl smithay_client_toolkit::compositor::CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.runtime.wayland.compositor
    }

    fn scale_factor_changed(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        surf: &WlSurface,
        new_factor: i32
    ) {
        if let Some(data) = SurfaceData::try_from_wl(surf) {
            surf.set_buffer_scale(new_factor);
            if data.damage_full() {
                self.request_draw();
            }
        }
    }
    fn frame(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        surf: &WlSurface,
        _time: u32
    ) {
        let data = SurfaceData::from_wl(surf);
        let prev = data.state.fetch_and(!SurfaceData::THROTTLED, Ordering::Relaxed);
        if prev & SurfaceData::THROTTLED != 0 {
            self.request_draw();
        }
    }
}

impl smithay_client_toolkit::shell::layer::LayerHandler for State {
    fn layer_state(&mut self) -> &mut LayerState {
        &mut self.runtime.wayland.layer
    }
    fn closed(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        ls: &sctk_layer::LayerSurface,
    ) {
        self.bars.retain(|bar| bar.ls != *ls);
    }

    fn configure(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        ls: &sctk_layer::LayerSurface,
        config: sctk_layer::LayerSurfaceConfigure,
        _serial: u32
    ) {
        let data = SurfaceData::from_wl(ls.wl_surface());
        data.width.store(config.new_size.0, Ordering::Relaxed);
        data.height.store(config.new_size.1, Ordering::Relaxed);
        data.state.fetch_or(SurfaceData::CONFIGURED | SurfaceData::DAMAGED, Ordering::Relaxed);
        self.request_draw();
    }
}

impl smithay_client_toolkit::output::OutputHandler for State {
    fn output_state(&mut self) -> &mut OutputState {
        &mut self.runtime.wayland.output
    }

    fn new_output(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        output: WlOutput
    ) {
        self.output_ready(&output);
    }
    fn update_output(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        _: WlOutput
    ) {
        // anything we care about will get applied via configure requests on our surface
    }
    fn output_destroyed(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        _: WlOutput
    ) {
        // do nothing and rely on the Closed event for destroy
    }
}

#[derive(Default,Debug)]
pub struct PointerData {
    pdata: smithay_client_toolkit::seat::pointer::PointerData,
    state: Mutex<PointerState>,
}

impl smithay_client_toolkit::seat::pointer::PointerDataExt for PointerData {
    fn pointer_data(&self) -> &smithay_client_toolkit::seat::pointer::PointerData {
        &self.pdata
    }
}

impl PointerHandler for State {
    fn pointer_frame(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        pointer: &WlPointer,
        events: &[PointerEvent],
    ) {
        use PointerEventKind::*;
        for event in events {
            match event.kind {
                Enter { serial } => {
                    self.renderer.set_cursor(&self.runtime.wayland, &pointer, serial);

                    self.dispatch_surface_event(&event.surface, |surf, rt| {
                        surf.hover(event.position, rt);
                    });
                }
                Leave { .. } => {
                    self.dispatch_surface_event(&event.surface, |surf, rt| {
                        surf.no_hover(rt);
                    });
                }
                Motion { .. } => {
                    self.dispatch_surface_event(&event.surface, |surf, rt| {
                        surf.hover(event.position, rt);
                    });
                }
                Press { button, .. } => {
                    let button_id = match button {
                        0x110 => Button::Left, // BTN_LEFT
                        0x111 => Button::Right, // BTN_RIGHT
                        0x112 => Button::Middle, // BTN_MIDDLE
                        0x113 => Button::Back, // BTN_SIDE or "back"
                        0x114 => Button::Forward, // BTN_EXTRA or "forward"
                        _ => {
                            debug!("You can add events for this button ({})", button);
                            return;
                        }
                    };
                    self.dispatch_pointer_button(&event.surface, event.position, button_id);
                }
                Release { .. } => {}
                Axis { time, horizontal, vertical, .. } => {
                    let mut p = pointer.data::<PointerData>().unwrap().state.lock().unwrap();

                    let decay = time.wrapping_sub(p.axis_ts);
                    if decay < 4000 {
                        // this combines the current scroll with prior scrolls, decaying
                        // the prior distance with a half-life of about 2/3 second
                        p.axis_h *= 0.999_f64.powi(decay as i32);
                        p.axis_v *= 0.999_f64.powi(decay as i32);
                    } else {
                        // after 4 seconds, just discard the prior scroll instead (4
                        // seconds already decays to 1.8% anyway)
                        p.axis_h = 0.0;
                        p.axis_v = 0.0;
                    }
                    p.axis_ts = time;

                    if vertical.discrete < 0 {
                        p.axis_v = 0.0;
                        self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollUp);
                    } else if vertical.discrete > 0 {
                        p.axis_v = 0.0;
                        self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollDown);
                    } else {
                        p.axis_v += vertical.absolute;
                        if p.axis_v >= 10.0 {
                            self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollDown);
                            p.axis_v = 0.0;
                        } else if p.axis_v <= -10.0 {
                            self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollUp);
                            p.axis_v = 0.0;
                        }
                    }

                    if horizontal.discrete < 0 {
                        p.axis_h = 0.0;
                        self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollLeft);
                    } else if horizontal.discrete > 0 {
                        p.axis_h = 0.0;
                        self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollRight);
                    } else {
                        p.axis_h += horizontal.absolute;
                        if p.axis_h >= 10.0 {
                            self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollRight);
                            p.axis_h = 0.0;
                        } else if p.axis_h <= -10.0 {
                            self.dispatch_pointer_button(&event.surface, event.position, Button::ScrollLeft);
                            p.axis_h = 0.0;
                        }
                    }
                }
            }
        }
    }
}

impl smithay_client_toolkit::seat::touch::TouchHandler for State {
    fn down(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        touch: &WlTouch,
        _serial: u32,
        _time: u32,
        surface: WlSurface,
        id: i32,
        (x, y): (f64, f64)
    ) {
        self.dispatch_surface_event(&surface, |surf, rt| {
            surf.hover((x, y), rt);
        });
        self.runtime.wayland.taps.push(TapState {
            touch: touch.clone(),
            surface,
            id, x, y,
        });
    }

    fn up(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        touch: &WlTouch,
        _serial: u32,
        _time: u32,
        id: i32
    ) {
        // drain_filter would be nice
        if let Some(i) = self.runtime.wayland.taps.iter().position(|tap| {
            tap.touch == *touch && tap.id == id
        }) {
            let tap = self.runtime.wayland.taps.remove(i);
            self.dispatch_surface_event(&tap.surface, |surf, rt| {
                surf.no_hover(rt);
                surf.button((tap.x, tap.y), Button::Tap, rt);
            });
        }
    }

    fn motion(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        touch: &WlTouch,
        _time: u32,
        id: i32,
        (x, y): (f64, f64)
    ) {
        let mut surf = None;
        for tap in &mut self.runtime.wayland.taps {
            if tap.touch == *touch && tap.id == id {
                tap.x = x;
                tap.y = y;
                surf = Some(tap.surface.clone());
            }
        }
        if let Some(surf) = surf {
            self.dispatch_surface_event(&surf, |surf, rt| {
                surf.hover((x, y), rt);
            });
        }
    }

    fn shape(
        &mut self,
        _conn: &Connection,
        _qh: &QueueHandle<Self>,
        _touch: &WlTouch,
        _id: i32,
        _major: f64,
        _minor: f64
    ) {}
    fn orientation(
        &mut self,
        _conn: &Connection,
        _qh: &QueueHandle<Self>,
        _touch: &WlTouch,
        _id: i32,
        _orientation: f64
    ) {}
    fn cancel(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        touch: &WlTouch
    ) {
        let mut surfaces = Vec::new();
        // drain_filter would be nice
        self.runtime.wayland.taps.retain(|tap| {
            if tap.touch == *touch {
                surfaces.push(tap.surface.clone());
                false
            } else {
                true
            }
        });
        for surface in surfaces {
            self.dispatch_surface_event(&surface, |surf, rt| {
                surf.no_hover(rt);
            });
        }
    }
}

impl smithay_client_toolkit::seat::SeatHandler for State {
    fn seat_state(&mut self) -> &mut SeatState {
        &mut self.runtime.wayland.seat
    }

    fn new_seat(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        _: WlSeat
    ) {}

    fn new_capability(
        &mut self,
        _: &Connection,
        qh: &QueueHandle<Self>,
        seat: WlSeat,
        capability: seat::Capability
    ) {
        match capability {
            seat::Capability::Pointer => {
                self.runtime.wayland.seat.get_pointer_with_data(qh, &seat, PointerData::default()).unwrap();
            }
            seat::Capability::Touch => {
                self.runtime.wayland.seat.get_touch(qh, &seat).unwrap();
            }
            _ => {}
        }
    }
    fn remove_capability(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        _: WlSeat,
        _: seat::Capability
    ) {}
    fn remove_seat(
        &mut self,
        _: &Connection,
        _: &QueueHandle<Self>,
        _: WlSeat
    ) {}
}

impl smithay_client_toolkit::shm::ShmHandler for State {
    fn shm_state(&mut self) -> &mut ShmState {
        &mut self.runtime.wayland.shm
    }
}

impl smithay_client_toolkit::shell::xdg::XdgShellHandler for State {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.runtime.wayland.xdg
    }
}

impl smithay_client_toolkit::shell::xdg::popup::PopupHandler for State {
    fn configure(&mut self, _: &Connection, _: &QueueHandle<Self>, popup: &popup::Popup, config: popup::PopupConfigure) {
        let data = SurfaceData::from_wl(popup.wl_surface());
        data.width.store(config.width as u32, Ordering::Relaxed);
        data.height.store(config.height as u32, Ordering::Relaxed);
        data.state.fetch_or(SurfaceData::CONFIGURED, Ordering::Relaxed);
        if data.damage_full() {
            self.request_draw();
        }
    }

    fn done(&mut self, _: &Connection, _: &QueueHandle<Self>, sctk: &popup::Popup) {
        for bar in &mut self.bars {
            if bar.popup.as_ref().map_or(false, |popup| popup.wl.sctk == *sctk) {
                bar.popup = None;
            }
        }
    }
}

pub trait SurfaceEvents {
    fn hover(&mut self, pos: (f64, f64), rt: &mut Runtime) {
        let _ = (pos, rt);
    }
    fn no_hover(&mut self, rt: &mut Runtime) {
        let _ = rt;
    }
    fn button(&mut self, pos: (f64, f64), button : Button, runtime : &mut Runtime);
}

impl WaylandClient {
    pub fn new() -> Result<(Self, wayland_client::EventQueue<State>), Box<dyn Error>> {
        let conn = wayland_client::Connection::connect_to_env()?;

        let wl_queue = conn.new_event_queue();
        let queue = wl_queue.handle();

        let registry = RegistryState::new(&conn, &queue);

        let client = WaylandClient {
            conn,
            queue,
            registry,

            compositor: CompositorState::new(),
            layer: LayerState::new(),
            output: OutputState::new(),
            seat: SeatState::new(),
            shm: ShmState::new(),
            xdg: XdgShellState::new(),
            wlr_dcm: None,

            taps: Default::default(),

            flush : None,
            need_flush : true,
        };

        Ok((client, wl_queue))
    }

    pub fn flush(&mut self) {
        self.need_flush = true;
        self.flush.take().map(|f| f.wake());
    }

    pub fn create_surface(&self, scale: i32) -> WlSurface {
        let sd = SurfaceData {
            sctk: smithay_client_toolkit::compositor::SurfaceData::with_initial_scale(scale),
            height: AtomicU32::new(0),
            width: AtomicU32::new(0),
            state: AtomicU8::new(SurfaceData::NEW),
        };
        self.compositor.create_surface_with_data(&self.queue, sd).unwrap()
    }

    pub fn create_layer_surface(&mut self, builder: sctk_layer::LayerSurfaceBuilder, layer: sctk_layer::Layer, scale: i32) -> LayerSurface {
        let surf = self.create_surface(scale);
        builder.map(&self.queue,
            &mut self.layer,
            surf,
            layer,
        ).unwrap()
    }
}

impl State {
    fn dispatch_pointer_button(&mut self, over: &WlSurface, position: (f64, f64), button: Button) {
        for bar in &mut self.bars {
            if bar.ls.wl_surface() == over {
                bar.button(position, button, &mut self.runtime);
            }
            if let Some(popup) = &mut bar.popup {
                if popup.wl.surf == *over {
                    popup.button(position, button, &mut self.runtime);
                }
            }
        }
    }

    fn dispatch_surface_event(&mut self, surf: &WlSurface, mut f: impl FnMut(&mut dyn SurfaceEvents, &mut Runtime)) {
        for bar in &mut self.bars {
            if surf == bar.ls.wl_surface() {
                f(bar, &mut self.runtime);
            }
            if let Some(popup) = &mut bar.popup {
                if *surf == popup.wl.surf {
                    f(popup, &mut self.runtime);
                }
            }
        }
    }
}

pub async fn run_queue(mut wl_queue : wayland_client::EventQueue<State>, state : Rc<RefCell<State>>) -> Result<Infallible, Box<dyn Error>> {
    let reader = wl_queue.prepare_read()?;
    let fd = AsyncFd::new(util::Fd(reader.connection_fd()))?;
    let mut reader = Some(reader);
    let mut rg = None;
    let mut wg = None;

    loop {
        if reader.is_none() {
            reader = Some(wl_queue.prepare_read()?);
        }

        futures_util::future::poll_fn(|ctx| {
            let mut state = state.borrow_mut();
            match &state.runtime.wayland.flush {
                Some(w) if w.will_wake(ctx.waker()) => (),
                _ => state.runtime.wayland.flush = Some(ctx.waker().clone()),
            }

            rg = match fd.poll_read_ready(ctx) {
                task::Poll::Ready(g) => Some(g?),
                task::Poll::Pending => None,
            };

            wg = match fd.poll_write_ready(ctx) {
                task::Poll::Ready(g) => Some(g?),
                task::Poll::Pending => None,
            };

            if state.runtime.wayland.need_flush && wg.is_some() {
                task::Poll::Ready(Ok(()))
            } else if rg.is_some() {
                task::Poll::Ready(Ok(()))
            } else {
                task::Poll::Pending::<io::Result<()>>
            }
        }).await?;

        if let Some(g) = &mut wg {
            match wl_queue.flush() {
                Ok(()) => {
                    state.borrow_mut().runtime.wayland.need_flush = false;
                }
                Err(WaylandError::Io(e)) if e.kind() == io::ErrorKind::WouldBlock => {
                    g.clear_ready();
                }
                Err(e) => Err(e)?,
            }
        }

        if let Some(g) = &mut rg {
            match reader.take().map(|g| g.read()) {
                None |
                Some(Ok(_)) => {
                    // dispatch events and continue
                }
                Some(Err(WaylandError::Io(e))) if e.kind() == io::ErrorKind::WouldBlock => {
                    g.clear_ready();
                }
                Some(Err(e)) => Err(e)?,
            }
        }

        let mut lock = state.borrow_mut();
        wl_queue.dispatch_pending(&mut *lock)?;
    }
}

/// An [xdg_popup::XdgPopup] with associated information
#[derive(Debug)]
pub struct Popup {
    pub queue: QueueHandle<State>,
    pub surf : WlSurface,
    pub sctk: popup::Popup,
    pub anchor : (i32, i32, i32, i32),
    pub req_size : (i32, i32), // requested logical size; may be rejected by compositor
    pub prefer_top : bool,
}

impl Popup {
    pub fn on_bar(wayland : &mut WaylandClient, bar : &crate::bar::Bar, anchor : (i32, i32, i32, i32), size : (i32, i32)) -> Popup {
        match bar.ls.kind() {
            smithay_client_toolkit::shell::layer::SurfaceKind::Wlr(ls) => {
                let scale = bar.ls.wl_surface().data::<SurfaceData>().map_or(1, |d| d.scale_factor());
                Self::new(wayland, &ls, !bar.anchor_top, anchor, size, scale)
            }
            _ => unreachable!(),
        }
    }

    pub fn new(wayland : &mut WaylandClient, parent: &ZwlrLayerSurfaceV1, prefer_top : bool, anchor : (i32, i32, i32, i32), size : (i32, i32), scale : i32) -> Self {
        use xdg_positioner::{Anchor,Gravity};

        let surf = wayland.create_surface(scale);

        let pos = wayland.xdg.create_positioner().unwrap();

        pos.set_size(size.0, size.1);
        pos.set_anchor_rect(anchor.0, anchor.1, anchor.2, anchor.3);
        pos.set_offset(0, 0);
        if prefer_top {
            pos.set_anchor(Anchor::Top);
            pos.set_gravity(Gravity::Top);
        } else {
            pos.set_anchor(Anchor::Bottom);
            pos.set_gravity(Gravity::Bottom);
        }
        pos.set_constraint_adjustment(0xF); // allow moving but not resizing

        let sctk = popup::Popup::from_surface(None, &pos, &wayland.queue, surf.clone(), &wayland.xdg).unwrap();

        parent.get_popup(sctk.xdg_popup());
        sctk.xdg_surface().set_window_geometry(0, 0, size.0, size.1);

        surf.set_buffer_scale(scale);
        surf.commit();
        Popup {
            surf,
            sctk,
            queue: wayland.queue.clone(),
            anchor,
            req_size: size,
            prefer_top,
        }
    }

    pub fn resize(&mut self, wayland : &mut WaylandClient, ls_surf : &ZwlrLayerSurfaceV1, size : (i32, i32), scale : i32) {
        if self.sctk.xdg_popup().version() >= xdg_popup::REQ_REPOSITION_SINCE {
            use xdg_positioner::{Anchor,Gravity};
            self.sctk.xdg_surface().set_window_geometry(0, 0, size.0, size.1);
            let pos = wayland.xdg.create_positioner().unwrap();

            pos.set_size(size.0, size.1);
            pos.set_anchor_rect(self.anchor.0, self.anchor.1, self.anchor.2, self.anchor.3);
            pos.set_offset(0, 0);
            if self.prefer_top {
                pos.set_anchor(Anchor::Top);
                pos.set_gravity(Gravity::Top);
            } else {
                pos.set_anchor(Anchor::Bottom);
                pos.set_gravity(Gravity::Bottom);
            }
            pos.set_constraint_adjustment(0xF); // allow moving but not resizing
            self.sctk.xdg_popup().reposition(&pos, 0);
            self.req_size = size;
        } else {
            // can't resize; emulate by destroying and re-creating.
            *self = Self::new(wayland, ls_surf, self.prefer_top, self.anchor, size, scale);
        }
    }
}
