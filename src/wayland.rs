use log::{debug,error};
use std::cell::RefCell;
use std::convert::Infallible;
use std::error::Error;
use std::io;
use std::rc::Rc;
use std::task;
use smithay_client_toolkit::environment::{SimpleGlobal,MultiGlobalHandler};
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::environment;
use smithay_client_toolkit::seat::SeatData;
use smithay_client_toolkit::shm::MemPool;
use tokio::io::unix::AsyncFd;
use wayland_client::Attached;
use wayland_client::protocol::wl_compositor::WlCompositor;
use wayland_client::protocol::wl_display::WlDisplay;
use wayland_client::protocol::wl_output::{WlOutput,Transform};
use wayland_client::protocol::wl_pointer::{ButtonState,Axis};
use wayland_client::protocol::wl_registry::WlRegistry;
use wayland_client::protocol::wl_seat::WlSeat;
use wayland_client::protocol::wl_shm::WlShm;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_manager_v1::ZxdgOutputManagerV1;
use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_v1::ZxdgOutputV1;
use wayland_protocols::wlr::unstable::layer_shell::v1::client::zwlr_layer_shell_v1::ZwlrLayerShellV1;
use wayland_protocols::wlr::unstable::layer_shell::v1::client::zwlr_layer_surface_v1::ZwlrLayerSurfaceV1;
use wayland_protocols::xdg_shell::client::xdg_popup::XdgPopup;
use wayland_protocols::xdg_shell::client::xdg_surface::XdgSurface;
use wayland_protocols::xdg_shell::client::xdg_wm_base::XdgWmBase;

use crate::state::{Bar,State};
use crate::util::{self,Cell};

/// Helper for populating [OutputData]
#[derive(Debug,Default)]
struct OutputHandler {
    outputs : Rc<Cell<Vec<OutputData>>>,
}

/// Metadata on a [WlOutput]
#[derive(Debug)]
pub struct OutputData {
    pub output : Attached<WlOutput>,
    pub xdg : Option<Attached<ZxdgOutputV1>>,

    pub scale : i32,
    pub transform : Transform,

    pub geometry : (i32, i32),  // real pixels
    pub position : (i32, i32),  // logical_position from xdg_output
    pub size : (i32, i32),      // logical_size from xdg_output
    pub name : String,          // from xdg_output
    pub make : String,
    pub model : String,
    pub description: String,
}


impl MultiGlobalHandler<WlOutput> for OutputHandler {
    fn created(&mut self, registry: Attached<WlRegistry>, id: u32, version: u32, _: wayland_client::DispatchData) {
        assert!(version > 1);
        let version = std::cmp::min(version, 3);
        let output = registry.bind::<WlOutput>(version, id);
        let outputs = self.outputs.clone();
        output.quick_assign(move |output, event, mut data| {
            use wayland_client::protocol::wl_output::Event;
            match event {
                Event::Geometry { x, y, make, model, transform, .. } => {
                    outputs.take_in(|outputs| {
                        for data in outputs {
                            if data.output != *output {
                                continue;
                            }
                            data.geometry.0 = x;
                            data.geometry.1 = y;
                            data.make = make;
                            data.model = model;
                            data.transform = transform;
                            return;
                        }
                        debug!("Got a Geometry event for an output we don't know about");
                    });
                }
                Event::Scale { factor } => {
                    outputs.take_in(|outputs| {
                        for data in outputs {
                            if data.output != *output {
                                continue;
                            }
                            data.scale = factor;
                        }
                    });
                }
                Event::Done => {
                    if let Some(state) = data.get::<State>() {
                        let i = outputs.take_in(|outputs| {
                            for (i, data) in outputs.iter_mut().enumerate() {
                                if data.output != *output {
                                    continue;
                                }
                                if data.xdg.is_some() {
                                    return Some(i);
                                } else {
                                    state.wayland.output_finish_setup(data);
                                    return None;
                                }
                            }
                            debug!("Strange, got a Done for an output we don't know about");
                            None
                        });
                        if let Some(i) = i {
                            state.output_ready(i);
                        }
                    }
                }
                _ => ()
            }
        });
        self.outputs.take_in(|outputs| {
            outputs.push(OutputData {
                output : output.into(),
                xdg : None,
                scale : 1,
                transform : Transform::Normal,
                geometry : (0, 0),
                position : (0, 0),
                size : (0, 0),
                name : String::new(),
                make : String::new(),
                model : String::new(),
                description: String::new(),
            });
        });
    }

    fn removed(&mut self, id: u32, _data: wayland_client::DispatchData) {
        self.outputs.take_in(|outputs| {
            outputs.retain(|out| {
                if out.output.as_ref().id() == id {
                    if let Some(xdg) = &out.xdg {
                        xdg.destroy();
                    }
                    out.output.release();
                    false
                } else {
                    true
                }
            });
        });
    }

    fn get_all(&self) -> Vec<Attached<WlOutput>> {
        self.outputs.take_in(|outputs| {
            outputs.iter().map(|data| data.output.clone()).collect()
        })
    }
}

/// Wayland globals (access via [Environment::require_global])
pub struct Globals {
    sctk_compositor: SimpleGlobal<WlCompositor>,
    sctk_shm: smithay_client_toolkit::shm::ShmHandler,
    sctk_seats: smithay_client_toolkit::seat::SeatHandler,
    sctk_shell: smithay_client_toolkit::shell::ShellHandler,
    outputs: OutputHandler,

    layer_shell : SimpleGlobal<ZwlrLayerShellV1>,
    xdg_out : SimpleGlobal<ZxdgOutputManagerV1>,
}

impl smithay_client_toolkit::seat::SeatHandling for Globals {
    fn listen<F>(&mut self, f: F) -> smithay_client_toolkit::seat::SeatListener
    where F: FnMut(Attached<smithay_client_toolkit::reexports::client::protocol::wl_seat::WlSeat>,
        &smithay_client_toolkit::seat::SeatData,
        wayland_client::DispatchData
    ) + 'static
    {
        self.sctk_seats.listen(f)
    }
}

environment!(Globals,
    singles = [
        WlCompositor => sctk_compositor,
        WlShm => sctk_shm,

        ZwlrLayerShellV1 => layer_shell,
        XdgWmBase => sctk_shell,
        ZxdgOutputManagerV1 => xdg_out,
    ],
    multis = [
        smithay_client_toolkit::reexports::client::protocol::wl_seat::WlSeat => sctk_seats,
        WlOutput => outputs,
    ],
);

/// Structures related to the Wayland display
pub struct WaylandClient {
    pub env : Environment<Globals>,
    pub wl_display : Attached<WlDisplay>,
    pub shm : MemPool,
    cursor : Cursor,
    flush : Option<task::Waker>,
    need_flush : bool,
}

struct Cursor {
    cursor_surf : Attached<WlSurface>,
    spot : (i32, i32),
}

impl Cursor {
    fn new(env : &Environment<Globals>, scale : i32) -> Self {
        let shm = env.require_global();
        let base_theme = std::env::var("XCURSOR_THEME").unwrap_or_else(|_| "default".into());
        let base_size = std::env::var("XCURSOR_SIZE").ok().and_then(|s| s.parse().ok()).unwrap_or(24u32);

        let mut cursor_theme = wayland_cursor::CursorTheme::load_from_name(&base_theme, base_size * scale as u32, &shm);
        let cursor = cursor_theme.get_cursor("default").expect("Could not load cursor, check XCURSOR_THEME").clone();

        let cursor_surf = env.create_surface();
        let cursor_img = &cursor[0];
        let dim = cursor_img.dimensions();
        let spot = cursor[0].hotspot();
        cursor_surf.set_buffer_scale(scale);
        cursor_surf.attach(Some(&cursor_img), 0, 0);
        cursor_surf.damage_buffer(0, 0, dim.0 as _, dim.1 as _);
        cursor_surf.commit();
        Cursor {
            spot : (spot.0 as i32 / scale, spot.1 as i32 / scale),
            cursor_surf,
        }
    }
}

impl WaylandClient {
    pub fn new() -> Result<(Self, wayland_client::EventQueue), Box<dyn Error>> {
        let display = wayland_client::Display::connect_to_env()?;

        let mut wl_queue = display.create_event_queue();
        let wl_display = display.attach(wl_queue.token());

        let env = smithay_client_toolkit::environment::Environment::new(&wl_display, &mut wl_queue, Globals {
            sctk_compositor: SimpleGlobal::new(),
            sctk_shm: smithay_client_toolkit::shm::ShmHandler::new(),
            sctk_seats : smithay_client_toolkit::seat::SeatHandler::new(),
            sctk_shell : smithay_client_toolkit::shell::ShellHandler::new(),
            outputs: OutputHandler::default(),
            layer_shell : SimpleGlobal::new(),
            xdg_out : SimpleGlobal::new(),
        })?;

        let shm = env.create_simple_pool(|mut data| {
            let state : &mut State = data.get().unwrap();
            state.shm_ok_callback();
        })?;

        let mut cursor_scale = 1;
        env.with_inner(|g| g.outputs.outputs.take_in(|outputs| {
            for output in outputs {
                if output.scale > cursor_scale {
                    cursor_scale = output.scale;
                }
            }
        }));
        let cursor = Cursor::new(&env, cursor_scale);

        let mut client = WaylandClient {
            env,
            shm,
            wl_display,
            cursor,
            flush : None,
            need_flush : true,
        };

        client.get_outputs().take_in(|outputs| {
            for output in outputs {
                client.output_finish_setup(output);
            }
        });

        let _seat_watcher = client.env.listen_for_seats(|seat, si, mut data| {
            let state : &mut State = data.get().unwrap();
            state.wayland.add_seat(&seat, si);
        });

        for seat in client.env.get_all_seats() {
            smithay_client_toolkit::seat::with_seat_data(&seat, |si| client.add_seat(&seat, si));
        }

        Ok((client, wl_queue))
    }

    pub fn get_outputs(&self) -> Rc<Cell<Vec<OutputData>>> {
        self.env.with_inner(|g| g.outputs.outputs.clone())
    }

    pub fn flush(&mut self) {
        self.need_flush = true;
        self.flush.take().map(|f| f.wake());
    }

    pub fn add_seat(&mut self, seat : &Attached<WlSeat>, si : &SeatData) {
        if si.has_pointer {
            let mouse = seat.get_pointer();
            let mut over = None;
            let mut x = 0.0;
            let mut y = 0.0;
            mouse.quick_assign(move |mouse, event, mut data| {
                use wayland_client::protocol::wl_pointer::Event;
                let state : &mut State = data.get().unwrap();
                match event {
                    Event::Enter { serial, surface, surface_x, surface_y, .. } => {
                        let spot = state.wayland.cursor.spot;
                        mouse.set_cursor(serial, Some(&state.wayland.cursor.cursor_surf), spot.0, spot.1);

                        over = Some(surface.as_ref().id());
                        x = surface_x;
                        y = surface_y;
                    }
                    Event::Motion { surface_x, surface_y, .. } => {
                        x = surface_x;
                        y = surface_y;
                    }
                    Event::Leave { surface, .. } => {
                        let id = surface.as_ref().id();
                        if Some(id) == over {
                            over = None;
                        }
                        for bar in &mut state.bars {
                            if bar.surf.as_ref().id() == id {
                                bar.no_hover(&mut state.runtime);
                            }
                            if let Some(popup) = &bar.popup {
                                if popup.wl.surf.as_ref().id() == id {
                                    bar.no_hover(&mut state.runtime);
                                }
                            }
                        }
                    }
                    Event::Button {
                        button, state : ButtonState::Pressed, ..
                    } => {
                        let button_id = match button {
                            0x110 => 0, // BTN_LEFT
                            0x111 => 1, // BTN_RIGHT
                            0x112 => 2, // BTN_MIDDLE
                            0x113 => 3, // BTN_SIDE or "back"
                            0x114 => 4, // BTN_EXTRA or "forward"
                            // note scrolling uses the next 4
                            _ => {
                                debug!("You can add events for this button ({})", button);
                                return;
                            }
                        };
                        for bar in &mut state.bars {
                            if Some(bar.surf.as_ref().id()) == over {
                                bar.sink.button(x,y,button_id, &mut state.runtime);
                            }
                            if bar.popup.as_ref().map(|p| p.wl.surf.as_ref().id()) == over {
                                bar.popup_button(x,y,button_id, &mut state.runtime);
                            }
                        }
                        state.request_update();
                        return;
                    }
                    Event::Axis { axis, value, .. } => {
                        dbg!(value);
                        // TODO minimum scroll distance and/or rate?
                        let button_id = match axis {
                            Axis::VerticalScroll if value < 0.0 => 5, // up
                            Axis::VerticalScroll if value > 0.0 => 6, // down
                            Axis::HorizontalScroll if value < 0.0 => 7, // left
                            Axis::HorizontalScroll if value > 0.0 => 8, // right
                            _ => return,
                        };
                        for bar in &mut state.bars {
                            if Some(bar.surf.as_ref().id()) == over {
                                bar.sink.button(x,y,button_id, &mut state.runtime);
                            }
                            if bar.popup.as_ref().map(|p| p.wl.surf.as_ref().id()) == over {
                                bar.popup_button(x,y,button_id, &mut state.runtime);
                            }
                        }
                        state.request_update();
                        return;
                    }
                    _ => {
                        return;
                    }
                }
                if let Some(id) = over {
                    for bar in &mut state.bars {
                        if bar.surf.as_ref().id() == id {
                            bar.hover(x,y, &state.wayland, &mut state.runtime);
                        }
                        if let Some(popup) = &bar.popup {
                            if popup.wl.surf.as_ref().id() == id {
                                bar.hover_popup(x, y, &state.wayland, &mut state.runtime);
                            }
                        }
                    }
                }
            });
        }
        if si.has_touch {
            let finger = seat.get_touch();
            finger.quick_assign(move |finger, event, mut data| {
                use wayland_client::protocol::wl_touch::Event;
                let state : &mut State = data.get().unwrap();
                drop(finger);
                match event {
                    Event::Down { surface, x, y, .. } => {
                        // TODO support gestures?  Wait for Up, detect Cancel
                        for bar in &mut state.bars {
                            if surface == *bar.surf {
                                bar.sink.button(x,y,0, &mut state.runtime);
                                break;
                            }
                        }
                    }
                    _ => ()
                }
            });
        }
    }

    pub fn output_finish_setup(&mut self, output : &mut OutputData) {
        let mgr : Attached<ZxdgOutputManagerV1> = self.env.require_global();
        let xdg_out = mgr.get_xdg_output(&output.output);

        xdg_out.quick_assign(move |xdg_out, event, mut data| {
            use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_v1::Event;
            let state : &mut State = data.get().unwrap();
            let outputs = state.wayland.get_outputs();
            let i = outputs.take_in(|outputs| {
                for (i, data) in outputs.iter_mut().enumerate() {
                    if data.xdg.as_ref() != Some(&*xdg_out) {
                        continue;
                    }
                    match event {
                        Event::LogicalPosition { x, y } => {
                            data.position = (x, y);
                        }
                        Event::LogicalSize { width, height } => {
                            data.size = (width, height);
                        }
                        Event::Name { name } => {
                            data.name = name;
                        }
                        Event::Description { description } => {
                            data.description = description;
                        }
                        Event::Done => {
                            return Some(i);
                        }
                        _ => ()
                    }
                    return None;
                }
                None
            });
            if let Some(i) = i {
                state.output_ready(i);
            }
        });

        output.xdg = Some(xdg_out.into());
    }

    pub fn new_popup(&self, bar : &Bar, anchor : (i32, i32, i32, i32), size : (i32, i32)) -> Popup {
        self.new_popup_on(&bar.ls_surf, !bar.anchor_top, anchor, size, bar.scale)
    }

    fn new_popup_on(&self, ls_surf : &ZwlrLayerSurfaceV1, prefer_top : bool, anchor : (i32, i32, i32, i32), size : (i32, i32), scale : i32) -> Popup {
        use wayland_protocols::xdg_shell::client::xdg_positioner::{Anchor,Gravity};
        let surf = self.env.create_surface();
        let wmb : Attached<XdgWmBase> = self.env.require_global();
        let pos = wmb.create_positioner();
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

        let as_xdg = wmb.get_xdg_surface(&surf);
        as_xdg.quick_assign(move |as_xdg, event, mut data| {
            use wayland_protocols::xdg_shell::client::xdg_surface::Event;
            let state : &mut State = data.get().unwrap();
            match event {
                Event::Configure { serial } => {
                    as_xdg.ack_configure(serial);
                    for bar in &mut state.bars {
                        if let Some(popup) = &mut bar.popup {
                            if popup.wl.as_xdg == *as_xdg {
                                popup.wl.waiting_on_configure = false;
                            }
                        }
                    }
                    state.request_draw();
                }
                _ => {}
            }
        });

        let as_popup = as_xdg.get_popup(None, &pos);
        as_popup.quick_assign(move |as_popup, event, mut data| {
            use wayland_protocols::xdg_shell::client::xdg_popup::Event;
            let state : &mut State = data.get().unwrap();
            match event {
                Event::Configure { .. } => {
                    // no-op as we didn't allow changing w/h and we don't care about x/y
                }
                Event::PopupDone => {
                    for bar in &mut state.bars {
                        if bar.popup.as_ref().map_or(false, |popup| popup.wl.as_popup == *as_popup) {
                            bar.popup = None;
                        }
                    }
                }
                _ => {}
            }
        });

        ls_surf.get_popup(&as_popup);
        as_xdg.set_window_geometry(0, 0, size.0, size.1);
        surf.set_buffer_scale(scale);
        surf.commit();
        Popup {
            surf,
            as_xdg : as_xdg.into(),
            as_popup : as_popup.into(),
            anchor,
            size,
            scale,
            prefer_top,
            waiting_on_configure : true,
        }
    }

    pub fn resize_popup(&self, ls_surf : &ZwlrLayerSurfaceV1, popup : &mut Popup, size : (i32, i32), scale : i32) {
        if popup.as_popup.as_ref().version() >= wayland_protocols::xdg_shell::client::xdg_popup::REQ_REPOSITION_SINCE {
            use wayland_protocols::xdg_shell::client::xdg_positioner::{Anchor,Gravity};
            popup.as_xdg.set_window_geometry(0, 0, size.0, size.1);
            let wmb : Attached<XdgWmBase> = self.env.require_global();
            let pos = wmb.create_positioner();
            pos.set_size(size.0, size.1);
            pos.set_anchor_rect(popup.anchor.0, popup.anchor.1, popup.anchor.2, popup.anchor.3);
            pos.set_offset(0, 0);
            if popup.prefer_top {
                pos.set_anchor(Anchor::Top);
                pos.set_gravity(Gravity::Top);
            } else {
                pos.set_anchor(Anchor::Bottom);
                pos.set_gravity(Gravity::Bottom);
            }
            pos.set_constraint_adjustment(0xF); // allow moving but not resizing
            popup.as_popup.reposition(&pos, 0);
            popup.size = size;
        } else {
            // can't resize; emulate by destroying and re-creating.
            popup.as_popup.destroy();
            popup.as_xdg.destroy();
            popup.surf.destroy();
            *popup = self.new_popup_on(ls_surf, popup.prefer_top, popup.anchor, size, scale);
        }
    }
}

pub async fn run_queue(mut wl_queue : wayland_client::EventQueue, state : Rc<RefCell<State>>) -> io::Result<Infallible> {
    let fd = AsyncFd::new(util::Fd(wl_queue.display().get_connection_fd()))?;
    let mut reader = None;
    let mut rg = None;
    let mut wg = None;

    loop {
        if reader.is_none() {
            reader = wl_queue.prepare_read();
        }

        futures_util::future::poll_fn(|ctx| {
            let mut state = state.borrow_mut();
            match &state.wayland.flush {
                Some(w) if w.will_wake(ctx.waker()) => (),
                _ => state.wayland.flush = Some(ctx.waker().clone()),
            }

            rg = match fd.poll_read_ready(ctx) {
                task::Poll::Ready(g) => Some(g?),
                task::Poll::Pending => None,
            };

            wg = match fd.poll_write_ready(ctx) {
                task::Poll::Ready(g) => Some(g?),
                task::Poll::Pending => None,
            };

            if state.wayland.need_flush && wg.is_some() {
                task::Poll::Ready(Ok(()))
            } else if rg.is_some() {
                task::Poll::Ready(Ok(()))
            } else {
                task::Poll::Pending::<io::Result<()>>
            }
        }).await?;

        if let Some(g) = &mut wg {
            match wl_queue.display().flush() {
                Ok(()) => {
                    state.borrow_mut().wayland.need_flush = false;
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    g.clear_ready();
                }
                Err(e) => Err(e)?,
            }
        }

        if let Some(g) = &mut rg {
            match reader.take().map(|g| g.read_events()) {
                None |
                Some(Ok(())) => {
                    // dispatch events and continue
                }
                Some(Err(e)) if e.kind() == io::ErrorKind::WouldBlock => {
                    g.clear_ready();
                }
                Some(Err(e)) => Err(e)?,
            }
        }

        let mut lock = state.borrow_mut();
        wl_queue.dispatch_pending(&mut *lock, |event, object, _| {
            error!("Orphan event: {}@{} : {}", event.interface, object.as_ref().id(), event.name);
        })?;
    }
}

pub struct Popup {
    pub surf : Attached<WlSurface>,
    pub as_xdg : Attached<XdgSurface>,
    pub as_popup : Attached<XdgPopup>,
    pub anchor : (i32, i32, i32, i32),
    pub size : (i32, i32), // logical size
    pub scale : i32,
    pub waiting_on_configure : bool,
    pub prefer_top : bool,
}

impl Drop for Popup {
    fn drop(&mut self) {
        self.as_popup.destroy();
        self.as_xdg.destroy();
        self.surf.destroy();
    }
}
