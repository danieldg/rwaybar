use log::{debug,error};
use std::cell::RefCell;
use std::convert::Infallible;
use std::error::Error;
use std::io;
use std::rc::Rc;
use std::task;
use smithay_client_toolkit::environment::SimpleGlobal;
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::environment;
use smithay_client_toolkit::output::OutputHandling;
use smithay_client_toolkit::output::OutputInfo;
use smithay_client_toolkit::output::OutputStatusListener;
use smithay_client_toolkit::seat::SeatData;
use smithay_client_toolkit::seat::SeatHandling;
use smithay_client_toolkit::seat::SeatListener;
use tokio::io::unix::AsyncFd;
use wayland_client::Attached;
use wayland_client::DispatchData;
use wayland_client::protocol::wl_compositor::WlCompositor;
use wayland_client::protocol::wl_display::WlDisplay;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_pointer::{ButtonState,Axis};
use wayland_client::protocol::wl_seat::WlSeat;
use wayland_client::protocol::wl_shm::WlShm;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_manager_v1::ZxdgOutputManagerV1;
use wayland_protocols::wlr::unstable::layer_shell::v1::client::zwlr_layer_shell_v1::ZwlrLayerShellV1;
use wayland_protocols::wlr::unstable::layer_shell::v1::client::zwlr_layer_surface_v1::ZwlrLayerSurfaceV1;
use wayland_protocols::xdg_shell::client::xdg_popup::XdgPopup;
use wayland_protocols::xdg_shell::client::xdg_surface::XdgSurface;
use wayland_protocols::xdg_shell::client::xdg_wm_base::XdgWmBase;

use crate::render::Renderer;
use crate::state::{Bar,State};
use crate::util;

/// Wayland globals (access via [Environment::require_global])
pub struct Globals {
    sctk_compositor: SimpleGlobal<WlCompositor>,
    sctk_shm: smithay_client_toolkit::shm::ShmHandler,
    sctk_seats: smithay_client_toolkit::seat::SeatHandler,
    sctk_shell: smithay_client_toolkit::shell::ShellHandler,
    sctk_outputs: smithay_client_toolkit::output::OutputHandler,
    sctk_xdg_out: smithay_client_toolkit::output::XdgOutputHandler,

    layer_shell : SimpleGlobal<ZwlrLayerShellV1>,
}

environment!(Globals,
    singles = [
        WlCompositor => sctk_compositor,
        WlShm => sctk_shm,

        ZwlrLayerShellV1 => layer_shell,
        XdgWmBase => sctk_shell,
        ZxdgOutputManagerV1 => sctk_xdg_out,
    ],
    multis = [
        WlSeat => sctk_seats,
        WlOutput => sctk_outputs,
    ],
);

/// Structures related to the Wayland display
pub struct WaylandClient {
    pub env : Environment<Globals>,
    pub wl_display : Attached<WlDisplay>,
    pub renderer : Renderer,
    #[allow(unused)] // need to hold this handle for the callback to remain alive
    seat_watcher : SeatListener,
    flush : Option<task::Waker>,
    need_flush : bool,
}

impl WaylandClient {
    pub fn new() -> Result<(Self, wayland_client::EventQueue), Box<dyn Error>> {
        let display = wayland_client::Display::connect_to_env()?;

        // TODO remove when wayland-client merges #363 to fix it there
        std::env::remove_var("WAYLAND_SOCKET");

        let mut wl_queue = display.create_event_queue();
        let wl_display = display.attach(wl_queue.token());

        let (sctk_outputs, sctk_xdg_out) = smithay_client_toolkit::output::XdgOutputHandler::new_output_handlers();
        let mut sctk_seats = smithay_client_toolkit::seat::SeatHandler::new();

        let seat_watcher = sctk_seats.listen(|seat, si, _data| {
            Self::add_seat(&seat, si);
        });

        let env = smithay_client_toolkit::environment::Environment::new(&wl_display, &mut wl_queue, Globals {
            sctk_compositor: SimpleGlobal::new(),
            sctk_shm: smithay_client_toolkit::shm::ShmHandler::new(),
            sctk_shell : smithay_client_toolkit::shell::ShellHandler::new(),
            sctk_seats,
            sctk_outputs,
            sctk_xdg_out,
            layer_shell : SimpleGlobal::new(),
        })?;

        let client = WaylandClient {
            renderer : Renderer::new(&env)?,
            env,
            wl_display,
            seat_watcher,
            flush : None,
            need_flush : true,
        };

        Ok((client, wl_queue))
    }

    pub fn add_output_listener<F>(&mut self, f : F) -> OutputStatusListener
        where F : FnMut(WlOutput, &OutputInfo, DispatchData) + 'static
    {
        self.env.with_inner(|globals| {
            globals.sctk_outputs.listen(f)
        })
    }

    pub fn flush(&mut self) {
        self.need_flush = true;
        self.flush.take().map(|f| f.wake());
    }

    fn add_seat(seat : &Attached<WlSeat>, si : &SeatData) {
        if si.has_pointer {
            let mouse = seat.get_pointer();
            let mut over = None;
            let mut x = 0.0;
            let mut y = 0.0;
            let mut axis_h = 0.0;
            let mut axis_v = 0.0;
            let mut axis_ts = 0;
            mouse.quick_assign(move |mouse, event, mut data| {
                use wayland_client::protocol::wl_pointer::Event;
                let state : &mut State = data.get().unwrap();
                match event {
                    Event::Enter { serial, surface, surface_x, surface_y, .. } => {
                        state.wayland.renderer.cursor.set(&mouse, serial);
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
                        return;
                    }
                    Event::Axis { time, axis, value } => {
                        let button_id;
                        if value < 10.0 && value > -10.0 {
                            // continuous scroll
                            let decay = time.wrapping_sub(axis_ts);
                            if decay < 4000 {
                                // this combines the current scroll with prior scrolls, decaying
                                // the prior distance with a half-life of about 2/3 second
                                axis_h *= 0.999_f64.powi(decay as i32);
                                axis_v *= 0.999_f64.powi(decay as i32);
                            } else {
                                // after 4 seconds, just discard the prior scroll instead (4
                                // seconds already decays to 1.8% anyway)
                                axis_h = 0.0;
                                axis_v = 0.0;
                            }
                            axis_ts = time;
                            let total = match axis {
                                Axis::VerticalScroll => &mut axis_v,
                                Axis::HorizontalScroll => &mut axis_h,
                                _ => return,
                            };
                            *total += value;
                            let value = *total;
                            button_id = match axis {
                                Axis::VerticalScroll if value <= -10.0 => 5, // up
                                Axis::VerticalScroll if value >= 10.0 => 6, // down
                                Axis::HorizontalScroll if value <= -10.0 => 7, // left
                                Axis::HorizontalScroll if value >= 10.0 => 8, // right
                                _ => return,
                            };
                            *total = 0.0;
                        } else {
                            button_id = match axis {
                                Axis::VerticalScroll if value < 0.0 => 5, // up
                                Axis::VerticalScroll if value > 0.0 => 6, // down
                                Axis::HorizontalScroll if value < 0.0 => 7, // left
                                Axis::HorizontalScroll if value > 0.0 => 8, // right
                                _ => return,
                            };
                        }
                        for bar in &mut state.bars {
                            if Some(bar.surf.as_ref().id()) == over {
                                bar.sink.button(x,y,button_id, &mut state.runtime);
                            }
                            if bar.popup.as_ref().map(|p| p.wl.surf.as_ref().id()) == over {
                                bar.popup_button(x,y,button_id, &mut state.runtime);
                            }
                        }
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
