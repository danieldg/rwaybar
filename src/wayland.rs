use log::debug;
use smithay_client_toolkit::WaylandSource;
use smithay_client_toolkit::environment::{SimpleGlobal,MultiGlobalHandler};
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::environment;
use smithay_client_toolkit::seat::SeatData;
use smithay_client_toolkit::shm::MemPool;
use wayland_client::Attached;
use wayland_client::protocol::wl_compositor::WlCompositor;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_pointer::{ButtonState,Axis};
use wayland_client::protocol::wl_registry::WlRegistry;
use wayland_client::protocol::wl_seat::WlSeat;
use wayland_client::protocol::wl_shm::WlShm;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_manager_v1::ZxdgOutputManagerV1;
use wayland_protocols::wlr::unstable::layer_shell::v1::client::zwlr_layer_shell_v1::ZwlrLayerShellV1;
use wayland_protocols::wlr::unstable::layer_shell::v1::client::zwlr_layer_surface_v1::ZwlrLayerSurfaceV1;
use wayland_protocols::xdg_shell::client::xdg_wm_base::XdgWmBase;

use std::error::Error;

use crate::state::State;

/// Helper for populating [OutputData]
#[derive(Debug,Default)]
struct OutputHandler {
    outputs : Vec<Attached<WlOutput>>,
}

impl MultiGlobalHandler<WlOutput> for OutputHandler {
    fn created(&mut self, registry: Attached<WlRegistry>, id: u32, version: u32, _: wayland_client::DispatchData) {
        assert!(version > 1);
        let version = std::cmp::min(version, 3);
        let output = registry.bind::<WlOutput>(version, id);
        // TODO is any of this info still useful?  Must record if so.
        output.quick_assign(move |output, event, mut data| {
            use wayland_client::protocol::wl_output::Event;
            match event {
                Event::Geometry { .. } => {
                }
                Event::Scale { factor } => {
                    let _ = factor;
                }
                Event::Done => {
                    if let Some(state) = data.get::<State>() {
                        for (i, data) in state.wayland.outputs.iter().enumerate() {
                            if data.output == **output {
                                // TODO state tracking, catch other causes of Done being sent?
                                state.output_ready(i);
                                return;
                            }
                        }
                        state.wayland.add_new_output(&output);
                    }
                }
                _ => ()
            }
        });
        self.outputs.push(output.into());
    }
    fn removed(&mut self, id: u32, _: wayland_client::DispatchData) {
        dbg!(id); // TODO destroy bars on output removal
    }
    fn get_all(&self) -> Vec<Attached<WlOutput>> {
        self.outputs.clone()
    }
}

/// Wayland globals (access via [Environment::require_global])
pub struct Globals {
    sctk_compositor: SimpleGlobal<WlCompositor>,
    sctk_shm: smithay_client_toolkit::shm::ShmHandler,
    sctk_seats: smithay_client_toolkit::seat::SeatHandler,
    outputs: OutputHandler,

    layer_shell : SimpleGlobal<ZwlrLayerShellV1>,
    xdg_wm : SimpleGlobal<XdgWmBase>,
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
        XdgWmBase => xdg_wm,
        ZxdgOutputManagerV1 => xdg_out,
    ],
    multis = [
        smithay_client_toolkit::reexports::client::protocol::wl_seat::WlSeat => sctk_seats,
        WlOutput => outputs,
    ],
);

/// Metadata on a [WlOutput]
#[derive(Debug)]
pub struct OutputData {
    pub output : WlOutput,

    pub pos_x : i32,
    pub pos_y : i32,
    pub size_x : i32,
    pub size_y : i32,
    pub name : String,
    pub description: String,
}

/// Structures related to the Wayland display
pub struct WaylandClient {
    pub env : Environment<Globals>,
    pub display : wayland_client::Display,
    pub shm : MemPool,
    pub outputs : Vec<OutputData>,
    cursor : wayland_cursor::Cursor,
    cursor_surf : Attached<WlSurface>,
}

impl WaylandClient {
    pub fn new(eloop : calloop::LoopHandle<State>) -> Result<Self, Box<dyn Error>> {
        let display = wayland_client::Display::connect_to_env()?;

        let mut wl_queue = display.create_event_queue();

        let env = smithay_client_toolkit::environment::Environment::new(&display.attach(wl_queue.token()), &mut wl_queue, Globals {
            sctk_compositor: SimpleGlobal::new(),
            sctk_shm: smithay_client_toolkit::shm::ShmHandler::new(),
            sctk_seats : smithay_client_toolkit::seat::SeatHandler::new(),
            outputs: OutputHandler::default(),
            layer_shell : SimpleGlobal::new(),
            xdg_wm : SimpleGlobal::new(),
            xdg_out : SimpleGlobal::new(),
        })?;

        WaylandSource::new(wl_queue).quick_insert(eloop)?;

        let shm = env.create_simple_pool(|_| ())?;

        let cursor_shm = env.require_global();
        let mut cursor_theme = wayland_cursor::CursorTheme::load(32, &cursor_shm);
        let cursor = cursor_theme.get_cursor("default").expect("Could not load cursor, check XCURSOR_THEME").clone();

        let cursor_surf = env.create_surface();
        let cursor_img = &cursor[0];
        let dim = cursor_img.dimensions();
        cursor_surf.attach(Some(&cursor_img), 0, 0);
        cursor_surf.damage_buffer(0, 0, dim.0 as _, dim.1 as _);
        cursor_surf.commit();

        let mut client = WaylandClient {
            env,
            shm,
            display,
            outputs : Vec::new(),
            cursor,
            cursor_surf,
        };

        for output in client.env.get_all_globals() {
            client.add_new_output(&output);
        }

        let _seat_watcher = client.env.listen_for_seats(|seat, si, mut data| {
            let state : &mut State = data.get().unwrap();
            state.wayland.add_seat(&seat, si);
        });

        for seat in client.env.get_all_seats() {
            smithay_client_toolkit::seat::with_seat_data(&seat, |si| client.add_seat(&seat, si));
        }

        // kick off the initial configuration events
        client.display.flush()?;

        Ok(client)
    }

    pub fn add_seat(&mut self, seat : &Attached<WlSeat>, si : &SeatData) {
        if si.has_pointer {
            let mouse = seat.get_pointer();
            let mut bar_idx = None;
            let mut x = 0.0;
            let mut y = 0.0;
            mouse.quick_assign(move |mouse, event, mut data| {
                use wayland_client::protocol::wl_pointer::Event;
                let state : &mut State = data.get().unwrap();
                match &event {
                    Event::Enter { serial, surface, surface_x, surface_y, .. } => {
                        let spot = state.wayland.cursor[0].hotspot();
                        mouse.set_cursor(*serial, Some(&state.wayland.cursor_surf), spot.0 as _, spot.1 as _);

                        for (i, bar) in state.bars.iter().enumerate() {
                            if *surface == *bar.surf {
                                bar_idx = Some(i);
                                break;
                            }
                        }
                        assert!(bar_idx.is_some());
                        x = *surface_x;
                        y = *surface_y;
                    }
                    Event::Motion { surface_x, surface_y, .. } => {
                        x = *surface_x;
                        y = *surface_y;
                    }
                    Event::Leave { surface, .. } => {
                        if *surface == *state.bars[bar_idx.unwrap()].surf {
                            bar_idx = None;
                        }
                    }
                    &Event::Button {
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
                        state.bars[bar_idx.unwrap()].sink.button(x,y,button_id, &mut state.runtime);
                        state.request_draw();
                    }
                    &Event::Axis { axis, value, .. } => {
                        dbg!(value);
                        // TODO minimum scroll distance and/or rate?
                        let button_id = match axis {
                            Axis::VerticalScroll if value < 0.0 => 5, // up
                            Axis::VerticalScroll if value > 0.0 => 6, // down
                            Axis::HorizontalScroll if value < 0.0 => 7, // left
                            Axis::HorizontalScroll if value > 0.0 => 8, // right
                            _ => return,
                        };
                        state.bars[bar_idx.unwrap()].sink.button(x,y,button_id, &mut state.runtime);
                        state.request_draw();
                    }
                    _ => ()
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
                        // TODO support gestures - wait for Up, detect Cancel
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

    pub fn add_new_output(&mut self, output : &WlOutput) {
        let i = self.outputs.len();
        let mgr : Attached<ZxdgOutputManagerV1> = self.env.require_global();
        let xdg_out = mgr.get_xdg_output(output);

        xdg_out.quick_assign(move |_xdg_out, event, mut data| {
            use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_v1::Event;
            let state : &mut State = data.get().unwrap();
            let data = &mut state.wayland.outputs[i];
            match event {
                Event::LogicalPosition { x, y } => {
                    data.pos_x = x;
                    data.pos_y = y;
                }
                Event::LogicalSize { width, height } => {
                    data.size_x = width;
                    data.size_y = height;
                }
                Event::Name { name } => {
                    data.name = name;
                }
                Event::Description { description } => {
                    data.description = description;
                }
                Event::Done => {
                    state.output_ready(i);
                }
                _ => ()
            }
        });
        self.outputs.push(OutputData {
            output : output.clone(),
            pos_x : 0,
            pos_y : 0,
            size_x : 0,
            size_y : 0,
            name : String::new(),
            description: String::new(),
        });
    }

    #[allow(dead_code)] // TODO wire up
    pub fn create_popup(&self, parent : &ZwlrLayerSurfaceV1) -> Attached<WlSurface> {
        let surf = self.env.create_surface();
        let wmb : Attached<XdgWmBase> = self.env.require_global();
        let pos = wmb.create_positioner();
/* TODO
        pos.set_size(232, 87)
        pos.set_anchor_rect(3614, -4, 122, 56) // gtk adds 4 pixels padding to the object
        pos.set_offset(0, 0)
        pos.set_anchor(2)
        pos.set_gravity(2)
        pos.set_constraint_adjustment(9)
*/
        let xdg_surf = wmb.get_xdg_surface(&surf);
        let popup = xdg_surf.get_popup(None, &pos);
        parent.get_popup(&popup);
        xdg_surf.set_window_geometry(0, 0, 100, 100);
        surf.commit();
        // TODO handle Configure on popup
        // TODO handle Configure on surface by ACKing
        // TODO actually draw something
        surf
    }
}
