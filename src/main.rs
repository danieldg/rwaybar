use smithay_client_toolkit::environment::{SimpleGlobal,MultiGlobalHandler};
use smithay_client_toolkit::environment;
use smithay_client_toolkit::WaylandSource;
use std::error::Error;
use std::time::Instant;

use wayland_client::Attached;
use wayland_client::protocol::wl_shm::WlShm;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_registry::WlRegistry;
use wayland_client::protocol::wl_compositor::WlCompositor;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;
use wayland_protocols::xdg_shell::client::xdg_wm_base::XdgWmBase;
use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_manager_v1::ZxdgOutputManagerV1;

use layer_shell::zwlr_layer_shell_v1::ZwlrLayerShellV1;

mod item;
mod data;

mod state;
use state::State;

#[derive(Debug,Default)]
struct OutputHandler {
    events : Vec<Attached<WlOutput>>,
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
                        state.add_output(&output);
                    }
                }
                _ => ()
            }
        });
        self.events.push(output.into());
    }
    fn removed(&mut self, id: u32, _: wayland_client::DispatchData) {
        dbg!(id);
    }
    fn get_all(&self) -> Vec<Attached<WlOutput>> {
        self.events.clone()
    }
}

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

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    // Avoid producing zombies.  We don't need exit status, and can detect end-of-file on pipes to
    // handle any respawning required.
    unsafe { libc::signal(libc::SIGCHLD, libc::SIG_IGN); }

    let mut eloop = calloop::EventLoop::new()?;

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

    WaylandSource::new(wl_queue).quick_insert(eloop.handle())?;

    let mut state = State::new(env, eloop.handle(), display)?;

    for output in state.env.get_all_globals() {
        state.add_output(&output);
    }

    let _seat_watcher = state.env.listen_for_seats(|seat, si, mut data| {
        let state : &mut State = data.get().unwrap();
        state.add_seat(&seat, si);
    });

    for seat in state.env.get_all_seats() {
        smithay_client_toolkit::seat::with_seat_data(&seat, |si| state.add_seat(&seat, si));
    }

    // kick off the initial configuration events
    state.display.flush()?;

    // everything else is event-triggered
    loop {
        let timeout = state.runtime.wake_at.get()
            .map(|alarm| alarm.saturating_duration_since(Instant::now()));
        eloop.dispatch(timeout, &mut state)?;
        if state.runtime.wake_at.get().map_or(false, |alarm| alarm <= Instant::now()) {
            state.runtime.wake_at.set(None);
            state.tick();
        }
    }
}
