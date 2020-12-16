use smithay_client_toolkit::environment::SimpleGlobal;
use smithay_client_toolkit::{new_default_environment,default_environment};
use smithay_client_toolkit::WaylandSource;
use std::error::Error;
use std::time::Instant;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;

use layer_shell::zwlr_layer_shell_v1::ZwlrLayerShellV1;

mod item;
mod data;

mod state;
use state::State;

default_environment!(MyEnv,
    fields = [
        layer_shell : SimpleGlobal<ZwlrLayerShellV1>,
    ],
    singles = [
        ZwlrLayerShellV1 => layer_shell,
    ]
);

fn main() -> Result<(), Box<dyn Error>> {
    let mut eloop = calloop::EventLoop::new()?;
    let (env, display, wl_queue) = new_default_environment!(MyEnv,
        fields=[
            layer_shell : SimpleGlobal::new(),
        ]
    )?;

    WaylandSource::new(wl_queue).quick_insert(eloop.handle())?;

    let mut state = State::new(env, eloop.handle(), display)?;

    let _out_watcher = state.env.listen_for_outputs(|out, oi, mut data| {
        let state : &mut State = data.get().unwrap();
        state.add_output(&out, oi);
    });

    for output in state.env.get_all_outputs() {
        smithay_client_toolkit::output::with_output_info(&output, |oi| state.add_output(&output, oi));
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
        let timeout = state.wake_at
            .map(|alarm| alarm.saturating_duration_since(Instant::now()));
        eloop.dispatch(timeout, &mut state)?;
        if state.wake_at.map_or(false, |alarm| alarm <= Instant::now()) {
            state.wake_at = None;
            state.tick();
        }
    }
}
