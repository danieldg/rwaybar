use std::error::Error;
use std::time::Instant;

mod data;
mod item;
mod state;
mod wayland;

use state::State;
use wayland::WaylandClient;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();

    // Avoid producing zombies.  We don't need exit status, and can detect end-of-file on pipes to
    // handle any respawning required.
    unsafe { libc::signal(libc::SIGCHLD, libc::SIG_IGN); }

    let mut eloop = calloop::EventLoop::new()?;

    let client = WaylandClient::new(eloop.handle())?;
    let mut state = State::new(client, eloop.handle())?;

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
