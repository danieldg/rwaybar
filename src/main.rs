use std::error::Error;
use std::time::Instant;

mod data;
mod item;
mod state;
mod sway;
mod wayland;

pub trait Variable : std::fmt::Debug {
    fn from_json(config : &json::JsonValue) -> Option<Self>
        where Self : Sized;
    fn init(&self, name : &str, rt : &state::Runtime) {
        let _ = (name, rt);
    }
    fn update(&self, name : &str, rt : &state::Runtime) {
        let _ = (name, rt);
    }
    fn read_in<F : FnOnce(&str) -> R,R>(&self, name : &str, key : &str, rt : &state::Runtime, f : F) -> R;
    fn write(&self, name : &str, key : &str, value : String, rt : &state::Runtime) {
        let _ = (name, key, value, rt);
    }
}

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
