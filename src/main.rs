use std::error::Error;

mod bar;
mod data;
#[cfg(feature = "dbus")]
mod dbus;
mod event;
mod font;
mod icon;
mod item;
#[cfg(feature = "dbus")]
mod mpris;
mod pipewire;
#[cfg(feature = "pulse")]
mod pulse;
mod render;
mod state;
mod sway;
#[cfg(feature = "dbus")]
mod tray;
mod util;
mod wayland;
mod wlr;

use state::State;
use wayland::WaylandClient;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::Builder::from_env(env_logger::Env::new().default_filter_or("warn")).init();

    // Avoid producing zombies.  We don't need exit status, and can detect end-of-file on pipes to
    // handle any respawning required.
    unsafe {
        libc::signal(libc::SIGCHLD, libc::SIG_IGN);
    }

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    tokio::task::LocalSet::new().block_on(&rt, async move {
        let (client, wl_queue) = WaylandClient::new()?;

        let state = State::new(client)?;

        match wayland::run_queue(wl_queue, state).await? {}
    })
}
