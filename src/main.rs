use std::error::Error;
use std::io;
use tokio::io::unix::AsyncFd;

mod data;
mod dbus;
mod icon;
mod item;
mod mpris;
mod pulse;
mod state;
mod sway;
mod tray;
mod util;
mod wayland;

use state::State;
use wayland::WaylandClient;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::Builder::from_env(env_logger::Env::new().default_filter_or("warn")).init();

    // Avoid producing zombies.  We don't need exit status, and can detect end-of-file on pipes to
    // handle any respawning required.
    unsafe { libc::signal(libc::SIGCHLD, libc::SIG_IGN); }

    let rt = tokio::runtime::Builder::new_current_thread().enable_all().build()?;

    let (client, mut wl_queue) = WaylandClient::new()?;

    tokio::task::LocalSet::new().block_on(&rt, async move {
        dbus::init()?;

        let state = State::new(client)?;
        let fd = AsyncFd::new(util::Fd(wl_queue.display().get_connection_fd()))?;

        loop {
            if let Some(reader) = wl_queue.prepare_read() {
                let mut rg = fd.readable().await?;
                match reader.read_events() {
                    Ok(()) => {
                        rg.retain_ready();
                    }
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                        rg.clear_ready();
                    }
                    Err(e) => Err(e)?,
                }
            }

            let mut lock = state.borrow_mut();
            wl_queue.dispatch_pending(&mut *lock, |event, object, _| {
                panic!("Orphan event: {}@{} : {}", event.interface, object.as_ref().id(), event.name);
            })?;
            drop(lock);


            match wl_queue.display().flush() {
                Ok(()) => {}
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    dbg!();
                    let mut wg = fd.writable().await?;
                    loop {
                        match wl_queue.display().flush() {
                            Ok(()) => {
                                wg.retain_ready();
                                break;
                            }
                            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                                wg.clear_ready();
                            }
                            Err(e) => Err(e)?,
                        }
                    }
                }
                Err(e) => Err(e)?,
            }
        }
    })
}
