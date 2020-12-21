use crate::util;
use dbus::channel::{BusType,Channel};
use dbus::nonblock::{LocalConnection,Process,NonblockReply};
use futures_util::future::Either;
use futures_util::future::select;
use futures_util::pin_mut;
use log::error;
use once_cell::unsync::OnceCell;
use std::error::Error;
use std::io;
use std::rc::Rc;
use tokio::io::unix::AsyncFd;
use tokio::sync::Notify;

thread_local! {
    static SOCK : Rc<OnceCell<ConnState>> = Default::default();
}

pub struct ConnState {
    pub local : LocalConnection,
    wake : Notify,
}

pub fn init() -> Result<(), Box<dyn Error>> {
    let rc = SOCK.with(|cell| cell.clone());

    let mut channel = Channel::get_private(BusType::Session)?;
    channel.set_watch_enabled(true);
    let watch = channel.watch();
    let afd = AsyncFd::new(util::Fd(watch.fd))?;

    let mut local = LocalConnection::from(channel);
    let wake = Notify::new();

    local.set_waker(Some(Box::new(|| {
        let rc = SOCK.with(|cell| cell.clone());
        match rc.get() {
            Some(conn) => conn.wake.notify_one(),
            None => error!("Ignoring dbus wake on wrong thread"),
        }
        Ok(())
    })));

    rc.set(ConnState { local, wake }).ok().expect("Called init twice");

    tokio::task::spawn_local(async move {
        let conn = rc.get().unwrap();
        let channel : &Channel = conn.local.as_ref();
        let rv : io::Result<()> = async { loop {
            let msg_in = afd.readable();
            let msg_out = conn.wake.notified();
            pin_mut!(msg_in, msg_out);
            let why = select(msg_in, msg_out).await;
            channel.read_write(Some(Default::default())).map_err(|()| io::Error::last_os_error())?;
            match why {
                Either::Left((rh, _)) => {
                    // if we woke due to readable, check to see if we are done reading and clear
                    // the ready status if so.
                    //
                    // https://github.com/diwic/dbus-rs/issues/254
                    let mut rh = rh?;
                    let mut buf = [0u8;1];
                    let rc = unsafe {
                        libc::recv(watch.fd, buf.as_mut_ptr() as *mut _, 1, libc::MSG_DONTWAIT | libc::MSG_PEEK)
                    };
                    if rc != 1 {
                        rh.clear_ready();
                    }
                }
                Either::Right(((), _)) => {}
            }
            conn.local.process_all();

            // clear out the send buffer.  This should only happen when a write was already blocked.
            if channel.has_messages_to_send() {
                loop {
                    let mut wh = afd.writable().await?;
                    channel.read_write(Some(Default::default())).map_err(|()| io::Error::last_os_error())?;
                    if channel.has_messages_to_send() {
                        wh.clear_ready();
                    } else {
                        break;
                    }
                }
            }
        } }.await;
        match rv {
            Ok(()) => {}
            Err(e) => {
                error!("Error in dbus connection: {}", e);
            }
        }
    });

    Ok(())
}

pub fn get() -> impl std::ops::Deref<Target=ConnState> {
    struct V(Rc<OnceCell<ConnState>>);

    impl std::ops::Deref for V {
        type Target = ConnState;
        fn deref(&self) -> &ConnState {
            self.0.get().expect("Must call dbus::init before dbus::get")
        }
    }
    V(SOCK.with(|cell| cell.clone()))
}
