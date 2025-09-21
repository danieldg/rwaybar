use crate::{dbus::DBus, state::State, util::spawn, value::Value};
use std::{
    cell::{Cell, RefCell},
    mem::ManuallyDrop,
    rc::{Rc, Weak},
};

#[derive(Debug)]
pub struct DbusApi(Rc<RefCell<State>>);

thread_local! {
    static DATA : ManuallyDrop<Cell<Weak<DbusApi>>> = Default::default();
}

fn get_state() -> Option<Rc<RefCell<State>>> {
    DATA.with(|d| {
        let w = d.take();
        let rv = w.upgrade().map(|rc| rc.0.clone());
        d.set(w);
        rv
    })
}

#[derive(Debug)]
pub struct Item;

#[zbus::interface(name = "net.danieldg.rwaybar")]
impl Item {
    fn items(&self) -> zbus::fdo::Result<Vec<String>> {
        let state = get_state().ok_or_else(|| zbus::Error::Unsupported)?;
        let state = state.borrow_mut();
        let mut rv: Vec<_> = state
            .runtime
            .items
            .keys()
            .map(|k| String::from(&**k))
            .collect();
        rv.sort();
        Ok(rv)
    }

    fn eval(&self, expr: &str) -> zbus::fdo::Result<String> {
        let state = get_state().ok_or_else(|| zbus::Error::Unsupported)?;
        let state = state.borrow_mut();
        let _guard = state.local.enter();
        // We need to hold the LocalEnterGuard to prevent tokio from rejecting spawn_local calls
        // made by an item's read function.  This can happen if an out-of-date value is read.
        state
            .runtime
            .eval(expr)
            .map(|v| v.into_text().into_owned())
            .map_err(|e| zbus::fdo::Error::InvalidArgs(format!("{e}")))
    }

    fn format(&self, expr: &str) -> zbus::fdo::Result<String> {
        let state = get_state().ok_or_else(|| zbus::Error::Unsupported)?;
        let state = state.borrow_mut();
        let _guard = state.local.enter();
        state
            .runtime
            .format(expr)
            .map(|v| v.into_text().into_owned())
            .map_err(|e| zbus::fdo::Error::InvalidArgs(format!("{e}")))
    }

    fn get(&self, expr: &str) -> zbus::fdo::Result<String> {
        let state = get_state().ok_or_else(|| zbus::Error::Unsupported)?;
        let state = state.borrow_mut();
        let _guard = state.local.enter();
        state
            .runtime
            .format(&format!("{{{expr}}}"))
            .map(|v| v.into_text().into_owned())
            .map_err(|e| zbus::fdo::Error::InvalidArgs(format!("{e}")))
    }

    fn write(&self, target: &str, value: &str) -> zbus::fdo::Result<()> {
        let state = get_state().ok_or_else(|| zbus::Error::Unsupported)?;
        let state = state.borrow_mut();
        let _guard = state.local.enter();
        let rt = &state.runtime;

        let (name, key) = match target.find('.') {
            Some(p) => (&target[..p], &target[p + 1..]),
            None => (&target[..], ""),
        };

        rt.items
            .get(name)
            .ok_or_else(|| zbus::fdo::Error::FileNotFound(name.into()))
            .map(|item| item.data.write(name, key, Value::Borrow(value), rt))
    }
}

impl Drop for DbusApi {
    fn drop(&mut self) {
        spawn("DBusAPI", async move {
            let dbus = DBus::get_session();
            let zbus = dbus.connection().await?;
            zbus.release_name("net.danieldg.rwaybar").await?;
            zbus.object_server().remove::<Item, _>("/rwaybar").await?;
            Ok(())
        })
    }
}

impl DbusApi {
    pub fn enable(state: &State) -> Rc<Self> {
        DATA.with(|d| {
            let mut w = d.take();
            let rv = match w.upgrade() {
                Some(rc) => rc,
                None => {
                    let state = state.get_ref();
                    let rc = Rc::new(DbusApi(state));
                    let held = rc.clone();
                    spawn("DBusAPI", async move {
                        let dbus = DBus::get_session();
                        let zbus = dbus.connection().await?;
                        zbus.object_server().at("/rwaybar", Item).await?;
                        zbus.request_name("net.danieldg.rwaybar").await?;
                        drop(held);
                        Ok(())
                    });
                    w = Rc::downgrade(&rc);
                    rc
                }
            };
            d.set(w);
            rv
        })
    }
}
