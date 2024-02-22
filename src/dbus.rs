use crate::{
    data::Value,
    state::{NotifierList, Runtime},
    util,
    util::{spawn_noerr, Cell},
};
use async_once_cell::Lazy;
use futures_channel::mpsc::{self, UnboundedSender};
use futures_util::{future::RemoteHandle, StreamExt};
use log::{error, info};
use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    fmt,
    rc::Rc,
};
use zbus::{zvariant, Connection};
use zvariant::{OwnedValue, Value as Variant};

pub struct DBus {
    send: UnboundedSender<zbus::Message>,

    bus: Lazy<zbus::Result<Connection>, RemoteHandle<zbus::Result<Connection>>>,
}

impl fmt::Debug for DBus {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "DBus")
    }
}

thread_local! {
    static SESSION : OnceCell<Rc<DBus>> = Default::default();
    static SYSTEM : OnceCell<Rc<DBus>> = Default::default();
}

impl DBus {
    pub fn get_session() -> Rc<Self> {
        SESSION.with(|s| s.get_or_init(|| Self::new(true)).clone())
    }

    pub fn get_system() -> Rc<Self> {
        SYSTEM.with(|s| s.get_or_init(|| Self::new(false)).clone())
    }

    pub async fn connection(&self) -> Result<&Connection, zbus::Error> {
        (&self.bus).await.as_ref().map_err(Clone::clone)
    }

    fn new(is_session: bool) -> Rc<Self> {
        let (send, mut recv) = mpsc::unbounded();
        let (init, rh) = futures_util::FutureExt::remote_handle(async move {
            use zbus::conn::Builder;
            let builder = if is_session {
                Builder::session()?
            } else {
                Builder::system()?
            };

            builder.internal_executor(false).build().await
        });

        let tb = Rc::new(DBus {
            send,
            bus: Lazy::new(rh),
        });

        let this = tb.clone();
        util::spawn("DBus Sender", async move {
            init.await;
            let zbus = this.connection().await?;
            while let Some(msg) = recv.next().await {
                zbus.send(&msg).await?;
            }
            Ok(())
        });

        tb
    }

    pub fn send(&self, msg: zbus::Message) {
        let _ = self.send.unbounded_send(msg);
    }
}

/// The "dbus" block
#[derive(Debug)]
pub struct DbusValue {
    bus: Rc<DBus>,
    bus_name: Box<str>,
    path: Box<str>,
    interface: Box<str>,
    member: Box<str>,
    args: Box<[toml::Value]>,
    sig: Cell<Option<Rc<str>>>,
    value: RefCell<Option<OwnedValue>>,
    interested: NotifierList,
    watch: Cell<Option<RemoteHandle<()>>>,
}

impl DbusValue {
    pub fn from_toml(value: &toml::Value) -> Result<Rc<Self>, &'static str> {
        let dbus = match value.get("bus").and_then(|v| v.as_str()) {
            None | Some("session") => DBus::get_session(),
            Some("system") => DBus::get_system(),
            _ => {
                return Err("bus must be either 'session' or 'system'");
            }
        };
        let bus_name = value
            .get("owner")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .into();
        let path = value
            .get("path")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .into();
        let method = value.get("method").and_then(|v| v.as_str());
        let property = value.get("property").and_then(|v| v.as_str());
        let (interface, member, args);
        match (
            method.map(|s| s.rsplit_once(".")),
            property.map(|s| s.rsplit_once(".")),
        ) {
            (Some(_), Some(_)) => {
                return Err("dbus cannot query both a property and a method");
            }
            (Some(Some((i, m))), None) => {
                interface = i.into();
                member = m.into();
                args = value
                    .get("args")
                    .and_then(|v| v.as_array())
                    .cloned()
                    .unwrap_or_default()
                    .into_boxed_slice();
            }
            (None, Some(Some((i, p)))) => {
                interface = "org.freedesktop.DBus.Properties".into();
                member = "Get".into();
                args = Box::new([i.into(), p.into()]);
            }
            _ => {
                return Err("dbus requires a member or property to query");
            }
        }

        let rc = Rc::new(DbusValue {
            bus: dbus.clone(),
            bus_name,
            path,
            interface,
            member,
            args,
            value: RefCell::new(None),
            sig: Default::default(),
            interested: Default::default(),
            watch: Default::default(),
        });

        let watch_path = value.get("watch-path").and_then(|v| v.as_str());
        let watch_method = value.get("watch-method").and_then(|v| v.as_str());
        match watch_method.map(|s| s.rsplit_once(".")) {
            Some(Some((i, m))) => {
                let mut expr = format!("type='signal',interface='{}',member='{}'", i, m);
                if let Some(path) = &watch_path {
                    expr.push_str(",path='");
                    expr.push_str(path);
                    expr.push_str("'");
                }
                // TODO remove matches when this object is dropped
                dbus.send(
                    zbus::Message::method("/org/freedesktop/DBus", "AddMatch")
                        .unwrap()
                        .destination("org.freedesktop.DBus")
                        .unwrap()
                        .interface("org.freedesktop.DBus")
                        .unwrap()
                        .build(&expr)
                        .unwrap(),
                );

                let watch_path: Option<Box<str>> = watch_path.map(Into::into);
                let watch_method = watch_method.unwrap().to_owned();
                let weak = Rc::downgrade(&rc);
                let h = util::spawn_handle("DBus value watcher", async move {
                    let zbus = dbus.connection().await?;
                    let mut stream = zbus::MessageStream::from(zbus);
                    while let Some(Ok(msg)) = stream.next().await {
                        if let Some(rc) = weak.upgrade() {
                            let (i, m) = watch_method.rsplit_once(".").unwrap();
                            if msg.header().interface().map(|n| n.as_str()) != Some(i)
                                || msg.header().member().map(|n| n.as_str()) != Some(m)
                            {
                                continue;
                            }
                            if let Some(path) = &watch_path {
                                if msg.header().path().map(|p| p.as_str()) != Some(&*path) {
                                    continue;
                                }
                            }
                            rc.call_now();
                        }
                    }
                    Ok(())
                });
                rc.watch.set(Some(h));
            }
            Some(None) if watch_method == Some("") => {}
            Some(None) => error!("Invalid dbus watch expression, ignoring"),
            None if property.is_some() => {
                let prop = property.unwrap_or_default().to_owned();

                let expr = format!("type='signal',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path='{}',arg0='{}'",
                    rc.path, rc.args[0].as_str().unwrap(),
                );
                dbus.send(
                    zbus::Message::method("/org/freedesktop/DBus", "AddMatch")
                        .unwrap()
                        .destination("org.freedesktop.DBus")
                        .unwrap()
                        .interface("org.freedesktop.DBus")
                        .unwrap()
                        .build(&expr)
                        .unwrap(),
                );

                let weak = Rc::downgrade(&rc);

                let h = util::spawn_handle("DBus value watcher", async move {
                    let zbus = dbus.connection().await?;
                    let mut stream = zbus::MessageStream::from(zbus);
                    while let Some(Ok(msg)) = stream.next().await {
                        let hdr = msg.header();
                        if hdr.interface().map(|n| n.as_str())
                            != Some("org.freedesktop.DBus.Properties")
                            || hdr.member().map(|n| n.as_str()) != Some("PropertiesChanged")
                        {
                            continue;
                        }
                        if let Some(rc) = weak.upgrade() {
                            if msg.header().path().map(|p| p.as_str()) != Some(&*rc.path) {
                                continue;
                            }

                            let body = msg.body();
                            let (iface, changed, inval): (
                                &str,
                                HashMap<&str, OwnedValue>,
                                Vec<&str>,
                            ) = body.deserialize()?;

                            if iface != rc.args[0].as_str().unwrap() {
                                continue;
                            }

                            if let Some(value) = changed.get(&*prop) {
                                // match the Get return type
                                let v = Variant::Structure(
                                    zvariant::StructureBuilder::new()
                                        .append_field((**value).try_clone().unwrap())
                                        .build(),
                                )
                                .try_to_owned()
                                .unwrap();
                                *rc.value.borrow_mut() = Some(v);
                            } else if inval.iter().any(|p| p == &prop) {
                                *rc.value.borrow_mut() = None;
                            }
                        }
                    }
                    Ok(())
                });
                rc.watch.set(Some(h));
            }
            None => {}
        }
        Ok(rc)
    }

    fn call_now(self: Rc<Self>) {
        spawn_noerr(async move {
            self.do_call().await;
        });
    }

    pub async fn do_call(self: Rc<Self>) {
        match self.try_call().await {
            Ok(()) => (),
            Err(e) => log::debug!("DBus error: {}", e),
        }
    }

    async fn try_call(self: Rc<Self>) -> zbus::Result<()> {
        use toml::value::Value;
        let dbus = &*self.bus;
        let zbus = dbus.connection().await?;

        let mut api = self.sig.take_in(|s| s.clone());

        if api.is_none() {
            let msg = zbus
                .call_method(
                    Some(&*self.bus_name),
                    &*self.path,
                    Some("org.freedesktop.DBus.Introspectable"),
                    "Introspect",
                    &(),
                )
                .await;
            match msg.as_ref().map(|m| m.body()) {
                Ok(body) => {
                    let xml = body.deserialize()?;
                    let mut reader = xml::EventReader::from_str(xml);
                    let mut sig = String::new();
                    let mut in_iface = false;
                    let mut in_method = false;
                    loop {
                        use xml::reader::XmlEvent;
                        match reader.next() {
                            Ok(XmlEvent::StartElement {
                                name, attributes, ..
                            }) if name.local_name == "interface" => {
                                in_iface = attributes.iter().any(|attr| {
                                    attr.name.local_name == "name" && attr.value == &*self.interface
                                });
                            }
                            Ok(XmlEvent::EndElement { name }) if name.local_name == "interface" => {
                                in_iface = false;
                            }
                            Ok(XmlEvent::StartElement {
                                name, attributes, ..
                            }) if name.local_name == "method" => {
                                in_method = attributes.iter().any(|attr| {
                                    attr.name.local_name == "name" && attr.value == &*self.member
                                });
                            }
                            Ok(XmlEvent::EndElement { name }) if name.local_name == "interface" => {
                                in_method = false;
                            }
                            Ok(XmlEvent::StartElement {
                                name, attributes, ..
                            }) if in_iface
                                && in_method
                                && name.local_name == "arg"
                                && attributes.iter().any(|attr| {
                                    attr.name.local_name == "direction" && attr.value == "in"
                                }) =>
                            {
                                for attr in attributes {
                                    if attr.name.local_name == "type" {
                                        sig.push_str(&attr.value);
                                        sig.push_str(",");
                                    }
                                }
                            }
                            Ok(XmlEvent::EndDocument) => {
                                sig.pop();
                                api = Some(sig.into());
                                self.sig.set(api.clone());
                                break;
                            }
                            Ok(_) => {}
                            Err(e) => {
                                info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                                break;
                            }
                        }
                    }
                }
                Err(e) => {
                    info!("Error introspecting {} {}: {}", self.bus_name, self.path, e);
                }
            }
        }

        let mut args = zvariant::StructureBuilder::new();
        if let Some(sig) = api {
            for (ty, arg) in sig.split(",").zip(self.args.iter()) {
                match match ty {
                    "s" => arg.as_str().map(|s| args.push_field(s)),
                    "d" => arg
                        .as_float()
                        .or_else(|| arg.as_integer().map(|i| i as f64))
                        .map(|f| args.push_field(f)),
                    "y" => arg
                        .as_integer()
                        .map(|i| i as u8)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "n" => arg
                        .as_integer()
                        .map(|i| i as i16)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "q" => arg
                        .as_integer()
                        .map(|i| i as u16)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "i" => arg
                        .as_integer()
                        .map(|i| i as i32)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "u" => arg
                        .as_integer()
                        .map(|i| i as u32)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "x" => arg
                        .as_integer()
                        .map(|i| i as i64)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "t" => arg
                        .as_integer()
                        .map(|i| i as u64)
                        .or_else(|| arg.as_float().map(|f| f as _))
                        .map(|i| args.push_field(i)),
                    "b" => arg
                        .as_bool()
                        .or_else(|| arg.as_integer().map(|i| i != 0))
                        .or_else(|| arg.as_float().map(|i| i != 0.0))
                        .map(|b| args.push_field(b)),
                    "v" => match arg {
                        // best guess as to the type of a variant parameter
                        Value::String(s) => Some(args.push_field(Variant::from(s))),
                        Value::Integer(i) => Some(args.push_field(Variant::from(i))),
                        Value::Float(f) => Some(args.push_field(Variant::from(f))),
                        Value::Boolean(b) => Some(args.push_field(Variant::from(b))),
                        _ => None,
                    },
                    _ => None,
                } {
                    Some(()) => {}
                    None => {
                        info!(
                            "Unsupported or mismatching dbus argument: expected type '{}' got '{}'",
                            ty, arg
                        );
                    }
                }
            }
        } else {
            // no introspection, we just have to guess the argument types
            for arg in self.args.iter() {
                match arg {
                    Value::String(s) => args.push_field(s),
                    Value::Integer(i) => args.push_field(i),
                    Value::Float(f) => args.push_field(f),
                    Value::Boolean(b) => args.push_field(b),
                    _ => {
                        info!("Invalid dbus argument '{}'", arg);
                    }
                }
            }
        }
        let args = args.build();
        let reply = zbus
            .call_method(
                Some(&*self.bus_name),
                &*self.path,
                Some(&*self.interface),
                &*self.member,
                &args,
            )
            .await?;

        *self.value.borrow_mut() =
            Some(Variant::Structure(reply.body().deserialize()?).try_to_owned()?);

        self.interested.notify_data("dbus-read");
        Ok(())
    }

    fn read_variant<'a, F: FnOnce(Value) -> R, R>(
        value: &Variant,
        mut keys: impl Iterator<Item = &'a str>,
        rt: &Runtime,
        f: F,
    ) -> R {
        match value {
            Variant::Bool(b) => f(Value::Bool(*b)),
            Variant::U8(v) => f(Value::Float(*v as _)),
            Variant::I16(v) => f(Value::Float(*v as _)),
            Variant::U16(v) => f(Value::Float(*v as _)),
            Variant::I32(v) => f(Value::Float(*v as _)),
            Variant::U32(v) => f(Value::Float(*v as _)),
            Variant::I64(v) => f(Value::Float(*v as _)),
            Variant::U64(v) => f(Value::Float(*v as _)),
            Variant::F64(v) => f(Value::Float(*v)),
            Variant::Str(s) => f(Value::Borrow(s)),
            Variant::Signature(s) => f(Value::Borrow(s)),
            Variant::ObjectPath(s) => f(Value::Borrow(s)),
            Variant::Value(v) => Self::read_variant(v, keys, rt, f),
            Variant::Array(a) => {
                match keys
                    .next()
                    .unwrap_or("0")
                    .parse::<usize>()
                    .ok()
                    .and_then(|i| a.get(i).ok().flatten())
                {
                    Some(v) => Self::read_variant(v, keys, rt, f),
                    None => f(Value::Null),
                }
            }
            Variant::Dict(d) => {
                let key = match keys.next() {
                    Some(k) => k,
                    None => return f(Value::Null),
                };
                // sig is "a{sv}" or "a{oa...}"
                let sig = d.full_signature().as_bytes();
                let v = match sig.get(2) {
                    Some(b's') => d.get(&key).ok().flatten().map(Variant::try_to_owned),
                    Some(b'o') => d
                        .get(&zvariant::ObjectPath::from_str_unchecked(key))
                        .ok()
                        .flatten()
                        .map(Variant::try_to_owned),
                    Some(b'g') => d
                        .get(&zvariant::Signature::from_str_unchecked(key))
                        .ok()
                        .flatten()
                        .map(Variant::try_to_owned),
                    _ => {
                        info!(
                            "Unsupported dict key in type: '{}'",
                            d.full_signature().as_str()
                        );
                        return f(Value::Null);
                    }
                };
                match v {
                    Some(Ok(v)) => Self::read_variant(&*v, keys, rt, f),
                    _ => f(Value::Null),
                }
            }
            Variant::Structure(s) => {
                let i = keys
                    .next()
                    .unwrap_or("0")
                    .parse::<usize>()
                    .ok()
                    .unwrap_or(0);
                match s.fields().get(i) {
                    Some(v) => Self::read_variant(v, keys, rt, f),
                    None => f(Value::Null),
                }
            }
            Variant::Fd(_) => f(Value::Null),
        }
    }

    pub fn read_in<F: FnOnce(Value) -> R, R>(&self, key: &str, rt: &Runtime, f: F) -> R {
        self.interested.add(rt);
        let value = self.value.borrow();
        match value.as_deref() {
            Some(value) => Self::read_variant(value, key.split("."), rt, f),
            None => f(Value::Null),
        }
    }
}
