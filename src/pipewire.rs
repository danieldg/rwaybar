#![allow(non_upper_case_globals)]
use crate::{
    data::{IterationItem, Value},
    state::{NotifierList, Runtime},
    util::spawn_noerr,
};
use bytes::{Buf, BufMut, BytesMut};
use log::{debug, error, trace, warn};
use std::{cell::RefCell, collections::HashMap, fmt, io, mem, rc::Rc};
use tokio::{net::UnixStream, sync::Notify};

thread_local! {
    static SOCK: RefCell<Option<Socket>> = RefCell::new(None);
}

#[derive(Debug, Clone, PartialEq)]
enum SPAValue<'a> {
    None,
    Bool(bool),
    Id(u32),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Str(&'a str),
    Bytes(&'a [u8]),
    Struct(Vec<Self>),
    Array(Vec<Self>),
    Object {
        kind: u32,
        id: u32,
        props: Vec<(u32, u32, Self)>,
    },

    Other(u32, &'a [u8]),
}

impl<'a> SPAValue<'a> {
    fn read(buf: &mut &'a [u8]) -> Self {
        let len = buf.get_u32_ne() as usize;
        let ty = buf.get_u32_ne();
        let (msg, rest) = buf.split_at((len + 7) & !7);
        *buf = rest;
        Self::read_ty(ty, &msg[..len])
    }

    fn read_ty(ty: u32, mut buf: &'a [u8]) -> Self {
        let len = buf.len();
        match ty {
            1 => Self::None,
            2 => Self::Bool(buf.get_u32_ne() != 0),
            3 => Self::Id(buf.get_u32_ne()),
            4 => Self::I32(buf.get_i32_ne()),
            5 => Self::I64(buf.get_i64_ne()),
            6 => Self::F32(buf.get_f32_ne()),
            7 => Self::F64(buf.get_f64_ne()),
            8 => std::str::from_utf8(&buf[..len - 1]).map_or_else(|_| Self::Bytes(buf), Self::Str),
            9 => Self::Bytes(buf),
            // rect, fract, bitmap
            13 => {
                let mut v = vec![];
                let clen = buf.get_u32_ne() as usize;
                let cty = buf.get_u32_ne();
                while !buf.is_empty() {
                    v.push(Self::read_ty(cty, &buf[..clen]));
                    buf.advance(clen);
                }
                Self::Array(v)
            }
            14 => {
                let mut v = vec![];
                while buf.remaining() != 0 {
                    v.push(Self::read(&mut buf));
                }
                Self::Struct(v)
            }
            15 => {
                let kind = buf.get_u32_ne();
                let id = buf.get_u32_ne();
                let mut props = vec![];
                while buf.remaining() != 0 {
                    let key = buf.get_u32_ne();
                    let flag = buf.get_u32_ne();
                    let value = Self::read(&mut buf);
                    props.push((key, flag, value));
                }
                Self::Object { kind, id, props }
            }
            // seq, ptr, fd, choice, pod
            _ => Self::Other(ty, buf),
        }
    }

    fn write(&self, buf: &mut Vec<u8>) {
        match self {
            Self::None => {
                buf.put_u32_ne(0);
                buf.put_u32_ne(1);
            }
            Self::Bool(b) => {
                buf.put_u32_ne(4);
                buf.put_u32_ne(2);
                buf.put_u32_ne(*b as u32);
                buf.put_u32_ne(0);
            }
            Self::Id(i) => {
                buf.put_u32_ne(4);
                buf.put_u32_ne(3);
                buf.put_u32_ne(*i);
                buf.put_u32_ne(0);
            }
            Self::I32(i) => {
                buf.put_u32_ne(4);
                buf.put_u32_ne(4);
                buf.put_i32_ne(*i);
                buf.put_u32_ne(0);
            }
            Self::I64(i) => {
                buf.put_u32_ne(8);
                buf.put_u32_ne(5);
                buf.put_i64_ne(*i);
            }
            Self::F32(i) => {
                buf.put_u32_ne(4);
                buf.put_u32_ne(6);
                buf.put_f32_ne(*i);
                buf.put_u32_ne(0);
            }
            Self::F64(i) => {
                buf.put_u32_ne(8);
                buf.put_u32_ne(7);
                buf.put_f64_ne(*i);
            }
            Self::Str(s) => {
                let len = (s.len() + 8) & !7;
                buf.put_u32_ne(s.len() as u32 + 1);
                buf.put_u32_ne(8);
                buf.extend(s.as_bytes());
                for _ in s.len()..len {
                    buf.push(0);
                }
            }
            Self::Struct(s) => {
                let len_off = buf.len();
                buf.put_u32_ne(0);
                buf.put_u32_ne(14);
                let start = buf.len();
                for e in s {
                    e.write(buf);
                }
                let len = buf.len() - start;
                let mut w = &mut buf[len_off..][..4];
                w.put_u32_ne(len as u32);
            }
            Self::Array(a) => {
                let len_off = buf.len();
                buf.put_u32_ne(0);
                buf.put_u32_ne(13);
                let start = buf.len();
                let mut i = a.iter();
                i.next().unwrap().write(buf);
                let item_len = (&mut &buf[start..]).get_u32_ne() as usize;
                buf.truncate(start + 8 + item_len);
                for e in i {
                    let elt = buf.len();
                    e.write(buf);
                    buf.truncate(elt + 8 + item_len);
                    buf.splice(elt..(elt + 8), []);
                }
                let len = buf.len() - start;
                let plen = (len + 7) & !7;
                for _ in len..plen {
                    buf.push(0);
                }
                let mut w = &mut buf[len_off..][..4];
                w.put_u32_ne(len as u32);
            }
            Self::Object { kind, id, props } => {
                let len_off = buf.len();
                buf.put_u32_ne(0);
                buf.put_u32_ne(15);
                let start = buf.len();
                buf.put_u32_ne(*kind);
                buf.put_u32_ne(*id);
                for (key, flag, value) in props {
                    buf.put_u32_ne(*key);
                    buf.put_u32_ne(*flag);
                    value.write(buf);
                }
                let len = buf.len() - start;
                let mut w = &mut buf[len_off..][..4];
                w.put_u32_ne(len as u32);
            }
            _ => todo!("{self:?}"),
        }
    }

    fn into_str_map(&self) -> HashMap<&str, &str> {
        let mut rv = HashMap::new();
        if let SPAValue::Struct(s) = self {
            let mut i = s.iter();
            _ = i.next();
            while let (Some(&SPAValue::Str(k)), Some(&SPAValue::Str(v))) = (i.next(), i.next()) {
                rv.insert(k, v);
            }
        }
        rv
    }

    fn struct_field(&self, id: usize) -> Option<&Self> {
        match self {
            SPAValue::Struct(s) => s.get(id),
            _ => None,
        }
    }

    fn as_props(&self, kind: u32, id: u32) -> Option<&[(u32, u32, Self)]> {
        match self {
            SPAValue::Object {
                kind: k,
                id: i,
                props,
            } if kind == *k && id == *i => Some(props),
            _ => None,
        }
    }
}

#[rustfmt::skip]
mod consts {
    pub const PW_CORE_OID: u32 = 0;
    pub const PW_SELF_OID: u32 = 1;

    pub const PW_CORE_EVT_INFO: u32 = 0;
    pub const PW_CORE_EVT_DONE: u32 = 1;
    pub const PW_CORE_EVT_PING: u32 = 2;
    pub const PW_CORE_EVT_ERROR: u32 = 3;
    pub const PW_CORE_EVT_REMOVE_ID: u32 = 4;
    pub const PW_CORE_EVT_BOUND_ID: u32 = 5;
    pub const PW_CORE_REQ_HELLO: u32 = 1;
    pub const PW_CORE_REQ_SYNC: u32 = 2;
    pub const PW_CORE_REQ_PONG: u32 = 3;
    pub const PW_CORE_REQ_GET_REGISTRY: u32 = 5;

    pub const PW_REGISTRY_EVT_GLOBAL: u32 = 0;
    pub const PW_REGISTRY_EVT_REMOVE: u32 = 1;
    pub const PW_REGISTRY_REQ_BIND: u32 = 1;

    pub const PW_CLIENT_EVT_INFO: u32 = 0;
    pub const PW_CLIENT_REQ_UPDATE_PROPS: u32 = 2;

    pub const PW_DEVICE_EVT_INFO: u32 = 0;
    pub const PW_DEVICE_EVT_PARAM: u32 = 1;
    pub const PW_DEVICE_REQ_SUBSCRIBE: u32 = 1;
    pub const PW_DEVICE_REQ_SET_PARAM: u32 = 3;

    pub const PW_NODE_EVT_INFO: u32 = 0;
    pub const PW_NODE_EVT_PARAM: u32 = 1;
    pub const PW_NODE_REQ_SUBSCRIBE: u32 = 1;

    pub const PW_LINK_EVT_INFO: u32 = 0;

    // <spa/param/param.h>
    pub const SPA_PARAM_Props: u32 = 2;             /* properties as SPA_TYPE_OBJECT_Props */
    pub const SPA_PARAM_Route: u32 = 13;            /* routing configuration as SPA_TYPE_OBJECT_ParamRoute */

    // <spa/utils/type.h>
    pub const SPA_TYPE_OBJECT_Props: u32 = 0x40002;
    pub const SPA_TYPE_OBJECT_ParamRoute: u32 = 0x40009;

    // <spa/param/props.h>
    pub const SPA_PROP_mute: u32 = 0x10004;
    pub const SPA_PROP_channelVolumes: u32 = 0x10008;

    // <spa/param/route.h>
    pub const SPA_PARAM_ROUTE_index: u32 = 1;       /* index of the routing destination (Int) */
    pub const SPA_PARAM_ROUTE_direction: u32 = 2;   /* direction, input/output (Id enum spa_direction) */
    pub const SPA_PARAM_ROUTE_device: u32 = 3;      /* device id (Int) */
    pub const SPA_PARAM_ROUTE_name: u32 = 4;        /* name of the routing destination (String) */
    pub const SPA_PARAM_ROUTE_description: u32 = 5; /* description of the destination (String) */
    //  const SPA_PARAM_ROUTE_priority: u32 = 6;    /* priority of the destination (Int) */
    //  const SPA_PARAM_ROUTE_available: u32 = 7;   /* availability of the destination (Id enum spa_param_availability) */
    //  const SPA_PARAM_ROUTE_info: u32 = 8;        /* info - <String,String> map */
    //  const SPA_PARAM_ROUTE_profiles: u32 = 9;    /* associated profile indexes (Array of Int) */
    pub const SPA_PARAM_ROUTE_props: u32 = 10;      /* properties SPA_TYPE_OBJECT_Props */
    //  const SPA_PARAM_ROUTE_devices: u32 = 11;    /* associated device indexes (Array of Int) */
    //  const SPA_PARAM_ROUTE_profile: u32 = 12;    /* profile id (Int) */
    pub const SPA_PARAM_ROUTE_save: u32 = 13;       /* If route should be saved (Bool) */
}
use consts::*;

#[derive(Debug)]
struct Socket {
    seq: u32,
    wbuf: Vec<u8>,
    write_notify: Rc<Notify>,

    registry: HashMap<i32, RegistryEntry>,
    by_oid: Vec<Proxy>,

    interested: NotifierList,
}

#[derive(Debug)]
struct RegistryEntry {
    // ty: String,
    // version: i32,
    // global_props: HashMap<String, String>,
    bound_oid: i32,
}

#[derive(Debug)]
enum Proxy {
    None,
    Core,
    ClientSelf,
    Registry,
    Device(Device),
    Node(Node),
    Link(Link),
    // We don't (yet) bind to ports, clients, etc
}

#[derive(Debug)]
struct Device {
    /// Global ID (in registry, not temporarlly unique)
    reg_id: i32,

    /// Info: device.name
    name: String,
    /// Info: device.description
    description: String,

    /// Current list of routes.  Will be replaced atomically.
    routes: Vec<RouteEntry>,

    /// Updated route list; will replace routes once the subscribe op is done
    pending_routes: Vec<RouteEntry>,

    /// Some(_) if pending_routes is being populated, Some(true) if we got a route change
    /// notice during that population (and should re-subscribe).
    pending_retry: Option<bool>,
}

#[derive(Debug)]
struct RouteEntry {
    index: i32,
    device: i32,
    name: String,
    desc: String,
    mute: Option<bool>,
    is_input: Option<bool>,
    channels: u8,
    volume: f32,
}

#[derive(Debug)]
struct Node {
    /// Global ID (in registry, not temporarlly unique)
    reg_id: i32,

    /// If Some, this Node corresponds to the Device with this Id
    ///
    /// None for nodes corresponding to clients.
    device_id: Option<i32>,

    /// Name provided by the application itself
    app_name: String,
    /// An ID not provided by the application (PID, flatpak id)
    sec_id: String,

    /// Channel count, required to set volume
    channels: u8,
    volume: f32,
    mute: bool,
}

#[derive(Debug)]
struct Link {
    out_node: i32,
    in_node: i32,
    state: LinkState,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
enum LinkState {
    Inactive,
    Paused,
    Active,
}

impl Socket {
    async fn connect() -> io::Result<UnixStream> {
        let mut err = Err(io::ErrorKind::NotFound.into());
        for dir in ["PIPEWIRE_RUNTIME_DIR", "XDG_RUNTIME_DIR", "USERPROFILE"] {
            let Some(mut path) = std::env::var_os(dir) else {
                continue;
            };
            path.push("/pipewire-0");
            match UnixStream::connect(path).await {
                Ok(stream) => return Ok(stream),
                Err(e) => err = Err(e),
            }
        }
        err
    }

    fn init() -> Self {
        let write_notify = Rc::new(Notify::new());
        let notify = write_notify.clone();

        spawn_noerr(async move {
            let (mut rh, mut wh) = match Self::connect().await {
                Ok(sock) => sock.into_split(),
                Err(e) => {
                    error!("Could not connect to pipewire: {e}");
                    return;
                }
            };

            spawn_noerr(async move {
                let mut wbuf = Vec::new();
                loop {
                    debug_assert!(wbuf.is_empty());
                    SOCK.with(|cell| {
                        let mut borrow = cell.borrow_mut();
                        let sock = borrow.as_mut().unwrap();
                        mem::swap(&mut wbuf, &mut sock.wbuf);
                    });

                    if !wbuf.is_empty() {
                        match tokio::io::AsyncWriteExt::write_all(&mut wh, &wbuf).await {
                            Ok(()) => wbuf.clear(),
                            Err(e) => {
                                error!("Error writing to pipewire: {}", e);
                                return;
                            }
                        }
                    }
                    notify.notified().await;
                }
            });

            let mut rbuf = BytesMut::with_capacity(1 << 15);
            'read: loop {
                match tokio::io::AsyncReadExt::read_buf(&mut rh, &mut rbuf).await {
                    Ok(0) => {
                        error!("End of file when reading from pipewire");
                        return;
                    }
                    Err(e) => {
                        error!("Error reading from pipewire: {}", e);
                        return;
                    }
                    Ok(_) => {}
                }
                loop {
                    if rbuf.len() < 16 {
                        continue 'read;
                    }
                    let mut msg = &rbuf[..];
                    let oid = msg.get_u32_ne();
                    let size_opcode = msg.get_u32_ne();
                    let body_len = (size_opcode & 0xfff_fff) as usize;
                    let opcode = size_opcode >> 24;
                    let seq = msg.get_u32_ne();
                    let _nfds = msg.get_u32_ne();

                    if msg.len() < body_len {
                        rbuf.reserve(body_len - msg.len());
                        continue 'read;
                    }
                    msg = &msg[..body_len];

                    SOCK.with(|cell| {
                        let mut borrow = cell.borrow_mut();
                        let sock = borrow.as_mut().unwrap();
                        sock.read_msg(oid, opcode, seq, msg);
                    });

                    rbuf.advance(16 + body_len);
                }
            }
        });

        let mut this = Self {
            seq: 0,
            wbuf: vec![],
            write_notify,
            by_oid: vec![Proxy::Core, Proxy::ClientSelf, Proxy::Registry],
            registry: HashMap::new(),
            interested: NotifierList::default(),
        };

        this.send(
            PW_CORE_OID,
            PW_CORE_REQ_HELLO,
            &SPAValue::Struct(vec![SPAValue::I32(3)]),
        );

        this.send(
            PW_SELF_OID,
            PW_CLIENT_REQ_UPDATE_PROPS,
            &SPAValue::Struct(vec![SPAValue::Struct(vec![
                SPAValue::I32(1),
                SPAValue::Str("application.name"),
                SPAValue::Str("rwaybar"),
            ])]),
        );

        // Core::GetRegistry(version, new_oid)
        this.send(
            PW_CORE_OID,
            PW_CORE_REQ_GET_REGISTRY,
            &SPAValue::Struct(vec![SPAValue::I32(3), SPAValue::I32(2)]),
        );

        this
    }

    fn send(&mut self, oid: u32, opcode: u32, msg: &SPAValue) {
        let seq = self.seq;
        trace!("->{seq} {oid}.{opcode} {msg:?}");
        self.seq += 1;
        self.wbuf.put_u32_ne(oid);
        let opcode_pos = self.wbuf.len();
        self.wbuf.put_u32_ne(0);
        self.wbuf.put_u32_ne(seq);
        self.wbuf.put_u32_ne(0);
        let start = self.wbuf.len();
        msg.write(&mut self.wbuf);

        let len = self.wbuf.len() - start;
        let mut w = &mut self.wbuf[opcode_pos..];
        w.put_u32_ne(len as u32 | (opcode << 24));

        self.write_notify.notify_one();
    }

    fn read_msg(&mut self, oid: u32, opcode: u32, _seq: u32, mut buf: &[u8]) {
        let msg = SPAValue::read(&mut buf);
        trace!("<-{_seq} {oid}.{opcode} {msg:?}");
        let mut none = Proxy::None;
        let obj = self.by_oid.get_mut(oid as usize).unwrap_or(&mut none);

        match (obj, opcode) {
            (Proxy::Core, PW_CORE_EVT_INFO) => {}
            (Proxy::Core, PW_CORE_EVT_DONE) => {
                // This handles finishing any request whose reply may consist of multiple messages
                // (for now, only used for route subscribe).
                //
                // The pipewire server also sends this spontaneously with done_oid = -1.
                let SPAValue::Struct(s) = msg else {
                    return;
                };
                let Some(&SPAValue::I32(done_oid)) = s.get(0) else {
                    return;
                };
                match self.by_oid.get_mut(done_oid as isize as usize) {
                    Some(Proxy::Device(dev)) => {
                        dev.routes = mem::replace(&mut dev.pending_routes, Vec::new());

                        if dev.pending_retry == Some(true) {
                            dev.pending_retry = Some(false);
                            self.send(
                                done_oid as u32,
                                PW_DEVICE_REQ_SUBSCRIBE,
                                &SPAValue::Struct(vec![SPAValue::Array(vec![SPAValue::Id(
                                    SPA_PARAM_Route,
                                )])]),
                            );
                            self.send(
                                PW_CORE_OID,
                                PW_CORE_REQ_SYNC,
                                &SPAValue::Struct(vec![SPAValue::I32(done_oid), SPAValue::I32(0)]),
                            );
                        } else {
                            self.interested.notify_data("pw-route");
                            dev.pending_retry = None;
                        }
                    }
                    _ => {}
                }
            }
            (Proxy::Core, PW_CORE_EVT_PING) => {
                self.send(0, PW_CORE_REQ_PONG, &msg);
            }
            (Proxy::Core, PW_CORE_EVT_ERROR) => {
                let SPAValue::Struct(s) = msg else {
                    return;
                };
                warn!(
                    "Pipewire error: id={:?}, seq={:?}, res={:?} msg={:?}",
                    s[0], s[1], s[2], s[3]
                );
            }
            (Proxy::Core, PW_CORE_EVT_REMOVE_ID) => debug!("RemoveId: {msg:?}"), // TODO does this actually need any action?
            (Proxy::Core, PW_CORE_EVT_BOUND_ID) => {}
            (Proxy::ClientSelf, PW_CLIENT_EVT_INFO) => {}
            (Proxy::Registry, PW_REGISTRY_EVT_GLOBAL) => {
                let SPAValue::Struct(s) = msg else {
                    return;
                };
                let SPAValue::I32(reg_id) = s[0] else {
                    return;
                };
                let SPAValue::Str(ty) = s[2] else {
                    return;
                };
                let SPAValue::I32(version) = s[3] else {
                    return;
                };
                let global_props = s[4].into_str_map();
                let mut bound_oid = 0;
                if ty == "PipeWire:Interface:Device" {
                    // object.serial factory.id client.id device.api device.description device.name device.nick media.class
                    bound_oid = self.by_oid.len() as _;
                    self.by_oid.push(Proxy::Device(Device {
                        reg_id,
                        name: global_props
                            .get("device.name")
                            .copied()
                            .unwrap_or_default()
                            .to_owned(),
                        description: global_props
                            .get("device.description")
                            .copied()
                            .unwrap_or_default()
                            .to_owned(),
                        routes: Vec::new(),
                        pending_routes: Vec::new(),
                        pending_retry: None,
                    }));
                    self.send(
                        oid,
                        PW_REGISTRY_REQ_BIND,
                        &SPAValue::Struct(vec![
                            SPAValue::I32(reg_id),
                            SPAValue::Str(ty),
                            SPAValue::I32(version.min(3)),
                            SPAValue::I32(bound_oid),
                        ]),
                    );
                } else if ty == "PipeWire:Interface:Node" {
                    // object.serial factory.id client.id client.api application.name node.name media.class
                    bound_oid = self.by_oid.len() as _;
                    self.by_oid.push(Proxy::Node(Node {
                        reg_id,
                        app_name: global_props
                            .get("application.name")
                            .copied()
                            .unwrap_or_default()
                            .to_owned(),
                        sec_id: String::new(),
                        device_id: None,
                        channels: 0,
                        mute: false,
                        volume: f32::NAN,
                    }));
                    self.send(
                        oid,
                        PW_REGISTRY_REQ_BIND,
                        &SPAValue::Struct(vec![
                            SPAValue::I32(reg_id),
                            SPAValue::Str(ty),
                            SPAValue::I32(version.min(3)),
                            SPAValue::I32(bound_oid),
                        ]),
                    );
                } else if ty == "PipeWire:Interface:Link" {
                    // object.serial factory.id client.id link.output.port link.input.port link.output.node link.input.node
                    bound_oid = self.by_oid.len() as _;
                    self.by_oid.push(Proxy::Link(Link {
                        out_node: 0,
                        in_node: 0,
                        state: LinkState::Inactive,
                    }));
                    self.send(
                        oid,
                        PW_REGISTRY_REQ_BIND,
                        &SPAValue::Struct(vec![
                            SPAValue::I32(reg_id),
                            SPAValue::Str(ty),
                            SPAValue::I32(version.min(3)),
                            SPAValue::I32(bound_oid),
                        ]),
                    );
                }
                self.registry.insert(reg_id, RegistryEntry { bound_oid });
                // Wait for the Info event to call notify-data
            }
            (Proxy::Registry, PW_REGISTRY_EVT_REMOVE) => {
                let Some(SPAValue::I32(reg_id)) = msg.struct_field(0) else {
                    return;
                };
                if let Some(re) = self.registry.remove(&reg_id) {
                    if re.bound_oid != 0 {
                        self.interested.notify_data("pw-removal");
                        self.by_oid[re.bound_oid as usize] = Proxy::None;
                        // No need for self.send(0, 7, &SPAValue::Struct(vec![SPAValue::I32(re.bound_oid)]));
                    }
                }
            }
            (Proxy::Device(dev), PW_DEVICE_EVT_INFO) => {
                let SPAValue::Struct(s) = msg else {
                    return;
                };
                let SPAValue::I64(mask) = s[1] else {
                    return;
                };
                if mask & 1 != 0 {
                    let props = s[2].into_str_map();
                    dev.name = props.get("device.name").copied().unwrap_or_default().into();
                    dev.description = props
                        .get("device.description")
                        .copied()
                        .unwrap_or_default()
                        .into();
                }
                if let (2, Some(SPAValue::Struct(params))) = (mask & 2, s.get(3)) {
                    if params.contains(&SPAValue::Id(SPA_PARAM_Route)) {
                        if dev.pending_retry.is_none() {
                            dev.pending_retry = Some(false);
                            self.send(
                                oid,
                                PW_DEVICE_REQ_SUBSCRIBE,
                                &SPAValue::Struct(vec![SPAValue::Array(vec![SPAValue::Id(
                                    SPA_PARAM_Route,
                                )])]),
                            );
                            self.send(
                                PW_CORE_OID,
                                PW_CORE_REQ_SYNC,
                                &SPAValue::Struct(vec![
                                    SPAValue::I32(oid as i32),
                                    SPAValue::I32(0),
                                ]),
                            );
                        } else {
                            dev.pending_retry = Some(true);
                        }
                    }
                }
            }
            (Proxy::Device(dev), PW_DEVICE_EVT_PARAM) => {
                if let Some(props) = msg
                    .struct_field(4)
                    .and_then(|o| o.as_props(SPA_TYPE_OBJECT_ParamRoute, SPA_PARAM_Route))
                {
                    let mut index = None;
                    let mut device = None;
                    for (key, _flag, value) in props {
                        match (*key, value) {
                            (SPA_PARAM_ROUTE_index, SPAValue::I32(i)) => index = Some(*i),
                            (SPA_PARAM_ROUTE_device, SPAValue::I32(i)) => device = Some(*i),
                            _ => {}
                        }
                    }
                    let (Some(index), Some(device)) = (index, device) else {
                        warn!("Got SPA_TYPE_OBJECT_ParamRoute without index or device: {props:?}");
                        return;
                    };

                    let i = dev.pending_routes.len();
                    dev.pending_routes.push(RouteEntry {
                        index,
                        device,
                        name: String::new(),
                        desc: String::new(),
                        mute: None,
                        is_input: None,
                        volume: f32::NAN,
                        channels: 0,
                    });
                    let route = &mut dev.pending_routes[i];

                    for (key, _flag, value) in props {
                        match (*key, value) {
                            (SPA_PARAM_ROUTE_name, &SPAValue::Str(s)) => {
                                route.name = s.into();
                            }
                            (SPA_PARAM_ROUTE_description, &SPAValue::Str(s)) => {
                                route.desc = s.into();
                            }
                            (SPA_PARAM_ROUTE_direction, &SPAValue::Id(d)) => {
                                route.is_input = Some(d == 0);
                            }
                            (SPA_PARAM_ROUTE_props, SPAValue::Object { props, .. }) => {
                                for (key, _flag, value) in props {
                                    match (*key, value) {
                                        (SPA_PROP_mute, SPAValue::Bool(b)) => {
                                            route.mute = Some(*b);
                                        }
                                        (SPA_PROP_channelVolumes, SPAValue::Array(a)) => {
                                            let mut v = 0.0;
                                            for e in a {
                                                match e {
                                                    SPAValue::F32(x) => v += *x,
                                                    _ => unreachable!(),
                                                }
                                            }
                                            route.channels = a.len() as u8;
                                            route.volume = (v / a.len() as f32).cbrt() * 100.0;
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                } else {
                    // we should only get objects we subscribed to
                    debug!("Unexpected device param {msg:?}");
                }
            }
            (Proxy::Node(node), PW_NODE_EVT_INFO) => {
                // Info
                let SPAValue::Struct(s) = msg else {
                    return;
                };
                let SPAValue::I64(mask) = s[3] else {
                    return;
                };
                if mask & 8 != 0 {
                    let props = s[8].into_str_map();
                    node.sec_id = props
                        .get("pipewire.access.portal.app_id")
                        .or_else(|| props.get("pipewire.sec.pid"))
                        .or_else(|| props.get("application.process.id"))
                        .copied()
                        .unwrap_or("")
                        .into();

                    node.device_id = props.get("device.id").and_then(|&s| s.parse().ok());
                }
                if mask & 16 != 0 {
                    let SPAValue::Struct(params) = &s[9] else {
                        return;
                    };

                    if params.contains(&SPAValue::Id(SPA_PARAM_Props)) {
                        self.send(
                            oid,
                            PW_NODE_REQ_SUBSCRIBE,
                            &SPAValue::Struct(vec![SPAValue::Array(vec![SPAValue::Id(
                                SPA_PARAM_Props,
                            )])]),
                        );
                    }
                }
            }
            (Proxy::Node(node), PW_NODE_EVT_PARAM) => {
                if let Some(props) = msg
                    .struct_field(4)
                    .and_then(|o| o.as_props(SPA_TYPE_OBJECT_Props, SPA_PARAM_Props))
                {
                    // Note: resetting the values to 'unknown' here is more correct
                    for (key, _, value) in props {
                        match (*key, value) {
                            (SPA_PROP_mute, SPAValue::Bool(b)) => node.mute = *b,
                            (SPA_PROP_channelVolumes, SPAValue::Array(a)) => {
                                let mut v = 0.0;
                                for e in a {
                                    match e {
                                        SPAValue::F32(x) => v += *x,
                                        _ => unreachable!(),
                                    }
                                }
                                node.channels = a.len() as _;
                                node.volume = (v / a.len() as f32).cbrt() * 100.0;
                            }

                            _ => {}
                        }
                    }
                    self.interested.notify_data("pw-node");
                } else {
                    // we should only get objects we subscribed to
                    debug!("Unexpected node param {msg:?}");
                }
            }
            (Proxy::Link(link), PW_LINK_EVT_INFO) => {
                let SPAValue::Struct(s) = msg else {
                    return;
                };
                if let Some(SPAValue::I32(i)) = s.get(1) {
                    link.out_node = *i;
                }
                if let Some(SPAValue::I32(i)) = s.get(3) {
                    link.in_node = *i;
                }
                let Some(SPAValue::I64(mask)) = s.get(5) else {
                    return;
                };
                if mask & 1 != 0 {
                    link.state = match s.get(6) {
                        Some(SPAValue::I32(3)) => LinkState::Paused,
                        Some(SPAValue::I32(4)) => LinkState::Active,
                        _ => LinkState::Inactive,
                    };
                }
                self.interested.notify_data("pw-link");
            }
            _ => {
                warn!("Unknown message: {oid}:{opcode} {msg:?}");
            }
        }
    }

    fn nodes(&self) -> impl Iterator<Item = &Node> {
        self.by_oid.iter().filter_map(|p| match p {
            Proxy::Node(node) => Some(node),
            _ => None,
        })
    }

    fn links(&self) -> impl Iterator<Item = &Link> {
        self.by_oid.iter().filter_map(|p| match p {
            Proxy::Link(link) => Some(link),
            _ => None,
        })
    }

    fn oid_devices(&self) -> impl Iterator<Item = (usize, &Device)> {
        self.by_oid
            .iter()
            .enumerate()
            .filter_map(|(oid, p)| match p {
                Proxy::Device(dev) => Some((oid, dev)),
                _ => None,
            })
    }

    fn for_each_dev_link(&self, dev_id: i32, mut f: impl FnMut(i32, LinkState, bool)) {
        for node in self.nodes() {
            if node.device_id != Some(dev_id) {
                continue;
            }

            for link in self.links() {
                if node.reg_id == link.in_node {
                    f(link.out_node, link.state, true);
                }
                if node.reg_id == link.out_node {
                    f(link.in_node, link.state, false);
                }
            }
        }
    }

    fn find_device(&self, target: &str) -> Option<(usize, usize)> {
        if target.starts_with("oid-route:") {
            let mut segs = target.split(':').skip(1);
            let oid: usize = segs.next()?.parse().ok()?;
            let rid: usize = segs.next()?.parse().ok()?;
            if let Some(Proxy::Device(dev)) = self.by_oid.get(oid) {
                if rid < dev.routes.len() {
                    return Some((oid, rid));
                }
            }
            return None;
        }
        let mut rv = None;
        let mut rv_state = LinkState::Inactive;
        for (oid, dev) in self.oid_devices() {
            if dev.routes.is_empty() {
                continue;
            }
            if dev.name == target {
                return Some((oid, 0));
            }
            if target.starts_with(&dev.name) && target[dev.name.len()..].starts_with(" ") {
                let rest = &target[dev.name.len() + 1..];
                for (rid, route) in dev.routes.iter().enumerate() {
                    if route.name == rest {
                        return Some((oid, rid));
                    }
                }
                return None;
            }

            let mut dev_state = LinkState::Inactive;
            self.for_each_dev_link(dev.reg_id, |_, state, _| {
                dev_state = dev_state.max(state);
            });

            if dev_state >= rv_state {
                rv = Some((oid, 0));
                rv_state = dev_state;
            }
        }
        rv
    }
}

impl RouteEntry {
    /// Create a message suitable for the Device::SetParam request that sets the specified property
    /// of this route to the specified value.
    fn set_prop_msg<'a>(&self, prop: u32, value: SPAValue<'a>) -> SPAValue<'a> {
        SPAValue::Struct(vec![
            SPAValue::Id(SPA_PARAM_Route),
            SPAValue::I32(0), // flags
            SPAValue::Object {
                kind: SPA_TYPE_OBJECT_ParamRoute,
                id: SPA_PARAM_Route,
                props: vec![
                    (SPA_PARAM_ROUTE_index, 0, SPAValue::I32(self.index)),
                    (SPA_PARAM_ROUTE_device, 0, SPAValue::I32(self.device)),
                    (
                        SPA_PARAM_ROUTE_props,
                        0,
                        SPAValue::Object {
                            kind: SPA_TYPE_OBJECT_Props,
                            id: SPA_PARAM_Route,
                            props: vec![
                                (prop, 0, value),
                                (SPA_PARAM_ROUTE_save, 0, SPAValue::Bool(true)),
                            ],
                        },
                    ),
                ],
            },
        ])
    }
}

pub fn read_focus_list<F: FnMut(bool, IterationItem)>(rt: &Runtime, target: &str, mut f: F) {
    let mut items = Vec::new();
    let (do_src, do_sink) = match target {
        "inputs" | "sources" => (true, false),
        "outputs" | "sinks" => (false, true),
        "" | "all" => (true, true),
        _ => {
            warn!("Invalid target {} for focus-list", target);
            return;
        }
    };
    SOCK.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let sock = borrow.get_or_insert_with(Socket::init);

        sock.interested.add(&rt);

        for (oid, dev) in sock.oid_devices() {
            let mut dev_state = LinkState::Inactive;
            if !dev.routes.iter().any(|route| match route.is_input {
                Some(true) if do_src => true,
                Some(false) if do_sink => true,
                _ => false,
            }) {
                continue;
            }

            sock.for_each_dev_link(dev.reg_id, |_, state, _| {
                dev_state = dev_state.max(state);
            });

            for (rid, route) in dev.routes.iter().enumerate() {
                match route.is_input {
                    Some(true) if do_src => {}
                    Some(false) if do_sink => {}
                    _ => continue,
                }
                items.push((
                    dev_state == LinkState::Active,
                    format!("oid-route:{oid}:{rid}").into(),
                ));
            }
        }
    });

    for (def, target) in items {
        f(def, IterationItem::Pipewire { target });
    }
}

pub fn read_in<F: FnOnce(Value) -> R, R>(
    _cfg_name: &str,
    target: &str,
    mut key: &str,
    rt: &Runtime,
    f: F,
) -> R {
    let mut target = target;
    if target.is_empty() {
        if let Some(pos) = key.rfind('.') {
            target = &key[..pos];
            key = &key[pos + 1..];
        }
    }

    use fmt::Write;
    SOCK.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let sock = borrow.get_or_insert_with(Socket::init);

        sock.interested.add(&rt);

        let Some((oid, rid)) = sock.find_device(target) else {
            return f(Value::Null);
        };

        let Proxy::Device(dev) = &sock.by_oid[oid] else {
            return f(Value::Null);
        };
        let route = &dev.routes[rid];

        match key {
            "tooltip" => {
                let mut v = String::new();
                let m = match route.mute {
                    Some(true) => "✕",
                    Some(false) => "",
                    None => "",
                };
                _ = write!(
                    v,
                    "{m}{:.0}% - {} on {}\n",
                    route.volume, route.desc, dev.description,
                );

                let mut links = HashMap::new();
                sock.for_each_dev_link(dev.reg_id, |node_id, state, dir| {
                    let e = links.entry(node_id).or_insert((false, false, state));
                    e.2 = e.2.max(state);
                    if dir {
                        e.0 = true;
                    } else {
                        e.1 = true;
                    }
                });

                for node in sock.nodes() {
                    let dir = match links.get(&node.reg_id) {
                        Some((true, false, LinkState::Active)) => '⇐',
                        Some((true, false, LinkState::Paused)) => '←',
                        Some((true, false, LinkState::Inactive)) => '⇠',
                        Some((false, true, LinkState::Active)) => '⇒',
                        Some((false, true, LinkState::Paused)) => '→',
                        Some((false, true, LinkState::Inactive)) => '⇢',
                        Some((true, true, LinkState::Active)) => '⇔',
                        Some((true, true, LinkState::Paused)) => '↔',
                        Some((true, true, LinkState::Inactive)) => '⇹',
                        _ => continue,
                    };
                    if node.volume.is_nan() {
                        continue;
                    }
                    if node.mute {
                        _ = write!(v, "{dir} ✕");
                    } else {
                        _ = write!(v, "{dir} {:.0}%", node.volume);
                    }
                    _ = write!(v, " - {} ({})\n", node.app_name, node.sec_id);
                }
                v.pop();
                f(Value::Owned(v))
            }
            "active" => {
                let mut active = false;
                sock.for_each_dev_link(dev.reg_id, |_, state, _| {
                    active |= state == LinkState::Active;
                });

                f(Value::Bool(active))
            }
            "" | "text" => f(Value::Owned(format!("{:.0}%", route.volume))),
            "device" => f(Value::Borrow(&dev.description)),
            "mute" => f(route.mute.map_or(Value::Null, Value::Bool)),
            "name" => f(Value::Owned(format!("{} {}", dev.name, route.name))),
            "route" => f(Value::Borrow(&route.desc)),
            "volume" => f(Value::Float(route.volume as f64)),
            _ => f(Value::Null),
        }
    })
}

pub fn do_write(_name: &str, target: &str, mut key: &str, value: Value, _rt: &Runtime) {
    let mut target = target;
    if target.is_empty() {
        if let Some(pos) = key.rfind('.') {
            target = &key[..pos];
            key = &key[pos + 1..];
        }
    }

    // Volume changes match the command-line tool:
    // pw-cli s 41 Route '{"index":0, "device":4, "props":{ "channelVolumes":[0.1,0.1] } }'

    SOCK.with(|cell| {
        let mut borrow = cell.borrow_mut();
        let Some(sock) = &mut *borrow else {
            warn!("Ignoring write to uninitialized pipewire system");
            return;
        };

        let Some((oid, rid)) = sock.find_device(target) else {
            warn!("Could not find pipewire device '{target}', ignoring");
            return;
        };

        let Proxy::Device(dev) = &sock.by_oid[oid] else {
            return;
        };
        let route = &dev.routes[rid];

        match key {
            "volume" => {
                let value = value.into_text();
                let mut amt = &value[..];
                let dir = amt.chars().next();
                if matches!(dir, Some('+') | Some('-')) {
                    amt = &amt[1..];
                }
                if amt.ends_with('%') {
                    amt = &amt[..amt.len() - 1];
                }
                let step = match amt.parse::<f32>() {
                    _ if amt.is_empty() => 1.0,
                    Ok(v) => v,
                    Err(e) => {
                        error!("Cannot parse volume adjustment '{}': {}", value, e);
                        return;
                    }
                };
                let volume = match dir {
                    Some('+') => route.volume + step,
                    Some('-') => route.volume - step,
                    _ => step,
                };
                let volume = (volume / 100.0).powf(3.0).max(0.0);
                sock.send(
                    oid as _,
                    PW_DEVICE_REQ_SET_PARAM,
                    &route.set_prop_msg(
                        SPA_PROP_channelVolumes,
                        SPAValue::Array(vec![SPAValue::F32(volume); route.channels as usize]),
                    ),
                );
            }
            "mute" => {
                let new = match value.parse_bool() {
                    Some(b) => b,
                    None => match value.as_str_fast() {
                        "toggle" => !route.mute.unwrap_or_default(),
                        "on" | "1" => true,
                        "off" | "0" => false,
                        _ => {
                            error!("Invalid mute request '{value}'");
                            return;
                        }
                    },
                };

                sock.send(
                    oid as _,
                    PW_DEVICE_REQ_SET_PARAM,
                    &route.set_prop_msg(SPA_PROP_mute, SPAValue::Bool(new)),
                );
            }
            _ => {
                warn!("Ignoring write to unknown key '{key}'");
            }
        }
    });
}
