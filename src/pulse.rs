use crate::data::{IterationItem, Value};
use crate::pulse_tokio::TokioMain;
use crate::state::NotifierList;
use crate::state::Runtime;
use crate::util::{self, Cell};
use libpulse_binding::callbacks::ListResult;
use libpulse_binding::context::introspect::{
    ClientInfo, Introspector, SinkInfo, SinkInputInfo, SourceInfo, SourceOutputInfo,
};
use libpulse_binding::context::subscribe::{Facility, Operation};
use libpulse_binding::context::{self, Context};
use libpulse_binding::def::DevicePortType;
use libpulse_binding::error::PAErr;
use libpulse_binding::proplist;
use libpulse_binding::volume::{ChannelVolumes, Volume};
use log::{debug, error, info, warn};
use once_cell::unsync::OnceCell;
use std::rc::Rc;

/// An error type that prints an error message when created using ?
///
/// Needed because PAErr doesn't implement std::error::Error
#[derive(Debug)]
pub struct Error;

impl From<PAErr> for Error {
    fn from(e: PAErr) -> Error {
        error!("{}", e);
        Error
    }
}

impl From<&str> for Error {
    fn from(e: &str) -> Error {
        error!("{}", e);
        Error
    }
}

/// Information on one audio port (source or sink)
#[derive(Debug)]
struct PortInfo {
    index: u32,
    name: String,
    desc: String,
    port: String,
    volume: ChannelVolumes,
    mute: bool,
    monitor: bool,
    port_type: Option<DevicePortType>,
}

/// Information on one playback or recording stream
#[derive(Debug)]
struct WireInfo {
    index: u32,
    client: Option<u32>,
    port: u32,
    volume: u32,
    #[allow(unused)] // not currently queried
    mute: bool,
}

/// A singleton structure that collects introspection data from the pulse server
#[derive(Debug, Default)]
struct PulseData {
    context: Cell<Option<Context>>,
    default_sink: Cell<String>,
    default_source: Cell<String>,
    sources: Cell<Vec<PortInfo>>,
    sinks: Cell<Vec<PortInfo>>,
    clients: Cell<Vec<(u32, String)>>,
    sink_ins: Cell<Vec<WireInfo>>,
    src_outs: Cell<Vec<WireInfo>>,
    interested: Cell<NotifierList>,
}

thread_local! {
    static DATA : OnceCell<Rc<PulseData>> = Default::default();
}

impl PulseData {
    fn init() -> Rc<Self> {
        let me = Rc::new(PulseData::default());

        let rv = me.clone();
        util::spawn_noerr(async move {
            match me.mainloop().await {
                Ok(()) | Err(Error) => {}
            }
        });

        rv
    }

    async fn mainloop(self: Rc<Self>) -> Result<(), Error> {
        let mut main = TokioMain::new();

        let mut props = proplist::Proplist::new().ok_or("proplist")?;
        props
            .set_str(proplist::properties::APPLICATION_NAME, "rwaybar")
            .unwrap();
        let mut context = Context::new_with_proplist(&main, "rwaybar", &props).ok_or("context")?;
        context.connect(None, context::FlagSet::NOFAIL, None)?;

        match main.wait_for_ready(&context).await {
            Ok(context::State::Ready) => {}
            Ok(c) => {
                error!("Pulse context {:?}, not continuing", c);
                return Ok(());
            }
            Err(_) => {
                error!("Pulse mainloop exited while waiting on context, not continuing");
                return Ok(());
            }
        }

        // now that the context is ready, subscribe to changes and query the current state
        let data = self.clone();
        let inspect = context.introspect();
        context.set_subscribe_callback(Some(Box::new(move |facility, op, idx| {
            data.subscribe_cb(&inspect, facility, op, idx);
        })));

        context.subscribe(context::subscribe::InterestMaskSet::ALL, |_| ());

        let inspect = context.introspect();

        // hand off the context so it's available to callers
        self.context.set(Some(context));

        let data = self.clone();
        inspect.get_server_info(move |info| {
            if let Some(name) = &info.default_sink_name {
                data.default_sink.set((**name).to_owned());
            }
            if let Some(name) = &info.default_source_name {
                data.default_source.set((**name).to_owned());
            }
        });

        let data = self.clone();
        inspect.get_sink_info_list(move |item| {
            data.add_sink(item);
        });

        let data = self.clone();
        inspect.get_source_info_list(move |item| {
            data.add_source(item);
        });

        let data = self.clone();
        inspect.get_client_info_list(move |item| {
            data.add_client(item);
        });

        let data = self.clone();
        inspect.get_sink_input_info_list(move |item| {
            data.add_sink_input(item);
        });

        let data = self.clone();
        inspect.get_source_output_info_list(move |item| {
            data.add_source_output(item);
        });

        main.run().await;
        error!("Pulse mainloop exited");
        Ok(())
    }

    fn subscribe_cb(
        self: &Rc<Self>,
        inspect: &Introspector,
        facility: Option<Facility>,
        op: Option<Operation>,
        idx: u32,
    ) {
        match (facility, op) {
            (Some(Facility::Sink), Some(Operation::New))
            | (Some(Facility::Sink), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_sink_info_by_index(idx, move |item| {
                    data.add_sink(item);
                });
            }
            (Some(Facility::Sink), Some(Operation::Removed)) => {
                self.sinks.take_in(|sinks| {
                    sinks.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data("pulse");
            }
            (Some(Facility::Source), Some(Operation::New))
            | (Some(Facility::Source), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_source_info_by_index(idx, move |item| {
                    data.add_source(item);
                });
            }
            (Some(Facility::Source), Some(Operation::Removed)) => {
                self.sources.take_in(|sources| {
                    sources.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data("pulse");
            }
            (Some(Facility::Client), Some(Operation::New))
            | (Some(Facility::Client), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_client_info(idx, move |item| {
                    data.add_client(item);
                });
            }
            (Some(Facility::Client), Some(Operation::Removed)) => {
                self.clients.take_in(|clients| {
                    clients.retain(|info| info.0 != idx);
                });
                self.interested.take().notify_data("pulse");
            }
            (Some(Facility::SinkInput), Some(Operation::New))
            | (Some(Facility::SinkInput), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_sink_input_info(idx, move |item| {
                    data.add_sink_input(item);
                });
            }
            (Some(Facility::SinkInput), Some(Operation::Removed)) => {
                self.sink_ins.take_in(|sink_ins| {
                    sink_ins.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data("pulse");
            }
            (Some(Facility::SourceOutput), Some(Operation::New))
            | (Some(Facility::SourceOutput), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_source_output_info(idx, move |item| {
                    data.add_source_output(item);
                });
            }
            (Some(Facility::SourceOutput), Some(Operation::Removed)) => {
                self.src_outs.take_in(|src_outs| {
                    src_outs.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data("pulse");
            }
            _ => {}
        }
    }

    fn add_sink(&self, item: ListResult<&SinkInfo>) {
        self.interested.take().notify_data("pulse");
        match item {
            ListResult::Item(info) => {
                let pi = PortInfo {
                    index: info.index,
                    name: info.name.as_deref().unwrap_or("").to_owned(),
                    desc: info.description.as_deref().unwrap_or("").to_owned(),
                    port: info
                        .active_port
                        .as_ref()
                        .and_then(|port| port.description.as_deref())
                        .unwrap_or("")
                        .to_owned(),
                    volume: info.volume,
                    mute: info.mute,
                    monitor: false,
                    port_type: info.active_port.as_ref().map(|port| port.r#type),
                };
                self.sinks.take_in(|sinks| {
                    for info in &mut *sinks {
                        if info.index == pi.index {
                            *info = pi;
                            return;
                        }
                    }
                    sinks.push(pi);
                });
            }
            ListResult::End => {}
            ListResult::Error => debug!("get_sink_info failed"),
        }
    }

    fn add_source(&self, item: ListResult<&SourceInfo>) {
        self.interested.take().notify_data("pulse");
        match item {
            ListResult::Item(info) => {
                let pi = PortInfo {
                    index: info.index,
                    name: info.name.as_deref().unwrap_or("").to_owned(),
                    desc: info.description.as_deref().unwrap_or("").to_owned(),
                    port: info
                        .active_port
                        .as_ref()
                        .and_then(|port| port.description.as_deref())
                        .unwrap_or("")
                        .to_owned(),
                    volume: info.volume,
                    mute: info.mute,
                    monitor: info.monitor_of_sink.is_some(),
                    port_type: info.active_port.as_ref().map(|port| port.r#type),
                };
                self.sources.take_in(|sources| {
                    for info in &mut *sources {
                        if info.index == pi.index {
                            *info = pi;
                            return;
                        }
                    }
                    sources.push(pi);
                });
            }
            ListResult::End => {}
            ListResult::Error => debug!("get_source_info failed"),
        }
    }

    fn add_client(&self, item: ListResult<&ClientInfo>) {
        self.interested.take().notify_data("pulse");
        match item {
            ListResult::Item(info) => {
                self.clients.take_in(|clients| {
                    let name = info.name.as_deref().unwrap_or("").to_owned();
                    for client in &mut *clients {
                        if client.0 == info.index {
                            client.1 = name;
                            return;
                        }
                    }
                    clients.push((info.index, name));
                });
            }
            ListResult::End => {}
            ListResult::Error => debug!("get_client_info failed"),
        }
    }

    fn add_sink_input(&self, item: ListResult<&SinkInputInfo>) {
        self.interested.take().notify_data("pulse");
        match item {
            ListResult::Item(info) => {
                self.sink_ins.take_in(|sink_ins| {
                    let new = WireInfo {
                        index: info.index,
                        client: info.client,
                        port: info.sink,
                        mute: info.mute,
                        volume: info.volume.avg().0,
                    };
                    for client in &mut *sink_ins {
                        if client.index == info.index {
                            *client = new;
                            return;
                        }
                    }
                    sink_ins.push(new);
                });
            }
            ListResult::End => {}
            ListResult::Error => debug!("get_sink_input failed"),
        }
    }

    fn add_source_output(&self, item: ListResult<&SourceOutputInfo>) {
        self.interested.take().notify_data("pulse");
        match item {
            ListResult::Item(info) => {
                self.src_outs.take_in(|src_outs| {
                    let new = WireInfo {
                        index: info.index,
                        client: info.client,
                        port: info.source,
                        mute: info.mute,
                        volume: info.volume.avg().0,
                    };
                    for client in &mut *src_outs {
                        if client.index == info.index {
                            *client = new;
                            return;
                        }
                    }
                    src_outs.push(new);
                });
            }
            ListResult::End => {}
            ListResult::Error => debug!("get_source_output failed"),
        }
    }

    pub fn with_target<F, R>(&self, target: &str, f: F) -> R
    where
        F: FnOnce(Option<&mut PortInfo>, bool, &[WireInfo], &[(u32, String)]) -> R,
    {
        let name;
        let is_sink;
        let pi_list;
        let wi_list;

        match target {
            "sink" | "speaker" | "sink:default" => {
                name = self.default_sink.take_in(|v| v.clone());
                is_sink = true;
                pi_list = &self.sinks;
                wi_list = &self.sink_ins;
            }
            t if t.starts_with("sink:") => {
                name = t[5..].to_owned();
                is_sink = true;
                pi_list = &self.sinks;
                wi_list = &self.sink_ins;
            }
            "source" | "mic" | "source:default" => {
                name = self.default_source.take_in(|v| v.clone());
                is_sink = false;
                pi_list = &self.sources;
                wi_list = &self.src_outs;
            }
            t if t.starts_with("source:") => {
                name = t[7..].to_owned();
                is_sink = false;
                pi_list = &self.sources;
                wi_list = &self.src_outs;
            }
            _ => {
                error!(
                    "Invalid target specification '{}' - should be source:<name> or sink:<name>",
                    target
                );
                name = String::new();
                is_sink = false;
                pi_list = &self.sources;
                wi_list = &self.src_outs;
            }
        }

        self.clients.take_in(|clients| {
            wi_list.take_in(|wi| {
                pi_list.take_in(|infos| {
                    for info in infos {
                        if info.name == name {
                            return f(Some(info), is_sink, &wi, &clients);
                        }
                    }
                    f(None, is_sink, &[], &clients)
                })
            })
        })
    }
}

pub fn read_focus_list<F: FnMut(bool, IterationItem)>(rt: &Runtime, target: &str, mut f: F) {
    let mut items = Vec::new();
    let (do_src, do_sink, do_mon) = match target {
        "sources" => (true, false, false),
        "monitors" => (false, false, true),
        "all-sources" => (true, false, true),
        "sinks" => (false, true, false),
        "" | "all" => (true, true, true),
        _ => {
            warn!("Invalid target {} for focus-list", target);
            return;
        }
    };

    DATA.with(|cell| {
        let pulse = cell.get_or_init(PulseData::init);
        pulse.interested.take_in(|interest| interest.add(&rt));

        if do_src || do_mon {
            let default_source = pulse.default_source.take_in(|v| v.clone());
            pulse.sources.take_in(|srcs| {
                for item in srcs {
                    let is_def = item.name == default_source;
                    if item.monitor && !do_mon {
                        continue;
                    }
                    if !item.monitor && !do_src {
                        continue;
                    }
                    items.push((is_def, format!("source:{}", item.name).into()));
                }
            });
        }

        if do_sink {
            let default_sink = pulse.default_sink.take_in(|v| v.clone());
            pulse.sinks.take_in(|sinks| {
                for item in sinks {
                    let is_def = item.name == default_sink;
                    items.push((is_def, format!("sink:{}", item.name).into()));
                }
            });
        }
    });

    for (def, target) in items {
        f(def, IterationItem::Pulse { target });
    }
}

pub fn read_in<F: FnOnce(Value) -> R, R>(
    cfg_name: &str,
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
        } else {
            target = "sink";
        }
    }
    DATA.with(|cell| {
        let pulse = cell.get_or_init(PulseData::init);
        pulse.interested.take_in(|interest| interest.add(&rt));

        pulse.with_target(target, |port, _is_sink, wires, clients| {
            let port = match port {
                Some(port) => port,
                None => {
                    return f(Value::Null);
                }
            };
            match key {
                "tooltip" => {
                    let mut v = String::new();
                    let volume = port.volume.avg();
                    v.push_str(volume.print().trim());
                    v.push_str(" - ");
                    v.push_str(&port.port);
                    v.push_str(" on ");
                    v.push_str(&port.desc);
                    v.push_str("\n");
                    let p_id = port.index;

                    for info in wires {
                        if info.port == p_id {
                            let volume = Volume(info.volume);
                            v.push_str(volume.print().trim());
                            v.push_str(" - ");
                            if let Some(cid) = info.client {
                                for client in clients {
                                    if client.0 == cid {
                                        v.push_str(&client.1);
                                        break;
                                    }
                                }
                            } else {
                                v.push_str("(no client)");
                            }
                            v.push_str("\n");
                        }
                    }
                    v.pop();
                    f(Value::Owned(v))
                }
                "text" | "volume" => {
                    if key == "text" && port.mute {
                        f(Value::Borrow("-"))
                    } else {
                        let volume = port.volume.avg();
                        f(Value::Borrow(volume.print().trim()))
                    }
                }
                "type" => {
                    if let Some(ty) = port.port_type {
                        f(Value::Owned(format!("{:?}", ty)))
                    } else {
                        f(Value::Null)
                    }
                }
                "mute" => f(Value::Bool(port.mute)),
                _ => {
                    info!("Unknown key '{}' in '{}'", key, cfg_name);
                    f(Value::Null)
                }
            }
        })
    })
}

pub fn do_write(_name: &str, target: &str, mut key: &str, value: Value, _rt: &Runtime) {
    let mut target = target;
    if target.is_empty() {
        if let Some(pos) = key.rfind('.') {
            target = &key[..pos];
            key = &key[pos + 1..];
        } else {
            target = "sink";
        }
    }
    DATA.with(|cell| {
        let pulse = cell.get_or_init(PulseData::init);

        pulse.context.take_in_some(|ctx| {
            pulse.with_target(target, |port, is_sink, _wires, _clients| {
                let port = match port {
                    Some(port) => port,
                    None => {
                        info!("Ignoring write to unknown item '{}'", target);
                        return;
                    }
                };

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
                        let step = match amt.parse::<f64>() {
                            _ if amt.is_empty() => 1.0,
                            Ok(v) => v,
                            Err(e) => {
                                error!("Cannot parse volume adjustment '{}': {}", value, e);
                                return;
                            }
                        };
                        let value = Volume::NORMAL.0 as f64 * step / 100.0;
                        match dir {
                            Some('+') => {
                                port.volume.increase(Volume(value as u32));
                            }
                            Some('-') => {
                                port.volume.decrease(Volume(value as u32));
                            }
                            _ => {
                                port.volume.scale(Volume(value as u32));
                            }
                        }
                        if is_sink {
                            ctx.introspect().set_sink_volume_by_index(
                                port.index,
                                &port.volume,
                                None,
                            );
                        } else {
                            ctx.introspect().set_source_volume_by_index(
                                port.index,
                                &port.volume,
                                None,
                            );
                        }
                        pulse.interested.take().notify_data("pulse:write-volume");
                    }
                    "mute" => {
                        let old = port.mute;
                        let new = match value.parse_bool() {
                            Some(b) => b,
                            None => match value.as_str_fast() {
                                "toggle" => !old,
                                "on" | "1" => true,
                                "off" | "0" => false,
                                _ => {
                                    error!("Invalid mute request '{}'", value);
                                    return;
                                }
                            },
                        };
                        if old == new {
                            return;
                        }
                        port.mute = new;
                        if is_sink {
                            ctx.introspect()
                                .set_sink_mute_by_index(port.index, new, None);
                        } else {
                            ctx.introspect()
                                .set_source_mute_by_index(port.index, new, None);
                        }
                        pulse.interested.take().notify_data("pulse:write-mute");
                    }
                    _ => {
                        info!("Ignoring write to unknown key '{}'", target);
                    }
                }
            });
        });
    });
}
