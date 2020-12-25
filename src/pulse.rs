use libc::timeval;
use std::os::raw::c_void;
use libpulse_binding::callbacks::ListResult;
use libpulse_binding::context::{self,Context};
use libpulse_binding::context::introspect::{SinkInfo,SourceInfo,ClientInfo,SinkInputInfo,SourceOutputInfo};
use libpulse_binding::def::RetvalActual;
use libpulse_binding::error::PAErr;
use libpulse_binding::mainloop::api::DeferEventCb;
use libpulse_binding::mainloop::api::DeferEventDestroyCb;
use libpulse_binding::mainloop::api::IoEventCb;
use libpulse_binding::mainloop::api::IoEventDestroyCb;
use libpulse_binding::mainloop::api::Mainloop as MainloopTrait;
use libpulse_binding::mainloop::api::MainloopApi;
use libpulse_binding::mainloop::api::{MainloopInnerType,MainloopInternalType};
use libpulse_binding::mainloop::api::TimeEventCb;
use libpulse_binding::mainloop::api::TimeEventDestroyCb;
use libpulse_binding::mainloop::events::deferred::DeferEventInternal;
use libpulse_binding::mainloop::events::io::FlagSet as IoEventFlagSet;
use libpulse_binding::mainloop::events::io::IoEventInternal;
use libpulse_binding::mainloop::events::timer::TimeEventInternal;
use libpulse_binding::volume::Volume;
use libpulse_binding::proplist;
use log::error;
use once_cell::unsync::OnceCell;
use std::rc::Rc;
use std::cell::UnsafeCell;
use std::future::Future;
use std::pin::Pin;
use std::task;
use std::time::{Duration,SystemTime,UNIX_EPOCH};
use tokio::io::unix::AsyncFd;
use crate::util::{self,Fd,Cell};
use crate::state::NotifierList;
use crate::state::Runtime;

#[derive(Debug)]
pub struct Error;

impl From<PAErr> for Error {
    fn from(e : PAErr) -> Error {
        error!("{}", e);
        Error
    }
}

impl From<&str> for Error {
    fn from(e : &str) -> Error {
        error!("{}", e);
        Error
    }
}

#[derive(Debug)]
enum Item {
    Defer {
        dead : bool,
        enabled : bool,
        cb : Option<DeferEventCb>,
        userdata : *mut c_void,
        free : Option<DeferEventDestroyCb>,
    },
    Timer {
        dead : bool,
        ts : Cell<Option<Duration>>,
        cb : Option<TimeEventCb>,
        userdata : *mut c_void,
        free : Option<TimeEventDestroyCb>,
    },
    Event {
        dead : Cell<bool>,
        fd : i32,
        afd : Cell<Option<AsyncFd<Fd>>>,
        cb : Option<IoEventCb>,
        events : Cell<IoEventFlagSet>,
        userdata : *mut c_void,
        free : Option<IoEventDestroyCb>,
    },
}

impl Item {
    fn is_dead(&self) -> bool {
        match self {
            Item::Defer { dead, .. } |
            Item::Timer { dead, .. } => {
                *dead
            }
            Item::Event { dead, .. } => {
                dead.get()
            }
        }
    }

    fn kill(&mut self) {
        match self {
            Item::Defer { dead, .. } |
            Item::Timer { dead, .. } => {
                *dead = true;
            }
            Item::Event { .. } => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct Inner {
    items : Vec<Box<Item>>,
}

struct TokioMain {
    mi : Rc<MainInner>,
}

struct MainInner {
    api : MainloopApi,
    data : UnsafeCell<Inner>,
    sleep : UnsafeCell<Option<tokio::time::Sleep>>,
}

impl MainloopTrait for TokioMain {
    type MI = MainInner;
    fn inner(&self) -> Rc<MainInner> {
        self.mi.clone()
    }
}

impl MainloopInternalType for Inner {
}

impl MainloopInnerType for MainInner {
    type I = Inner;
    fn get_ptr(&self) -> *mut Inner {
        self.data.get()
    }

    fn get_api(&self) -> &MainloopApi {
        &self.api
    }

    fn supports_rtclock(&self) -> bool {
        false
    }
}

impl Drop for MainInner {
    fn drop(&mut self) {
        unsafe {
            // drop the circular reference set up by TokioMain::new
            std::rc::Weak::from_raw(self.api.userdata);
        }
    }
}

impl TokioMain {
    fn new() -> Self {
        let mut mi = Rc::new(MainInner {
            api : MainloopApi {
                userdata : 0 as *mut _,
                io_new : Some(Inner::io_new),
                io_enable : Some(Inner::io_enable),
                io_free : Some(Inner::io_free),
                io_set_destroy : Some(Inner::io_set_destroy),
                time_new : Some(Inner::time_new),
                time_restart : Some(Inner::time_restart),
                time_free : Some(Inner::time_free),
                time_set_destroy : Some(Inner::time_set_destroy),
                defer_new : Some(Inner::defer_new),
                defer_enable : Some(Inner::defer_enable),
                defer_free : Some(Inner::defer_free),
                defer_set_destroy : Some(Inner::defer_set_destroy),
                quit : Some(Inner::quit),
            },
            data : UnsafeCell::new(Inner {
                items : Vec::new(),
            }),
            sleep : UnsafeCell::new(None),
        });
        let v = Rc::get_mut(&mut mi).unwrap();
        v.api.userdata = v.data.get() as *mut _; // reserved below
        Rc::downgrade(&mi).into_raw();
        TokioMain { mi }
    }

    fn iter_get_item(&mut self, i : usize) -> Option<(&MainloopApi, &Item)> {
        loop {
            let api = &self.mi.api;
            let data = self.mi.data.get();
            let items = unsafe { &mut (*data).items };
            if i >= items.len() {
                return None;
            }
            if items[i].is_dead() {
                let mut dead = items.swap_remove(i);
                match &*dead {
                    &Item::Defer { free : Some(cb), userdata, .. } => {
                        let raw_item = &mut *dead as *mut Item;
                        cb(api, raw_item as *mut _, userdata);
                    }
                    &Item::Timer { free : Some(cb), userdata, .. } => {
                        let raw_item = &mut *dead as *mut Item;
                        cb(api, raw_item as *mut _, userdata);
                    }
                    &Item::Event { free : Some(cb), userdata, .. } => {
                        let raw_item = &mut *dead as *mut Item;
                        cb(api, raw_item as *mut _, userdata);
                    }
                    _ => {}
                }
                drop(dead);
                continue;
            }
            return Some((api, &mut *items[i]));
        }
    }

    fn tick(&mut self, ctx : &mut task::Context) -> task::Poll<()> {
        let inow = tokio::time::Instant::now();
        let now  = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let mut wake = None::<Duration>;
        let mut rv = task::Poll::Pending;
        let mut i = 0;
        while let Some((api, item)) = self.iter_get_item(i) {
            let raw_item = item as *const Item;
            i += 1;
            match &*item {
                &Item::Defer { enabled : true, cb : Some(cb), userdata, .. } => {
                    cb(api, raw_item as *mut _, userdata);
                }
                &Item::Defer { .. } => continue,
                &Item::Timer { cb : None, .. } => continue,
                &Item::Timer { cb : Some(cb), userdata, ref ts, .. } => {
                    match ts.replace(None) {
                        Some(ts) if ts < now => {
                            rv = task::Poll::Ready(());
                            let tv = timeval { tv_sec : ts.as_secs() as i64, tv_usec : ts.subsec_micros() as i64 };
                            cb(api, raw_item as *mut _, &tv, userdata);
                        }
                        later => ts.set(later),
                    }

                    if let Some(ts) = ts.get() {
                        if wake.is_some() {
                            wake = std::cmp::min(wake, Some(ts));
                        } else {
                            wake = Some(ts);
                        }
                    }
                }
                &Item::Event { cb : None, .. } => continue,
                &Item::Event { cb : Some(cb), userdata, fd, ref afd, ref events, ref dead, .. } => {
                    afd.take_in(|afd_opt| {
                        let afd = afd_opt.get_or_insert_with(|| AsyncFd::new(Fd(fd)).expect("Pulse fed a bad FD"));
                        let mut ready = IoEventFlagSet::NULL;
                        let mut rg = None;
                        let mut wg = None;
                        if events.get().contains(IoEventFlagSet::INPUT) {
                            match afd.poll_read_ready(ctx) {
                                task::Poll::Ready(Ok(g)) => {
                                    ready |= IoEventFlagSet::INPUT;
                                    rg = Some(g);
                                }
                                task::Poll::Ready(Err(_)) => ready |= IoEventFlagSet::ERROR,
                                task::Poll::Pending => {}
                            }
                        }
                        if events.get().contains(IoEventFlagSet::OUTPUT) {
                            match afd.poll_write_ready(ctx) {
                                task::Poll::Ready(Ok(g)) => {
                                    ready |= IoEventFlagSet::OUTPUT;
                                    wg = Some(g);
                                }
                                task::Poll::Ready(Err(_)) => ready |= IoEventFlagSet::ERROR,
                                task::Poll::Pending => {}
                            }
                        }
                        if ready == IoEventFlagSet::NULL {
                            return;
                        }

                        rv = task::Poll::Ready(());
                        cb(api, raw_item as *mut _, fd, ready, userdata);
                        if dead.get() {
                            // the free function only set the taken afd to None; don't replace
                            // it with the existing one
                            *afd_opt = None;
                            return;
                        }
                        let wants = events.get();
                        if wants.intersects(ready) {
                            // pulse still wants an event that was reported as ready.  We might
                            // need to inform tokio that the FD is not ready
                            let mut pfd = libc::pollfd {
                                fd : fd,
                                events : 0,
                                revents : 0,
                            };
                            if wants.contains(IoEventFlagSet::INPUT) && rg.is_some() {
                                pfd.events |= libc::POLLIN;
                            }
                            if wants.contains(IoEventFlagSet::OUTPUT) && wg.is_some() {
                                pfd.events |= libc::POLLOUT;
                            }
                            unsafe { libc::poll(&mut pfd, 1, 0); }
                            if let Some(mut g) = rg {
                                if (pfd.revents & libc::POLLIN) != 0{
                                    g.retain_ready();
                                } else {
                                    g.clear_ready();
                                }
                            }
                            if let Some(mut g) = wg {
                                if (pfd.revents & libc::POLLOUT) != 0 {
                                    g.retain_ready();
                                } else {
                                    g.clear_ready();
                                }
                            }
                        }
                    });
                }
            }
        }
        if rv.is_pending() {
            let mut sleep = unsafe { Pin::new_unchecked(&mut *self.mi.sleep.get()) };
            if let Some(d) = wake {
                sleep.set(Some(tokio::time::sleep_until(inow + d)));
                match sleep.as_mut().as_pin_mut().map(|f| f.poll(ctx)) {
                    Some(task::Poll::Ready(())) => {
                        sleep.set(None);
                        rv = task::Poll::Ready(());
                    }
                    _ => {}
                }
            } else {
                sleep.set(None);
            }
        }
        rv
    }
}

impl Inner {
    extern "C" fn io_new(a: *const MainloopApi, fd: i32, events: IoEventFlagSet, cb: Option<IoEventCb>, userdata: *mut c_void) -> *mut IoEventInternal {
        unsafe {
            let inner : &mut Inner = &mut *((*a).userdata as *mut _);
            let events = Cell::new(events);
            let mut item = Box::new(Item::Event {
                fd, cb, events, userdata, free : None,
                afd : Cell::new(None), dead : Cell::new(false),
            });
            let rv = &mut *item as *mut Item as *mut _;
            inner.items.push(item);
            rv
        }
    }
    extern "C" fn io_enable(e: *mut IoEventInternal, new: IoEventFlagSet) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Event { events, .. } => {
                    events.set(new);
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn io_free(e: *mut IoEventInternal) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Event { dead, afd, .. } => {
                    dead.set(true);
                    afd.set(None);
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn io_set_destroy(e: *mut IoEventInternal, cb: Option<IoEventDestroyCb>) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Event { free, .. } => {
                    *free = cb;
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn time_new(a: *const MainloopApi, tv: *const timeval, cb: Option<TimeEventCb>, userdata: *mut c_void) -> *mut TimeEventInternal {
        unsafe {
            let inner : &mut Inner = &mut *((*a).userdata as *mut _);
            let tv = tv.read();
            let ts = Cell::new(Some(Duration::from_secs(tv.tv_sec as u64) + Duration::from_micros(tv.tv_usec as u64)));
            let mut item = Box::new(Item::Timer {
                ts, cb, userdata, free : None, dead : false
            });
            let rv = &mut *item as *mut Item as *mut _;
            inner.items.push(item);
            rv
        }
    }
    extern "C" fn time_restart(e: *mut TimeEventInternal, tv: *const timeval) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Timer { ts, .. } => {
                    let tv = tv.read();
                    ts.set(Some(Duration::from_secs(tv.tv_sec as u64) + Duration::from_micros(tv.tv_usec as u64)));
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn time_free(e: *mut TimeEventInternal) {
        unsafe {
            let item : *mut Item = e.cast();
            (*item).kill();
        }
    }
    extern "C" fn time_set_destroy(e: *mut TimeEventInternal, cb: Option<TimeEventDestroyCb>) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Timer { free, .. } => {
                    *free = cb;
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn defer_new(a: *const MainloopApi, cb: Option<DeferEventCb>, userdata: *mut c_void) -> *mut DeferEventInternal {
        unsafe {
            let inner : &mut Inner = &mut *((*a).userdata as *mut _);
            let mut item = Box::new(Item::Defer {
                cb, userdata, free : None, dead : false, enabled : true
            });
            let rv = &mut *item as *mut Item as *mut _;
            inner.items.push(item);
            rv
        }
    }
    extern "C" fn defer_enable(e: *mut DeferEventInternal, b: i32) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Defer { enabled, .. } => *enabled = b != 0,
                _ => panic!()
            }
        }
    }
    extern "C" fn defer_free(e: *mut DeferEventInternal) {
        unsafe {
            let item : *mut Item = e.cast();
            (*item).kill();
        }
    }
    extern "C" fn defer_set_destroy(e: *mut DeferEventInternal, cb: Option<DeferEventDestroyCb>) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Defer { free, .. } => {
                    *free = cb;
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn quit(_: *const MainloopApi, retval: RetvalActual) {
        error!("Got pulsaudio quit: {}", retval);
    }
}

thread_local! {
    static DATA : OnceCell<Rc<PulseData>> = Default::default();
}

/// Information on one audio jack (source or sink)
#[derive(Debug)]
struct JackInfo {
    index : u32,
    name : String,
    desc : String,
    port : String,
    volume : u32,
    mute : bool,
}

#[derive(Debug)]
struct WireInfo {
    index : u32,
    client : Option<u32>,
    jack : u32,
    volume : u32,
    mute : bool,
}

#[derive(Debug,Default)]
struct PulseData {
    default_sink : Cell<String>,
    default_source : Cell<String>,
    sources : Cell<Vec<JackInfo>>,
    sinks : Cell<Vec<JackInfo>>,
    clients : Cell<Vec<(u32, String)>>,
    sink_ins : Cell<Vec<WireInfo>>,
    src_outs : Cell<Vec<WireInfo>>,
    interested : Cell<NotifierList>,
}

impl PulseData {
    fn init() -> Rc<Self> {
        let me = Rc::new(PulseData::default());

        let rv = me.clone();
        util::spawn_noerr(async move {
            let _ = me.mainloop().await;
        });

        rv
    }

    async fn mainloop(self : Rc<Self>) -> Result<(), Error> {
        let mut main = TokioMain::new();

        let mut props = proplist::Proplist::new().ok_or("proplist")?;
        props.set_str(proplist::properties::APPLICATION_NAME, "rwaybar").unwrap();
        let mut context = Context::new_with_proplist(&main, "rwaybar-ctx", &props).ok_or("context")?;
        context.connect(None, context::FlagSet::NOFAIL, None)?;

        loop {
            futures_util::future::poll_fn(|ctx| main.tick(ctx)).await;
            match context.get_state() {
                context::State::Ready => { break; },
                context::State::Failed |
                context::State::Terminated => {
                    error!("Pulse context state failed/terminated, quitting...");
                    return Ok(());
                },
                _ => {},
            }
        }

        let data = self.clone();
        let inspect = context.introspect();
        context.set_subscribe_callback(Some(Box::new(move |facility, op, idx| {
            use libpulse_binding::context::subscribe::Facility;
            use libpulse_binding::context::subscribe::Operation;
            match (facility, op) {
                (Some(Facility::Sink), Some(Operation::New)) |
                (Some(Facility::Sink), Some(Operation::Changed)) => {
                    let data = data.clone();
                    inspect.get_sink_info_by_index(idx, move |item| {
                        data.add_sink(item);
                    });
                }
                (Some(Facility::Sink), Some(Operation::Removed)) => {
                    data.sinks.take_in(|sinks| {
                        sinks.retain(|info| info.index != idx);
                    });
                    data.interested.take().notify_data();
                }
                (Some(Facility::Source), Some(Operation::New)) |
                (Some(Facility::Source), Some(Operation::Changed)) => {
                    let data = data.clone();
                    inspect.get_source_info_by_index(idx, move |item| {
                        data.add_source(item);
                    });
                }
                (Some(Facility::Source), Some(Operation::Removed)) => {
                    data.sources.take_in(|sources| {
                        sources.retain(|info| info.index != idx);
                    });
                    data.interested.take().notify_data();
                }
                (Some(Facility::Client), Some(Operation::New)) |
                (Some(Facility::Client), Some(Operation::Changed)) => {
                    let data = data.clone();
                    inspect.get_client_info(idx, move |item| {
                        data.add_client(item);
                    });
                }
                (Some(Facility::Client), Some(Operation::Removed)) => {
                    data.clients.take_in(|clients| {
                        clients.retain(|info| info.0 != idx);
                    });
                    data.interested.take().notify_data();
                }
                (Some(Facility::SinkInput), Some(Operation::New)) |
                (Some(Facility::SinkInput), Some(Operation::Changed)) => {
                    let data = data.clone();
                    inspect.get_sink_input_info(idx, move |item| {
                        data.add_sink_input(item);
                    });
                }
                (Some(Facility::SinkInput), Some(Operation::Removed)) => {
                    data.sink_ins.take_in(|sink_ins| {
                        sink_ins.retain(|info| info.index != idx);
                    });
                    data.interested.take().notify_data();
                }
                (Some(Facility::SourceOutput), Some(Operation::New)) |
                (Some(Facility::SourceOutput), Some(Operation::Changed)) => {
                    let data = data.clone();
                    inspect.get_source_output_info(idx, move |item| {
                        data.add_source_output(item);
                    });
                }
                (Some(Facility::SourceOutput), Some(Operation::Removed)) => {
                    data.src_outs.take_in(|src_outs| {
                        src_outs.retain(|info| info.index != idx);
                    });
                    data.interested.take().notify_data();
                }
                _ => {
                    dbg!(facility, op, idx);
                }
            }
        })));

        let op = context.subscribe(context::subscribe::InterestMaskSet::ALL, |_| ());
        std::mem::forget(op);
        let inspect = context.introspect();
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

        loop {
            futures_util::future::poll_fn(|ctx| main.tick(ctx)).await;
        }
    }

    fn add_sink(&self, item : ListResult<&SinkInfo>) {
        self.interested.take().notify_data();
        match item {
            ListResult::Item(info) => {
                let pi = JackInfo {
                    index: info.index,
                    name : info.name.as_deref().unwrap_or("").to_owned(),
                    desc : info.description.as_deref().unwrap_or("").to_owned(),
                    port : info.active_port.as_ref().and_then(|port| port.description.as_deref()).unwrap_or("").to_owned(),
                    volume : info.volume.avg().0,
                    mute : info.mute,
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
            ListResult::Error => error!("get_sink_info failed"),
        }
    }

    fn add_source(&self, item : ListResult<&SourceInfo>) {
        self.interested.take().notify_data();
        match item {
            ListResult::Item(info) => {
                let pi = JackInfo {
                    index: info.index,
                    name : info.name.as_deref().unwrap_or("").to_owned(),
                    desc : info.description.as_deref().unwrap_or("").to_owned(),
                    port : info.active_port.as_ref().and_then(|port| port.description.as_deref()).unwrap_or("").to_owned(),
                    volume : info.volume.avg().0,
                    mute : info.mute,
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
            ListResult::Error => error!("get_source_info failed"),
        }
    }

    fn add_client(&self, item : ListResult<&ClientInfo>) {
        self.interested.take().notify_data();
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
            ListResult::Error => error!("get_client_info failed"),
        }
    }

    fn add_sink_input(&self, item : ListResult<&SinkInputInfo>) {
        self.interested.take().notify_data();
        match item {
            ListResult::Item(info) => {
                self.sink_ins.take_in(|sink_ins| {
                    let new = WireInfo {
                        index : info.index,
                        client : info.client,
                        jack : info.sink,
                        mute : info.mute,
                        volume : info.volume.avg().0,
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
            ListResult::Error => error!("get_sink_input failed"),
        }
    }

    fn add_source_output(&self, item : ListResult<&SourceOutputInfo>) {
        self.interested.take().notify_data();
        match item {
            ListResult::Item(info) => {
                self.src_outs.take_in(|src_outs| {
                    let new = WireInfo {
                        index : info.index,
                        client : info.client,
                        jack : info.source,
                        mute : info.mute,
                        volume : info.volume.avg().0,
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
            ListResult::Error => error!("get_source_output failed"),
        }
    }
}

pub fn read_in<F : FnOnce(&str) -> R, R>(_name : &str, target : &str, mut key : &str, rt : &Runtime, f : F) -> R {
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

        let name;
        let ji_list;
        let wi_list;

        match target {
            "sink" | "speaker" | "sink:default" => {
                name = pulse.default_sink.take_in(|v| v.clone());
                ji_list = &pulse.sinks;
                wi_list = &pulse.sink_ins;
            }
            t if t.starts_with("sink:") => {
                name = t[5..].to_owned();
                ji_list = &pulse.sinks;
                wi_list = &pulse.sink_ins;
            }
            "source" | "mic" | "source:default" => {
                name = pulse.default_source.take_in(|v| v.clone());
                ji_list = &pulse.sources;
                wi_list = &pulse.src_outs;
            }
            t if t.starts_with("source:") => {
                name = t[7..].to_owned();
                ji_list = &pulse.sources;
                wi_list = &pulse.src_outs;
            }
            _ => {
                error!("Invalid target specification '{}' - should be source:<name> or sink:<name>", target);
                return f("");
            }
        }

        match key {
            "tooltip" => {
                let mut v = String::new();
                let mut j_id = None;
                let clients = &pulse.clients;
                ji_list.take_in(|infos| {
                    for info in infos {
                        if info.name == name {
                            let volume = Volume(info.volume);
                            v.push_str(volume.print().trim());
                            v.push_str(" - ");
                            v.push_str(&info.port);
                            v.push_str(" on ");
                            v.push_str(&info.desc);
                            v.push_str("\n");
                            j_id = Some(info.index);
                        }
                    }
                });

                wi_list.take_in(|si| {
                    for info in si {
                        if Some(info.jack) == j_id {
                            let volume = Volume(info.volume);
                            v.push_str(" - ");
                            v.push_str(volume.print().trim());
                            v.push_str(" ");
                            if let Some(cid) = info.client {
                                clients.take_in(|clients| {
                                    for client in clients {
                                        if client.0 == cid {
                                            v.push_str(&client.1);
                                            return;
                                        }
                                    }
                                });
                            } else {
                                v.push_str("?");
                            }
                            v.push_str("\n");
                        }
                    }
                });

                v.pop();
                f(&v)
            }
            "text" | "volume" => {
                ji_list.take_in(|infos| {
                    for info in infos {
                        if info.name == name {
                            let volume = Volume(info.volume);
                            return f(volume.print().trim());
                        }
                    }
                    f("")
                })
            }
            "mute" => {
                ji_list.take_in(|infos| {
                    for info in infos {
                        if info.name == name {
                            if info.mute {
                                return f("1")
                            }
                            break;
                        }
                    }
                    f("")
                })
            }
            _ => f(""),
        }
    })
}
