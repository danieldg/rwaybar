use crate::state::NotifierList;
use crate::state::Runtime;
use crate::util::{self,Fd,Cell};
use libc::timeval;
use libpulse_binding::callbacks::ListResult;
use libpulse_binding::context::introspect::{Introspector,SinkInfo,SourceInfo,ClientInfo,SinkInputInfo,SourceOutputInfo};
use libpulse_binding::context::{self,Context};
use libpulse_binding::context::subscribe::Facility;
use libpulse_binding::context::subscribe::Operation;
use libpulse_binding::def::RetvalActual;
use libpulse_binding::error::PAErr;
use libpulse_binding::mainloop::api::DeferEventCb;
use libpulse_binding::mainloop::api::DeferEventDestroyCb;
use libpulse_binding::mainloop::api::IoEventCb;
use libpulse_binding::mainloop::api::IoEventDestroyCb;
use libpulse_binding::mainloop::api::Mainloop as MainloopTrait;
use libpulse_binding::mainloop::api::MainloopApi;
use libpulse_binding::mainloop::api::TimeEventCb;
use libpulse_binding::mainloop::api::TimeEventDestroyCb;
use libpulse_binding::mainloop::api::{MainloopInnerType,MainloopInternalType};
use libpulse_binding::mainloop::events::deferred::DeferEventInternal;
use libpulse_binding::mainloop::events::io::FlagSet as IoEventFlagSet;
use libpulse_binding::mainloop::events::io::IoEventInternal;
use libpulse_binding::mainloop::events::timer::TimeEventInternal;
use libpulse_binding::proplist;
use libpulse_binding::volume::{ChannelVolumes,Volume};
use log::{debug,info,error};
use once_cell::unsync::OnceCell;
use std::cell::UnsafeCell;
use std::future::Future;
use std::os::raw::c_void;
use std::pin::Pin;
use std::rc::{Rc,Weak};
use std::task;
use std::time::{Duration,SystemTime,UNIX_EPOCH};
use tokio::io::unix::AsyncFd;

/// An error type that prints an error message when created using ?
///
/// Needed because PAErr doesn't implement std::error::Error
#[derive(Debug)]
struct Error;

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

enum Item {
    Defer {
        main : Weak<MainInner>,
        dead : bool,
        enabled : bool,
        cb : Option<DeferEventCb>,
        userdata : *mut c_void,
        free : Option<DeferEventDestroyCb>,
    },
    Timer {
        main : Weak<MainInner>,
        dead : bool,
        ts : Cell<Option<Duration>>,
        cb : Option<TimeEventCb>,
        userdata : *mut c_void,
        free : Option<TimeEventDestroyCb>,
    },
    Event {
        main : Weak<MainInner>,
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

/// An implementation of the [pulse](libpulse_binding) [Mainloop](MainloopTrait) trait that
/// dispatches through tokio.
pub struct TokioMain {
    mi : Rc<MainInner>,
}

/// The state structure passed to pulse.
///
/// Note: this structure is pinned in memory because it has a self-reference (api.userdata).
pub struct MainInner {
    api : MainloopApi,
    /// Note: items are stored as raw pointers because the actual items are also available to C and
    /// via iter_get_item.  Otherwise, they are Box pointers owned by this vector.
    items : Cell<Vec<*mut Item>>,
    /// Note: only allow access following the rules for Pin
    sleep : UnsafeCell<Option<tokio::time::Sleep>>,
    waker : Cell<Option<task::Waker>>,
}

impl MainloopTrait for TokioMain {
    type MI = MainInner;
    fn inner(&self) -> Rc<MainInner> {
        self.mi.clone()
    }
}

impl MainloopInternalType for MainInner {
}

impl MainloopInnerType for MainInner {
    type I = Self;
    fn get_ptr(&self) -> *mut Self {
        panic!("This function is not well-defined and is never called")
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
            Weak::from_raw(self.api.userdata);
            // drop any remaining items (they should all be dead anyway)
            for item in self.items.replace(Vec::new()) {
                Box::from_raw(item);
            }
        }
    }
}

impl TokioMain {
    pub fn new() -> Self {
        let mut mi = Rc::new(MainInner {
            api : MainloopApi {
                userdata : 0 as *mut _,
                io_new : Some(MainInner::io_new),
                io_enable : Some(MainInner::io_enable),
                io_free : Some(MainInner::io_free),
                io_set_destroy : Some(MainInner::io_set_destroy),
                time_new : Some(MainInner::time_new),
                time_restart : Some(MainInner::time_restart),
                time_free : Some(MainInner::time_free),
                time_set_destroy : Some(MainInner::time_set_destroy),
                defer_new : Some(MainInner::defer_new),
                defer_enable : Some(MainInner::defer_enable),
                defer_free : Some(MainInner::defer_free),
                defer_set_destroy : Some(MainInner::defer_set_destroy),
                quit : Some(MainInner::quit),
            },
            items : Cell::new(Vec::new()),
            sleep : UnsafeCell::new(None),
            waker : Cell::new(None),
        });
        let v = Rc::get_mut(&mut mi).unwrap();
        v.api.userdata = v as *mut MainInner as *mut _;
        Rc::downgrade(&mi).into_raw();
        TokioMain { mi }
    }

    fn iter_get_item(&mut self, i : usize) -> Option<(&MainloopApi, &Item)> {
        let api = &self.mi.api;
        self.mi.items.take_in(|items| {
            loop {
                if i >= items.len() {
                    return None;
                }
                if unsafe { (*items[i]).is_dead() } {
                    let mut dead = unsafe { Box::from_raw(items.swap_remove(i)) };
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
                let item = unsafe { &*items[i] };
                return Some((api, item));
            }
        })
    }

    pub fn tick(&mut self, ctx : &mut task::Context) -> task::Poll<()> {
        let inow = tokio::time::Instant::now();
        let now  = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let mut wake = None::<Duration>;
        let mut rv = task::Poll::Pending;
        let mut i = 0;
        self.mi.waker.set(Some(ctx.waker().clone()));
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
            // Note: we can't pin the Rc normally because the Mainloop trait inner() returns an
            // unpinned Rc.  However, the value is in fact pinned.
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

impl MainInner {
    unsafe fn from_api(api : *const MainloopApi) -> Rc<Self> {
        let ptr = Weak::from_raw((*api).userdata as *const Self);
        let rv = ptr.upgrade();
        ptr.into_raw(); // we only want to borrow the Weak, not own it...
        rv.expect("Called from_api on a dropped MainloopApi")
    }

    fn push(&self, item : Box<Item>) {
        self.items.take_in(|items| {
            items.push(Box::into_raw(item));
        });
    }

    fn wake(main : &Weak<MainInner>) {
        main.upgrade()
            .and_then(|inner| inner.waker.replace(None))
            .map(|waker| waker.wake());
    }

    extern "C" fn io_new(a: *const MainloopApi, fd: i32, events: IoEventFlagSet, cb: Option<IoEventCb>, userdata: *mut c_void) -> *mut IoEventInternal {
        unsafe {
            let inner = MainInner::from_api(a);
            let events = Cell::new(events);
            let mut item = Box::new(Item::Event {
                fd, cb, events, userdata, free : None,
                afd : Cell::new(None), dead : Cell::new(false),
                main : Rc::downgrade(&inner),
            });
            let rv = &mut *item as *mut Item as *mut _;
            inner.push(item);
            rv
        }
    }
    extern "C" fn io_enable(e: *mut IoEventInternal, new: IoEventFlagSet) {
        unsafe {
            let item : *mut Item = e.cast();
            match &*item {
                Item::Event { main, events, .. } => {
                    events.set(new);
                    MainInner::wake(main);
                }
                _ => panic!()
            }
        }
    }
    extern "C" fn io_free(e: *mut IoEventInternal) {
        unsafe {
            let item : *mut Item = e.cast();
            match &*item {
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
            let inner = MainInner::from_api(a);
            let tv = tv.read();
            let ts = Cell::new(Some(Duration::from_secs(tv.tv_sec as u64) + Duration::from_micros(tv.tv_usec as u64)));
            let mut item = Box::new(Item::Timer {
                main : Rc::downgrade(&inner), ts, cb, userdata, free : None, dead : false,
            });
            let rv = &mut *item as *mut Item as *mut _;
            inner.push(item);
            rv
        }
    }
    extern "C" fn time_restart(e: *mut TimeEventInternal, tv: *const timeval) {
        unsafe {
            let item : *mut Item = e.cast();
            match &*item {
                Item::Timer { main, ts, .. } => {
                    let tv = tv.read();
                    ts.set(Some(Duration::from_secs(tv.tv_sec as u64) + Duration::from_micros(tv.tv_usec as u64)));
                    MainInner::wake(main);
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
            let inner = MainInner::from_api(a);
            let mut item = Box::new(Item::Defer {
                main : Rc::downgrade(&inner), cb, userdata, free : None, dead : false, enabled : true
            });
            let rv = &mut *item as *mut Item as *mut _;
            inner.push(item);
            rv
        }
    }
    extern "C" fn defer_enable(e: *mut DeferEventInternal, b: i32) {
        unsafe {
            let item : *mut Item = e.cast();
            match &mut *item {
                Item::Defer { main, enabled, .. } => {
                    *enabled = b != 0;
                    if b != 0 {
                        MainInner::wake(main);
                    }
                }
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
        error!("Got pulseaudio quit request: {}", retval);
    }
}

/// Information on one audio port (source or sink)
#[derive(Debug)]
struct PortInfo {
    index : u32,
    name : String,
    desc : String,
    port : String,
    volume : ChannelVolumes,
    mute : bool,
}

/// Information on one playback or recording stream
#[derive(Debug)]
struct WireInfo {
    index : u32,
    client : Option<u32>,
    port : u32,
    volume : u32,
    mute : bool,
}

/// A singleton structure that collects introspection data from the pulse server
#[derive(Debug,Default)]
struct PulseData {
    context : Cell<Option<Context>>,
    default_sink : Cell<String>,
    default_source : Cell<String>,
    sources : Cell<Vec<PortInfo>>,
    sinks : Cell<Vec<PortInfo>>,
    clients : Cell<Vec<(u32, String)>>,
    sink_ins : Cell<Vec<WireInfo>>,
    src_outs : Cell<Vec<WireInfo>>,
    interested : Cell<NotifierList>,
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

    async fn mainloop(self : Rc<Self>) -> Result<(), Error> {
        let mut main = TokioMain::new();

        let mut props = proplist::Proplist::new().ok_or("proplist")?;
        props.set_str(proplist::properties::APPLICATION_NAME, "rwaybar").unwrap();
        let mut context = Context::new_with_proplist(&main, "rwaybar", &props).ok_or("context")?;
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

        loop {
            futures_util::future::poll_fn(|ctx| main.tick(ctx)).await;
        }
    }

    fn subscribe_cb(self : &Rc<Self>, inspect : &Introspector, facility : Option<Facility>, op : Option<Operation>, idx : u32) {
        match (facility, op) {
            (Some(Facility::Sink), Some(Operation::New)) |
            (Some(Facility::Sink), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_sink_info_by_index(idx, move |item| {
                    data.add_sink(item);
                });
            }
            (Some(Facility::Sink), Some(Operation::Removed)) => {
                self.sinks.take_in(|sinks| {
                    sinks.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data();
            }
            (Some(Facility::Source), Some(Operation::New)) |
            (Some(Facility::Source), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_source_info_by_index(idx, move |item| {
                    data.add_source(item);
                });
            }
            (Some(Facility::Source), Some(Operation::Removed)) => {
                self.sources.take_in(|sources| {
                    sources.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data();
            }
            (Some(Facility::Client), Some(Operation::New)) |
            (Some(Facility::Client), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_client_info(idx, move |item| {
                    data.add_client(item);
                });
            }
            (Some(Facility::Client), Some(Operation::Removed)) => {
                self.clients.take_in(|clients| {
                    clients.retain(|info| info.0 != idx);
                });
                self.interested.take().notify_data();
            }
            (Some(Facility::SinkInput), Some(Operation::New)) |
            (Some(Facility::SinkInput), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_sink_input_info(idx, move |item| {
                    data.add_sink_input(item);
                });
            }
            (Some(Facility::SinkInput), Some(Operation::Removed)) => {
                self.sink_ins.take_in(|sink_ins| {
                    sink_ins.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data();
            }
            (Some(Facility::SourceOutput), Some(Operation::New)) |
            (Some(Facility::SourceOutput), Some(Operation::Changed)) => {
                let data = self.clone();
                inspect.get_source_output_info(idx, move |item| {
                    data.add_source_output(item);
                });
            }
            (Some(Facility::SourceOutput), Some(Operation::Removed)) => {
                self.src_outs.take_in(|src_outs| {
                    src_outs.retain(|info| info.index != idx);
                });
                self.interested.take().notify_data();
            }
            _ => { }
        }
    }

    fn add_sink(&self, item : ListResult<&SinkInfo>) {
        self.interested.take().notify_data();
        match item {
            ListResult::Item(info) => {
                let pi = PortInfo {
                    index: info.index,
                    name : info.name.as_deref().unwrap_or("").to_owned(),
                    desc : info.description.as_deref().unwrap_or("").to_owned(),
                    port : info.active_port.as_ref().and_then(|port| port.description.as_deref()).unwrap_or("").to_owned(),
                    volume : info.volume,
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
            ListResult::Error => debug!("get_sink_info failed"),
        }
    }

    fn add_source(&self, item : ListResult<&SourceInfo>) {
        self.interested.take().notify_data();
        match item {
            ListResult::Item(info) => {
                let pi = PortInfo {
                    index: info.index,
                    name : info.name.as_deref().unwrap_or("").to_owned(),
                    desc : info.description.as_deref().unwrap_or("").to_owned(),
                    port : info.active_port.as_ref().and_then(|port| port.description.as_deref()).unwrap_or("").to_owned(),
                    volume : info.volume,
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
            ListResult::Error => debug!("get_source_info failed"),
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
            ListResult::Error => debug!("get_client_info failed"),
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
                        port : info.sink,
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
            ListResult::Error => debug!("get_sink_input failed"),
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
                        port : info.source,
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
            ListResult::Error => debug!("get_source_output failed"),
        }
    }

    pub fn with_target<F,R>(&self, target : &str, f : F) -> R
        where F : FnOnce(Option<&mut PortInfo>, bool, &[WireInfo], &[(u32, String)]) -> R
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
                error!("Invalid target specification '{}' - should be source:<name> or sink:<name>", target);
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

pub fn read_in<F : FnOnce(&str) -> R, R>(cfg_name : &str, target : &str, mut key : &str, rt : &Runtime, f : F) -> R {
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
                    return f("");
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
                    f(&v)
                }
                "text" | "volume" => {
                    if key == "text" && port.mute {
                        f("-")
                    } else {
                        let volume = port.volume.avg();
                        f(volume.print().trim())
                    }
                }
                "mute" => {
                    if port.mute {
                        f("1")
                    } else {
                        f("0")
                    }
                }
                _ => {
                    info!("Unknown key '{}' in '{}'", key, cfg_name);
                    f("")
                }
            }
        })
    })
}

pub fn do_write(_name : &str, target : &str, mut key : &str, value : String, rt : &Runtime) {
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
                            Some('+') => { port.volume.increase(Volume(value as u32)); }
                            Some('-') => { port.volume.decrease(Volume(value as u32)); }
                            _ => { port.volume.scale(Volume(value as u32)); }
                        }
                        if is_sink {
                            ctx.introspect().set_sink_volume_by_index(port.index, &port.volume, None);
                        } else {
                            ctx.introspect().set_source_volume_by_index(port.index, &port.volume, None);
                        }
                        rt.notify.notify_data();
                    }
                    "mute" => {
                        let old = port.mute;
                        let new = match &*value {
                            "toggle" => !old,
                            "on" | "1" => true,
                            "off" | "0" => false,
                            _ => {
                                error!("Invalid mute request '{}'", value);
                                return;
                            }
                        };
                        if old == new {
                            return;
                        }
                        port.mute = new;
                        if is_sink {
                            ctx.introspect().set_sink_mute_by_index(port.index, new, None);
                        } else {
                            ctx.introspect().set_source_mute_by_index(port.index, new, None);
                        }
                        rt.notify.notify_data();
                    }
                    _ => {
                        info!("Ignoring write to unknown key '{}'", target);
                    }
                }
            });
        });
    });
}
