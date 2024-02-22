use crate::{
    data::Value,
    state::{NotifierList, Runtime, State},
    util::spawn,
};
use std::{
    cell::RefCell,
    collections::VecDeque,
    rc::{Rc, Weak},
};
// TODO use std::sync::Mutex;
use bytes::{Bytes, BytesMut};
use futures_channel::oneshot;
use futures_util::future::{select, Either};
use wayland_client::{protocol::wl_seat::WlSeat, Connection, Proxy, QueueHandle};
use wayland_protocols_wlr::data_control::v1::client::{
    zwlr_data_control_device_v1,
    zwlr_data_control_offer_v1::{self, ZwlrDataControlOfferV1},
};

#[derive(Debug)]
enum OfferValue {
    Available,
    Running {
        data: oneshot::Receiver<Bytes>,
        interested: Rc<NotifierList>,
    },
    Finished(Bytes),
    Failed,
}

#[derive(Debug)]
struct OfferType {
    mime: Box<str>,
    value: OfferValue,
}

#[derive(Debug)]
pub struct OfferData {
    mimes: RefCell<Vec<OfferType>>,
}

// XXX this would need reworking to be threadsafe, rely on no threads
unsafe impl Send for OfferData {}
unsafe impl Sync for OfferData {}

#[derive(Debug)]
struct Clipboard {
    seat: WlSeat,
    selection: bool,
    contents: Option<ZwlrDataControlOfferV1>,
    interested: Vec<Weak<ClipboardData>>,
}

thread_local! {
    static CLIPBOARDS: RefCell<Option<VecDeque<Clipboard>>> = RefCell::new(None);
}

impl wayland_client::Dispatch<zwlr_data_control_device_v1::ZwlrDataControlDeviceV1, WlSeat>
    for State
{
    fn event(
        _: &mut Self,
        dcd: &zwlr_data_control_device_v1::ZwlrDataControlDeviceV1,
        event: zwlr_data_control_device_v1::Event,
        seat: &WlSeat,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        use zwlr_data_control_device_v1::Event;
        match event {
            Event::DataOffer { .. } => {}
            Event::Selection { id } => {
                set_seat_offer(seat, id, false);
            }
            Event::PrimarySelection { id } => {
                set_seat_offer(seat, id, true);
            }
            Event::Finished => {
                set_seat_offer(seat, None, false);
                set_seat_offer(seat, None, true);
                dcd.destroy();
            }
            _ => {}
        }
    }

    wayland_client::event_created_child!(State, zwlr_data_control_device_v1::ZwlrDataControlDeviceV1, [
        0 => (ZwlrDataControlOfferV1, OfferData {
            mimes: RefCell::new(Vec::new())
        }),
    ]);
}

impl wayland_client::Dispatch<ZwlrDataControlOfferV1, OfferData> for State {
    fn event(
        _: &mut Self,
        _: &zwlr_data_control_offer_v1::ZwlrDataControlOfferV1,
        event: zwlr_data_control_offer_v1::Event,
        data: &OfferData,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            zwlr_data_control_offer_v1::Event::Offer { mime_type } => {
                data.mimes.borrow_mut().push(OfferType {
                    mime: mime_type.into(),
                    value: OfferValue::Available,
                });
            }
            _ => {}
        }
    }
}

fn start_dcm(rt: &Runtime) -> VecDeque<Clipboard> {
    let mut rv = VecDeque::new();
    match rt.wayland.wlr_dcm.get() {
        Ok(dcm) => {
            for seat in rt.wayland.seat.seats() {
                rv.push_back(Clipboard {
                    seat: seat.clone(),
                    selection: true,
                    contents: None,
                    interested: Vec::new(),
                });
                rv.push_back(Clipboard {
                    seat: seat.clone(),
                    selection: false,
                    contents: None,
                    interested: Vec::new(),
                });

                dcm.get_data_device(&seat, &rt.wayland.queue, seat.clone());
            }
        }
        Err(e) => {
            log::error!("Clipboard not available: {e:?}");
        }
    }
    rv
}

fn set_seat_offer(seat: &WlSeat, contents: Option<ZwlrDataControlOfferV1>, selection: bool) {
    CLIPBOARDS.with(|clips| {
        let mut clips = clips.borrow_mut();
        let clips = clips.as_mut().unwrap();
        for i in 0..clips.len() {
            let clip = &clips[i];
            if clip.seat != *seat || clip.selection != selection {
                continue;
            }
            if let Some(prev) = &clip.contents {
                prev.destroy();
            }
            // use the prior interest list to read the new clipboard
            let data = contents.as_ref().map(|c| c.data::<OfferData>());
            for view in clip.interested.iter().filter_map(Weak::upgrade) {
                if let (Some(contents), &Some(Some(data))) = (&contents, &data) {
                    if let Some(idx) = view.find_best_mime(data) {
                        let mut mimes = data.mimes.borrow_mut();
                        let best = &mut mimes[idx];
                        view.start_read(contents, best);
                        continue;
                    }
                }
                view.interested.notify_data("empty-clipboard");
            }
            clips.remove(i);
            break;
        }
        clips.push_front(Clipboard {
            seat: seat.clone(),
            selection,
            contents,
            interested: Vec::new(),
        });
    });
}

#[derive(Debug)]
pub struct ClipboardData {
    pub seat: Option<Box<str>>,
    pub mime_list: Vec<Box<str>>,
    pub selection: bool,
    pub interested: NotifierList,
}

impl ClipboardData {
    fn find_best_mime(&self, data: &OfferData) -> Option<usize> {
        let offered = data.mimes.borrow();
        if self.mime_list.is_empty() {
            for &mime in &[
                "text/plain;charset=utf-8",
                "text/plain",
                "UTF8_STRING",
                "STRING",
                "TEXT",
            ] {
                if let Some(i) = offered.iter().position(|t| &*t.mime == mime) {
                    return Some(i);
                }
            }
        } else {
            for mime in &self.mime_list {
                if let Some(i) = offered.iter().position(|t| &t.mime == mime) {
                    return Some(i);
                }
            }
        }
        None
    }

    fn start_read(&self, contents: &ZwlrDataControlOfferV1, offer: &mut OfferType) {
        match &mut offer.value {
            OfferValue::Available => {
                let (mut send, recv) = oneshot::channel();
                let (tx, rx) = match std::os::unix::net::UnixStream::pair() {
                    Ok(p) => p,
                    Err(_) => {
                        offer.value = OfferValue::Failed;
                        return;
                    }
                };
                use std::os::fd::AsFd;
                contents.receive(String::from(&*offer.mime), tx.as_fd());

                let interested = Rc::new(NotifierList::default());
                interested.merge(&self.interested);
                offer.value = OfferValue::Running {
                    data: recv,
                    interested: interested.clone(),
                };

                spawn("Clipboard read", async move {
                    use tokio::io::AsyncReadExt;
                    let mut rx = tokio::net::UnixStream::from_std(rx)?;
                    let mut buf = BytesMut::new();
                    let mut cancel = send.cancellation();
                    loop {
                        let read = rx.read_buf(&mut buf);
                        futures_util::pin_mut!(read);
                        match select(cancel, read).await {
                            Either::Left(_) => return Ok(()),
                            Either::Right((Ok(0), _)) => break,
                            Either::Right((rv, c)) => {
                                rv?;
                                cancel = c;
                            }
                        }
                    }
                    drop(send.send(buf.into()));
                    interested.notify_data("clipboard-data");
                    Ok(())
                });
            }
            OfferValue::Running { data, interested } => {
                interested.merge(&self.interested);
                match data.try_recv() {
                    Ok(Some(v)) => {
                        offer.value = OfferValue::Finished(v);
                    }
                    Ok(None) => {}
                    Err(_) => {
                        offer.value = OfferValue::Failed;
                    }
                }
            }
            OfferValue::Finished(_) => {}
            OfferValue::Failed => {}
        }
    }

    pub fn read_in<F: FnOnce(Value) -> R, R>(
        self: &Rc<Self>,
        _name: &str,
        key: &str,
        rt: &Runtime,
        f: F,
    ) -> R {
        self.interested.add(rt);
        CLIPBOARDS.with(|clips| {
            let mut clips = clips.borrow_mut();
            let clips = clips.get_or_insert_with(|| start_dcm(rt));
            for clip in &mut *clips {
                if clip.selection != self.selection {
                    continue;
                }
                if let Some(seat) = &self.seat {
                    if rt
                        .wayland
                        .seat
                        .info(&clip.seat)
                        .map(|data| data.name.as_deref() == Some(&**seat))
                        != Some(true)
                    {
                        continue;
                    }
                }

                if clip
                    .interested
                    .iter()
                    .all(|e| e.as_ptr() != Rc::as_ptr(self))
                {
                    clip.interested.push(Rc::downgrade(self));
                }

                if let Some(contents) = &clip.contents {
                    if let Some(data) = contents.data::<OfferData>() {
                        if let Some(idx) = self.find_best_mime(data) {
                            let mut mimes = data.mimes.borrow_mut();
                            let best = &mut mimes[idx];
                            if key == "mime" {
                                return f(Value::Borrow(&best.mime));
                            }

                            self.start_read(contents, best);

                            if let OfferValue::Finished(v) = &best.value {
                                return f(String::from_utf8_lossy(&v).into());
                            } else {
                                return f(Value::Null);
                            }
                        }
                    }
                }
                return f(Value::Null);
            }
            f(Value::Null)
        })
    }
}
