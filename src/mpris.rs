use crate::dbus::DBus;
use crate::data::{IterationItem,Value};
use crate::state::{Runtime,NotifierList};
use crate::util::{self,Cell};
use once_cell::unsync::OnceCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::error::Error;
use std::rc::Rc;
use log::{debug,warn,error};
use zvariant::Value as Variant;
use zvariant::OwnedValue;
use zbus::dbus_proxy;
use zbus::fdo::AsyncDBusProxy;
use zbus::names::UniqueName;

#[dbus_proxy(interface = "org.mpris.MediaPlayer2")]
trait MediaPlayer2 {
    /// Quit method
    fn quit(&self) -> zbus::Result<()>;

    /// Raise method
    fn raise(&self) -> zbus::Result<()>;

    /// CanQuit property
    #[dbus_proxy(property)]
    fn can_quit(&self) -> zbus::Result<bool>;

    /// CanRaise property
    #[dbus_proxy(property)]
    fn can_raise(&self) -> zbus::Result<bool>;

    /// DesktopEntry property
    #[dbus_proxy(property)]
    fn desktop_entry(&self) -> zbus::Result<String>;

    /// Identity property
    #[dbus_proxy(property)]
    fn identity(&self) -> zbus::Result<String>;

    /// SupportedMimeTypes property
    #[dbus_proxy(property)]
    fn supported_mime_types(&self) -> zbus::Result<Vec<String>>;

    /// SupportedUriSchemes property
    #[dbus_proxy(property)]
    fn supported_uri_schemes(&self) -> zbus::Result<Vec<String>>;
}

// TODO need nonblocking caching
#[dbus_proxy(interface = "org.mpris.MediaPlayer2.Player")]
trait Player {
    /// Next method
    fn next(&self) -> zbus::Result<()>;

    /// Pause method
    fn pause(&self) -> zbus::Result<()>;

    /// Play method
    fn play(&self) -> zbus::Result<()>;

    /// PlayPause method
    fn play_pause(&self) -> zbus::Result<()>;

    /// Previous method
    fn previous(&self) -> zbus::Result<()>;

    /// Seek method
    fn seek(&self, offset: i64) -> zbus::Result<()>;

    /// SetPosition method
    fn set_position(
        &self,
        trackid: &zvariant::ObjectPath<'_>,
        position: i64,
    ) -> zbus::Result<()>;

    /// Stop method
    fn stop(&self) -> zbus::Result<()>;

    /// Seeked signal
    #[dbus_proxy(signal)]
    fn seeked(&self, position: i64) -> zbus::Result<()>;

    /// CanControl property
    #[dbus_proxy(property)]
    fn can_control(&self) -> zbus::Result<bool>;

    /// CanGoNext property
    #[dbus_proxy(property)]
    fn can_go_next(&self) -> zbus::Result<bool>;

    /// CanGoPrevious property
    #[dbus_proxy(property)]
    fn can_go_previous(&self) -> zbus::Result<bool>;

    /// CanPause property
    #[dbus_proxy(property)]
    fn can_pause(&self) -> zbus::Result<bool>;

    /// CanPlay property
    #[dbus_proxy(property)]
    fn can_play(&self) -> zbus::Result<bool>;

    /// CanSeek property
    #[dbus_proxy(property)]
    fn can_seek(&self) -> zbus::Result<bool>;

    /// Metadata property
    #[dbus_proxy(property)]
    fn metadata(
        &self,
    ) -> zbus::Result<OwnedValue>; // HashMap<String, zvariant::OwnedValue>>;

    /// PlaybackStatus property
    #[dbus_proxy(property)]
    fn playback_status(&self) -> zbus::Result<String>;

    /// Position property
    #[dbus_proxy(property)]
    fn position(&self) -> zbus::Result<i64>;

    /// Volume property
    #[dbus_proxy(property)]
    fn volume(&self) -> zbus::Result<f64>;
    #[dbus_proxy(property)]
    fn set_volume(&self, value: f64) -> zbus::Result<()>;
}

#[derive(Debug,Eq,PartialEq,Copy,Clone)]
enum PlayState {
    Playing,
    Paused,
    Stopped,
}

impl PlayState {
    fn parse(s : &str) -> Option<Self> {
        match s {
            "Playing" => Some(Self::Playing),
            "Paused" => Some(Self::Paused),
            "Stopped" => Some(Self::Stopped),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Player {
    name : String,
    owner : UniqueName<'static>,
    playing : Option<PlayState>,
    meta : Option<HashMap<String, OwnedValue>>,
}

#[derive(Debug,Default)]
struct Inner {
    players : Vec<Player>,
    interested : NotifierList,
}

#[derive(Debug)]
struct MediaPlayer2(Cell<Option<Inner>>);

async fn initial_query(target : Rc<MediaPlayer2>, name : String) -> Result<(), Box<dyn Error>> {
    let dbus = DBus::get_session();
    let zbus = dbus.connection().await;
    let owner = AsyncDBusProxy::builder(&zbus)
        .cache_properties(false)
        .build().await?
        .get_name_owner((&*name).try_into()?)
        .await?
        .into_inner();

    let rc = target.clone();
    let no = owner.clone();
    dbus.spawn_call_err(zbus::Message::method(
        None::<&str>,
        Some(&*name),
        "/org/mpris/MediaPlayer2",
        Some("org.freedesktop.DBus.Properties"),
        "Get",
        &("org.mpris.MediaPlayer2.Player", "PlaybackStatus")
    )?, move |msg| {
        let playing : Variant = msg.body()?;
        let playing = match playing.downcast_ref() {
            Some(playing) => PlayState::parse(playing),
            _ => return Ok(()),
        };
        rc.0.take_in_some(|inner| {
            for player in &mut inner.players {
                if player.owner != no {
                    continue;
                }
                player.playing = playing;
            }
        });
        Ok(())
    }, |err : zbus::Error| warn!("Could not read MPRIS properties: {}", err));

    let rc = target.clone();
    let no = owner.clone();
    dbus.spawn_call_err(zbus::Message::method(
        None::<&str>,
        Some(&*name),
        "/org/mpris/MediaPlayer2",
        Some("org.freedesktop.DBus.Properties"),
        "Get",
        &("org.mpris.MediaPlayer2.Player", "Metadata")
    )?, move |msg| {
        let meta : OwnedValue = msg.body()?;
        let meta = match (*meta).clone() {
            Variant::Dict(meta) => meta.try_into().ok(),
            _ => None,
        };

        rc.0.take_in_some(|inner| {
            for player in &mut inner.players {
                if player.owner != no {
                    continue;
                }
                player.meta = meta;
                break;
            }
            inner.interested.notify_data("mpris:init");
        });
        Ok(())
    }, |err : zbus::Error| warn!("Could not read MPRIS properties: {}", err));

    target.0.take_in_some(|inner| {
        inner.players.push(Player {
            name,
            owner,
            playing : None,
            meta : None,
        });
    });

    Ok(())
}

thread_local! {
    static DATA : OnceCell<Rc<MediaPlayer2>> = Default::default();
}

impl MediaPlayer2 {
    pub fn new() -> Rc<Self> {
        let rv = Rc::new(MediaPlayer2(Cell::new(Some(Inner::default()))));

        let mpris = rv.clone();
        util::spawn("MPRIS setup", async move {
            let dbus = DBus::get_session();
            // Watch for property updates to any active mpris player and update our internal map
            let target = mpris.clone();
            dbus.add_property_change_watcher(move |hdr, iface, changed, _inval| {
                match target.handle_mpris_update(hdr, iface, changed) {
                    Ok(()) => (),
                    Err(e) => warn!("MPRIS update error: {}", e),
                }
            });

            // Watch for new players and player exits
            let this = mpris.clone();
            dbus.add_name_watcher(move |name, old, new| {
                if !new.is_empty() && name.starts_with("org.mpris.MediaPlayer2.") {
                    util::spawn("MPRIS state query", initial_query(this.clone(), name.to_owned()));
                }
                if !old.is_empty() {
                    this.0.take_in_some(|inner| {
                        let interested = &mut inner.interested;
                        inner.players.retain(|player| {
                            if player.owner == name || player.name == name {
                                interested.notify_data("mpris:remove");
                                false
                            } else {
                                true
                            }
                        });
                    });
                }
            });

            // Now that watching is active, populate our map with initial values for all active players
            dbus.spawn_call_err(zbus::Message::method(
                None::<&str>,
                Some("org.freedesktop.DBus"),
                "/org/freedesktop/DBus",
                Some("org.freedesktop.DBus"),
                "ListNames",
                &()
            )?, move |msg| {
                let names : Vec<String> = msg.body()?;
                for name in names {
                    if !name.starts_with("org.mpris.MediaPlayer2.") {
                        continue;
                    }

                    // query them all in parallel
                    util::spawn("MPRIS state query", initial_query(mpris.clone(), name));
                }
                Ok(())
            }, |err : Box<dyn Error>| {
                log::info!("MPRIS setup error: {}", err);
            });

            Ok(())
        });

        rv
    }

    fn handle_mpris_update(&self, hdr : &zbus::MessageHeader, iface : &str, changed : &HashMap<&str, OwnedValue>) -> zbus::Result<()> {
        if hdr.path()?.map(|p| p.as_str()) != Some("/org/mpris/MediaPlayer2") {
            return Ok(());
        }
        let src = hdr.sender()?.unwrap();
        if !src.starts_with(':') {
            log::warn!("Ignoring MPRIS update from {}", src);
            return Ok(());
        }
        if iface != "org.mpris.MediaPlayer2.Player" {
            return Ok(());
        }
        self.0.take_in_some(|inner| {
            let mut new = None;
            let found = inner.players.iter_mut().find(|p| p.owner == *src);
            let player = found.unwrap_or_else(|| {
                new.get_or_insert(Player {
                    name: Default::default(),
                    owner: src.to_owned(),
                    playing : None,
                    meta : None,
                })
            });
            for (&prop, value) in changed {
                match prop {
                    "PlaybackStatus" => {
                        if let Ok(status) = value.try_into() {
                            player.playing = PlayState::parse(status);
                        }
                    }
                    "Metadata" => {
                        player.meta = match (**value).clone() {
                            Variant::Dict(meta) => meta.try_into().ok(),
                            _ => None,
                        };
                    }
                    _ => ()
                }
            }
            if let Some(new) = new {
                inner.players.push(new);
            }
            inner.interested.notify_data("mpris:props");
        });
        Ok(())
    }
}

pub fn read_in<F : FnOnce(Value) -> R, R>(_name : &str, target : &str, key : &str, rt : &Runtime, f : F) -> R {
    DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.0.take_in(|oi| {
            oi.as_mut().map(|inner| inner.interested.add(rt));
            let player;
            let field;

            let skip = "org.mpris.MediaPlayer2.".len();
            if !target.is_empty() {
                field = key;
                player = oi.as_ref().and_then(|inner| inner.players.iter().find(|p| p.name.get(skip..) == Some(target)));
            } else if let Some(dot) = key.find('.') {
                let name = &key[..dot];
                field = &key[dot + 1..];
                player = oi.as_ref().and_then(|inner| inner.players.iter().find(|p| p.name.get(skip..) == Some(name)));
            } else {
                field = key;
                // Prefer playing players, then paused, then any
                //
                player = oi.as_ref()
                    .and_then(|inner| {
                        inner.players.iter()
                            .filter(|p| p.playing == Some(PlayState::Playing))
                            .chain(inner.players.iter().filter(|p| p.playing == Some(PlayState::Paused)))
                            .chain(inner.players.iter())
                            .next()
                    });
            }

            if field == "state" {
                return match player.and_then(|p| p.playing) {
                    Some(PlayState::Playing) => f(Value::Borrow("Playing")),
                    Some(PlayState::Paused) => f(Value::Borrow("Paused")),
                    Some(PlayState::Stopped) => f(Value::Borrow("Stopped")),
                    None => f(Value::Null),
                };
            }

            if let Some(player) = player {
                match field {
                    "player.name" => {
                        f(Value::Borrow(player.name.get(skip..).unwrap_or_default()))
                    }
                    "length" => {
                        match player.meta.as_ref()
                            .and_then(|md| md.get("mpris:length"))
                            .and_then(|v| v.downcast_ref::<u64>())
                        {
                            Some(len) => f(Value::Float(*len as f64 / 1_000_000.0)),
                            _ => f(Value::Null),
                        }
                    }
                    _ if field.contains('.') => {
                        let real_field = field.replace('.', ":");
                        match player.meta.as_ref()
                            .and_then(|md| md.get(&real_field).or(md.get(field)))
                            .and_then(|v| v.downcast_ref::<str>())
                        {
                            Some(v) => f(Value::Borrow(v)),
                            _ => f(Value::Null),
                        }
                    }
                    // See http://www.freedesktop.org/wiki/Specifications/mpris-spec/metadata for
                    // a list of valid names
                    _ => {
                        let xeasm = format!("xesam:{}", field);
                        let mut tmp = String::new();
                        let value = player.meta.as_ref()
                            .and_then(|md| md.get(&xeasm))
                            .map(|variant| match &**variant {
                                Variant::Str(v) => v.as_str(),
                                Variant::Array(a) => {
                                    for e in a.get().iter() {
                                        if let Variant::Str(s) = e {
                                            tmp.push_str(s);
                                            tmp.push_str(", ");
                                        }
                                    }
                                    tmp.pop(); tmp.pop();
                                    tmp.as_str()
                                }
                                _ => ""
                            })
                            .unwrap_or("");
                        f(Value::Borrow(value))
                    }
                }
            } else {
                debug!("No media players found");
                f(Value::Null)
            }
        })
    })
}

pub fn read_focus_list<F : FnMut(bool, IterationItem)>(rt : &Runtime, mut f : F) {
    let players : Vec<_> = DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.0.take_in_some(|inner| {
            inner.interested.add(rt);

            let skip = "org.mpris.MediaPlayer2.".len();
            inner.players.iter().map(|p| (p.name.get(skip..).unwrap_or_default().into(), p.playing == Some(PlayState::Playing))).collect()
        }).unwrap_or_default()
    });

    for (player, playing) in players {
        f(playing, IterationItem::MediaPlayer2 { target : player });
    }
}

pub fn write(_name : &str, target : &str, key : &str, command : Value, _rt : &Runtime) {
    DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.0.take_in(|oi| {
            let player;

            let skip = "org.mpris.MediaPlayer2.".len();
            if !target.is_empty() {
                player = oi.as_ref().and_then(|inner| inner.players.iter().find(|p| p.name.get(skip..) == Some(target)));
            } else if !key.is_empty() {
                player = oi.as_ref().and_then(|inner| inner.players.iter().find(|p| p.name.get(skip..) == Some(key)));
            } else {
                // Prefer playing players, then paused, then any
                player = oi.as_ref()
                    .and_then(|inner| {
                        inner.players.iter()
                            .filter(|p| p.playing == Some(PlayState::Playing))
                            .chain(inner.players.iter().filter(|p| p.playing == Some(PlayState::Paused)))
                            .chain(inner.players.iter())
                            .next()
                    });
            }

            let player = match player {
                Some(p) => p,
                None => { warn!("No player found when sending {}", key); return }
            };

            let dbus = DBus::get_session();
            let command = command.into_text();
            match &*command {
                "Next" | "Previous" | "Pause" | "PlayPause" | "Stop" | "Play" => {
                    dbus.send(zbus::Message::method(
                        None::<&str>,
                        Some(player.owner.clone()),
                        "/org/mpris/MediaPlayer2",
                        Some("org.mpris.MediaPlayer2.Player"),
                        &*command,
                        &()
                    ).unwrap());
                }
                // TODO seek, volume?
                "Raise" | "Quit" => {
                    dbus.send(zbus::Message::method(
                        None::<&str>,
                        Some(player.owner.clone()),
                        "/org/mpris/MediaPlayer2",
                        Some("org.mpris.MediaPlayer2"),
                        &*command,
                        &()
                    ).unwrap());
                }
                _ => {
                    error!("Unknown command {}", command);
                }
            }
        })
    })
}
