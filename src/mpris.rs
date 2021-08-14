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
use zbus::zvariant;
use zvariant::Value as Variant;
use zvariant::{Dict,OwnedValue};
use zbus::dbus_proxy;
use zbus::fdo::AsyncDBusProxy;
use zbus::names::BusName;

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
#[dbus_proxy(
    interface = "org.mpris.MediaPlayer2.Player",
    default_path = "/org/mpris/MediaPlayer2",
)]
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
    ) -> zbus::Result<Dict<'static, 'static>>;

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
    name_tail : Rc<str>,
    proxy : AsyncPlayerProxy<'static>,
    playing : Option<PlayState>,
    meta : Dict<'static, 'static>,
}

#[derive(Debug,Default)]
struct MediaPlayer2 {
    players : Cell<Vec<Player>>,
    interested : Cell<NotifierList>,
}

async fn initial_query(target : Rc<MediaPlayer2>, bus_name : BusName<'static>) -> Result<(), Box<dyn Error>> {
    let skip = "org.mpris.MediaPlayer2.".len();
    let name_tail = bus_name[skip..].into();
    let dbus = DBus::get_session();
    let zbus = dbus.connection().await;
    let owner = AsyncDBusProxy::builder(&zbus)
        .cache_properties(false)
        .build().await?
        .get_name_owner(bus_name)
        .await?
        .into_inner();

    let proxy = AsyncPlayerProxy::builder(&zbus)
        .destination(owner)?
        .build().await?;

    let playing = PlayState::parse(&proxy.playback_status().await?);
    let meta = proxy.metadata().await?;

    target.players.take_in(|players| {
        players.push(Player {
            name_tail,
            proxy,
            playing,
            meta,
        });
    });

    Ok(())
}

thread_local! {
    static DATA : OnceCell<Rc<MediaPlayer2>> = Default::default();
}

impl MediaPlayer2 {
    pub fn new() -> Rc<Self> {
        let rv = Rc::new(MediaPlayer2::default());

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
                if !old.is_empty() {
                    this.players.take_in(|players| {
                        players.retain(|player| {
                            if *player.proxy.destination() == *old {
                                this.interested.take().notify_data("mpris:remove");
                                false
                            } else {
                                true
                            }
                        });
                    });
                }
                if !new.is_empty() && name.starts_with("org.mpris.MediaPlayer2.") {
                    util::spawn("MPRIS state query", initial_query(this.clone(), name.to_owned()));
                }
            });

            let zbus = dbus.connection().await;
            let names = AsyncDBusProxy::builder(&zbus)
                .cache_properties(false)
                .build().await?
                .list_names().await?;
            for name in names {
                if !name.starts_with("org.mpris.MediaPlayer2.") {
                    continue;
                }
                let name = name.into_inner();

                // query them all in parallel
                util::spawn("MPRIS state query", initial_query(mpris.clone(), name));
            }

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
        self.players.take_in(|players| {
            let player = match players.iter_mut().find(|p| p.proxy.destination() == src) {
                Some(player) => player,
                None => return,
            };
            for (&prop, value) in changed {
                match prop {
                    "PlaybackStatus" => {
                        if let Ok(status) = value.try_into() {
                            player.playing = PlayState::parse(status);
                        }
                    }
                    "Metadata" => {
                        if let Variant::Dict(meta) = &**value {
                            player.meta = meta.clone();
                        }
                    }
                    _ => ()
                }
            }
            self.interested.take().notify_data("mpris:props");
        });
        Ok(())
    }
}

pub fn read_in<F : FnOnce(Value) -> R, R>(_name : &str, target : &str, key : &str, rt : &Runtime, f : F) -> R {
    DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.interested.take_in(|i| i.add(rt));

        state.players.take_in(|players| {
            let player;
            let field;

            if !target.is_empty() {
                field = key;
                player = players.iter().find(|p| &*p.name_tail == target);
            } else if let Some(dot) = key.find('.') {
                let name = &key[..dot];
                field = &key[dot + 1..];
                player = players.iter().find(|p| &*p.name_tail == name);
            } else {
                field = key;
                // Prefer playing players, then paused, then any
                //
                player = players.iter().filter(|p| p.playing == Some(PlayState::Playing))
                    .chain(players.iter().filter(|p| p.playing == Some(PlayState::Paused)))
                    .chain(players.iter())
                    .next();
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
                        f(Value::Borrow(&player.name_tail))
                    }
                    "length" => {
                        match player.meta.get::<_,u64>("mpris:length") {
                            Ok(Some(len)) => f(Value::Float(*len as f64 / 1_000_000.0)),
                            _ => f(Value::Null),
                        }
                    }
                    _ if field.contains('.') => {
                        let real_field = field.replace('.', ":");
                        let qf = player.meta.get::<str,str>(&field);
                        let rf = player.meta.get::<str,str>(&real_field);

                        match (qf, rf) {
                            (Ok(Some(v)), _) |
                            (_, Ok(Some(v))) => f(Value::Borrow(v)),
                            _ => f(Value::Null),
                        }
                    }
                    // See http://www.freedesktop.org/wiki/Specifications/mpris-spec/metadata for
                    // a list of valid names
                    _ => {
                        let xeasm = format!("xesam:{}", field);
                        let value = player.meta.get::<str,Variant>(&xeasm).ok().flatten();
                        match value {
                            Some(Variant::Str(v)) => f(Value::Borrow(v.as_str())),
                            Some(Variant::Array(a)) => {
                                let mut tmp = String::new();
                                for e in a.get().iter() {
                                    if let Variant::Str(s) = e {
                                        tmp.push_str(s);
                                        tmp.push_str(", ");
                                    }
                                }
                                tmp.pop(); tmp.pop();
                                f(Value::Owned(tmp))
                            }
                            _ => f(Value::Null)
                        }
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
        state.interested.take_in(|i| i.add(rt));
        state.players.take_in(|players| {
            players.iter().map(|p| (p.name_tail.clone(), p.playing == Some(PlayState::Playing))).collect()
        })
    });

    for (player, playing) in players {
        f(playing, IterationItem::MediaPlayer2 { target : player });
    }
}

pub fn write(_name : &str, target : &str, key : &str, command : Value, _rt : &Runtime) {
    DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.players.take_in(|players| {
            let player;

            if !target.is_empty() {
                player = players.iter().find(|p| &*p.name_tail == target);
            } else if !key.is_empty() {
                player = players.iter().find(|p| &*p.name_tail == key);
            } else {
                // Prefer playing players, then paused, then any
                player = players.iter().filter(|p| p.playing == Some(PlayState::Playing))
                    .chain(players.iter().filter(|p| p.playing == Some(PlayState::Paused)))
                    .chain(players.iter())
                    .next();
            }

            let player = match player {
                Some(p) => p,
                None => { warn!("No player found when sending {}", key); return }
            };

            // TODO call/nowait
            let dbus = DBus::get_session();
            let command = command.into_text();
            match &*command {
                "Next" | "Previous" | "Pause" | "PlayPause" | "Stop" | "Play" => {
                    dbus.send(zbus::Message::method(
                        None::<&str>,
                        Some(player.proxy.destination().clone()),
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
                        Some(player.proxy.destination().clone()),
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
