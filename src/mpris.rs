use crate::{
    data::{IterationItem, Value},
    dbus::DBus,
    state::{NotifierList, Runtime},
    util::{self, Cell},
};
use futures_util::{future::RemoteHandle, StreamExt};
use log::{debug, error, warn};
use std::{cell::OnceCell, collections::HashMap, convert::TryInto, error::Error, rc::Rc};
use zbus::{fdo::DBusProxy, names::BusName, names::UniqueName, zvariant};
use zvariant::{OwnedValue, Value as Variant};

// TODO need nonblocking caching
#[zbus::proxy(
    interface = "org.mpris.MediaPlayer2.Player",
    default_path = "/org/mpris/MediaPlayer2"
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
    fn set_position(&self, trackid: &zvariant::ObjectPath<'_>, position: i64) -> zbus::Result<()>;

    /// Stop method
    fn stop(&self) -> zbus::Result<()>;

    /// Seeked signal
    #[zbus(signal)]
    fn seeked(&self, position: i64) -> zbus::Result<()>;

    /// CanControl property
    #[zbus(property)]
    fn can_control(&self) -> zbus::Result<bool>;

    /// CanGoNext property
    #[zbus(property)]
    fn can_go_next(&self) -> zbus::Result<bool>;

    /// CanGoPrevious property
    #[zbus(property)]
    fn can_go_previous(&self) -> zbus::Result<bool>;

    /// CanPause property
    #[zbus(property)]
    fn can_pause(&self) -> zbus::Result<bool>;

    /// CanPlay property
    #[zbus(property)]
    fn can_play(&self) -> zbus::Result<bool>;

    /// CanSeek property
    #[zbus(property)]
    fn can_seek(&self) -> zbus::Result<bool>;

    /// Metadata property
    #[zbus(property)]
    fn metadata(&self) -> zbus::Result<HashMap<String, OwnedValue>>;

    /// PlaybackStatus property
    #[zbus(property)]
    fn playback_status(&self) -> zbus::Result<String>;

    /// Position property
    #[zbus(property)]
    fn position(&self) -> zbus::Result<i64>;

    /// Volume property
    #[zbus(property)]
    fn volume(&self) -> zbus::Result<f64>;
    #[zbus(property)]
    fn set_volume(&self, value: f64) -> zbus::Result<()>;
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum PlayState {
    Playing,
    Paused,
    Stopped,
}

impl PlayState {
    fn parse(s: &str) -> Option<Self> {
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
    name_tail: Rc<str>,
    proxy: PlayerProxy<'static>,
    playing: Option<PlayState>,
    meta: HashMap<String, OwnedValue>,
    #[allow(unused)] // for Drop
    update_handle: RemoteHandle<()>,
}

#[derive(Debug, Default)]
struct MediaPlayer2 {
    players: Cell<Vec<Player>>,
    interested: Cell<NotifierList>,
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
            let zbus = dbus.connection().await;

            let bus = DBusProxy::builder(&zbus)
                .cache_properties(zbus::CacheProperties::No)
                .build()
                .await?;

            let this = mpris.clone();
            let mut names = bus.receive_name_owner_changed().await?;
            util::spawn("MPRIS client add/remove watcher", async move {
                while let Some(noc) = names.next().await {
                    let event = noc.args()?;
                    if !event.name.starts_with("org.mpris.MediaPlayer2.") {
                        continue;
                    }
                    if let Some(old) = &*event.old_owner {
                        this.players.take_in(|players| {
                            players.retain(|player| {
                                if *player.proxy.inner().destination() == *old {
                                    this.interested.take().notify_data("mpris:remove");
                                    false
                                } else {
                                    true
                                }
                            });
                        });
                    }

                    if let Some(new) = &*event.new_owner {
                        util::spawn(
                            "MPRIS state query",
                            this.clone()
                                .initial_query(event.name.to_owned(), Some(new.to_owned())),
                        );
                    }
                }
                Ok(())
            });

            let names = bus.list_names().await?;

            for name in names {
                if !name.starts_with("org.mpris.MediaPlayer2.") {
                    continue;
                }
                let name = name.into_inner();

                // query them all in parallel
                util::spawn("MPRIS state query", mpris.clone().initial_query(name, None));
            }

            Ok(())
        });

        rv
    }

    async fn initial_query(
        self: Rc<Self>,
        bus_name: BusName<'static>,
        owner: Option<UniqueName<'static>>,
    ) -> Result<(), Box<dyn Error>> {
        let skip = "org.mpris.MediaPlayer2.".len();
        let name_tail: Rc<str> = bus_name[skip..].into();
        let dbus = DBus::get_session();
        let zbus = dbus.connection().await;
        let owner = match owner {
            Some(owner) => owner,
            None => DBusProxy::builder(&zbus)
                .cache_properties(zbus::CacheProperties::No)
                .build()
                .await?
                .get_name_owner(bus_name)
                .await?
                .into_inner(),
        };

        let proxy = PlayerProxy::builder(&zbus)
            .destination(owner.clone())?
            .build()
            .await?;

        let property_proxy =
            zbus::fdo::PropertiesProxy::new(&zbus, owner, "/org/mpris/MediaPlayer2").await?;

        let prop_stream = property_proxy.receive_properties_changed().await?;
        let update_handle = util::spawn_handle(
            "MPRIS prop-watcher",
            self.clone()
                .watch_properties(prop_stream, name_tail.clone()),
        );

        let playing = PlayState::parse(&proxy.playback_status().await?);
        let meta = proxy.metadata().await?;

        self.interested.take().notify_data("mpris:add");
        self.players.take_in(|players| {
            players.push(Player {
                name_tail,
                proxy,
                playing,
                meta,
                update_handle,
            });
        });

        Ok(())
    }

    async fn watch_properties(
        self: Rc<Self>,
        mut s: impl StreamExt<Item = zbus::fdo::PropertiesChanged> + Unpin,
        name_tail: Rc<str>,
    ) -> Result<(), Box<dyn Error>> {
        while let Some(msg) = s.next().await {
            let args = msg.args()?;
            self.players.take_in(|players| {
                let player = match players
                    .iter_mut()
                    .find(|p| Rc::ptr_eq(&p.name_tail, &name_tail))
                {
                    Some(player) => player,
                    None => return,
                };
                for (&prop, value) in &args.changed_properties {
                    match prop {
                        "PlaybackStatus" => {
                            if let Ok(status) = value.try_into() {
                                player.playing = PlayState::parse(status);
                            }
                        }
                        "Metadata" => {
                            if let Variant::Dict(meta) = &*value {
                                player.meta = meta.try_clone().unwrap().try_into().unwrap();
                            }
                        }
                        _ => (),
                    }
                }
                self.interested.take().notify_data("mpris:props");
            });
        }
        Ok(())
    }
}

pub fn read_in<F: FnOnce(Value) -> R, R>(
    _name: &str,
    target: &str,
    key: &str,
    rt: &Runtime,
    f: F,
) -> R {
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
                player = players
                    .iter()
                    .filter(|p| p.playing == Some(PlayState::Playing))
                    .chain(
                        players
                            .iter()
                            .filter(|p| p.playing == Some(PlayState::Paused)),
                    )
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
                    "player.name" => f(Value::Borrow(&player.name_tail)),
                    "length" => match player
                        .meta
                        .get("mpris:length")
                        .map(|v| v.downcast_ref::<i64>())
                    {
                        Some(Ok(len)) => f(Value::Float(len as f64 / 1_000_000.0)),
                        _ => f(Value::Null),
                    },
                    _ if field.contains('.') => {
                        let real_field = field.replace('.', ":");
                        let qf = player.meta.get(&*field).map(|v| v.downcast_ref());
                        let rf = player.meta.get(&*real_field).map(|v| v.downcast_ref());

                        match (qf, rf) {
                            (Some(Ok(v)), _) | (_, Some(Ok(v))) => f(Value::Borrow(v)),
                            _ => f(Value::Null),
                        }
                    }
                    // See http://www.freedesktop.org/wiki/Specifications/mpris-spec/metadata for
                    // a list of valid names
                    _ => {
                        let xeasm = format!("xesam:{}", field);
                        let value = player.meta.get(&xeasm).map(|x| &**x);
                        match value {
                            Some(Variant::Str(v)) => f(Value::Borrow(v.as_str())),
                            Some(Variant::Array(a)) => {
                                let mut tmp = String::new();
                                for e in a.iter() {
                                    if let Variant::Str(s) = e {
                                        tmp.push_str(s);
                                        tmp.push_str(", ");
                                    }
                                }
                                tmp.pop();
                                tmp.pop();
                                f(Value::Owned(tmp))
                            }
                            _ => f(Value::Null),
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

pub fn read_focus_list<F: FnMut(bool, IterationItem)>(rt: &Runtime, mut f: F) {
    let players: Vec<_> = DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.interested.take_in(|i| i.add(rt));
        state.players.take_in(|players| {
            players
                .iter()
                .map(|p| (p.name_tail.clone(), p.playing == Some(PlayState::Playing)))
                .collect()
        })
    });

    for (player, playing) in players {
        f(playing, IterationItem::MediaPlayer2 { target: player });
    }
}

pub fn write(_name: &str, target: &str, key: &str, command: Value, _rt: &Runtime) {
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
                player = players
                    .iter()
                    .filter(|p| p.playing == Some(PlayState::Playing))
                    .chain(
                        players
                            .iter()
                            .filter(|p| p.playing == Some(PlayState::Paused)),
                    )
                    .chain(players.iter())
                    .next();
            }

            let player = match player {
                Some(p) => p,
                None => {
                    warn!("No player found when sending {}", key);
                    return;
                }
            };

            // TODO call/nowait
            let dbus = DBus::get_session();
            let command = command.into_text();
            match &*command {
                "Next" | "Previous" | "Pause" | "PlayPause" | "Stop" | "Play" => {
                    dbus.send(
                        zbus::Message::method("/org/mpris/MediaPlayer2", &*command)
                            .unwrap()
                            .destination(player.proxy.inner().destination().clone())
                            .unwrap()
                            .interface("org.mpris.MediaPlayer2.Player")
                            .unwrap()
                            .build(&())
                            .unwrap(),
                    );
                }
                // TODO seek, volume?
                "Raise" | "Quit" => {
                    dbus.send(
                        zbus::Message::method("/org/mpris/MediaPlayer2", &*command)
                            .unwrap()
                            .destination(player.proxy.inner().destination().clone())
                            .unwrap()
                            .interface("org.mpris.MediaPlayer2")
                            .unwrap()
                            .build(&())
                            .unwrap(),
                    );
                }
                _ => {
                    error!("Unknown command {}", command);
                }
            }
        })
    })
}
