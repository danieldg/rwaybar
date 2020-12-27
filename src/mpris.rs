use crate::dbus::get as get_dbus;
use crate::dbus as dbus_util;
use crate::data::IterationItem;
use crate::state::{Runtime,NotifierList};
use crate::util::{self,Cell};
use dbus::message::Message;
use dbus::nonblock::Proxy;
use dbus::nonblock::stdintf::org_freedesktop_dbus::Properties;
use dbus::nonblock::stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged;
use dbus::arg::Variant;
use dbus::arg::RefArg;
use once_cell::unsync::OnceCell;
use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;
use std::time::Duration;
use log::{debug,warn,error};

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

#[derive(Debug,Default)]
struct Player {
    name : String,
    owner : String,
    playing : Option<PlayState>,
    meta : Option<HashMap<String, Variant<Box<dyn RefArg>>>>,
}

#[derive(Debug,Default)]
struct Inner {
    players : Vec<Player>,
    interested : NotifierList,
}

#[derive(Debug)]
struct MediaPlayer2(Cell<Option<Inner>>);

async fn initial_query(target : Rc<MediaPlayer2>, name : String) -> Result<(), Box<dyn Error>> {
    let dbus = get_dbus();
    let meta_bus = Proxy::new("org.freedesktop.DBus", "/org/freedesktop/DBus", Duration::from_secs(10), &dbus.local);
    let (owner,) = meta_bus.method_call("org.freedesktop.DBus", "GetNameOwner", (&name,)).await?;
    let player = Proxy::new(&name, "/org/mpris/MediaPlayer2", Duration::from_secs(10), &dbus.local);
    let status : String = player.get("org.mpris.MediaPlayer2.Player", "PlaybackStatus").await?;
    let playing = PlayState::parse(&status);
    let meta = player.get("org.mpris.MediaPlayer2.Player", "Metadata").await?;

    target.0.take_in_some(|inner| {
        for player in &mut inner.players {
            if player.owner != owner {
                continue;
            }
            // if it already exists, our values are likely to be out of date
            player.name = name;
            player.playing = player.playing.or(playing);
            player.meta.get_or_insert(meta);
            return;
        }
        inner.players.push(Player {
            name,
            owner,
            playing,
            meta : Some(meta),
        });
        inner.interested.notify_data();
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
            let dbus = get_dbus();
            let meta_bus = Proxy::new("org.freedesktop.DBus", "/org/freedesktop/DBus", Duration::from_secs(10), &dbus.local);

            // Watch for property updates to any active mpris player and update our internal map
            let target = mpris.clone();
            dbus.add_property_change_watcher(move |msg, ppc, _dbus| {
                if msg.path().as_deref() != Some("/org/mpris/MediaPlayer2") {
                    return;
                }
                if ppc.interface_name != "org.mpris.MediaPlayer2.Player" {
                    return;
                }
                target.handle_mpris_update(msg, ppc);
            }).await;

            // Watch for new players and player exits
            let this = mpris.clone();
            dbus.add_name_watcher(move |name, old, new, _dbus| {
                if !new.is_empty() && name.starts_with("org.mpris.MediaPlayer2.") {
                    util::spawn("MPRIS state query", initial_query(this.clone(), name.to_owned()));
                }
                if !old.is_empty() {
                    this.0.take_in_some(|inner| {
                        let interested = &mut inner.interested;
                        inner.players.retain(|player| {
                            if player.owner == name || player.name == name {
                                interested.notify_data();
                                false
                            } else {
                                true
                            }
                        });
                    });
                }
            }).await;

            // Now that watching is active, populate our map with initial values for all active players
            let (names,) : (Vec<String>,) = meta_bus.method_call("org.freedesktop.DBus", "ListNames", ()).await?;
            for name in names {
                if !name.starts_with("org.mpris.MediaPlayer2.") {
                    continue;
                }

                // query them all in parallel
                util::spawn("MPRIS state query", initial_query(mpris.clone(), name));
            }

            Ok(())
        });

        rv
    }

    fn handle_mpris_update(&self, msg : &Message, p : &PropertiesPropertiesChanged) {
        let src = msg.sender().unwrap();
        assert!(src.starts_with(':'));
        self.0.take_in_some(|inner| {
            let mut new = None;
            let found = inner.players.iter_mut().find(|p| *p.owner == *src);
            let player = found.unwrap_or_else(|| {
                let player = new.get_or_insert(Player::default());
                player.owner = (*src).to_owned();
                player
            });
            for (prop, value) in &p.changed_properties {
                match prop.as_str() {
                    "PlaybackStatus" => {
                        value.as_str().map(|status| {
                            player.playing = PlayState::parse(status);
                        });
                    }
                    "Metadata" => {
                        player.meta = dbus_util::read_hash_map(&value.0);
                    }
                    _ => ()
                }
            }
            if let Some(new) = new {
                inner.players.push(new);
            }
            inner.interested.notify_data();
        });
    }
}

pub fn read_in<F : FnOnce(&str) -> R, R>(_name : &str, target : &str, key : &str, rt : &Runtime, f : F) -> R {
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
                    Some(PlayState::Playing) => f("Playing"),
                    Some(PlayState::Paused) => f("Paused"),
                    Some(PlayState::Stopped) => f("Stopped"),
                    None => f(""),
                };
            }

            if let Some(player) = player {
                match field {
                    "player.name" => {
                        f(&player.name.get(skip..).unwrap_or_default())
                    }
                    "length" => {
                        match player.meta.as_ref().and_then(|md| md.get("mpris:length")).and_then(|v| v.as_u64()) {
                            Some(len) => f(&format!("{}.{:06}", len / 1_000_000, len % 1_000_000)),
                            None => f(""),
                        }
                    }
                    _ if field.contains('.') => {
                        let real_field = field.replace('.', ":");
                        f(player.meta.as_ref().and_then(|md| md.get(&real_field).or(md.get(field))).and_then(|v| v.as_str()).unwrap_or(""))
                    }
                    // See http://www.freedesktop.org/wiki/Specifications/mpris-spec/metadata for
                    // a list of valid names
                    _ => {
                        let xeasm = format!("xesam:{}", field);
                        let mut tmp = String::new();
                        let value = player.meta.as_ref()
                            .and_then(|md| md.get(&xeasm))
                            .and_then(|variant| {
                                // Support both strings and lists of strings
                                variant.0.as_str()
                                    .or_else(|| variant.0.as_iter().map(|iter| {
                                        for v in iter.filter_map(|e| e.as_str()) {
                                            tmp.push_str(v);
                                            tmp.push_str(", ");
                                        }
                                        tmp.pop(); tmp.pop();
                                        tmp.as_str()
                                    }))
                            })
                            .unwrap_or("");
                        f(value)
                    }
                }
            } else {
                debug!("No media players found");
                f("")
            }
        })
    })
}

pub fn read_focus_list<F : FnMut(bool, Rc<IterationItem>)>(rt : &Runtime, mut f : F) {
    let players : Vec<_> = DATA.with(|cell| {
        let state = cell.get_or_init(MediaPlayer2::new);
        state.0.take_in_some(|inner| {
            inner.interested.add(rt);

            let skip = "org.mpris.MediaPlayer2.".len();
            inner.players.iter().map(|p| (p.name.get(skip..).unwrap_or_default().to_owned(), p.playing == Some(PlayState::Playing))).collect()
        }).unwrap_or_default()
    });

    for (player, playing) in players {
        f(playing, Rc::new(IterationItem::MediaPlayer2 { target : player }));
    }
}

pub fn write(_name : &str, target : &str, key : &str, command : String, _rt : &Runtime) {
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

            let name = player.owner.clone();

            util::spawn("MPRIS click", async move {
                let dbus = get_dbus();
                let player = Proxy::new(&name, "/org/mpris/MediaPlayer2", Duration::from_secs(10), &dbus.local);
                match command.as_str() {
                    "Next" | "Previous" | "Pause" | "PlayPause" | "Stop" | "Play" => {
                        player.method_call("org.mpris.MediaPlayer2.Player", command, ()).await?;
                    }
                    // TODO seek, volume?
                    "Raise" | "Quit" => {
                        player.method_call("org.mpris.MediaPlayer2", command, ()).await?;
                    }
                    _ => {
                        error!("Unknown command {}", command);
                    }
                }
                Ok(())
            });
        })
    })
}
