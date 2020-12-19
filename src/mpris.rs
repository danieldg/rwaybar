use crate::data::Cell;
use crate::dbus::get as get_dbus;
use crate::state::Runtime;
use dbus::channel::MatchingReceiver;
use dbus::message::{MatchRule,Message};
use dbus::nonblock::Proxy;
use dbus::nonblock::stdintf::org_freedesktop_dbus::Properties;
use dbus::nonblock::stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged;
use dbus::arg::Variant;
use dbus::arg::RefArg;
use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;
use std::time::Duration;
use log::debug;

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
    redraw : Option<Rc<tokio::sync::Notify>>,
}

#[derive(Debug)]
pub struct MediaPlayer2(Cell<Option<Inner>>);

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
        inner.redraw.take().map(|n| n.notify_one());
    });

    Ok(())
}

impl MediaPlayer2 {
    pub fn new() -> Rc<Self> {
        let rv = Rc::new(MediaPlayer2(Cell::new(Some(Inner::default()))));

        let mpris = rv.clone();
        tokio::task::spawn_local(async move {
            let dbus = get_dbus();
            let meta_bus = Proxy::new("org.freedesktop.DBus", "/org/freedesktop/DBus", Duration::from_secs(10), &dbus.local);

            // TODO we need to watch for new players and player exits
            //
            // Could use arg0namespace as shown in https://dbus.freedesktop.org/doc/dbus-specification.html#message-bus-routing-match-rules

            // Watch for property updates to any active mpris player and update our internal map
            let mut mpris_prop_rule = MatchRule::new_signal("org.freedesktop.DBus.Properties", "PropertiesChanged");
            mpris_prop_rule.path = Some("/org/mpris/MediaPlayer2".into());
            dbus.local.add_match_no_cb(&mpris_prop_rule.match_str()).await?;
            let target = mpris.clone();
            dbus.local.start_receive(mpris_prop_rule, Box::new(move |msg, _local| {
                target.handle_mpris_update(msg);
                true
            }));

            // Now that property watching is active, populate our map with initial values for all active players
            let (names,) : (Vec<String>,) = meta_bus.method_call("org.freedesktop.DBus", "ListNames", ()).await?;
            for name in names {
                if !name.starts_with("org.mpris.MediaPlayer2.") {
                    continue;
                }

                // query them all in parallel
                tokio::task::spawn_local(initial_query(mpris.clone(), name));
            }

            // the start_recieve callback handles most of the update logic
            // (we could use the Token returned there to cancel)
            Ok::<(), Box<dyn Error>>(())
        });

        rv
    }

    fn handle_mpris_update(&self, msg : Message) {
        let src = msg.sender().unwrap();
        assert!(src.starts_with(':'));
        let p = match msg.read_all::<PropertiesPropertiesChanged>() {
            Ok(p) if p.interface_name == "org.mpris.MediaPlayer2.Player" => p,
            _ => return,
        };
        self.0.take_in_some(|inner| {
            let mut new = None;
            let found = inner.players.iter_mut().find(|p| *p.owner == *src);
            let player = found.unwrap_or_else(|| {
                let player = new.get_or_insert(Player::default());
                player.owner = (*src).to_owned();
                player
            });
            for (prop, value) in p.changed_properties {
                match prop.as_str() {
                    "PlaybackStatus" => {
                        value.as_str().map(|status| {
                            player.playing = PlayState::parse(status);
                        });
                    }
                    "Metadata" => {
                        if let Some(iter) = value.0.as_iter() {
                            let mut map = HashMap::new();
                            let mut k = None;
                            for i in iter {
                                match k.take() {
                                    None => {
                                        k = i.as_str().map(String::from);
                                    }
                                    Some(k) => {
                                        map.insert(k, Variant(i.box_clone()));
                                    }
                                }
                            }
                            assert!(k.is_none());
                            player.meta = Some(map);
                        }
                    }
                    _ => ()
                }
            }
            if let Some(new) = new {
                inner.players.push(new);
            }
            inner.redraw.take().map(|n| n.notify_one());
        });
    }

    pub fn read_in<F : FnOnce(&str) -> R, R>(&self, _name : &str, key : &str, rt : &Runtime, f : F) -> R {
        self.0.take_in(|oi| {
            oi.as_mut().map(|inner| inner.redraw = Some(rt.notify.clone()));

            if let Some(player) = oi.as_ref()
                .and_then(|inner| {
                    inner.players.iter()
                        .filter(|p| p.playing == Some(PlayState::Playing))
                        .chain(inner.players.iter().filter(|p| p.playing == Some(PlayState::Paused)))
                        .chain(inner.players.iter())
                        .next()
                })
            {
                match key {
                    "length" => {
                        match player.meta.as_ref().and_then(|md| md.get("mpris:length")).and_then(|v| v.as_u64()) {
                            Some(len) => f(&format!("{}.{:06}", len / 1_000_000, len % 1_000_000)),
                            None => f(""),
                        }
                    }
                    _ if key.contains(':') => {
                        f(player.meta.as_ref().and_then(|md| md.get(key)).and_then(|v| v.as_str()).unwrap_or(""))
                    }
                    // See http://www.freedesktop.org/wiki/Specifications/mpris-spec/metadata for
                    // a list of valid names
                    _ => {
                        let xeasm = format!("xesam:{}", key);
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
    }
}
