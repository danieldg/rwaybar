use crate::util::{Cell,spawn};
use crate::dbus::get as get_dbus;
use crate::dbus::SigWatcherToken;
use crate::dbus as dbus_util;
use crate::data::Module;
use crate::icon;
use crate::item::{Item,Render,EventSink,PopupDesc};
use crate::state::{Runtime,NotifierList};
use dbus::arg::{ArgType,RefArg,Variant};
use dbus::channel::{MatchingReceiver,Sender};
use dbus::message::{MatchRule,Message};
use dbus::nonblock::LocalConnection;
use dbus::nonblock::Proxy;
use dbus::nonblock::stdintf::org_freedesktop_dbus::Properties;
use dbus::nonblock::stdintf::org_freedesktop_dbus::RequestNameReply;
use once_cell::unsync::OnceCell;
use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;
use std::time::{SystemTime,Duration,UNIX_EPOCH};
use log::{debug,warn};

thread_local! {
    static DATA : OnceCell<Tray> = Default::default();
}

#[derive(Debug)]
struct TrayItem {
    owner : Rc<str>,
    path : Rc<str>,
    is_kde : bool,

    id : String,
    title : Option<Rc<str>>,
    icon : Box<str>,
    icon_path : Box<str>,
    menu : Rc<TrayPopupMenu>,
}

impl Drop for TrayItem {
    fn drop(&mut self) {
        let sni_path = if self.is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
        let mut rule = MatchRule::new_signal(sni_path, "x");
        rule.member = None;
        rule.path = Some((&*self.path).into());
        rule.sender = Some((&*self.owner).into());
        let rule = rule.match_str();
        spawn("TrayItem cleanup", async move {
            let dbus = get_dbus();
            dbus.local.remove_match_no_cb(&rule).await?;
            Ok(())
        });
    }
}

#[derive(Debug,Default)]
struct Tray {
    reg_db : Cell<Vec<String>>,
    items : Cell<Vec<TrayItem>>,
    interested : Cell<NotifierList>,
}

impl Tray {
    fn init() -> Tray {
        spawn("Tray StatusNotifierWatcher", async move {
            let dbus = get_dbus();

            let mut snw_match = MatchRule::new_method_call();
            snw_match.path = Some("/StatusNotifierWatcher".into());
            dbus.local.start_receive(snw_match, Box::new(move |msg : Message, local| {
                DATA.with(|cell| {
                    let tray = cell.get();
                    let tray = tray.as_ref().unwrap();
                    tray.handle_snw(msg, local);
                });

                true
            }));

            dbus.add_name_watcher(move |name, old, _new, dbus| {
                if old.is_empty() {
                    // we don't care about adds
                    return;
                }

                DATA.with(|cell| {
                    let tray = cell.get();
                    let tray = tray.as_ref().unwrap();
                    tray.reg_db.take_in(|reg_db| {
                        reg_db.retain(|path| {
                            if let Some(pos) = path.find('/') {
                                let owner = &path[..pos];
                                if old == owner || name == owner {
                                    for &iface in &["org.kde.StatusNotifierWatcher", "org.freedesktop.StatusNotifierWatcher"] {
                                        dbus.local.send(Message::new_signal("/StatusNotifierWatcher", iface, "StatusNotifierItemUnregistered")
                                            .unwrap().append1(&path)).unwrap();
                                    }
                                    return false;
                                }
                            }
                            true
                        });
                    });
                });
            }).await;

            spawn("Tray enumeration (KDE)", init_snw(true));
            spawn("Tray enumeration (freedesktop)", init_snw(false));
            Ok(())
        });

        Tray::default()
    }

    fn handle_snw(&self, msg : Message, local : &LocalConnection) {
        let mut rsp = dbus::channel::default_reply(&msg);
        self.reg_db.take_in(|reg_db| {
            match msg.interface().as_deref() {
                Some(iface @ "org.kde.StatusNotifierWatcher") |
                Some(iface @ "org.freedesktop.StatusNotifierWatcher") => {
                    match msg.member().as_deref() {
                        Some("RegisterStatusNotifierItem") => {
                            if let Some(path) = msg.get1::<String>() {
                                if path.starts_with('/') {
                                    let service = format!("{}{}", msg.sender().unwrap(), path);
                                    local.send(Message::new_signal("/StatusNotifierWatcher", iface, "StatusNotifierItemRegistered").unwrap()
                                        .append1(&service)).unwrap();
                                    reg_db.push(service);
                                    rsp = Some(msg.return_with_args(()));
                                } else if path.starts_with(':') {
                                    // kde uses this style
                                    let service = format!("{}/StatusNotifierItem", path);
                                    local.send(Message::new_signal("/StatusNotifierWatcher", iface, "StatusNotifierItemRegistered").unwrap()
                                        .append1(&service)).unwrap();
                                    reg_db.push(service);
                                    rsp = Some(msg.return_with_args(()));
                                } else {
                                    warn!("Unknown RegisterStatusNotifierItem from {:?}: {}", msg.sender(), path);
                                }
                            }
                        }
                        Some("RegisterStatusNotifierHost") => {
                            let sig = Message::new_signal("/StatusNotifierWatcher", iface, "StatusNotifierHostRegistered").unwrap();
                            local.send(sig).unwrap();
                            rsp = Some(msg.return_with_args(()));
                        }
                        _ => {}
                    }
                }
                Some("org.freedesktop.DBus.Properties") => {
                    match msg.member().as_deref() {
                        Some("Get") => {
                            let (mut iface, prop) = msg.get2::<String,String>();
                            match iface.as_deref() {
                                Some("org.kde.StatusNotifierWatcher") |
                                Some("org.freedesktop.StatusNotifierWatcher") => {}
                                _ => iface = None,
                            }
                            match (iface, prop.as_deref()) {
                                (Some(_), Some("RegisteredStatusNotifierItems")) => {
                                    rsp = Some(msg.return_with_args((Variant(&*reg_db),)));
                                }
                                (Some(_), Some("IsStatusNotifierHostRegistered")) => {
                                    rsp = Some(msg.return_with_args((Variant(true),)));
                                }
                                (Some(_), Some("ProtocolVersion")) => {
                                    rsp = Some(msg.return_with_args((Variant(0i32),)));
                                }
                                _ => {}
                            }
                        }
                        Some("GetAll") => {
                            let iface = msg.get1::<String>();
                            match iface.as_deref() {
                                Some("org.kde.StatusNotifierWatcher") |
                                Some("org.freedesktop.StatusNotifierWatcher") => {
                                    let rv : Vec<(String, Variant<&dyn RefArg>)> = vec![
                                        ("RegisteredStatusNotifierItems".into(), Variant(&*reg_db)),
                                        ("IsStatusNotifierHostRegistered".into(), Variant(&true)),
                                        ("ProtocolVersion".into(), Variant(&0i32)),
                                    ];
                                    // if only Dict implemented RefArg...
                                    let rv : HashMap<_,_> = rv.into_iter().collect();
                                    rsp = Some(msg.method_return().append_ref(&[rv]));
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
        let _ = rsp.map(|rsp| local.send(rsp));
    }
}

async fn init_snw(is_kde : bool) -> Result<(), Box<dyn Error>> {
    let who = if is_kde { "kde" } else { "freedesktop" };
    let snw_path = if is_kde { "org.kde.StatusNotifierWatcher" } else { "org.freedesktop.StatusNotifierWatcher" };
    let dbus = get_dbus();
    let name = format!("org.{}.StatusNotifierHost-{}", who, std::process::id());
    match dbus.local.request_name(&name, false, false, true).await {
        Ok(RequestNameReply::PrimaryOwner) => {}
        _ => {
            warn!("Could not register as tray ({})", who);
            return Ok(());
        }
    }

    match dbus.local.request_name(snw_path, true, false, false).await {
        Ok(RequestNameReply::PrimaryOwner) => {
            let sig = Message::new_signal("/StatusNotifierWatcher", snw_path, "StatusNotifierHostRegistered")?;
            let _ = dbus.local.send(sig);
        }
        Ok(_) => {}
        _ => {
            warn!("Could not register as StatusNotifierWatcher, tray may not work ({})", who);
        }
    }

    dbus.add_property_change_watcher(move |msg, ppc, _dbus| {
        let src = msg.sender().unwrap();
        let path = msg.path().unwrap();
        match &*ppc.interface_name {
            "org.kde.StatusNotifierItem" => {}
            "org.freedesktop.StatusNotifierItem" => {}
            _ => return,
        }
        handle_item_update(&src, &path, &ppc.changed_properties);
    }).await;

    let mut item_rule = MatchRule::new_signal(snw_path, "x");
    item_rule.member = None;
    dbus.local.add_match_no_cb(&item_rule.match_str()).await?;

    dbus.add_signal_watcher(move |msg : &Message, _dbus| {
        if msg.interface().as_deref() != Some(snw_path) {
            return;
        }
        let item : String = match msg.get1() {
            Some(s) => s,
            None => return,
        };
        match msg.member().as_deref().unwrap_or("") {
            "StatusNotifierItemRegistered" => do_add_item(is_kde, item),
            "StatusNotifierItemUnregistered" => do_del_item(item),
            _ => ()
        }
    });

    let sni_path = if is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
    dbus.add_signal_watcher(move |msg, _local| {
        if msg.interface().as_deref() != Some(sni_path) {
            return;
        }
        let path = msg.path().unwrap().into_static();
        let owner = msg.sender().unwrap().into_static();
        spawn("Tray item refresh", async move {
            let dbus = get_dbus();
            let proxy = Proxy::new(&*owner, &*path, Duration::from_secs(10), &dbus.local);
            let props = proxy.get_all(&sni_path).await?;

            handle_item_update(&owner, &path, &props);
            Ok(())
        });
    });

    let watcher = Proxy::new(snw_path, "/StatusNotifierWatcher", Duration::from_secs(10), &dbus.local);
    watcher.method_call(snw_path, "RegisterStatusNotifierHost", (&name,)).await?;

    let items : Vec<String> = watcher.get(snw_path, "RegisteredStatusNotifierItems").await?;

    for item in items {
        do_add_item(is_kde, item);
    }

    Ok(())
}

fn do_add_item(is_kde : bool, item : String) {
    let sni_path = if is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
    spawn("Tray item inspection", async move {
        let dbus = get_dbus();

        let (owner, path) : (Rc<str>, Rc<str>) = match item.find('/') {
            Some(pos) => (Rc::from(&item[..pos]), Rc::from(&item[pos..])),
            None => return Ok(()),
        };

        let mut rule = MatchRule::new_signal(sni_path, "x");
        rule.member = None; // all signals with the below path/sender
        rule.path = Some((&*path).into());
        rule.sender = Some((&*owner).into());
        dbus.local.add_match_no_cb(&rule.match_str()).await?;
        let item = TrayItem {
            owner : owner.clone(),
            path : path.clone(),
            is_kde,
            id : String::new(),
            title : None,
            icon : Default::default(),
            icon_path : Default::default(),
            menu : Rc::new(TrayPopupMenu {
                owner : owner.clone(),
                menu_path : Default::default(),
                fresh : Default::default(),
                dbus_token : Default::default(),
                items : Default::default(),
                interested : Default::default(),
            }),
        };

        if DATA.with(|cell| {
            let tray = cell.get();
            let tray = tray.as_ref().unwrap();
            tray.items.take_in(|items| {
                for item in &*items {
                    if &*item.owner == &*owner && item.path == path {
                        return true;
                    }
                }
                items.push(item);
                false
            })
        }) {
            return Ok(());
        }

        let proxy = Proxy::new(&*owner, &*path, Duration::from_secs(10), &dbus.local);
        let props = proxy.get_all(&sni_path).await?;

        handle_item_update(&owner, &path, &props);
        Ok(())
    });
}

fn do_del_item(item : String) {
    let (owner, path) = match item.find('/') {
        Some(pos) => (&item[..pos], &item[pos..]),
        None => return,
    };

    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            items.retain(|item| &*item.owner != owner || &*item.path != path);
        });
        tray.interested.take().notify_data("tray:item");
    });
}

fn handle_item_update(owner : &str, path : &str, props : &HashMap<String, Variant<Box<dyn RefArg + 'static>>>) {
    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            for item in items {
                if &*item.owner != owner || &*item.path != path {
                    continue;
                }

                for (key, value) in props {
                    match key.as_str() {
                        "Id" => value.as_str().map(|v| item.id = v.into()),
                        "Title" => value.as_str().map(|v| item.title = Some(v.into())),
                        "IconName" => value.as_str().map(|v| item.icon = v.into()),
                        "IconThemePath" => value.as_str().map(|v| item.icon_path = v.into()),
                        "Menu" => value.as_str().map(|v| {
                            if item.menu.menu_path.take_in(|mp| {
                                match mp.as_deref() {
                                    None => {
                                        *mp = Some(v.into());
                                        false
                                    }
                                    Some(mp) if mp == v => false,
                                    _ => true
                                }
                            }) {
                                item.menu = Rc::new(TrayPopupMenu {
                                    owner : item.owner.clone(),
                                    menu_path : Cell::new(Some(v.into())),
                                    fresh : Default::default(),
                                    dbus_token : Default::default(),
                                    items : Default::default(),
                                    interested : Default::default(),
                                });
                            }
                        }),
                        _ => None
                    };
                }
            }
        });
        tray.interested.take().notify_data("tray:item");
    });
}

#[derive(Clone,Debug)]
pub struct TrayPopup {
    owner : Rc<str>,
    title : Option<Rc<str>>,
    menu : Rc<TrayPopupMenu>,
    rendered_ids : Vec<(f64, f64, i32)>,
}

#[derive(Debug)]
struct TrayPopupMenu {
    owner : Rc<str>,
    menu_path : Cell<Option<Rc<str>>>,
    fresh : Cell<bool>,
    dbus_token : Cell<SigWatcherToken>,
    items : Cell<Vec<MenuItem>>,
    interested : Cell<NotifierList>,
}

#[derive(Debug,Default)]
struct MenuItem {
    id : i32,
    depth : u32,
    visible : bool,
    enabled : bool,
    is_sep : bool,
    label : String,
}

impl TrayPopupMenu {
    fn add_items<I>(items : &mut Vec<MenuItem>, iter : &mut I, depth : u32)
        where I : Iterator, I::Item : RefArg
    {
        let mut item = MenuItem::default();
        for v in iter {
            let mut iter = match v.as_iter() { Some(i) => i, None => continue };
            if v.arg_type() == ArgType::Variant {
                Self::add_items(items, &mut iter, depth);
                continue;
            }
            for (i, value) in iter.enumerate() {
                match i {
                    0 => { value.as_i64().map(|id| item.id = id as i32); }
                    1 => {
                        let props = dbus_util::read_hash_map(&value);
                        let props = match props { Some(i) => i, None => continue };
                        props.get("label").and_then(|v| v.as_str())
                            .map(|label| item.label = label.to_owned());
                        item.visible = props.get("visible").and_then(|v| v.as_i64()) != Some(0);
                        item.enabled = props.get("enabled").and_then(|v| v.as_i64()) != Some(0);
                        if !item.visible {
                            // Note: this is needed to correctly hide the sub-menu
                            break;
                        }
                        props.get("type").and_then(|v| v.as_str())
                            .map(|v| match v {
                                "separator" => item.is_sep = true,
                                _ => debug!("Unknown menu item type: {}", v),
                            });
                        item.depth = depth;
                        items.push(item);
                        item = MenuItem::default();
                    }
                    2 => {
                        match value.as_iter() {
                            Some(mut i) => Self::add_items(items, &mut i, depth + 1),
                            None => {}
                        }
                    }
                    _ => break,
                }
            }
        }
    }
}

impl Drop for TrayPopupMenu {
    fn drop(&mut self) {
        let mut token = self.dbus_token.take();
        if !token.is_active() {
            return;
        }
        get_dbus().stop_signal_watcher(&mut token);

        if let Some(menu_path) = self.menu_path.take_in(|m| m.clone()) {
            let mut rule = MatchRule::new_signal("com.canonical.dbusmenu", "ItemsPropertiesUpdated");
            rule.path = Some((&*menu_path).into());
            rule.sender = Some((&*self.owner).into());
            let rule1 = rule.match_str();
            rule.member = Some("LayoutUpdated".into());
            let rule2 = rule.match_str();

            spawn("TrayPopupMenu cleanup", async move {
                let dbus = get_dbus();
                dbus.local.remove_match_no_cb(&rule1).await?;
                dbus.local.remove_match_no_cb(&rule2).await?;
                Ok(())
            });
        }
    }
}

async fn refresh_menu(menu : Rc<TrayPopupMenu>) -> Result<(), Box<dyn Error>> {
    let menu_path = match menu.menu_path.take_in(|m| m.clone()) {
        Some(mp) => mp,
        None => return Ok(())
    };
    let dbus = get_dbus();
    let proxy = Proxy::new(&*menu.owner, &*menu_path, Duration::from_secs(10), &dbus.local);
    let _ : (bool,) = proxy.method_call("com.canonical.dbusmenu", "AboutToShow", (0i32,)).await?;

    if menu.dbus_token.take_in(|t| !t.is_active()) {
        let mut rule = MatchRule::new_signal("com.canonical.dbusmenu", "ItemsPropertiesUpdated");
        rule.path = Some((&*menu_path).into());
        rule.sender = Some((&*menu.owner).into());
        dbus.local.add_match_no_cb(&rule.match_str()).await?;

        rule.member = Some("LayoutUpdated".into());
        dbus.local.add_match_no_cb(&rule.match_str()).await?;

        rule.member = None;

        let weak = Rc::downgrade(&menu);
        let token = dbus.add_signal_watcher(move |msg, _local| {
            if let Some(menu) = weak.upgrade() {
                if msg.interface().as_deref() != Some("com.canonical.dbusmenu") ||
                    msg.member().as_deref() != Some("ItemsPropertiesUpdated") ||
                    msg.sender().as_deref() != Some(&*menu.owner) ||
                    menu.menu_path.take_in(|mp| mp.as_deref() != msg.path().as_deref())
                {
                    return;
                }
                if menu.fresh.replace(false) {
                    spawn("Tray menu refresh", refresh_menu(menu));
                }
            }
        });
        menu.dbus_token.set(token);
    }

    // ? MatchRule::new_signal("com.canonical.dbusmenu", "ItemActivationRequested");

    let (_rev, (_id, _props, contents)) : (u32, (i32, HashMap<String, Variant<Box<dyn RefArg>>>, Vec<Variant<Box<dyn RefArg>>>))
        = proxy.method_call("com.canonical.dbusmenu", "GetLayout", (0i32, -1i32, &["type", "label", "visible", "enabled"] as &[&str])).await?;

    let mut items = Vec::new();
    TrayPopupMenu::add_items(&mut items, &mut contents.into_iter(), 0);
    menu.items.set(items);
    menu.interested.take().notify_data("tray:menu");
    menu.fresh.set(true);

    Ok(())
}

impl TrayPopup {
    pub fn get_size(&self) -> (i32, i32) {
        let tmp = cairo::RecordingSurface::create(cairo::Content::ColorAlpha, None).unwrap();
        let ctx = cairo::Context::new(&tmp);
        let layout = pangocairo::create_layout(&ctx).unwrap();
        layout.set_text(self.title.as_deref().unwrap_or_default());
        let psize = layout.get_size();
        let mut size = (pango::units_to_double(psize.0), pango::units_to_double(psize.1));
        if !self.menu.fresh.get() &&
            self.menu.dbus_token.take_in(|t| !t.is_active()) &&
            self.menu.menu_path.take_in_some(|_| ()).is_some()
        {
            // need to kick off the first refresh
            let menu = self.menu.clone();
            spawn("Tray menu population", refresh_menu(menu));
        }
        self.menu.items.take_in(|items| {
            if !items.is_empty() {
                size.1 += 9.0;
            }
            for item in items {
                if !item.visible {
                    continue;
                }
                let indent = item.depth as f64 * 20.0;
                if item.is_sep {
                    size.1 += 9.0;
                } else {
                    let layout = pangocairo::create_layout(&ctx).unwrap();
                    layout.set_text(&item.label);
                    let tsize = layout.get_size();
                    size.0 = f64::max(size.0, indent + pango::units_to_double(tsize.0));
                    size.1 += pango::units_to_double(tsize.1) + 5.0;
                }
            }
        });
        (size.0 as i32 + 4, size.1 as i32 + 4)
    }

    pub fn render(&mut self, ctx : &cairo::Context, runtime : &Runtime) -> (i32, i32) {
        let clip = ctx.clip_extents(); 
        ctx.move_to(2.0, 2.0);
        let layout = pangocairo::create_layout(&ctx).unwrap();
        layout.set_text(self.title.as_deref().unwrap_or_default());
        let psize = layout.get_size();
        pangocairo::show_layout(&ctx, &layout);
        let mut pos = 2.0 + pango::units_to_double(psize.1);
        let rendered_ids = &mut self.rendered_ids;

        self.menu.interested.take_in(|i| i.add(runtime));

        self.menu.items.take_in(|items| {
            if !items.is_empty() {
                ctx.move_to(0.0, pos + 4.0);
                ctx.line_to(clip.2, pos + 4.0);
                ctx.stroke();
                pos += 9.0;
            }
            for item in items {
                if !item.visible {
                    continue;
                }
                let indent = item.depth as f64 * 20.0;
                if item.is_sep {
                    ctx.move_to(indent + 5.0, pos + 4.0);
                    ctx.line_to(clip.2 - 5.0, pos + 4.0);
                    ctx.stroke();
                    pos += 9.0;
                } else {
                    ctx.move_to(indent + 2.0, pos);
                    let layout = pangocairo::create_layout(&ctx).unwrap();
                    layout.set_text(&item.label);
                    let tsize = layout.get_size();
                    pangocairo::show_layout(&ctx, &layout);
                    let end = pos + pango::units_to_double(tsize.1);
                    rendered_ids.push((pos, end, item.id));
                    pos = end + 5.0;
                }
            }
        });
        // This is required because pango won't report render cropping due to widths being short
        self.get_size()
    }

    pub fn button(&mut self, x : f64, y : f64, button : u32, _runtime : &mut Runtime) {
        let _ = (x, button);
        for &(min, max, id) in &self.rendered_ids {
            let ts = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
            if y < min || y > max {
                continue;
            }
            if let Some(menu_path) = self.menu.menu_path.take_in_some(|v| v.clone()) {
                let owner = self.owner.clone();
                spawn("Tray menu click", async move {
                    let dbus = get_dbus();
                    let proxy = Proxy::new(&*owner, &*menu_path, Duration::from_secs(10), &dbus.local);
                    proxy.method_call("com.canonical.dbusmenu", "Event", (id, "clicked", Variant(0i32), ts as u32)).await?;
                    Ok(())
                });
            }
        }
    }
}

pub fn show(ctx : &Render, ev : &mut EventSink, spacing : f64) {
    DATA.with(|cell| {
        let tray = cell.get_or_init(Tray::init);
        tray.interested.take_in(|interest| interest.add(&ctx.runtime));
        tray.items.take_in(|items| {
            ctx.render_pos.set(ctx.render_pos.get() + spacing);
            for item in items {
                let x0 = ctx.render_pos.get();
                let mut done = false;
                if !done && !item.icon_path.is_empty() {
                    let icon = format!("{}/{}.svg", item.icon_path, item.icon);
                    if icon::render(ctx, &icon).is_ok() {
                        done = true;
                    }
                }
                if !done && !item.icon_path.is_empty() {
                    let icon = format!("{}/{}.png", item.icon_path, item.icon);
                    if icon::render(ctx, &icon).is_ok() {
                        done = true;
                    }
                }
                if !done && icon::render(ctx, &item.icon).is_ok() {
                    done = true;
                }
                if !done {
                    let item : Item = Module::new_value(item.title.as_deref().unwrap_or_default()).into();
                    Rc::new(item).render(ctx);
                }
                let x1 = ctx.render_pos.get();
                let mut es = EventSink::from_tray(item.owner.clone(), item.path.clone());
                es.offset_clamp(0.0, x0, x1);
                es.add_hover(x0, x1, PopupDesc::Tray(TrayPopup {
                    owner : item.owner.clone(),
                    title : item.title.clone(),
                    menu : item.menu.clone(),
                    rendered_ids : Vec::new(),
                }));
                ev.merge(es);
                ctx.render_pos.set(ctx.render_pos.get() + spacing);
            }
        });
    });
}

pub fn do_click(owner : &str, path : &str, how : u32) {
    let method = match how {
        0 => "Activate",
        1 => "ContextMenu",
        2 => "SecondaryActivate",
        5 | 6 => "vertical",
        7 | 8 => "horizontal",
        _ => return,
    };
    let owner = owner.to_owned();
    let path = path.to_owned();
    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            for item in items {
                if &*item.owner != owner || &*item.path != path {
                    continue;
                }
                let sni_path = if item.is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
                let id = item.id.clone();
                spawn("Tray click", async move {
                    let dbus = get_dbus();
                    let proxy = Proxy::new(&owner, &path, Duration::from_secs(10), &dbus.local);
                    debug!("Invoking {} on {}", method, id);
                    if how < 3 {
                        proxy.method_call(sni_path, method, (0i32,0i32)).await?;
                    } else {
                        proxy.method_call(sni_path, "Scroll", (15i32, method)).await?;
                    }
                    Ok(())
                });
                return;
            }
        });
    });
}
