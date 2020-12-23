use crate::util::{Cell,spawn};
use crate::dbus::get as get_dbus;
use crate::dbus as dbus_util;
use crate::data::Module;
use crate::icon;
use crate::item::{Item,Render,EventSink,PopupDesc};
use crate::state::{Runtime,NotifierList};
use dbus::arg::{ArgType,RefArg,Variant};
use dbus::channel::{MatchingReceiver,Token,Sender};
use dbus::message::{MatchRule,Message};
use dbus::nonblock::Proxy;
use dbus::nonblock::stdintf::org_freedesktop_dbus::Properties;
use dbus::nonblock::stdintf::org_freedesktop_dbus::PropertiesPropertiesChanged;
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

#[derive(Debug,Default)]
struct TrayItem {
    owner : String,
    path : String,
    is_kde : bool,

    id : String,
    title : String,
    icon : String,
    icon_path : String,
    menu_path : String,
    menu : Rc<TrayPopupMenu>,
}

#[derive(Debug,Default)]
struct Tray {
    items : Cell<Vec<TrayItem>>,
    interested : Cell<NotifierList>,
}

fn init() -> Tray {
    spawn("Tray StatusNotifierWatcher", async move {
        let dbus = get_dbus();

        let mut reg_db : Vec<String> = Vec::new();

        let mut snw_match = MatchRule::new_method_call();
        snw_match.path = Some("/StatusNotifierWatcher".into());
        dbus.local.start_receive(snw_match, Box::new(move |msg : Message, local| {
            let mut rsp = dbus::channel::default_reply(&msg);
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
                                    rsp = Some(msg.return_with_args((Variant(&reg_db),)));
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
                                        ("RegisteredStatusNotifierItems".into(), Variant(&reg_db)),
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
            let _ = rsp.map(|rsp| local.send(rsp));
            true
        }));

        spawn("Tray enumeration (KDE)", init_snw(true));
        spawn("Tray enumeration (freedesktop)", init_snw(false));
        Ok(())
    });

    Tray::default()
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

    let prop_rule = MatchRule::new_signal("org.freedesktop.DBus.Properties", "PropertiesChanged");
    dbus.local.start_receive(prop_rule, Box::new(move |msg, _local| {
        handle_item_update_msg(msg);
        true
    }));

    let mut item_rule = MatchRule::new_signal(snw_path, "x");
    item_rule.member = None;
    dbus.local.add_match_no_cb(&item_rule.match_str()).await?;

    dbus.local.start_receive(item_rule, Box::new(move |msg : Message, _local| {
        let item : String = match msg.get1() {
            Some(s) => s,
            None => return true,
        };
        match msg.member().as_deref().unwrap_or("") {
            "StatusNotifierItemRegistered" => do_add_item(is_kde, item),
            "StatusNotifierItemUnregistered" => do_del_item(item),
            _ => ()
        }
        true
    }));

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

        let (owner, path) = match item.find('/') {
            Some(pos) => (&item[..pos], &item[pos..]),
            None => return Ok(()),
        };

        let mut item = TrayItem::default();
        item.owner = owner.into();
        item.path = path.into();
        item.is_kde = is_kde;
        if DATA.with(|cell| {
            let tray = cell.get();
            let tray = tray.as_ref().unwrap();
            tray.items.take_in(|items| {
                for item in &*items {
                    if item.owner == owner && item.path == path {
                        return true;
                    }
                }
                items.push(item);
                false
            })
        }) {
            return Ok(());
        }

        let mut notify_rule = MatchRule::new_signal("org.freedesktop.DBus.Properties", "PropertiesChanged");
        notify_rule.path = Some(path.into());
        notify_rule.sender = Some(owner.into());
        dbus.local.add_match_no_cb(&notify_rule.match_str()).await?;

        let proxy = Proxy::new(owner, path, Duration::from_secs(10), &dbus.local);
        let props = proxy.get_all(&sni_path).await?;

        handle_item_update(owner, path, &props);
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
            items.retain(|item| item.owner != owner || item.path != path);
        });
        tray.interested.take().notify_data();
    });
}

fn handle_item_update_msg(msg : Message) {
    let src = msg.sender().unwrap();
    let path = msg.path().unwrap();
    let p = match msg.read_all::<PropertiesPropertiesChanged>() {
        Ok(p) if p.interface_name == "org.kde.StatusNotifierItem" => p,
        Ok(p) if p.interface_name == "org.freedesktop.StatusNotifierItem" => p,
        _ => return,
    };
    handle_item_update(&src, &path, &p.changed_properties);
}

fn handle_item_update(owner : &str, path : &str, props : &HashMap<String, Variant<Box<dyn RefArg + 'static>>>) {
    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            for item in items {
                if item.owner != owner || item.path != path {
                    continue;
                }

                for (key, value) in props {
                    match key.as_str() {
                        "Id" => value.as_str().map(|v| item.id = v.into()),
                        "Title" => value.as_str().map(|v| item.title = v.into()),
                        "IconName" => value.as_str().map(|v| item.icon = v.into()),
                        "IconThemePath" => value.as_str().map(|v| item.icon_path = v.into()),
                        "Menu" => value.as_str().map(|v| item.menu_path = v.into()),
                        _ => None
                    };
                }
            }
        });
        tray.interested.take().notify_data();
    });
}

#[derive(Clone,Debug)]
pub struct TrayPopup {
    owner : String,
    menu_path : String,
    title : String,
    menu : Rc<TrayPopupMenu>,
    rendered_ids : Vec<(f64, f64, i32)>,
}

#[derive(Debug,Default)]
struct TrayPopupMenu {
    fresh : Cell<bool>,
    dbus_token : Cell<Option<Token>>,
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
        if let Some(token) = self.dbus_token.replace(None) {
            let dbus = get_dbus();
            dbus.local.stop_receive(token);
        }
    }
}

async fn refresh_menu(menu : Rc<TrayPopupMenu>, owner : String, menu_path : String) -> Result<(), Box<dyn Error>> {
    let dbus = get_dbus();
    let proxy = Proxy::new(&owner, &menu_path, Duration::from_secs(10), &dbus.local);
    let _ : (bool,) = proxy.method_call("com.canonical.dbusmenu", "AboutToShow", (0i32,)).await?;

    if menu.dbus_token.take_in_some(|_| ()).is_none() {
        let mut rule = MatchRule::new_signal("com.canonical.dbusmenu", "ItemsPropertiesUpdated");
        rule.path = Some(menu_path.clone().into());
        rule.sender = Some(owner.clone().into());
        dbus.local.add_match_no_cb(&rule.match_str()).await?;

        rule.member = Some("LayoutUpdated".into());
        dbus.local.add_match_no_cb(&rule.match_str()).await?;

        rule.member = None;

        let weak = Rc::downgrade(&menu);
        let owner = owner.clone();
        let menu_path = menu_path.clone();
        let token = dbus.local.start_receive(rule, Box::new(move |_msg, _local| {
            if let Some(menu) = weak.upgrade() {
                let owner = owner.clone();
                let menu_path = menu_path.clone();
                if menu.fresh.replace(false) {
                    spawn("Tray menu refresh", refresh_menu(menu, owner, menu_path));
                }
                true
            } else {
                false
            }
        }));
        menu.dbus_token.set(Some(token));
    }

    // ? MatchRule::new_signal("com.canonical.dbusmenu", "ItemActivationRequested");

    let (_rev, (_id, _props, contents)) : (u32, (i32, HashMap<String, Variant<Box<dyn RefArg>>>, Vec<Variant<Box<dyn RefArg>>>))
        = proxy.method_call("com.canonical.dbusmenu", "GetLayout", (0i32, -1i32, &["type", "label", "visible", "enabled"] as &[&str])).await?;

    let mut items = Vec::new();
    TrayPopupMenu::add_items(&mut items, &mut contents.into_iter(), 0);
    menu.items.set(items);
    menu.interested.take().notify_data();
    menu.fresh.set(true);

    Ok(())
}

impl TrayPopup {
    pub fn get_size(&self) -> (i32, i32) {
        let tmp = cairo::RecordingSurface::create(cairo::Content::ColorAlpha, None).unwrap();
        let ctx = cairo::Context::new(&tmp);
        let layout = pangocairo::create_layout(&ctx).unwrap();
        layout.set_text(&self.title);
        let psize = layout.get_size();
        let mut size = (pango::units_to_double(psize.0), pango::units_to_double(psize.1));
        if !self.menu.fresh.get() && self.menu.dbus_token.take_in_some(|_| ()).is_none() {
            // need to kick off the first refresh
            let menu = self.menu.clone();
            let owner = self.owner.clone();
            let menu_path = self.menu_path.clone();

            spawn("Tray menu population", refresh_menu(menu, owner, menu_path));
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
        layout.set_text(&self.title);
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
            let owner = self.owner.clone();
            let menu_path = self.menu_path.clone();
            spawn("Tray menu click", async move {
                let dbus = get_dbus();
                let proxy = Proxy::new(owner, menu_path, Duration::from_secs(10), &dbus.local);
                proxy.method_call("com.canonical.dbusmenu", "Event", (id, "clicked", Variant(0i32), ts as u32)).await?;
                Ok(())
            });
        }
    }
}

pub fn show(ctx : &Render, ev : &mut EventSink, spacing : f64) {
    DATA.with(|cell| {
        let tray = cell.get_or_init(init);
        tray.interested.take_in(|interest| interest.add(&ctx.runtime));
        tray.items.take_in(|items| {
            ctx.cairo.rel_move_to(spacing, 0.0);
            for item in items {
                let x0 = ctx.cairo.get_current_point().0;
                let mut done = false;
                if !done && item.icon_path != "" {
                    let icon = format!("{}/{}.svg", item.icon_path, item.icon);
                    if icon::render(ctx, &icon).is_ok() {
                        done = true;
                    }
                }
                if !done && item.icon_path != "" {
                    let icon = format!("{}/{}.png", item.icon_path, item.icon);
                    if icon::render(ctx, &icon).is_ok() {
                        done = true;
                    }
                }
                if !done && icon::render(ctx, &item.icon).is_ok() {
                    done = true;
                }
                if !done {
                    let item : Item = Module::Value { value : Cell::new(item.title.clone()) }.into();
                    item.render(ctx);
                }
                let x1 = ctx.cairo.get_current_point().0;
                let mut es = EventSink::from_tray(item.owner.clone(), item.path.clone());
                es.offset_clamp(0.0, x0, x1);
                es.add_hover(x0, x1, PopupDesc::Tray(TrayPopup {
                    owner : item.owner.clone(),
                    title : item.title.clone(),
                    menu_path : item.menu_path.clone(),
                    menu : item.menu.clone(),
                    rendered_ids : Vec::new(),
                }));
                ev.merge(es);
                ctx.cairo.rel_move_to(spacing, 0.0);
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
                if item.owner != owner || item.path != path {
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
