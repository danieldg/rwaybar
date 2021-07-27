use crate::data::{IterationItem,Value};
use crate::dbus::{DBus,SigWatcherToken};
use crate::event::EventSink;
use crate::font::render_font;
use crate::item::{Item,PopupDesc};
use crate::render::Render;
use crate::state::{Runtime,NotifierList};
use crate::util::{Cell,spawn};
use once_cell::unsync::OnceCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::error::Error;
use std::rc::Rc;
use std::time::Instant;
use std::mem::ManuallyDrop;
use std::time::{SystemTime,Duration,UNIX_EPOCH};
use log::{debug,warn};
use zvariant::Value as Variant;
use zvariant::OwnedValue;

thread_local! {
    static DATA : ManuallyDrop<OnceCell<Tray>> = Default::default();
}

#[derive(Debug)]
struct TrayItem {
    owner : Rc<str>,
    path : Rc<str>,
    is_kde : bool,

    rule : Box<str>,

    id : String,
    title : Option<Rc<str>>,
    icon : Box<str>,
    icon_path : Box<str>,
    status : Box<str>,
    tooltip : Option<Rc<str>>,
    menu : Rc<TrayPopupMenu>,
}

impl Drop for TrayItem {
    fn drop(&mut self) {
        let dbus = DBus::get_session();
        dbus.send(zbus::Message::method(
            None::<&str>,
            Some("org.freedesktop.DBus"),
            "/org/freedesktop/DBus",
            Some("org.freedesktop.DBus"),
            "RemoveMatch",
            &&*self.rule
        ).unwrap());
    }
}

#[derive(Debug,Default)]
struct Tray {
    reg_db : Cell<Vec<(String, bool)>>,
    items : Cell<Vec<TrayItem>>,
    interested : Cell<NotifierList>,
}

impl Tray {
    fn init() -> Tray {
        spawn("Tray StatusNotifierWatcher", async move {
            let dbus = DBus::get_session();

            dbus.add_api("/StatusNotifierWatcher", Box::new(move |msg| {
                DATA.with(|cell| {
                    let tray = cell.get();
                    let tray = tray.as_ref().unwrap();
                    match tray.handle_snw(&msg) {
                        Ok(()) => (),
                        Err(e) => warn!("Error in StatusNotifierWatcher: {}", e),
                    }
                });
            }));

            dbus.add_name_watcher(move |name, old, _new| {
                if old.is_empty() {
                    // we don't care about adds
                    return;
                }

                DATA.with(|cell| {
                    let tray = cell.get();
                    let tray = tray.as_ref().unwrap();
                    tray.reg_db.take_in(|reg_db| {
                        let dbus = DBus::get_session();
                        reg_db.retain(|(path, is_kde)| {
                            if let Some(pos) = path.find('/') {
                                let owner = &path[..pos];
                                if old == owner || name == owner {
                                    let iface = if *is_kde { "org.kde.StatusNotifierWatcher" } else { "org.freedesktop.StatusNotifierWatcher" };
                                    dbus.send(zbus::Message::signal(
                                        None::<&str>,
                                        None::<&str>,
                                        "/StatusNotifierWatcher",
                                        iface,
                                        "StatusNotifierItemUnregistered",
                                        &path,
                                    ).unwrap());
                                    return false;
                                }
                            }
                            true
                        });
                    });
                });
            });

            spawn("Tray enumeration (KDE)", init_snw(true));
            spawn("Tray enumeration (freedesktop)", init_snw(false));
            Ok(())
        });

        Tray::default()
    }

    fn handle_snw(&self, msg : &zbus::Message) -> zbus::Result<()> {
        let hdr = msg.header()?;
        let dbus = DBus::get_session();
        let mut rsp = zbus::Message::method_error(None::<&str>, &msg, "org.freedesktop.DBus.Error.UnknownMethod", &"Method not found")?;
        let rv = self.reg_db.take_in(|reg_db| {
            match hdr.interface().ok().flatten().map(|i| i.as_str()) {
                Some(iface @ "org.kde.StatusNotifierWatcher") |
                Some(iface @ "org.freedesktop.StatusNotifierWatcher") => {
                    match hdr.member()?.map(|m| m.as_str()) {
                        Some("RegisterStatusNotifierItem") => {
                            let is_kde = iface == "org.kde.StatusNotifierWatcher";
                            let path : &str = msg.body()?;
                            if path.starts_with('/') {
                                let service = format!("{}{}", hdr.sender()?.unwrap(), path);
                                dbus.send(zbus::Message::signal(
                                    None::<&str>,
                                    None::<&str>,
                                    "/StatusNotifierWatcher",
                                    iface,
                                    "StatusNotifierItemRegistered",
                                    &service,
                                )?);
                                reg_db.push((service, is_kde));
                                reg_db.sort();
                                reg_db.dedup();
                                rsp = zbus::Message::method_reply(None::<&str>, &msg, &())?;
                            } else if path.starts_with(':') {
                                // kde uses this style
                                let service = format!("{}/StatusNotifierItem", path);
                                dbus.send(zbus::Message::signal(
                                    None::<&str>,
                                    None::<&str>,
                                    "/StatusNotifierWatcher",
                                    iface,
                                    "StatusNotifierItemRegistered",
                                    &service,
                                )?);
                                reg_db.push((service, is_kde));
                                reg_db.sort();
                                reg_db.dedup();
                                rsp = zbus::Message::method_reply(None::<&str>, &msg, &())?;
                            } else {
                                warn!("Unknown RegisterStatusNotifierItem from {:?}: {}", hdr.sender(), path);
                            }
                        }
                        Some("RegisterStatusNotifierHost") => {
                            dbus.send(zbus::Message::signal(
                                None::<&str>,
                                None::<&str>,
                                "/StatusNotifierWatcher",
                                iface,
                                "StatusNotifierHostRegistered",
                                &()
                            )?);
                            rsp = zbus::Message::method_reply(None::<&str>, &msg, &())?;
                        }
                        _ => {}
                    }
                }
                Some("org.freedesktop.DBus.Properties") => {
                    match hdr.member()?.map(|m| m.as_str()) {
                        Some("Get") => {
                            let (iface, mut prop) : (&str, &str) = msg.body()?;
                            let is_kde = match iface {
                                "org.kde.StatusNotifierWatcher" => true,
                                "org.freedesktop.StatusNotifierWatcher" => false,
                                _ => { prop = ""; false }
                            };
                            match prop {
                                "RegisteredStatusNotifierItems" => {
                                    let db : Vec<_> = reg_db.iter().filter(|v| v.1 == is_kde).map(|v| &v.0).collect();
                                    rsp = zbus::Message::method_reply(None::<&str>, &msg, &Variant::new(db))?;
                                }
                                "IsStatusNotifierHostRegistered" => {
                                    rsp = zbus::Message::method_reply(None::<&str>, &msg, &Variant::Bool(true))?;
                                }
                                "ProtocolVersion" => {
                                    rsp = zbus::Message::method_reply(None::<&str>, &msg, &Variant::I32(0))?;
                                }
                                _ => {}
                            }
                        }
                        Some("GetAll") => {
                            let iface : &str = msg.body()?;
                            match match iface {
                                "org.kde.StatusNotifierWatcher" => Some(true),
                                "org.freedesktop.StatusNotifierWatcher" => Some(false),
                                _ => None,
                            } {
                                Some(is_kde) => {
                                    let db : Vec<_> = reg_db.iter().filter(|v| v.1 == is_kde).map(|v| &v.0).collect();
                                    let rv : Vec<(String, Variant)> = vec![
                                        ("RegisteredStatusNotifierItems".into(), Variant::new(db)),
                                        ("IsStatusNotifierHostRegistered".into(), Variant::Bool(true)),
                                        ("ProtocolVersion".into(), Variant::I32(0)),
                                    ];
                                    let rv : HashMap<_,_> = rv.into_iter().collect();
                                    rsp = zbus::Message::method_reply(None::<&str>, &msg, &rv)?;
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                Some("org.freedesktop.DBus.Introspectable") => {
                    match hdr.member()?.map(|m| m.as_str()) {
                        Some("Introspect") => {
                            rsp = zbus::Message::method_reply(None::<&str>, &msg, &concat!(
                                r#"<!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">"#,
                                r#"<node><interface name="org.freedesktop.StatusNotifierWatcher">"#,
                                    r#"<method name="RegisterStatusNotifierItem"><arg name="name" direction="in" type="s"/></method>"#,
                                    r#"<method name="RegisterStatusNotifierHost"/>"#,
                                    r#"<property type="as" name="RegisteredStatusNotifierItems" access="read"/>"#,
                                    r#"<property type="b" name="IsStatusNotifierHostRegistered" access="read"/>"#,
                                    r#"<property type="i" name="ProtocolVersion" access="read"/>"#,
                                r#"</interface><interface name="org.kde.StatusNotifierWatcher">"#,
                                    r#"<method name="RegisterStatusNotifierItem"><arg name="name" direction="in" type="s"/></method>"#,
                                    r#"<method name="RegisterStatusNotifierHost"/>"#,
                                    r#"<property type="as" name="RegisteredStatusNotifierItems" access="read"/>"#,
                                    r#"<property type="b" name="IsStatusNotifierHostRegistered" access="read"/>"#,
                                    r#"<property type="i" name="ProtocolVersion" access="read"/>"#,
                                r#"</interface><interface name="org.freedesktop.DBus.Properties">"#,
                                    r#"<method name="Get"><arg name="interface_name" type="s" direction="in"/><arg name="property_name" type="s" direction="in"/><arg name="value" type="v" direction="out"/></method>"#,
                                    r#"<method name="GetAll"><arg name="interface_name" type="s" direction="in"/><arg name="values" type="a{sv}" direction="out"/></method>"#,
                                r#"</interface></node>"#,
                            ))?;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
            Ok(())
        });
        dbus.send(rsp);
        rv
    }
}

async fn init_snw(is_kde : bool) -> Result<(), Box<dyn Error>> {
    let who = if is_kde { "kde" } else { "freedesktop" };
    let snw_path = if is_kde { "org.kde.StatusNotifierWatcher" } else { "org.freedesktop.StatusNotifierWatcher" };
    let snw_rule = if is_kde {
        "type='signal',interface='org.kde.StatusNotifierWatcher',path='/StatusNotifierWatcher'"
    } else {
        "type='signal',interface='org.freedesktop.StatusNotifierWatcher',path='/StatusNotifierWatcher'"
    };
    let dbus = DBus::get_session();
    let name = format!("org.{}.StatusNotifierHost-{}", who, std::process::id());

    dbus.send(zbus::Message::method(
        None::<&str>,
        Some("org.freedesktop.DBus"),
        "/org/freedesktop/DBus",
        Some("org.freedesktop.DBus"),
        "RequestName",
        &(&name, 0u32)
    )?);

    dbus.spawn_call_err(zbus::Message::method(
        None::<&str>,
        Some("org.freedesktop.DBus"),
        "/org/freedesktop/DBus",
        Some("org.freedesktop.DBus"),
        "RequestName",
        &(snw_path, 0u32)
    )?, move |msg| {
        use zbus::fdo::RequestNameReply;
        match msg.body()? {
            RequestNameReply::PrimaryOwner => {
                let dbus = DBus::get_session();
                dbus.send(zbus::Message::signal(None::<&str>, None::<&str>, "/StatusNotifierWatcher", snw_path, "StatusNotifierHostRegistered", &())?);
            }
            _ => {}
        }
        Ok(())
    }, |err : zbus::Error| {
        warn!("Could not register as StatusNotifierWatcher, tray may not work: {})", err);
    });

    dbus.add_property_change_watcher(move |msg, iface, props, _inval| {
        match iface {
            "org.kde.StatusNotifierItem" => {}
            "org.freedesktop.StatusNotifierItem" => {}
            _ => return,
        }
        let src = msg.sender().unwrap().unwrap();
        let path = msg.path().unwrap().unwrap();
        handle_item_update(&src, &path, props);
    });

    dbus.send(zbus::Message::method(
        None::<&str>,
        Some("org.freedesktop.DBus"),
        "/org/freedesktop/DBus",
        Some("org.freedesktop.DBus"),
        "AddMatch",
        &snw_rule,
    )?);

    dbus.add_signal_watcher(move |_path, iface, memb, msg| {
        if iface != snw_path {
            return;
        }
        let item : String = match msg.body() {
            Ok(s) => s,
            _ => return,
        };
        match memb {
            "StatusNotifierItemRegistered" => do_add_item(is_kde, item),
            "StatusNotifierItemUnregistered" => do_del_item(item),
            _ => ()
        }
    });

    let sni_path = if is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
    dbus.add_signal_watcher(move |path, iface, _member, msg| {
        if iface != sni_path {
            return;
        }
        let path = path.to_owned();
        let hdr = msg.header().unwrap();
        let owner = hdr.sender().unwrap().unwrap().to_owned();
        spawn("Tray item refresh", async move {
            let zbus = DBus::get_session().connection().await;
            let props = zbus.call_method(
                Some(&*owner),
                &path,
                Some("org.freedesktop.DBus.Properties"),
                "GetAll",
                &()
            ).await?;
            let props = props.body()?;

            handle_item_update(&owner, &path, &props);
            Ok(())
        });
    });

    dbus.send(zbus::Message::method(
        None::<&str>,
        Some(snw_path),
        "/StatusNotifierWatcher",
        Some(snw_path),
        "RegisterStatusNotifierHost",
        &name
    )?);

    let zbus = dbus.connection().await;
    match zbus.call_method(
        Some(snw_path),
        "/StatusNotifierWatcher",
        Some("org.freedesktop.DBus.Properties"),
        "Get",
        &(snw_path, "RegisteredStatusNotifierItems"),
    ).await?.body()? {
        Variant::Array(items) => for item in items.get() {
            do_add_item(is_kde, item.try_into()?);
        }
        _ => {}
    }

    Ok(())
}

fn do_add_item(is_kde : bool, item : String) {
    let sni_path = if is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
    spawn("Tray item inspection", async move {
        let dbus = DBus::get_session();
        let zbus = dbus.connection().await;

        let (owner, path) : (Rc<str>, Rc<str>) = match item.find('/') {
            Some(pos) => (Rc::from(&item[..pos]), Rc::from(&item[pos..])),
            None => return Ok(()),
        };
        let rule = format!("type='signal',interface='{}',sender='{}',path='{}'", sni_path, owner, path);

        dbus.send(zbus::Message::method(
            None::<&str>,
            Some("org.freedesktop.DBus"),
            "/org/freedesktop/DBus",
            Some("org.freedesktop.DBus"),
            "AddMatch",
            &rule,
        )?);

        let item = TrayItem {
            owner : owner.clone(),
            path : path.clone(),
            is_kde,
            id : String::new(),
            title : None,
            icon : Default::default(),
            icon_path : Default::default(),
            status : Default::default(),
            tooltip : None,
            rule : rule.into(),
            menu : Rc::new(TrayPopupMenu {
                owner : owner.clone(),
                menu_path : Default::default(),
                fresh : Cell::new(None),
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

        let props = zbus.call_method(
            Some(&*owner),
            &*path,
            Some("org.freedesktop.DBus.Properties"),
            "GetAll",
            &sni_path,
        ).await?;

        let props = props.body()?;
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

fn handle_item_update(owner : &str, path : &str, props : &HashMap<&str, OwnedValue>) {
    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            for item in items {
                if &*item.owner != owner || &*item.path != path {
                    continue;
                }

                for (key, value) in props {
                    let value = &**value;
                    match &**key {
                        "Id" => { drop(value.try_into().map(|v| item.id = v)); }
                        "Title" => { drop(value.try_into().map(|v : String| item.title = Some(v.into()))); }
                        "IconName" => { drop(value.try_into().map(|v : String| item.icon = v.into())); }
                        "IconThemePath" => { drop(value.try_into().map(|v : String| item.icon_path = v.into())); }
                        "Status" => { drop(value.try_into().map(|v : String| item.status = v.into())); }
                        "ToolTip" => match value {
                            Variant::Structure(v) => {
                                // value is (icon-name, icon-bitmap, title, text)
                                // see https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierItem/#org.freedesktop.statusnotifieritem.tooltip
                                v.fields().get(3).and_then(|v| v.try_into().ok())
                                    .map(|v: String| item.tooltip = Some(v.into()));
                            }
                            _ => ()
                        }
                        "Menu" => { drop(value.try_into().map(|v: zvariant::ObjectPath| {
                            let v : Rc<str> = v.as_str().into();
                            if item.menu.menu_path.take_in(|mp| {
                                match mp.as_deref() {
                                    None => {
                                        *mp = Some(v.clone());
                                        false
                                    }
                                    Some(mp) if mp == &*v => false,
                                    _ => true
                                }
                            }) {
                                item.menu = Rc::new(TrayPopupMenu {
                                    owner : item.owner.clone(),
                                    menu_path : Cell::new(Some(v)),
                                    fresh : Default::default(),
                                    dbus_token : Default::default(),
                                    items : Default::default(),
                                    interested : Default::default(),
                                });
                            }
                        })); }
                        _ => ()
                    }
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
    tooltip : Option<Rc<str>>,
    menu : Rc<TrayPopupMenu>,
    rendered_ids : Vec<(f32, f32, i32)>,
}

impl PartialEq for TrayPopup {
    fn eq(&self, rhs : &Self) -> bool {
        Rc::ptr_eq(&self.owner, &rhs.owner) &&
            Rc::ptr_eq(&self.menu, &rhs.menu)
    }
}

#[derive(Debug)]
struct TrayPopupMenu {
    owner : Rc<str>,
    menu_path : Cell<Option<Rc<str>>>,
    fresh : Cell<Option<Instant>>,
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
    fn add_items<'a, I>(items : &mut Vec<MenuItem>, iter : I, depth : u32)
        where I : Iterator<Item=&'a Variant<'a>>
    {
        let mut item = MenuItem::default();
        for entry in iter {
            let entry = match entry {
                Variant::Value(v) => &v,
                v => v,
            };
            let fields = match entry {
                Variant::Structure(s) => s.fields(),
                _ => continue,
            };
            if let Some(id) = fields.get(0).and_then(|v| v.try_into().ok()) {
                item.id = id;
            }
            if let Some(props) = fields.get(1).and_then(|v| v.downcast_ref::<zvariant::Dict>()) {
                if let Ok(Some(label)) = props.get::<_, str>("label") {
                    let mut text = String::with_capacity(label.len());
                    let mut esc = false;
                    for c in label.chars() {
                        if c == '_' && !esc {
                            esc = true;
                            continue;
                        }
                        esc = false;
                        text.push(c);
                    }
                    item.label = text.into();
                }
                item.visible = props.get("visible").ok().flatten().copied().unwrap_or(true);
                item.enabled = props.get("enabled").ok().flatten().copied().unwrap_or(true);
                if !item.visible {
                    // Note: this is needed to correctly hide the sub-menu
                    continue;
                }
                match props.get::<_, str>("type") {
                    Ok(Some("separator")) => item.is_sep = true,
                    Ok(Some(v)) => debug!("Unknown menu item type: {}", v),
                    _ => ()
                }
                item.depth = depth;
                items.push(item);
                item = MenuItem::default();
            }
            if let Some(Variant::Array(submenu)) = fields.get(2) {
                Self::add_items(items, submenu.into_iter(), depth + 1);
            }
        }
    }
    fn add_remove_match(&self, dbus : &DBus, method : &str) {
        if let Some(menu_path) = self.menu_path.take_in(|m| m.clone()) {
            dbus.send(zbus::Message::method(
                None::<&str>,
                Some("org.freedesktop.DBus"),
                "/org/freedesktop/DBus",
                Some("org.freedesktop.DBus"),
                method,
                &format!("type='signal',interface='com.canonical.dbusmenu',member='ItemsPropertiesUpdated',sender='{}',path='{}'", self.owner, menu_path),
            ).unwrap());
            dbus.send(zbus::Message::method(
                None::<&str>,
                Some("org.freedesktop.DBus"),
                "/org/freedesktop/DBus",
                Some("org.freedesktop.DBus"),
                method,
                &format!("type='signal',interface='com.canonical.dbusmenu',member='LayoutUpdated',sender='{}',path='{}'", self.owner, menu_path),
            ).unwrap());
        }
    }
}

impl Drop for TrayPopupMenu {
    fn drop(&mut self) {
        let mut token = self.dbus_token.take();
        if !token.is_active() {
            return;
        }
        let dbus = DBus::get_session();
        self.add_remove_match(&dbus, "RemoveMatch");
        dbus.stop_signal_watcher(&mut token);
    }
}

async fn refresh_menu(menu : Rc<TrayPopupMenu>) -> Result<(), Box<dyn Error>> {
    let menu_path = match menu.menu_path.take_in(|m| m.clone()) {
        Some(mp) => mp,
        None => return Ok(())
    };
    let dbus = DBus::get_session();
    let zbus = dbus.connection().await;

    zbus.call_method(
        Some(&*menu.owner),
        &*menu_path,
        Some("com.canonical.dbusmenu"),
        "AboutToShow",
        &0i32,
    ).await?;

    if menu.dbus_token.take_in(|t| !t.is_active()) {
        menu.add_remove_match(&dbus, "AddMatch");
        let weak = Rc::downgrade(&menu);
        let token = dbus.add_signal_watcher(move |msg_path, msg_iface, _member, msg| {
            if msg_iface != "com.canonical.dbusmenu" {
                return;
            }
            if let Some(menu) = weak.upgrade() {
                let hdr = msg.header().unwrap();
                if hdr.sender().ok().flatten().map_or(true, |s| *s != &*menu.owner) ||
                    menu.menu_path.take_in(|mp| Some(msg_path.as_str()) != mp.as_deref())
                {
                    return;
                }
                if menu.fresh.replace(None).is_none() {
                    spawn("Tray menu refresh", refresh_menu(menu));
                }
            }
        });
        menu.dbus_token.set(token);
    }

    // ? MatchRule::new_signal("com.canonical.dbusmenu", "ItemActivationRequested");

    let rv = zbus.call_method(
        Some(&*menu.owner),
        &*menu_path,
        Some("com.canonical.dbusmenu"),
        "GetLayout",
        &(0i32, -1i32, &["type", "label", "visible", "enabled"] as &[&str])
    ).await?;
    let (_rev, (_id, _props, contents)) : (u32, (i32, HashMap<&str, Variant>, Vec<Variant>)) = rv.body()?;

    let mut items = Vec::new();
    TrayPopupMenu::add_items(&mut items, contents.iter(), 0);
    menu.items.set(items);
    menu.interested.take().notify_data("tray:menu");
    menu.fresh.set(Some(Instant::now()));

    Ok(())
}

impl TrayPopup {
    pub fn render(&mut self, ctx : &mut Render) {
        let width = ctx.render_extents.2;
        let rendered_ids = &mut self.rendered_ids;
        rendered_ids.clear();

        let (mut xsize, mut ypos) = render_font(ctx.canvas, ctx.font, ctx.font_size, ctx.font_color, &ctx.runtime, (2.0, 2.0), self.title.as_deref().unwrap_or_default(), false);
        xsize += 2.0;
        ypos = 2.0 + ypos.ceil();

        if let Some(tooltip) = self.tooltip.as_ref() {
            let tsize = render_font(ctx.canvas, ctx.font, ctx.font_size, ctx.font_color, &ctx.runtime, (10.0, ypos), &tooltip, true);
            xsize = xsize.max(tsize.0 + 10.0);
            ypos += tsize.1.ceil();
        }

        if self.menu.fresh.get().map_or(true, |i| i.elapsed() > Duration::from_secs(60)) &&
            self.menu.menu_path.take_in_some(|_| ()).is_some()
        {
            // need to kick off the first refresh
            let menu = self.menu.clone();
            spawn("Tray menu population", refresh_menu(menu));
        }

        self.menu.interested.take_in(|i| i.add(ctx.runtime));

        self.menu.items.take_in(|items| {
            if !items.is_empty() {
                ctx.canvas.fill_rect(0.0, ypos + 4.0, width, 2.0,
                    &raqote::Color::new(255, 255, 255, 255).into(),
                    &Default::default());

                ypos += 9.0;
            }
            for item in items {
                if !item.visible {
                    continue;
                }
                let indent = 2.0 + item.depth as f32 * 20.0;
                if item.is_sep {
                    ctx.canvas.fill_rect(indent + 3.0, ypos + 3.0, width - indent - 5.0, 1.0,
                        &raqote::Color::new(255, 255, 255, 255).into(),
                        &Default::default());

                    ypos += 7.0;
                } else {
                    let tsize = render_font(ctx.canvas, ctx.font, ctx.font_size, ctx.font_color, &ctx.runtime, (indent, ypos), &item.label, false);
                    let end = ypos + tsize.1.ceil();
                    xsize = xsize.max(indent + tsize.0);
                    rendered_ids.push((ypos, end, item.id));
                    ypos = end + 5.0;
                }
            }
        });
        ctx.render_pos = xsize.ceil() + 2.0;
        ctx.render_ypos = Some(ypos);
    }

    pub fn button(&mut self, x : f64, y : f64, button : u32, _runtime : &mut Runtime) {
        let y = y as f32;
        let _ = (x, button);
        for &(min, max, id) in &self.rendered_ids {
            let ts = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
            if y < min || y > max {
                continue;
            }
            self.menu.menu_path.take_in_some(|menu_path| {
                let dbus = DBus::get_session();
                debug!("Clicking {} {} id {}", self.owner, menu_path, id);
                dbus.send(zbus::Message::method(
                    None::<&str>,
                    Some(&*self.owner),
                    &**menu_path,
                    Some("com.canonical.dbusmenu"),
                    "Event",
                    &(id, "clicked", Variant::I32(0), ts as u32)
                ).unwrap());
            });
        }
    }
}

pub fn show(ctx : &mut Render, rv : &mut EventSink, [passive, active, urgent]: [&Rc<Item>;3]) {
    let items = DATA.with(|cell| {
        let tray = cell.get_or_init(Tray::init);
        tray.interested.take_in(|interest| interest.add(&ctx.runtime));
        tray.items.take_in(|items| {
            items.iter()
                .map(|item| {
                    let owner = item.owner.clone();
                    let path = item.path.clone();
                    let title = item.title.clone();
                    let menu = item.menu.clone();
                    let tooltip = item.tooltip.clone();
                    let render = match &*item.status {
                        "Passive" => passive,
                        "NeedsAttention" => urgent,
                        _ => active,
                    };
                    (owner, path, title, menu, tooltip, render)
                }).collect::<Vec<_>>()
        })
    });
    for (owner, path, title, menu, tooltip, render) in items {
        let item = IterationItem::Tray { owner : owner.clone(), path : path.clone() };
        let x0 = ctx.render_pos;
        render.render_clamped_item(ctx, rv, &item);
        let x1 = ctx.render_pos;
        if x0 != x1 {
            let mut es = EventSink::from_tray(owner.clone(), path);
            es.offset_clamp(0.0, x0, x1);
            es.add_hover(x0, x1, PopupDesc::Tray(TrayPopup {
                owner, title, menu, tooltip, rendered_ids : Vec::new(),
            }));
            rv.merge(es);
        }
    }
}

pub fn read_in<F : FnOnce(Value) -> R, R>(_name : &str, owner : &str, path : &str, key : &str, rt : &Runtime, f : F) -> R {
    DATA.with(|cell| {
        let tray = cell.get_or_init(Tray::init);
        tray.interested.take_in(|interest| interest.add(rt));
        tray.items.take_in(|items| {
            for item in items {
                if &*item.owner != owner || &*item.path != path {
                    continue;
                }
                match key {
                    "icon" => {
                        if !item.icon_path.is_empty() {
                            return f(Value::Owned(format!("{}/{}", item.icon_path, item.icon)));
                        }
                        return f(Value::Borrow(&item.icon));
                    }
                    "title" => return f(item.title.as_deref().map_or(Value::Null, Value::Borrow)),
                    "status" => return f(Value::Borrow(&item.status)),
                    "tooltip" => return f(item.tooltip.as_deref().map_or(Value::Null, Value::Borrow)),
                    "id" => return f(Value::Borrow(&item.id)),
                    _ => break,
                }
            }
            f(Value::Null)
        })
    })
}

pub fn write(name : &str, owner : &str, path : &str, key : &str, value : Value, rt : &Runtime) {
    // Ignored for now, see EventSink::from_tray
    let _ = (name, owner, path, key, value, rt);
}

/// A click or scroll on the tray icon itself
pub fn do_click(owner : &Rc<str>, path : &Rc<str>, how : u32) {
    let method = match how {
        0 => "Activate",
        1 => "ContextMenu",
        2 => "SecondaryActivate",
        5 | 6 => "vertical",
        7 | 8 => "horizontal",
        _ => return,
    };
    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            for item in items {
                if item.owner != *owner || item.path != *path {
                    continue;
                }
                let dbus = DBus::get_session();
                let sni_path = if item.is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };

                debug!("Invoking {} on {}", method, item.id);
                if how < 3 {
                    dbus.send(zbus::Message::method(
                        None::<&str>,
                        Some(&**owner),
                        &**path,
                        Some(sni_path),
                        method,
                        &(0i32,0i32)
                    ).unwrap());
                } else {
                    dbus.send(zbus::Message::method(
                        None::<&str>,
                        Some(&**owner),
                        &**path,
                        Some(sni_path),
                        "Scroll",
                        &(15i32,method)
                    ).unwrap());
                }
            }
        });
    });
}
