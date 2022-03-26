use crate::data::{IterationItem,Value};
use crate::dbus::DBus;
use crate::event::EventSink;
use crate::font::render_font;
use crate::item::{Item,PopupDesc};
use crate::render::Render;
use crate::state::{Runtime,NotifierList};
use crate::util::{Cell,spawn,spawn_handle};
use crate::wayland::Button;
use futures_util::future::RemoteHandle;
use once_cell::unsync::OnceCell;
use async_once_cell::OnceCell as AsyncOnceCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::error::Error;
use std::rc::Rc;
use std::time::Instant;
use std::mem::ManuallyDrop;
use std::time::{SystemTime,Duration,UNIX_EPOCH};
use log::{debug,warn};
use zbus::fdo::DBusProxy;
use zbus::dbus_proxy;
use zbus::fdo;
use zbus::zvariant;
use zvariant::Value as Variant;
use zvariant::OwnedValue;

#[dbus_proxy(interface="com.canonical.dbusmenu")]
trait DBusMenu {
    /*
    <property name="Version" type="u" access="read"/>
    <property name="Status" type="s" access="read"/>

    <signal name="ItemsPropertiesUpdated">
      <arg type="a(ia{sv})" direction="out"/>
      <annotation name="org.qtproject.QtDBus.QtTypeName.Out0" value="DBusMenuItemList"/>
      <arg type="a(ias)" direction="out"/>
      <annotation name="org.qtproject.QtDBus.QtTypeName.Out1" value="DBusMenuItemKeysList"/>
    </signal>
    <signal name="LayoutUpdated">
      <arg name="revision" type="u" direction="out"/>
      <arg name="parentId" type="i" direction="out"/>
    </signal>
    <signal name="ItemActivationRequested">
      <arg name="id" type="i" direction="out"/>
      <arg name="timeStamp" type="u" direction="out"/>
    </signal>

    <method name="GetProperty">
      <arg type="v" direction="out"/>
      <arg name="id" type="i" direction="in"/>
      <arg name="property" type="s" direction="in"/>
    </method>
    <method name="GetGroupProperties">
      <arg type="a(ia{sv})" direction="out"/>
      <arg name="ids" type="ai" direction="in"/>
      <arg name="propertyNames" type="as" direction="in"/>
    </method>
*/
    fn about_to_show(&self, id: i32) -> fdo::Result<bool>;

    fn get_layout(&self, parent_id: i32, recursion_depth: i32, property_names: &[&str])
        -> fdo::Result<(u32, (i32, HashMap<String, OwnedValue>, Vec<OwnedValue>))>;

    // TODO noreply
    fn event(&self, id: i32, event_id: &str, data: &Variant<'_>, timestamp: u32)
        -> fdo::Result<()>;
}

#[dbus_proxy(interface="org.freedesktop.StatusNotifierItem")]
// Note: override interface to org.kde.StatusNotifierItem if needed
trait StatusNotifierItem {
/*
 * Can't actually use the properties because caching is broken
 * (thanks a lot, KDE)
    <property access="read" type="s" name="Category"/>
    <property access="read" type="s" name="Id"/>
    <property access="read" type="s" name="Title"/>
    <property access="read" type="s" name="Status"/>
    <property access="read" type="i" name="WindowId"/>
    <property access="read" type="s" name="IconThemePath"/>
    <property access="read" type="o" name="Menu"/>
    <property access="read" type="b" name="ItemIsMenu"/>
    <property access="read" type="s" name="IconName"/>
    <property access="read" type="a(iiay)" name="IconPixmap">
      <annotation value="KDbusImageVector" name="org.qtproject.QtDBus.QtTypeName"/>
    </property>
    <property access="read" type="s" name="OverlayIconName"/>
    <property access="read" type="a(iiay)" name="OverlayIconPixmap">
      <annotation value="KDbusImageVector" name="org.qtproject.QtDBus.QtTypeName"/>
    </property>
    <property access="read" type="s" name="AttentionIconName"/>
    <property access="read" type="a(iiay)" name="AttentionIconPixmap">
      <annotation value="KDbusImageVector" name="org.qtproject.QtDBus.QtTypeName"/>
    </property>
    <property access="read" type="s" name="AttentionMovieName"/>
    <property access="read" type="(sa(iiay)ss)" name="ToolTip">
      <annotation value="KDbusToolTipStruct" name="org.qtproject.QtDBus.QtTypeName"/>
    </property>

 * Methods are called as sync/NoReply
    <method name="ContextMenu">
      <arg direction="in" type="i" name="x"/>
      <arg direction="in" type="i" name="y"/>
    </method>
    <method name="Activate">
      <arg direction="in" type="i" name="x"/>
      <arg direction="in" type="i" name="y"/>
    </method>
    <method name="SecondaryActivate">
      <arg direction="in" type="i" name="x"/>
      <arg direction="in" type="i" name="y"/>
    </method>
    <method name="Scroll">
      <arg direction="in" type="i" name="delta"/>
      <arg direction="in" type="s" name="orientation"/>
    </method>

 * Signals could be useful, but not as they are now.
    <signal name="NewTitle"/>
    <signal name="NewIcon"/>
    <signal name="NewAttentionIcon"/>
    <signal name="NewOverlayIcon"/>
    <signal name="NewMenu"/>
    <signal name="NewToolTip"/>
    <signal name="NewStatus">
      <arg type="s" name="status"/>
    </signal>
*/
}

thread_local! {
    static DATA : ManuallyDrop<OnceCell<Tray>> = Default::default();
}

#[derive(Debug)]
pub struct TrayItem {
    owner : Rc<str>,
    path : Rc<str>,
    // sni: AsyncStatusNotifierItemProxy<'static>,
    is_kde : bool,

    rule : Box<str>,

    id : Cell<Box<str>>,
    title : Cell<Option<Rc<str>>>,
    icon : Cell<Box<str>>,
    icon_path : Cell<Box<str>>,
    status : Cell<Box<str>>,
    tooltip : Cell<Option<Rc<str>>>,
    inspection: Cell<Option<RemoteHandle<()>>>,
    menu : Cell<Option<Rc<TrayPopupMenu>>>,
    interested : Cell<NotifierList>,
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
    items : Cell<Vec<Rc<TrayItem>>>,
    interested : Cell<NotifierList>,
}

impl Tray {
    fn init() -> Tray {
        spawn("StatusNotifierWatcher", async move {
            let dbus = DBus::get_session();
            let zbus = dbus.connection().await;

            zbus.object_server().at("/StatusNotifierWatcher", snw_kde::StatusNotifierWatcher).await?;
            zbus.object_server().at("/StatusNotifierWatcher", snw_fdo::StatusNotifierWatcher).await?;

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
}

macro_rules! build_snw {
    ($ident:ident, $name:tt, $is_kde:ident) => {
        mod $ident {
            use super::*;
            #[derive(Debug)]
            pub struct StatusNotifierWatcher;

            #[zbus::dbus_interface(name = $name)]
            impl StatusNotifierWatcher {
                fn register_status_notifier_item(&self, path: &str, #[zbus(header)] hdr: zbus::MessageHeader) -> zbus::fdo::Result<()> {
                    DATA.with(|cell| {
                        let tray = cell.get().unwrap();
                        tray.reg_db.take_in(|reg_db| {
                            let dbus = DBus::get_session();
                            if path.starts_with('/') {
                                let service = format!("{}{}", hdr.sender()?.unwrap(), path);
                                dbus.send(zbus::Message::signal(
                                    None::<&str>,
                                    None::<&str>,
                                    "/StatusNotifierWatcher",
                                    $name,
                                    "StatusNotifierItemRegistered",
                                    &service,
                                )?);
                                reg_db.push((service, $is_kde));
                                reg_db.sort();
                                reg_db.dedup();
                            } else if path.starts_with(':') {
                                // kde uses this style
                                let service = format!("{}/StatusNotifierItem", path);
                                dbus.send(zbus::Message::signal(
                                    None::<&str>,
                                    None::<&str>,
                                    "/StatusNotifierWatcher",
                                    $name,
                                    "StatusNotifierItemRegistered",
                                    &service,
                                )?);
                                reg_db.push((service, $is_kde));
                                reg_db.sort();
                                reg_db.dedup();
                            } else {
                                warn!("Unknown RegisterStatusNotifierItem from {:?}: {}", hdr.sender(), path);
                            }
                            Ok(())
                        })
                    })
                }

                #[dbus_interface(signal)]
                async fn status_notifier_host_registered(ctxt: &zbus::SignalContext<'_>) -> zbus::Result<()>;

                async fn register_status_notifier_host(&self, _host: &str, #[zbus(signal_context)] ctxt: zbus::SignalContext<'_>) -> zbus::fdo::Result<()> {
                    Self::status_notifier_host_registered(&ctxt).await.map_err(Into::into)
                }

                #[dbus_interface(property)]
                fn is_status_notifier_host_registered(&self) -> bool { true }

                #[dbus_interface(property)]
                fn protocol_version(&self) -> i32 { 0 }

                #[dbus_interface(property)]
                fn registered_status_notifier_items(&self) -> Vec<String> {
                    DATA.with(|cell| {
                        let tray = cell.get().unwrap();
                        tray.reg_db.take_in(|reg_db| {
                            reg_db.iter().filter(|v| v.1 == $is_kde).map(|v| v.0.clone()).collect()
                        })
                    })
                }
            }
        }
    };
}
build_snw!(snw_kde, "org.kde.StatusNotifierWatcher", true);
build_snw!(snw_fdo, "org.freedesktop.StatusNotifierWatcher", false);

async fn init_snw(is_kde : bool) -> Result<(), Box<dyn Error>> {
    let who = if is_kde { "kde" } else { "freedesktop" };
    let snw_path = if is_kde { "org.kde.StatusNotifierWatcher" } else { "org.freedesktop.StatusNotifierWatcher" };
    let snw_rule = if is_kde {
        "type='signal',interface='org.kde.StatusNotifierWatcher',path='/StatusNotifierWatcher'"
    } else {
        "type='signal',interface='org.freedesktop.StatusNotifierWatcher',path='/StatusNotifierWatcher'"
    };
    let dbus = DBus::get_session();
    let zbus = dbus.connection().await;
    let name = format!("org.{}.StatusNotifierHost-{}", who, std::process::id());

    let dbif = DBusProxy::builder(&zbus)
        .cache_properties(zbus::CacheProperties::No)
        .build().await?;

    match futures_util::future::join(
        dbif.request_name((&*name).try_into()?, Default::default()),
        dbif.request_name(snw_path.try_into()?, Default::default()),
    ).await.1? {
        zbus::fdo::RequestNameReply::PrimaryOwner => {
            dbus.send(zbus::Message::signal(None::<&str>, None::<&str>, "/StatusNotifierWatcher", snw_path, "StatusNotifierHostRegistered", &())?);
        }
        _ => {}
    }

    dbus.add_property_change_watcher(move |msg, iface, props, _inval| {
        match iface {
            "org.kde.StatusNotifierItem" => {}
            "org.freedesktop.StatusNotifierItem" => {}
            _ => return,
        }
        let src = msg.sender().unwrap().unwrap();
        let path = msg.path().unwrap().unwrap();
        DATA.with(|cell| {
            let tray = cell.get();
            let tray = tray.as_ref().unwrap();
            tray.items.take_in(|items| {
                for item in items {
                    if *item.owner != **src || *item.path != **path {
                        continue;
                    }

                    item.handle_update(props)
                }
            });
        });
    });

    zbus.send_message(zbus::Message::method(
        None::<&str>,
        Some("org.freedesktop.DBus"),
        "/org/freedesktop/DBus",
        Some("org.freedesktop.DBus"),
        "AddMatch",
        &snw_rule,
    )?).await?;

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
        let hdr = msg.header().unwrap();
        let owner = hdr.sender().unwrap().unwrap();

        DATA.with(|cell| {
            let tray = cell.get();
            let tray = tray.as_ref().unwrap();
            tray.items.take_in(|items| {
                for item in items {
                    if &*item.owner == &**owner && &*item.path == &**path {
                        item.reinspect();
                    }
                }
            });
        });
    });

    zbus.send_message(zbus::Message::method(
        None::<&str>,
        Some(snw_path),
        "/StatusNotifierWatcher",
        Some(snw_path),
        "RegisterStatusNotifierHost",
        &name
    )?).await?;

    // Note: this must be well-ordered after the above RequestName
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
    let (owner, path) : (Rc<str>, Rc<str>) = match item.find('/') {
        Some(pos) => (Rc::from(&item[..pos]), Rc::from(&item[pos..])),
        None => return,
    };

    DATA.with(|cell| {
        let tray = cell.get();
        let tray = tray.as_ref().unwrap();
        tray.items.take_in(|items| {
            for item in &*items {
                if &*item.owner == &*owner && item.path == path {
                    return;
                }
            }

            let sni_path = if is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
            let dbus = DBus::get_session();
            let rule = format!("type='signal',interface='{}',sender='{}',path='{}'", sni_path, owner, path);

            dbus.send(zbus::Message::method(
                None::<&str>,
                Some("org.freedesktop.DBus"),
                "/org/freedesktop/DBus",
                Some("org.freedesktop.DBus"),
                "AddMatch",
                &rule,
            ).unwrap());

            /* This is a nice idea but not actually useful
             * It also needs AsyncOnceCell now
            let sni = StatusNotifierItemProxy::builder(&zbus)
                .destination(String::from(&*owner))?
                .path(String::from(&*path))?
                .interface(sni_path)?
                .build()
                .await?;
            */

            let item = Rc::new(TrayItem {
                owner,
                path,
                is_kde,
                id : Default::default(),
                title : Default::default(),
                icon : Default::default(),
                icon_path : Default::default(),
                status : Default::default(),
                tooltip : Default::default(),
                rule : rule.into(),
                inspection: Default::default(),
                menu : Default::default(),
                interested : Default::default(),
            });

            item.reinspect();
            items.push(item);
            // No need to update tray.interested; reinspect->handle_update will do it
        });
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
        tray.interested.take().notify_data("tray:item-remove");
    });
}

impl TrayItem {
    fn reinspect(self : &Rc<Self>) {
        self.inspection.take_in(|i| {
            i.get_or_insert_with(|| {
                let sni_path = if self.is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };
                let owner = self.owner.clone();
                let path = self.path.clone();
                let this = Rc::downgrade(self);
                spawn_handle("Tray item inspection", async move {
                    let dbus = DBus::get_session();
                    let zbus = dbus.connection().await;
                    let props = zbus.call_method(
                        Some(&*owner),
                        &*path,
                        Some("org.freedesktop.DBus.Properties"),
                        "GetAll",
                        &sni_path,
                    ).await?;

                    let props = props.body()?;
                    // Avoid holding a strong reference from inside the self-owned task
                    if let Some(this) = this.upgrade() {
                        this.inspection.set(None);
                        this.handle_update(&props);
                    }
                    Ok(())
                })
            });
        });
    }

    fn handle_update(&self, props : &HashMap<&str, OwnedValue>) {
        for (key, value) in props {
            let value = &**value;
            match &**key {
                "Id" => { drop(value.try_into().map(|v: String| self.id.set(v.into()))); }
                "Title" => { drop(value.try_into().map(|v : String| self.title.set(Some(v.into())))); }
                "IconName" => { drop(value.try_into().map(|v : String| self.icon.set(v.into()))); }
                "IconThemePath" => { drop(value.try_into().map(|v : String| self.icon_path.set(v.into()))); }
                "Status" => drop(value.try_into().map(|v : String| {
                    self.status.set(v.into());
                    // item status is queried without setting self.interested, so updates need to
                    // notify anyone who iterated the tray and not just those who query this item
                    DATA.with(|cell| {
                        let tray = cell.get();
                        let tray = tray.as_ref().unwrap();
                        tray.interested.take().notify_data("tray:item-status");
                    });
                })),
                "ToolTip" => match value {
                    Variant::Structure(v) => {
                        // value is (icon-name, icon-bitmap, title, text)
                        // see https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierItem/#org.freedesktop.statusnotifieritem.tooltip
                        v.fields().get(3).and_then(|v| v.try_into().ok())
                            .map(|v: String| self.tooltip.set(Some(v.into())));
                    }
                    _ => ()
                }
                "Menu" => { drop(value.try_into().map(|v: zvariant::ObjectPath| {
                    self.menu.take_in(|menu| {
                        let old_path = menu.as_ref().and_then(|menu| {
                            menu.menu_path.take_in(|mp| mp.clone())
                        });

                        if old_path.as_deref() != Some(&*v) {
                            let v : Rc<str> = v.as_str().into();
                            *menu = Some(Rc::new(TrayPopupMenu {
                                owner : self.owner.clone(),
                                menu_path : Cell::new(Some(v)),
                                menu: AsyncOnceCell::new(),
                                watcher: Default::default(),
                                refresh : Cell::new(None),
                                fresh : Default::default(),
                                items : Default::default(),
                                interested : Default::default(),
                            }));
                        }
                    });
                })); }
                _ => ()
            }
        }

        self.interested.take().notify_data("tray:update");
    }
}

#[derive(Clone,Debug)]
pub struct TrayPopup {
    title : Option<Rc<str>>,
    tooltip : Option<Rc<str>>,
    menu : Rc<TrayPopupMenu>,
    rendered_ids : Vec<(f32, f32, i32)>,
}

impl PartialEq for TrayPopup {
    fn eq(&self, rhs : &Self) -> bool {
        Rc::ptr_eq(&self.menu, &rhs.menu)
    }
}

#[derive(Debug)]
struct TrayPopupMenu {
    owner : Rc<str>,
    menu_path : Cell<Option<Rc<str>>>,
    menu : AsyncOnceCell<DBusMenuProxy<'static>>,
    watcher : OnceCell<RemoteHandle<()>>,
    refresh: Cell<Option<RemoteHandle<()>>>,
    fresh : Cell<Option<Instant>>,
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

    async fn get_proxy<'a>(self : &'a Rc<Self>) -> Result<Option<&'a DBusMenuProxy<'static>>, Box<dyn Error>> {
        if let Some(dbm) = self.proxy() {
            return Ok(Some(dbm));
        }

        let menu_path = match self.menu_path.take_in(|m| m.clone()) {
            Some(mp) => mp,
            None => return Ok(None)
        };
        let dbm = self.menu.get_or_try_init(async {
            let dbus = DBus::get_session();
            let zbus = dbus.connection().await;
            DBusMenuProxy::builder(&zbus)
                .destination(String::from(&*self.owner))?
                .path(String::from(&*menu_path))?
                .build().await
        }).await?;
        self.watcher.get_or_init(|| {
            use futures_util::StreamExt;
            let weak = Rc::downgrade(&self);
            // Not using SignalStream because we want all signals
            let mut conn : zbus::MessageStream = dbm.connection().into();
            spawn_handle("Menu refresh", async move {
                while let Some(Ok(msg)) = conn.next().await {
                    if let Some(menu) = weak.upgrade() {
                        menu.refrsh_signal(&msg).await;
                    }
                }
                Ok(())
            })
        });

        Ok(Some(dbm))
    }

    async fn refrsh_signal(self: Rc<Self>, msg: &zbus::Message) {
        if msg.primary_header().msg_type() != zbus::MessageType::Signal {
            return;
        }
        let dbm = self.proxy().unwrap();
        if msg.interface() != Some(dbm.interface().as_ref()) ||
            msg.path() != Some(dbm.path().as_ref())
        {
            return;
        }
        match msg.header() {
            Ok(h) if h.sender().ok().flatten().map(|s| s.as_str()) == Some(&*self.owner) => {}
            _ => return
        }

        if self.fresh.replace(None).is_some() {
            match self.refresh().await {
                Ok(()) => (),
                Err(e) => {
                    warn!("Error refreshing menu: {}", e);
                }
            }
        }
    }

    fn proxy(&self) -> Option<&DBusMenuProxy<'static>> {
        self.menu.get()
    }

    async fn refresh(self : Rc<Self>) -> Result<(), Box<dyn Error>> {
        let dbm = match self.get_proxy().await? {
            Some(dbm) => dbm,
            None => return Ok(()),
        };

        dbm.about_to_show(0).await?;

        let (_rev, (_id, _props, contents)) = dbm.get_layout(0, -1, &["type", "label", "visible", "enabled"] as &[&str]).await?;

        let mut items = Vec::new();
        TrayPopupMenu::add_items(&mut items, contents.iter().map(|v| &**v), 0);
        self.items.set(items);
        self.interested.take().notify_data("tray:menu");
        self.fresh.set(Some(Instant::now()));

        Ok(())
    }

    fn add_remove_match(&self, dbus : &DBus, method : &str) {
        if let Some(menu_path) = self.menu_path.take_in(|m| m.clone()) {
            // NoReply would be nice
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
        let dbus = DBus::get_session();
        self.add_remove_match(&dbus, "RemoveMatch");
    }
}


impl TrayPopup {
    pub fn render(&mut self, ctx : &mut Render) {
        let width = ctx.render_extents.1.x;
        let rendered_ids = &mut self.rendered_ids;
        rendered_ids.clear();

        let line_paint = tiny_skia::Paint {
            shader: tiny_skia::Shader::SolidColor(tiny_skia::Color::WHITE),
            ..Default::default()
        };

        let (mut xsize, mut ypos) = render_font(ctx, (2.0, 2.0), self.title.as_deref().unwrap_or_default(), false);
        xsize += 2.0;
        ypos = 2.0 + ypos.ceil();

        if let Some(tooltip) = self.tooltip.as_ref() {
            if !tooltip.is_empty() {
                let tsize = render_font(ctx, (10.0, ypos), &tooltip, true);
                xsize = xsize.max(tsize.0 + 10.0);
                ypos += tsize.1.ceil();
            }
        }

        if self.menu.fresh.get().map_or(true, |i| i.elapsed() > Duration::from_secs(60)) &&
            self.menu.menu_path.take_in_some(|_| ()).is_some()
        {
            // need to kick off the first refresh
            let menu = self.menu.clone();
            self.menu.refresh.set(Some(
                spawn_handle("Tray menu population", menu.refresh())
            ));
        }

        self.menu.interested.take_in(|i| i.add(ctx.runtime));

        self.menu.items.take_in(|items| {
            if !items.is_empty() {
                if let Some(rect) = tiny_skia::Rect::from_xywh(0.0, ypos + 4.0, width, 2.0) {
                    ctx.canvas.fill_rect(rect, &line_paint, ctx.render_xform, None);
                }

                ypos += 9.0;
            }
            for item in items {
                if !item.visible {
                    continue;
                }
                let indent = 2.0 + item.depth as f32 * 20.0;
                if item.is_sep {
                    if let Some(rect) = tiny_skia::Rect::from_xywh(indent + 3.0, ypos + 3.0, width - indent - 5.0, 1.0) {
                        ctx.canvas.fill_rect(rect, &line_paint, ctx.render_xform, None);
                    }

                    ypos += 7.0;
                } else {
                    let tsize = render_font(ctx, (indent, ypos), &item.label, false);
                    let end = ypos + tsize.1.ceil();
                    xsize = xsize.max(indent + tsize.0);
                    rendered_ids.push((ypos, end, item.id));
                    ypos = end + 5.0;
                }
            }
        });
        ctx.render_pos = tiny_skia::Point { x: xsize.ceil() + 2.0, y: ypos };
    }

    pub fn button(&mut self, x : f64, y : f64, button : Button, _runtime : &mut Runtime) {
        let y = y as f32;
        let _ = (x, button);
        for &(min, max, id) in &self.rendered_ids {
            if y < min || y > max {
                continue;
            }
            if let Some(dbm) = self.menu.proxy() {
                let ts = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
                debug!("Clicking {} {} id {}", dbm.destination(), dbm.path(), id);
                let dbus = DBus::get_session();
                dbus.send(zbus::Message::method(
                    None::<&str>,
                    Some(dbm.destination()),
                    dbm.path(),
                    Some(dbm.interface()),
                    "Event",
                    &(id, "clicked", Variant::I32(0), ts as u32)
                ).unwrap());
            }
        }
    }
}

pub fn show(ctx : &mut Render, rv : &mut EventSink, [passive, active, urgent]: [&Rc<Item>;3]) {
    let items = DATA.with(|cell| {
        let tray = cell.get_or_init(Tray::init);
        tray.interested.take_in(|interest| interest.add(&ctx.runtime));
        tray.items.take_in(|items| {
            items.iter().cloned().collect::<Vec<_>>()
        })
    });

    for tray_item in items {
        let render = tray_item.status.take_in(|s| match &**s {
            "Passive" => passive,
            "NeedsAttention" => urgent,
            _ => active,
        });

        let iter_item = IterationItem::Tray(tray_item.clone());
        let x0 = ctx.render_pos.x;
        render.render_clamped_item(ctx, rv, &iter_item);
        let x1 = ctx.render_pos.x;
        if x0 != x1 {
            if let Some(menu) = tray_item.menu.take_in(|m| m.clone()) {
                let title = tray_item.title.take_in(|t| t.clone());
                let tooltip = tray_item.tooltip.take_in(|t| t.clone());
                let mut es = EventSink::from_tray(tray_item.clone());
                es.offset_clamp(0.0, x0, x1);
                es.add_hover(x0, x1, PopupDesc::Tray(TrayPopup {
                    title, menu, tooltip, rendered_ids : Vec::new(),
                }));
                rv.merge(es);
            }
        }
    }
}

pub fn read_in<F : FnOnce(Value) -> R, R>(_name : &str, item: &TrayItem, key : &str, rt : &Runtime, f : F) -> R {
    item.interested.take_in(|i| i.add(rt));
    match key {
        "icon" => item.icon.take_in(|icon| {
            item.icon_path.take_in(|path| {
                if path.is_empty() {
                    f(Value::Borrow(icon))
                } else {
                    f(Value::Owned(format!("{}/{}", path, icon)))
                }
            })
        }),
        "title" => item.title.take_in(|t| f(t.as_deref().map_or(Value::Null, Value::Borrow))),
        "status" => item.status.take_in(|s| f(Value::Borrow(&s))),
        "tooltip" => item.tooltip.take_in(|t| f(t.as_deref().map_or(Value::Null, Value::Borrow))),
        "id" => item.id.take_in(|id| f(Value::Borrow(&id))),
        _ => f(Value::Null)
    }
}

pub fn write(name : &str, item: &TrayItem, key : &str, value : Value, rt : &Runtime) {
    // Ignored for now, see EventSink::from_tray
    let _ = (name, item, key, value, rt);
}

/// A click or scroll on the tray icon itself
pub fn do_click(item : &Rc<TrayItem>, how : u32) {
    let method = match how {
        0 => "Activate",
        1 => "ContextMenu",
        2 => "SecondaryActivate",
        5 | 6 => "vertical",
        7 | 8 => "horizontal",
        _ => return,
    };

    let dbus = DBus::get_session();
    let sni_path = if item.is_kde { "org.kde.StatusNotifierItem" } else { "org.freedesktop.StatusNotifierItem" };

    debug!("Invoking {} on {}", method, item.id.take_in(|i| i.clone()));
    let _ = (|| -> zbus::Result<()> {
        if how < 3 {
            dbus.send(zbus::MessageBuilder::method_call(&*item.path, method)?
                .destination(&*item.owner)?
                .interface(sni_path)?
                .with_flags(zbus::MessageFlags::NoReplyExpected)?
                .build(&(0i32,0i32))?
            );
        } else {
            dbus.send(zbus::MessageBuilder::method_call(&*item.path, "Scroll")?
                .destination(&*item.owner)?
                .interface(sni_path)?
                .with_flags(zbus::MessageFlags::NoReplyExpected)?
                .build(&(15i32,method))?
            );
        }
        Ok(())
    })();
}
