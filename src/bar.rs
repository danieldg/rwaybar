use log::error;
use std::convert::TryInto;
use std::time::Instant;
use std::rc::Rc;
use smithay_client_toolkit::output::OutputInfo;
use smithay_client_toolkit::shell::layer::{Anchor,Layer,LayerSurface};
use wayland_client::protocol::wl_output::WlOutput;

use crate::event::EventSink;
use crate::item::*;
use crate::render::Renderer;
use crate::state::{NotifierList,Runtime};
use crate::util::{spawn_noerr, UID};
use crate::wayland::{Button,Popup,SurfaceData,SurfaceEvents,WaylandClient};

#[derive(Debug)]
pub struct BarPopup {
    pub wl : Popup,
    desc : PopupDesc,
    vanish : Option<Instant>,
}

/// A single taskbar on a single output
#[derive(Debug)]
pub struct Bar {
    pub name : Box<str>,
    pub ls: LayerSurface,
    pub popup : Option<BarPopup>,
    pub sink : EventSink,
    pub anchor_top : bool,
    click_size : u32,
    sparse : bool,
    pub item : Rc<Item>,
    pub cfg_index : usize,
    pub id: UID,
}

impl Bar {
    pub fn new(wayland : &mut WaylandClient, output : &WlOutput, output_data : &OutputInfo, cfg : toml::Value, cfg_index : usize) -> Bar {
        let scale = output_data.scale_factor;
        let layer = match cfg.get("layer").and_then(|v| v.as_str()) {
            Some("overlay") => Layer::Overlay,
            Some("bottom") => Layer::Bottom,
            Some("top") | None => Layer::Top,
            Some(layer) => {
                error!("Unknown layer '{layer}', defaulting to top");
                Layer::Top
            }
        };
        let size = cfg.get("size")
            .and_then(|v| v.as_integer())
            .filter(|&v| v > 0 && v < i32::MAX as _)
            .and_then(|v| v.try_into().ok())
            .unwrap_or(20);
        let size_excl = cfg.get("size-exclusive")
            .and_then(|v| v.as_integer())
            .filter(|&v| v >= -1 && v < i32::MAX as _)
            .and_then(|v| v.try_into().ok())
            .unwrap_or(size as i32);
        let click_size = cfg.get("size-clickable")
            .and_then(|v| v.as_integer())
            .filter(|&v| v > 0 && v < i32::MAX as _)
            .and_then(|v| v.try_into().ok())
            .or_else(|| size_excl.try_into().ok().filter(|&v| v > 0))
            .unwrap_or(size);
        let anchor_top = match cfg.get("side").and_then(|v| v.as_str()) {
            Some("top") => true,
            None | Some("bottom") => false,
            Some(side) => {
                error!("Unknown side '{}', defaulting to bottom", side);
                false
            }
        };

        let builder = LayerSurface::builder()
            .namespace("bar")
            .output(output)
            .size((0, size))
            .anchor(if anchor_top {
                Anchor::TOP | Anchor::LEFT | Anchor::RIGHT
            } else {
                Anchor::BOTTOM | Anchor::LEFT | Anchor::RIGHT
            })
            .exclusive_zone(size_excl);
        let ls = wayland.create_layer_surface(builder, layer, scale);
        let sparse = cfg.get("sparse-clicks").and_then(|v| v.as_bool()).unwrap_or(true);
        if size != click_size {
            // Only handle input in the exclusive region; clicks in the overhang region will go
            // through to the window we cover (hopefully transparently, to avoid confusion)
            let region = wayland.compositor.create_region(&wayland.queue).unwrap();
            let yoff = if anchor_top {
                0
            } else {
                size.saturating_sub(click_size) as i32
            };
            if sparse {
                // start with an empty region to match the empty EventSink
            } else {
                region.add(0, yoff, i32::MAX, click_size as i32);
            }
            ls.wl_surface().set_input_region(Some(&region));
            region.destroy();
        }
        // This will get applied before we commit the first buffer
        ls.wl_surface().set_buffer_scale(scale);

        Bar {
            name : output_data.name.clone().unwrap_or_default().into(),
            ls,
            item : Rc::new(Item::new_bar(cfg)),
            click_size,
            anchor_top,
            sink : EventSink::default(),
            sparse,
            popup : None,
            cfg_index,
            id: UID::new(),
        }
    }

    pub fn render_with(&mut self, runtime : &mut Runtime, renderer: &mut Renderer) {
        runtime.items.insert("bar".into(), self.item.clone());

        let surface_data = SurfaceData::from_wl(self.ls.wl_surface());
        if surface_data.start_render() {
            let surf = self.ls.wl_surface();
            renderer.render(runtime, surf, |ctx| {
                let new_sink = ctx.runtime.items["bar"].render(ctx);

                if self.sparse {
                    let mut old_regions = Vec::new();
                    let mut new_regions = Vec::new();
                    self.sink.for_active_regions(|lo, hi| {
                        old_regions.push((lo as i32, (hi - lo) as i32));
                    });
                    new_sink.for_active_regions(|lo, hi| {
                        new_regions.push((lo as i32, (hi - lo) as i32));
                    });

                    if old_regions != new_regions {
                        let region = ctx.runtime.wayland.compositor.create_region(&ctx.runtime.wayland.queue).unwrap();
                        let yoff = if self.anchor_top {
                            0
                        } else {
                            surface_data.height().saturating_sub(self.click_size) as i32
                        };
                        for (lo, len) in new_regions {
                            region.add(lo, yoff, len, self.click_size as i32);
                        }
                        surf.set_input_region(Some(&region));
                        region.destroy()
                    }
                }
                self.sink = new_sink;
            });

        }
        if let Some(popup) = &mut self.popup {
            if popup.vanish.map_or(false, |vanish| vanish < Instant::now()) {
                self.popup = None;
            }
        }
        let mut scale = 1;
        let popup = self.popup.as_mut().and_then(|popup| {
            let surface_data = SurfaceData::from_wl(&popup.wl.surf);
            if surface_data.start_render() {
                scale = surface_data.scale_factor();
                Some(popup)
            } else {
                None
            }
        });

        if let Some(popup) = popup {
            if let Some(new_size) = renderer.render(runtime, &popup.wl.surf, |ctx| {
                popup.desc.render_popup(ctx)
            }) {
                if new_size.0 > popup.wl.req_size.0 || new_size.1 > popup.wl.req_size.1 ||
                    new_size.0 + 10 < popup.wl.req_size.0 || new_size.1 + 10 < popup.wl.req_size.1
                {
                    match self.ls.kind() {
                        smithay_client_toolkit::shell::layer::SurfaceKind::Wlr(ls) => {
                            popup.wl.resize(&mut runtime.wayland, &ls, new_size, scale);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

impl SurfaceEvents for Bar {
    fn hover(&mut self, (x, y) : (f64, f64), runtime : &mut Runtime) {
        if let Some((min_x, max_x, desc)) = self.sink.get_hover(x as f32, y as f32) {
            if let Some(popup) = &mut self.popup {
                if x < popup.wl.anchor.0 as f64 || x > (popup.wl.anchor.0 + popup.wl.anchor.2) as f64 {
                    self.popup = None;
                } else if popup.desc == *desc {
                    return;
                } else {
                    self.popup = None;
                }
            }
            let surf_data = SurfaceData::from_wl(self.ls.wl_surface());
            let anchor = (min_x as i32, 0, (max_x - min_x) as i32, surf_data.height() as i32);

            runtime.items.insert("bar".into(), self.item.clone());
            let size = Renderer::render_dummy(runtime, |ctx| {
                desc.render_popup(ctx)
            });
            if size.0 <= 0 || size.1 <= 0 {
                return;
            }
            let desc = desc.clone();

            let popup = BarPopup {
                wl : Popup::on_bar(&mut runtime.wayland, self, anchor, size),
                desc,
                vanish : None,
            };
            self.popup = Some(popup);
        }
    }

    fn no_hover(&mut self, runtime : &mut Runtime) {
        if let Some(popup) = &mut self.popup {
            let vanish = Instant::now() + std::time::Duration::from_millis(100);
            popup.vanish = Some(vanish);
            let mut notify = NotifierList::active(runtime);
            spawn_noerr(async move {
                tokio::time::sleep_until(vanish.into()).await;
                notify.notify_data("bar-hover");
            });
        }
    }

    fn button(&mut self, (x, y): (f64, f64), button : Button, runtime : &mut Runtime) {
        self.sink.button(x as f32, y as f32, button, runtime);
    }
}

impl SurfaceEvents for BarPopup {
    fn hover(&mut self, _ : (f64, f64), _: &mut Runtime) {
        self.vanish = None;
    }

    fn no_hover(&mut self, runtime : &mut Runtime) {
        let vanish = Instant::now() + std::time::Duration::from_millis(100);
        self.vanish = Some(vanish);
        let mut notify = NotifierList::active(runtime);
        spawn_noerr(async move {
            tokio::time::sleep_until(vanish.into()).await;
            notify.notify_data("bar-hover");
        });
    }

    fn button(&mut self, (x, y) : (f64, f64), button : Button, runtime : &mut Runtime) {
        self.desc.button(x, y, button, runtime);
    }
}
