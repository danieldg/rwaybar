use log::error;
use std::time::Instant;
use std::rc::Rc;
use wayland_client::Attached;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_client::protocol::wl_callback::WlCallback;
use wayland_client::protocol::wl_compositor::WlCompositor;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;

use layer_shell::zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer};
use layer_shell::zwlr_layer_surface_v1::{ZwlrLayerSurfaceV1, Anchor};

use crate::item::*;
use crate::render::RenderTarget;
use crate::state::{Runtime,State};
use crate::util::Cell;
use crate::wayland::{Popup,WaylandClient};

pub struct BarPopup {
    pub wl : Popup,
    contents : cairo::RecordingSurface,
    desc : PopupDesc,
    vanish : Option<Instant>,
}

/// A single taskbar on a single output
pub struct Bar {
    pub surf : Attached<WlSurface>,
    pub ls_surf : Attached<ZwlrLayerSurfaceV1>,
    pub popup : Option<BarPopup>,
    pub sink : EventSink,
    pub anchor_top : bool,
    pub scale : i32,
    pixel_width : i32,
    size : i32,
    size_excl : u32,
    pub dirty : bool,
    sparse : bool,
    throttle : Option<Attached<WlCallback>>,
    pub item : Rc<Item>,
    pub cfg_index : usize,
}

impl Bar {
    pub fn new(wayland : &WaylandClient, output : &WlOutput, scale : i32, cfg : toml::Value, cfg_index : usize) -> Bar {
        let ls : Attached<ZwlrLayerShellV1> = wayland.env.require_global();
        let surf : Attached<_> = wayland.env.create_surface();
        let ls_surf = ls.get_layer_surface(&surf, Some(output), Layer::Top, "bar".to_owned());

        let size = cfg.get("size").and_then(|v| v.as_integer()).unwrap_or(20) as u32;
        let size_excl = cfg.get("size-exclusive")
            .and_then(|v| v.as_integer())
            .map(|v| v as u32)
            .unwrap_or(size);
        let anchor_top = match cfg.get("side").and_then(|v| v.as_str()) {
            Some("top") => true,
            None | Some("bottom") => false,
            Some(side) => {
                error!("Unknown side '{}', defaulting to bottom", side);
                false
            }
        };
        if anchor_top {
            ls_surf.set_anchor(Anchor::Top | Anchor::Left | Anchor::Right);
        } else {
            ls_surf.set_anchor(Anchor::Bottom | Anchor::Left | Anchor::Right);
        }
        ls_surf.set_size(0, size);
        ls_surf.set_exclusive_zone(size_excl as i32);
        let sparse = cfg.get("sparse-clicks").and_then(|v| v.as_bool()).unwrap_or(true);
        if !sparse && size != size_excl && size_excl != 0 {
            // Only handle input in the exclusive region; clicks in the overhang region will go
            // through to the window we cover (hopefully transparently, to avoid confusion)
            let comp : Attached<WlCompositor> = wayland.env.require_global();
            let region = comp.create_region();
            let yoff = if anchor_top {
                0
            } else {
                size.saturating_sub(size_excl) as i32
            };
            region.add(0, yoff, i32::MAX, size_excl as i32);
            surf.set_input_region(Some(&region));
            region.destroy();
        }
        ls_surf.quick_assign(move |ls_surf, event, mut data| {
            use layer_shell::zwlr_layer_surface_v1::Event;
            let state : &mut State = data.get().unwrap();
            match event {
                Event::Configure { serial, width, height } => {
                    for bar in &mut state.bars {
                        if bar.ls_surf != *ls_surf {
                            continue;
                        }

                        bar.pixel_width = width as i32 * bar.scale;
                        bar.size = height as i32;

                        ls_surf.ack_configure(serial);
                        bar.dirty = true;
                    }
                    state.request_draw();
                }
                Event::Closed => {
                    state.bars.retain(|bar| {
                        if bar.ls_surf == *ls_surf {
                            bar.ls_surf.destroy();
                            bar.surf.destroy();
                            false
                        } else {
                            true
                        }
                    });
                },
                _ => ()
            }
        });
        surf.set_buffer_scale(scale);

        surf.commit();

        Bar {
            surf,
            ls_surf : ls_surf.into(),
            item : Rc::new(Item::new_bar(cfg)),
            scale,
            pixel_width : 0,
            size : size as i32,
            size_excl,
            anchor_top,
            sink : EventSink::default(),
            dirty : false,
            sparse,
            throttle : None,
            popup : None,
            cfg_index,
        }
    }

    pub fn render_with(&mut self, runtime : &mut Runtime, target : &mut RenderTarget) {
        if self.dirty && self.throttle.is_none() {
            let rt_item = runtime.items.entry("bar".into()).or_insert_with(|| Rc::new(Item::none()));
            std::mem::swap(&mut self.item, rt_item);

            let surf = cairo::RecordingSurface::create(cairo::Content::ColorAlpha,
                cairo::Rectangle {
                    x : 0.0,
                    y : 0.0,
                    width : self.pixel_width as f64,
                    height : (self.size * self.scale) as f64,
                }).expect("Error creating bar rendering surface");
            let ctx = cairo::Context::new(&surf);
            let mut scale_matrix = cairo::Matrix::identity();
            scale_matrix.scale(self.scale as f64, self.scale as f64);
            ctx.set_matrix(scale_matrix);
            let font = pango::FontDescription::new();
            ctx.set_operator(cairo::Operator::Clear);
            ctx.paint();
            ctx.set_operator(cairo::Operator::Over);
            let render_extents = ctx.clip_extents();
            let render_pos = Cell::new(0.0);

            let ctx = Render {
                cairo : &ctx,
                font : &font,
                align : Align::bar_default(),
                render_extents : &render_extents,
                render_pos : &render_pos,
                render_ypos : None,
                err_name: "bar",
                text_stroke : None,
                runtime,
            };
            self.sink = ctx.runtime.items["bar"].render(&ctx);

            if self.sparse {
                let comp : Attached<WlCompositor> = target.wayland.env.require_global();
                let region = comp.create_region();
                let yoff = if self.anchor_top {
                    0
                } else {
                    (self.size as u32).saturating_sub(self.size_excl) as i32
                };
                self.sink.for_active_regions(|lo, hi| {
                    region.add(lo as i32, yoff, (hi - lo) as i32, self.size_excl as i32);
                });
                self.surf.set_input_region(Some(&region));
                region.destroy();
            }

            target.render((self.pixel_width, self.size * self.scale), &self.surf, &surf);
            std::mem::swap(&mut self.item, runtime.items.get_mut("bar").unwrap());

            let frame = self.surf.frame();
            let id = frame.as_ref().id();
            frame.quick_assign(move |_frame, _event, mut data| {
                let state : &mut State = data.get().unwrap();
                for bar in &mut state.bars {
                    let done = match bar.throttle.as_ref() {
                        Some(cb) if !cb.as_ref().is_alive() => true,
                        Some(cb) if cb.as_ref().id() == id => true,
                        _ => false,
                    };
                    if done {
                        bar.throttle.take();
                    }
                }
                state.request_draw();
            });
            self.surf.commit();
            self.throttle = Some(frame.into());
            self.dirty = false;
        }
        if let Some(popup) = &mut self.popup {
            if popup.vanish.map_or(false, |vanish| vanish < Instant::now()) {
                self.popup = None;
                return;
            }
            if popup.wl.waiting_on_configure {
                return;
            }
            let scale = popup.wl.scale;
            let pixel_size = (popup.wl.size.0 * scale, popup.wl.size.1 * scale);

            let new_size = popup.desc.render_popup(runtime, &popup.contents, scale);
            target.render(pixel_size, &popup.wl.surf, &popup.contents);
            popup.wl.surf.commit();
            if new_size.0 > popup.wl.size.0 || new_size.1 > popup.wl.size.1 {
                target.wayland.resize_popup(&self.ls_surf, &mut popup.wl, new_size, scale);
            }
        }
    }

    pub fn hover(&mut self, x : f64, y : f64, wayland : &WaylandClient, runtime : &Runtime) {
        if let Some((min_x, max_x, desc)) = self.sink.get_hover(x, y) {
            if let Some(popup) = &self.popup {
                if x < popup.wl.anchor.0 as f64 || x > (popup.wl.anchor.0 + popup.wl.anchor.2) as f64 {
                    self.popup = None;
                } else if popup.desc == *desc {
                    return;
                } else {
                    self.popup = None;
                }
            }
            let anchor = (min_x as i32, 0, (max_x - min_x) as i32, self.size as i32);
            let contents = cairo::RecordingSurface::create(cairo::Content::ColorAlpha,
                cairo::Rectangle {
                    x : 0.0,
                    y : 0.0,
                    width : self.pixel_width as f64,
                    height : self.pixel_width as f64 * 2.0,
                }).expect("Error creating popup rendering surface");

            let size = desc.render_popup(runtime, &contents, self.scale);
            if size.0 <= 0 || size.1 <= 0 {
                return;
            }

            let desc = desc.clone();
            let popup = BarPopup {
                wl : wayland.new_popup(self, anchor, size),
                desc,
                contents,
                vanish : None,
            };
            self.popup = Some(popup);
        }
    }

    pub fn no_hover(&mut self, runtime : &mut Runtime) {
        if let Some(popup) = &mut self.popup {
            let vanish = Instant::now() + std::time::Duration::from_millis(100);
            popup.vanish = Some(vanish);
            runtime.set_wake_at(vanish, "bar-hover");
        }
    }

    pub fn hover_popup(&mut self, x : f64, y : f64, _wayland : &WaylandClient, _runtime : &Runtime) {
        if let Some(popup) = &mut self.popup {
            popup.vanish = None;
            let _ = (x, y);
        }
    }

    pub fn popup_button(&mut self, x : f64, y : f64, button : u32, runtime : &mut Runtime) {
        if let Some(popup) = &mut self.popup {
            popup.desc.button(x, y, button, runtime);
        }
    }
}

impl Drop for Bar {
    fn drop(&mut self) {
        self.ls_surf.destroy();
        self.surf.destroy();
    }
}
