use json::JsonValue;
use linked_hash_map::LinkedHashMap;
use log::{info,warn,error};
use std::cell::Cell;
use std::collections::HashMap;
use std::error::Error;
use std::time::Instant;
use wayland_client::Attached;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;

use layer_shell::zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer};
use layer_shell::zwlr_layer_surface_v1::{ZwlrLayerSurfaceV1, Anchor};

use crate::item::*;
use crate::data::Variable;
use crate::wayland::WaylandClient;

/// A single taskbar on a single output
pub struct Bar {
    pub surf : Attached<WlSurface>,
    pub ls_surf : Attached<ZwlrLayerSurfaceV1>,
    pub sink : EventSink,
    width : i32,
    height : i32,
    dirty : bool,
    item : Item,
}

impl Bar {
    fn render(&mut self, surf : &cairo::ImageSurface, runtime : &Runtime) {
        let ctx = cairo::Context::new(surf);
        let font = pango::FontDescription::new();
        ctx.set_operator(cairo::Operator::Clear);
        ctx.paint();
        ctx.set_operator(cairo::Operator::Over);
        ctx.move_to(0.0, 0.0);

        let ctx = Render {
            cairo : &ctx,
            font : &font,
            runtime,
        };

        self.sink = self.item.render(&ctx);
    }
}

/// Common state available during rendering operations
pub struct Runtime {
    pub eloop : calloop::LoopHandle<State>,
    pub wake_at : Cell<Option<Instant>>,
    pub vars : LinkedHashMap<String, Variable>,
    pub items : HashMap<String, Item>,
}

impl Runtime {
    pub fn set_wake_at(&self, wake : Instant) {
        match self.wake_at.get() {
            Some(then) if wake > then => {}
            _ => { self.wake_at.set(Some(wake)) }
        }
    }

    pub fn format(&self, fmt : &str) -> Result<String, strfmt::FmtError> {
        strfmt::strfmt_map(fmt, &|mut q| {
            let (name, key) = match q.key.find('.') {
                Some(p) => (&q.key[..p], &q.key[p + 1..]),
                None => (&q.key[..], ""),
            };
            match self.vars.get(name) {
                Some(var) => {
                    var.read_in(name, key, self, |s| q.str(s))
                }
                None => Err(strfmt::FmtError::KeyError(name.to_string()))
            }
        })
    }

    pub fn format_or(&self, fmt : &str, context : &str) -> String {
        match self.format(fmt) {
            Ok(v) => v,
            Err(e) => {
                warn!("Error formatting '{}': {}", context, e);
                String::new()
            }
        }
    }
}

/// The singleton global state object bound to the calloop runner
pub struct State {
    pub wayland : WaylandClient,
    pub bars : Vec<Bar>,
    config : JsonValue,
    pub runtime : Runtime,
    draw_pending : bool,
}

impl State {
    pub fn new(wayland : WaylandClient, eloop : calloop::LoopHandle<State>) -> Result<Self, Box<dyn Error>> {
        let cfg = std::fs::read_to_string("rwaybar.json")?;
        let config = json::parse(&cfg)?;

        let items = config["items"].entries().map(|(key, value)| {
            let key = key.to_owned();
            let value = Item::from_json_txt(value);
            (key, value)
        }).collect();

        let vars = config["vars"].entries().map(Variable::new).collect();

        let mut state = Self {
            wayland,
            bars : Vec::new(),
            runtime : Runtime {
                eloop,
                wake_at : Cell::new(None),
                vars,
                items,
            },
            config,
            draw_pending : false,
        };

        state.runtime.vars.insert("item".into(), Variable::new_current_item());

        for (k,v) in &state.runtime.vars {
            v.init(k, &state.runtime);
        }
        state.set_data();
        Ok(state)
    }

    pub fn request_draw(&mut self) {
        if !self.draw_pending {
            self.draw_pending = true;
            self.runtime.eloop.insert_idle(|state| state.tick());
        }
    }

    fn draw_now(&mut self) -> Result<(), Box<dyn Error>> {
        let mut shm_size = 0;
        let shm_pos : Vec<_> = self.bars.iter().map(|bar| {
            let pos = shm_size;
            let len = if bar.dirty || self.draw_pending {
                let stride = cairo::Format::ARgb32.stride_for_width(bar.width as u32).unwrap();
                (bar.height as usize) * (stride as usize)
            } else {
                0
            };
            shm_size += len;
            (pos, len)
        }).collect();
        self.draw_pending = false;

        if shm_size == 0 {
            return Ok(());
        }

        self.wayland.shm.resize(shm_size).expect("OOM");

        for (bar, (pos, len)) in self.bars.iter_mut().zip(shm_pos) {
            if !bar.dirty {
                continue;
            }
            let stride = cairo::Format::ARgb32.stride_for_width(bar.width as u32).unwrap();
            let buf : &mut [u8] = &mut self.wayland.shm.mmap().as_mut()[pos..][..len];
            unsafe {
                // cairo::ImageSurface::create_for_data requires a 'static type, so give it that
                // (this could be done safely by using a wrapper object and mem::replace on the shm object)
                let buf : &'static mut [u8] = &mut *(buf as *mut [u8]);
                let surf = cairo::ImageSurface::create_for_data(buf, cairo::Format::ARgb32, bar.width, bar.height, stride)?;
                // safety: ImageSurface never gives out direct access to D
                bar.render(&surf, &self.runtime);
                // safety: we must finish the cairo surface to end the 'static borrow
                surf.finish();
                drop(surf);
            }
            let buf = self.wayland.shm.buffer(pos as i32, bar.width, bar.height, stride, smithay_client_toolkit::shm::Format::Argb8888);
            bar.surf.attach(Some(&buf), 0, 0);
            bar.surf.damage_buffer(0, 0, bar.width, bar.height);
            bar.surf.commit();
            bar.dirty = false;
        }
        self.wayland.display.flush()?;
        Ok(())
    }

    pub fn output_ready(&mut self, i : usize) {
        let data = &self.wayland.outputs[i];
        info!("Output[{}] name='{}' description='{}' at {},{} {}x{}",
            i, data.name, data.description, data.pos_x, data.pos_y, data.size_x, data.size_y);
        for cfg in self.config["bars"].members() {
            if let Some(name) = cfg["name"].as_str() {
                if name != &data.name {
                    continue;
                }
            }

            let bar = self.new_bar(&data.output, cfg);
            self.bars.push(bar);
        }
    }

    fn new_bar(&self, output : &WlOutput, cfg : &JsonValue) -> Bar {
        let i = self.bars.len();
        let ls : Attached<ZwlrLayerShellV1> = self.wayland.env.require_global();
        let surf : Attached<_> = self.wayland.env.create_surface();
        let ls_surf = ls.get_layer_surface(&surf, Some(output), Layer::Top, "bar".to_owned());

        let size = cfg["size"].as_u32().unwrap_or(20);

        match cfg["side"].as_str() {
            Some("top") => {
                ls_surf.set_size(0, size);
                ls_surf.set_anchor(Anchor::Top | Anchor::Left | Anchor::Right);
            }
            None | Some("bottom") => {
                ls_surf.set_size(0, size);
                ls_surf.set_anchor(Anchor::Bottom | Anchor::Left | Anchor::Right);
            }
            Some(side) => {
                error!("Unknown side '{}', defaulting to bottom", side);
                ls_surf.set_size(0, size);
                ls_surf.set_anchor(Anchor::Bottom | Anchor::Left | Anchor::Right);
            }
        }
//        ls_surf.set_exclusive_zone(size);
        ls_surf.quick_assign(move |ls_surf, event, mut data| {
            use layer_shell::zwlr_layer_surface_v1::Event;
            let state : &mut State = data.get().unwrap();
            match event {
                Event::Configure { serial, width, height } => {
                    state.bars[i].width = width as i32;
                    state.bars[i].height = height as i32;

                    ls_surf.ack_configure(serial);
                    if !state.bars[i].dirty {
                        state.bars[i].dirty = true;
                    }
                    state.request_draw();
                }
                Event::Closed => {
                    todo!();
                },
                _ => ()
            }
        });

        surf.commit();

        Bar {
            surf,
            ls_surf : ls_surf.into(),
            item : Item::new_bar(cfg),
            width : 0,
            height : 0,
            sink : EventSink::default(),
            dirty : false,
        }
    }

    fn set_data(&mut self) {
        for (k, v) in &self.runtime.vars {
            v.update(k, &self.runtime);
        }

        // TODO maybe don't refresh all bars all the time?  Needs real dirty tracking.
        for bar in &mut self.bars {
            bar.dirty = true;
        }
    }

    pub fn tick(&mut self) {
        self.set_data();
        self.draw_now().expect("Render error");
    }
}
