use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::output::OutputInfo;
use smithay_client_toolkit::seat::SeatData;
use smithay_client_toolkit::shm::MemPool;
use std::collections::HashMap;
use std::error::Error;
use std::time::Instant;
use wayland_client::Attached;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_client::protocol::wl_seat::WlSeat;
use wayland_client::protocol::wl_pointer::Event as MouseEvent;
use wayland_client::protocol::wl_touch::Event as TouchEvent;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;
use json::JsonValue;

use layer_shell::zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer};
use layer_shell::zwlr_layer_surface_v1::Anchor;
use layer_shell::zwlr_layer_surface_v1::Event as LayerSurfaceEvent;

use crate::item::*;
use crate::data::DataSource;

struct Bar {
    surf : Attached<WlSurface>,
    width : i32,
    height : i32,
    dirty : bool,
    left : Item,
    center : Item,
    right : Item,
}

impl Bar {
    fn render(&self, surf : &cairo::ImageSurface, data : &HashMap<String, String>, items : &HashMap<String, Item>) {
        let ctx = cairo::Context::new(surf);
        let font = cairo::FontFace::toy_create("Liberation Sans", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        ctx.set_operator(cairo::Operator::Clear);
        ctx.set_source_rgba(0.0, 0.0, 0.0, 0.0);
        ctx.paint();
        ctx.set_operator(cairo::Operator::Over);
        ctx.set_source_rgba(0.0, 0.0, 0.0, 1.0);
        ctx.set_font_face(&font);
        ctx.set_font_size(self.height as f64 * 0.8);

        let ctx = Render {
            pango : pangocairo::create_context(&ctx).unwrap(),
            cairo : ctx,
            data,
            items,
        };

        ctx.cairo.push_group();
        ctx.cairo.move_to(0.0, self.height as f64 * 0.8);
        self.left.render(&ctx);
        let left_size = ctx.cairo.get_current_point();
        let left = ctx.cairo.pop_group();

        ctx.cairo.push_group();
        ctx.cairo.move_to(0.0, self.height as f64 * 0.8);
        self.center.render(&ctx);
        let cent_size = ctx.cairo.get_current_point();
        let cent = ctx.cairo.pop_group();

        ctx.cairo.push_group();
        ctx.cairo.move_to(0.0, self.height as f64 * 0.8);
        self.right.render(&ctx);
        let right_size = ctx.cairo.get_current_point();
        let right = ctx.cairo.pop_group();

        ctx.cairo.set_source(&left);
        ctx.cairo.paint();
        let mut m = cairo::Matrix::identity();
        m.x0 = right_size.0 - (self.width as f64);
        right.set_matrix(m);
        ctx.cairo.set_source(&right);
        ctx.cairo.paint();

        let max_side = (self.width as f64 - cent_size.0) / 2.0;
        let total_room = self.width as f64 - (left_size.0 + right_size.0 + cent_size.0);
        if left_size.0 < max_side && right_size.0 < max_side {
            // Actually center the center module
            let mut m = cairo::Matrix::identity();
            m.x0 = -max_side;
            cent.set_matrix(m);
        } else if total_room >= 0.0 {
            // At least it will fit somewhere
            let mut m = cairo::Matrix::identity();
            m.x0 = -(left_size.0 + total_room / 2.0);
            cent.set_matrix(m);
        } else {
            return;
        }
        ctx.cairo.set_source(&cent);
        ctx.cairo.paint();
    }
}

pub struct State {
    pub eloop : calloop::LoopHandle<State>,
    pub wake_at : Option<Instant>,
    pub env : Environment<super::MyEnv>,
    pub ls : Attached<ZwlrLayerShellV1>,
    pub shm : MemPool,
    bars : Vec<Bar>,
    pub display : wayland_client::Display,
    pub sources : Vec<DataSource>,
    pub data : HashMap<String, String>,
    pub items : HashMap<String, Item>,
    pub config : JsonValue,
}

impl State {
    pub fn new(env : Environment<super::MyEnv>, eloop : calloop::LoopHandle<State>, display : wayland_client::Display) -> Result<Self, Box<dyn Error>> {
        let shm = env.create_simple_pool(|_| ())?;
        let ls = env.require_global();

        let cfg = std::fs::read_to_string("rwaybar.json")?;
        let config = json::parse(&cfg)?;

        let items = config["items"].entries().map(|(key, value)| {
            let key = key.to_owned();
            let value = Item::from_json_txt(value);
            (key, value)
        }).collect();

        let sources = config["vars"].entries().map(DataSource::new).collect();

        let mut state = Self {
            env,
            ls,
            eloop,
            shm,
            wake_at : None,
            bars : Vec::new(),
            display,
            sources,
            data : HashMap::new(),
            items,
            config,
        };

        for src in &mut state.sources {
            src.init(&mut state.data);
        }
        state.set_data();
        Ok(state)
    }

    fn config(&mut self, id : usize, width : i32, height : i32) {
        self.bars[id].width = width;
        self.bars[id].height = height;
    }

    fn draw(&mut self) -> Result<(), Box<dyn Error>> {
        let mut shm_size = 0;
        let shm_pos : Vec<_> = self.bars.iter().map(|bar| {
            let pos = shm_size;
            let len = if bar.dirty {
                let stride = cairo::Format::ARgb32.stride_for_width(bar.width as u32).unwrap();
                (bar.height as usize) * (stride as usize)
            } else {
                0
            };
            shm_size += len;
            (pos, len)
        }).collect();

        if shm_size == 0 {
            return Ok(());
        }

        self.shm.resize(shm_size).expect("OOM");

        for (bar, (pos, len)) in self.bars.iter_mut().zip(shm_pos) {
            if !bar.dirty {
                continue;
            }
            let stride = cairo::Format::ARgb32.stride_for_width(bar.width as u32).unwrap();
            let buf : &mut [u8] = &mut self.shm.mmap().as_mut()[pos..][..len];
            unsafe {
                // cairo::ImageSurface::create_for_data requires a 'static type, so give it that
                // (this could be done safely by using a wrapper object and mem::replace on the shm object)
                let buf : &'static mut [u8] = &mut *(buf as *mut [u8]);
                let surf = cairo::ImageSurface::create_for_data(buf, cairo::Format::ARgb32, bar.width, bar.height, stride)?;
                // safety: ImageSurface never gives out direct access to D
                bar.render(&surf, &self.data, &self.items);
                // safety: we must finish the cairo surface to end the 'static borrow
                surf.finish();
                drop(surf);
            }
            let buf = self.shm.buffer(pos as i32, bar.width, bar.height, stride, smithay_client_toolkit::shm::Format::Argb8888);
            bar.surf.attach(Some(&buf), 0, 0);
            bar.surf.damage_buffer(0, 0, bar.width, bar.height);
            bar.surf.commit();
            bar.dirty = false;
        }
        self.display.flush()?;
        Ok(())
    }

    pub fn add_output(&mut self, output : &WlOutput, oi : &OutputInfo) {
        dbg!(oi);
        // TODO use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_manager_v1::ZxdgOutputManagerV1
        // to get wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_v1::Event
        // which has a better Name event
        for cfg in self.config["bars"].members() {
            if let Some(id) = cfg["wl_id"].as_u32() {
                if oi.id != id {
                    continue;
                }
            }
            if let Some(model) = cfg["model"].as_str() {
                if model != &oi.model {
                    continue;
                }
            }

            let bar = self.new_bar(output, cfg);
            self.bars.push(bar);
        }
    }

    fn new_bar(&self, output : &WlOutput, cfg : &JsonValue) -> Bar {
        let i = self.bars.len();
        let surf : Attached<_> = self.env.create_surface();
        let ls_surf = self.ls.get_layer_surface(&surf, Some(output), Layer::Top, "bar".to_owned());

        ls_surf.set_size(0, 20);
        ls_surf.set_anchor(Anchor::Top | Anchor::Left | Anchor::Right);
        ls_surf.set_exclusive_zone(20);
        ls_surf.quick_assign(move |ls_surf, event, mut data| {
            let state : &mut State = data.get().unwrap();
            match event {
                LayerSurfaceEvent::Configure { serial, width, height } => {
                    state.config(i, width as i32, height as i32);
                    ls_surf.ack_configure(serial);
                    if !state.bars[i].dirty {
                        state.bars[i].dirty = true;
                        state.eloop.insert_idle(|state| state.draw().unwrap());
                    }
                }
                LayerSurfaceEvent::Closed => {
                    todo!();
                },
                _ => ()
            }
        });

        surf.commit();

        let left = Item::from_json_ref(&cfg["left"]);
        let right = Item::from_json_ref(&cfg["right"]);
        let center = Item::from_json_ref(&cfg["center"]);

        Bar {
            surf,
            left,
            center,
            right,
            width : 0,
            height : 0,
            dirty : false,
        }
    }

    pub fn add_seat(&mut self, seat : &Attached<WlSeat>, si : &SeatData) {
        dbg!(seat, si);
        if si.has_pointer {
            let mouse = seat.get_pointer();
            mouse.quick_assign(move |mouse, event, mut data| {
                let state : &mut State = data.get().unwrap();
                drop((state, mouse));
                match event {
                    MouseEvent::Button { .. } => {
                        dbg!(event);
                    }
                    _ => ()
                }
            });
        }
        if si.has_touch {
            let finger = seat.get_touch();
            finger.quick_assign(move |finger, event, mut data| {
                let state : &mut State = data.get().unwrap();
                drop((state, finger));
                match event {
                    TouchEvent::Down { .. } => {
                        dbg!(event);
                    }
                    _ => ()
                }
            });
        }
    }

    fn set_data(&mut self) {
        for src in &mut self.sources {
            src.update(&mut self.wake_at, &mut self.data);
        }

        // TODO maybe don't refresh all bars all the time?  Needs real dirty tracking.
        for bar in &mut self.bars {
            bar.dirty = true;
        }
    }

    pub fn tick(&mut self) {
        self.set_data();
        self.draw().unwrap();
    }
}
