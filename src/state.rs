use smithay_client_toolkit::environment::Environment;
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
use wayland_client::protocol::wl_pointer::{ButtonState,Axis};
use wayland_client::protocol::wl_touch::Event as TouchEvent;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;
use wayland_protocols::xdg_shell::client::xdg_wm_base::XdgWmBase;
use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_manager_v1::ZxdgOutputManagerV1;
use json::JsonValue;
use log::{debug,info,error};

use layer_shell::zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer};
use layer_shell::zwlr_layer_surface_v1::{ZwlrLayerSurfaceV1, Anchor};
use layer_shell::zwlr_layer_surface_v1::Event as LayerSurfaceEvent;

use crate::item::*;
use crate::data::Variable;

struct Bar {
    surf : Attached<WlSurface>,
    ls_surf : Attached<ZwlrLayerSurfaceV1>,
    sink : EventSink,
    width : i32,
    height : i32,
    dirty : bool,
    left : Item,
    center : Item,
    right : Item,
}

impl Bar {
    fn render(&mut self, surf : &cairo::ImageSurface, runtime : &Runtime) {
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
            runtime,
        };

        ctx.cairo.push_group();
        ctx.cairo.move_to(0.0, self.height as f64 * 0.8);
        let mut left_ev = self.left.render(&ctx);
        let left_size = ctx.cairo.get_current_point();
        let left = ctx.cairo.pop_group();

        ctx.cairo.push_group();
        ctx.cairo.move_to(0.0, self.height as f64 * 0.8);
        let mut cent_ev = self.center.render(&ctx);
        let cent_size = ctx.cairo.get_current_point();
        let cent = ctx.cairo.pop_group();

        ctx.cairo.push_group();
        ctx.cairo.move_to(0.0, self.height as f64 * 0.8);
        let mut right_ev = self.right.render(&ctx);
        let right_size = ctx.cairo.get_current_point();
        let right = ctx.cairo.pop_group();

        left_ev.offset_clamp(0.0, left_size.0);
        self.sink = left_ev;

        ctx.cairo.set_source(&left);
        ctx.cairo.paint();
        let mut m = cairo::Matrix::identity();
        m.x0 = right_size.0 - (self.width as f64);
        right_ev.offset_clamp(-m.x0, self.width as f64);
        self.sink.merge(right_ev);
        right.set_matrix(m);
        ctx.cairo.set_source(&right);
        ctx.cairo.paint();

        let max_side = (self.width as f64 - cent_size.0) / 2.0;
        let total_room = self.width as f64 - (left_size.0 + right_size.0 + cent_size.0);
        if left_size.0 < max_side && right_size.0 < max_side {
            // Actually center the center module
            m.x0 = -max_side;
        } else if total_room >= 0.0 {
            // At least it will fit somewhere
            m.x0 = -(left_size.0 + total_room / 2.0);
        } else {
            return;
        }
        cent.set_matrix(m);
        cent_ev.offset_clamp(-m.x0, cent_size.0 - m.x0);
        self.sink.merge(cent_ev);
        ctx.cairo.set_source(&cent);
        ctx.cairo.paint();
    }

    #[allow(dead_code)] // TODO wire up
    fn do_popup(&self, env : &Environment<super::Globals>) {
        let surf = env.create_surface();
        let wmb : Attached<XdgWmBase> = env.require_global();
        let pos = wmb.create_positioner();
/* TODO
        pos.set_size(232, 87)
        pos.set_anchor_rect(3614, -4, 122, 56) // gtk adds 4 pixels padding to the object
        pos.set_offset(0, 0)
        pos.set_anchor(2)
        pos.set_gravity(2)
        pos.set_constraint_adjustment(9)
*/
        let xdg_surf = wmb.get_xdg_surface(&surf);
        let popup = xdg_surf.get_popup(None, &pos);
        self.ls_surf.get_popup(&popup);
        xdg_surf.set_window_geometry(0, 0, 100, 100);
        surf.commit();
        // TODO handle Configure on popup
        // TODO handle Configure on surface by ACKing
        // TODO actually draw something
    }
}

pub struct Runtime {
    pub eloop : calloop::LoopHandle<State>,
    pub wake_at : Option<Instant>,
    pub sources : Vec<Variable>,
    pub vars : HashMap<String, String>,
    pub items : HashMap<String, Item>,
}

#[derive(Debug)]
pub struct OutputData {
    output : WlOutput,

    pos_x : i32,
    pos_y : i32,
    size_x : i32,
    size_y : i32,
    name : String,
    description: String,
}

pub struct State {
    pub env : Environment<super::Globals>,
    pub shm : MemPool,
    bars : Vec<Bar>,
    pub display : wayland_client::Display,
    pub outputs : Vec<OutputData>,
    pub config : JsonValue,
    pub runtime : Runtime,
    cursor : wayland_cursor::Cursor,
    cursor_surf : Attached<WlSurface>,
    draw_pending : bool,
}

impl State {
    pub fn new(env : Environment<super::Globals>, eloop : calloop::LoopHandle<State>, display : wayland_client::Display) -> Result<Self, Box<dyn Error>> {
        let shm = env.create_simple_pool(|_| ())?;

        let cursor_shm = env.require_global();
        let mut cursor_theme = wayland_cursor::CursorTheme::load(32, &cursor_shm);
        let cursor = cursor_theme.get_cursor("default").expect("Could not load cursor, check XCURSOR_THEME").clone();

        let cursor_surf = env.create_surface();
        let cursor_img = &cursor[0];
        let dim = cursor_img.dimensions();
        cursor_surf.attach(Some(&cursor_img), 0, 0);
        cursor_surf.damage_buffer(0, 0, dim.0 as _, dim.1 as _);
        cursor_surf.commit();

        let cfg = std::fs::read_to_string("rwaybar.json")?;
        let config = json::parse(&cfg)?;

        let items = config["items"].entries().map(|(key, value)| {
            let key = key.to_owned();
            let value = Item::from_json_txt(value);
            (key, value)
        }).collect();

        let sources = config["vars"].entries().map(Variable::new).collect();

        let mut state = Self {
            env,
            shm,
            bars : Vec::new(),
            display,
            outputs : Vec::new(),
            runtime : Runtime {
                eloop,
                wake_at : None,
                sources,
                vars : HashMap::new(),
                items,
            },
            config,
            cursor,
            cursor_surf,
            draw_pending : false,
        };

        for src in &mut state.runtime.sources {
            src.init(&state.runtime.eloop, &mut state.runtime.vars);
        }
        state.set_data();
        Ok(state)
    }

    fn config(&mut self, id : usize, width : i32, height : i32) {
        self.bars[id].width = width;
        self.bars[id].height = height;
    }

    pub fn draw(&mut self) {
        if !self.draw_pending {
            self.draw_pending = true;
            self.runtime.eloop.insert_idle(|state| state.draw_now().expect("Render error"));
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
                bar.render(&surf, &self.runtime);
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

    pub fn add_output(&mut self, output : &WlOutput) {
        for (i, data) in self.outputs.iter().enumerate() {
            if data.output == *output {
                self.output_ready(i);
                return;
            }
        }
        let i = self.outputs.len();
        let mgr : Attached<ZxdgOutputManagerV1> = self.env.require_global();
        let xdg_out = mgr.get_xdg_output(output);

        xdg_out.quick_assign(move |_xdg_out, event, mut data| {
            use wayland_protocols::unstable::xdg_output::v1::client::zxdg_output_v1::Event;
            let state : &mut State = data.get().unwrap();
            let data = &mut state.outputs[i];
            match event {
                Event::LogicalPosition { x, y } => {
                    data.pos_x = x;
                    data.pos_y = y;
                }
                Event::LogicalSize { width, height } => {
                    data.size_x = width;
                    data.size_y = height;
                }
                Event::Name { name } => {
                    data.name = name;
                }
                Event::Description { description } => {
                    data.description = description;
                }
                Event::Done => {
                    state.output_ready(i);
                }
                _ => ()
            }
        });
        self.outputs.push(OutputData {
            output : output.clone(),
            pos_x : 0,
            pos_y : 0,
            size_x : 0,
            size_y : 0,
            name : String::new(),
            description: String::new(),
        });
    }

    fn output_ready(&mut self, i : usize) {
        let output = self.outputs[i].output.clone();
        let data = &self.outputs[i];
        info!("Output name='{}' description='{}' at {},{} {}x{}",
            data.name, data.description, data.pos_x, data.pos_y, data.size_x, data.size_y);
        for cfg in self.config["bars"].members() {
            if let Some(name) = cfg["name"].as_str() {
                if name != &data.name {
                    continue;
                }
            }

            let bar = self.new_bar(&output, cfg);
            self.bars.push(bar);
        }
    }

    fn new_bar(&self, output : &WlOutput, cfg : &JsonValue) -> Bar {
        let i = self.bars.len();
        let ls : Attached<ZwlrLayerShellV1> = self.env.require_global();
        let surf : Attached<_> = self.env.create_surface();
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
            let state : &mut State = data.get().unwrap();
            match event {
                LayerSurfaceEvent::Configure { serial, width, height } => {
                    state.config(i, width as i32, height as i32);
                    ls_surf.ack_configure(serial);
                    if !state.bars[i].dirty {
                        state.bars[i].dirty = true;
                    }
                    state.draw();
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
            ls_surf : ls_surf.into(),
            left,
            center,
            right,
            width : 0,
            height : 0,
            sink : EventSink::default(),
            dirty : false,
        }
    }

    pub fn add_seat(&mut self, seat : &Attached<WlSeat>, si : &SeatData) {
        if si.has_pointer {
            let mouse = seat.get_pointer();
            let mut bar_idx = None;
            let mut x = 0.0;
            let mut y = 0.0;
            mouse.quick_assign(move |mouse, event, mut data| {
                let state : &mut State = data.get().unwrap();
                match &event {
                    MouseEvent::Enter { serial, surface, surface_x, surface_y, .. } => {
                        let spot = state.cursor[0].hotspot();
                        mouse.set_cursor(*serial, Some(&state.cursor_surf), spot.0 as _, spot.1 as _);

                        for (i, bar) in state.bars.iter().enumerate() {
                            if *surface == *bar.surf {
                                bar_idx = Some(i);
                                break;
                            }
                        }
                        assert!(bar_idx.is_some());
                        x = *surface_x;
                        y = *surface_y;
                    }
                    MouseEvent::Motion { surface_x, surface_y, .. } => {
                        x = *surface_x;
                        y = *surface_y;
                    }
                    MouseEvent::Leave { surface, .. } => {
                        if *surface == *state.bars[bar_idx.unwrap()].surf {
                            bar_idx = None;
                        }
                    }
                    &MouseEvent::Button {
                        button, state : ButtonState::Pressed, ..
                    } => {
                        let button_id = match button {
                            0x110 => 0, // BTN_LEFT
                            0x111 => 1, // BTN_RIGHT
                            0x112 => 2, // BTN_MIDDLE
                            0x113 => 3, // BTN_SIDE or "back"
                            0x114 => 4, // BTN_EXTRA or "forward"
                            // note scrolling uses the next 4
                            _ => {
                                debug!("You can add events for this button ({})", button);
                                return;
                            }
                        };
                        state.bars[bar_idx.unwrap()].sink.button(x,y,button_id, &mut state.runtime);
                    }
                    &MouseEvent::Axis { axis, value, .. } => {
                        dbg!(value);
                        // TODO minimum scroll distance and/or rate?
                        let button_id = match axis {
                            Axis::VerticalScroll if value < 0.0 => 5, // up
                            Axis::VerticalScroll if value > 0.0 => 6, // down
                            Axis::HorizontalScroll if value < 0.0 => 7, // left
                            Axis::HorizontalScroll if value > 0.0 => 8, // right
                            _ => return,
                        };
                        state.bars[bar_idx.unwrap()].sink.button(x,y,button_id, &mut state.runtime);
                    }
                    _ => ()
                }
            });
        }
        if si.has_touch {
            let finger = seat.get_touch();
            finger.quick_assign(move |finger, event, mut data| {
                let state : &mut State = data.get().unwrap();
                drop(finger);
                match event {
                    TouchEvent::Down { surface, x, y, .. } => {
                        // TODO support gestures - wait for Up, detect Cancel
                        for bar in &mut state.bars {
                            if surface == *bar.surf {
                                bar.sink.button(x,y,0, &mut state.runtime);
                                break;
                            }
                        }
                    }
                    _ => ()
                }
            });
        }
    }

    fn set_data(&mut self) {
        for src in &mut self.runtime.sources {
            src.update(&mut self.runtime.wake_at, &mut self.runtime.vars);
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
