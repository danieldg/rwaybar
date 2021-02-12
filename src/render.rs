use std::io;
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::shm::MemPool;
use wayland_client::Attached;
use wayland_client::protocol::wl_pointer::WlPointer;
use wayland_client::protocol::wl_surface::WlSurface;

use crate::state::State;
use crate::wayland::{Globals,WaylandClient};

pub struct Renderer {
    shm : MemPool,
    pub cursor : Cursor,
    draw_waiting_on_shm : bool,
}

impl Renderer {
    pub fn new(env : &Environment<Globals>) -> io::Result<Self> {
        let shm = env.create_simple_pool(|mut data| {
            let state : &mut State = data.get().unwrap();
            if state.wayland.renderer.draw_waiting_on_shm {
                state.wayland.renderer.draw_waiting_on_shm = false;
                state.draw_now();
            }
        })?;

        let mut cursor_scale = 1;

        for output in env.get_all_outputs() {
            smithay_client_toolkit::output::with_output_info(&output, |oi| {
                if oi.scale_factor > cursor_scale {
                    cursor_scale = oi.scale_factor;
                }
            });
        }
        let cursor = Cursor::new(&env, cursor_scale);

        Ok(Renderer {
            cursor,
            shm,
            draw_waiting_on_shm : false,
        })
    }

    pub fn request_draw(&mut self) -> bool {
        if self.draw_waiting_on_shm || self.shm.is_used() {
            self.draw_waiting_on_shm = true;
            false
        } else {
            true
        }
    }
}

pub struct Cursor {
    cursor_surf : Attached<WlSurface>,
    spot : (i32, i32),
}

impl Cursor {
    fn new(env : &Environment<Globals>, scale : i32) -> Self {
        let shm = env.require_global();
        let base_theme = std::env::var("XCURSOR_THEME").unwrap_or_else(|_| "default".into());
        let base_size = std::env::var("XCURSOR_SIZE").ok().and_then(|s| s.parse().ok()).unwrap_or(24u32);

        let mut cursor_theme = wayland_cursor::CursorTheme::load_from_name(&base_theme, base_size * scale as u32, &shm);
        let cursor = cursor_theme.get_cursor("default").expect("Could not load cursor, check XCURSOR_THEME").clone();

        let cursor_surf = env.create_surface();
        let cursor_img = &cursor[0];
        let dim = cursor_img.dimensions();
        let spot = cursor[0].hotspot();
        cursor_surf.set_buffer_scale(scale);
        cursor_surf.attach(Some(&cursor_img), 0, 0);
        cursor_surf.damage_buffer(0, 0, dim.0 as _, dim.1 as _);
        cursor_surf.commit();
        Cursor {
            spot : (spot.0 as i32 / scale, spot.1 as i32 / scale),
            cursor_surf,
        }
    }

    pub fn set(&self, mouse : &WlPointer, serial : u32) {
        let spot = self.spot;
        mouse.set_cursor(serial, Some(&self.cursor_surf), spot.0, spot.1);
    }
}

pub struct RenderTarget<'a> {
    pub wayland : &'a mut WaylandClient,
    pos : usize,
}

impl<'a> RenderTarget<'a> {
    pub fn new(wayland : &'a mut WaylandClient, len : usize) -> Self {
        wayland.renderer.shm.resize(len).expect("OOM");
        RenderTarget { wayland, pos : 0 }
    }

    pub fn render(&mut self, size : (i32, i32), target : &WlSurface, surf : &cairo::Surface) {
        let stride = cairo::Format::ARgb32.stride_for_width(size.0 as u32).unwrap();
        let len = (size.1 as usize) * (stride as usize);
        let buf : &mut [u8] = &mut self.wayland.renderer.shm.mmap().as_mut()[self.pos..][..len];

        unsafe {
            // cairo::ImageSurface::create_for_data requires a 'static type, so give it that.
            // This could be done safely by creating a type that takes ownership of the MemPool and
            // returns it on Drop, which would require starting with an Rc<State> handle.
            let buf : &'static mut [u8] = &mut *(buf as *mut [u8]);

            let is = cairo::ImageSurface::create_for_data(buf, cairo::Format::ARgb32, size.0, size.1, stride).unwrap();
            let ctx = cairo::Context::new(&is);
            surf.flush();
            ctx.set_source_surface(surf, 0.0, 0.0);
            ctx.set_operator(cairo::Operator::Source);
            ctx.paint();
            is.finish();
            drop(is);
        }

        let buf = self.wayland.renderer.shm.buffer(self.pos as i32, size.0, size.1, stride, smithay_client_toolkit::shm::Format::Argb8888);
        target.attach(Some(&buf), 0, 0);
        target.damage_buffer(0, 0, size.0, size.1);
        self.pos += len;
    }
}
