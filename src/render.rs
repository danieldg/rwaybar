use std::io;
use raqote::DrawTarget;
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::shm::AutoMemPool;
use wayland_client::Attached;
use wayland_client::protocol::wl_pointer::WlPointer;
use wayland_client::protocol::wl_surface::WlSurface;

use crate::wayland::{Globals,WaylandClient};

pub struct Renderer {
    shm : AutoMemPool,
    pub cursor : Cursor,
}

impl Renderer {
    pub fn new(env : &Environment<Globals>) -> io::Result<Self> {
        let shm = env.create_auto_pool()?;

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
        })
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
}

impl<'a> RenderTarget<'a> {
    pub fn new(wayland : &'a mut WaylandClient) -> Self {
        RenderTarget { wayland }
    }

    pub fn render(&mut self, size : (i32, i32), target : &WlSurface, surf : &DrawTarget) {
        let stride = size.0 * 4;
        let (buf, wl_buf) = self.wayland.renderer.shm
            .buffer(size.0, size.1, stride, smithay_client_toolkit::shm::Format::Argb8888)
            .expect("OOM");

        if let Some(src) = surf.get_data_u8().get(..buf.len()) {
            buf.copy_from_slice(src);
        } else {
            log::error!("DrawTarget too small for its rendering");
        }

        target.attach(Some(&wl_buf), 0, 0);
        target.damage_buffer(0, 0, size.0, size.1);
    }
}
