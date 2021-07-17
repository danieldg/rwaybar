use crate::font::FontMapped;
use crate::state::Runtime;
use crate::wayland::{Globals,WaylandClient};
use log::error;
use raqote::DrawTarget;
use std::borrow::Cow;
use std::io;
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::shm::AutoMemPool;
use wayland_client::Attached;
use wayland_client::protocol::wl_pointer::WlPointer;
use wayland_client::protocol::wl_surface::WlSurface;

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

    pub fn render(&mut self, size : (i32, i32), target : &WlSurface) -> &mut [u32] {
        let stride = size.0 * 4;
        let (canvas, wl_buf) = self.wayland.renderer.shm
            .buffer(size.0, size.1, stride, smithay_client_toolkit::shm::Format::Argb8888)
            .expect("OOM");

        target.attach(Some(&wl_buf), 0, 0);
        target.damage_buffer(0, 0, size.0, size.1);

        unsafe {
            let len = size.0 as usize * size.1 as usize;
            assert_eq!(canvas.len(), len * 4);
            std::slice::from_raw_parts_mut(canvas.as_mut_ptr().cast(), len)
        }
    }
}

/// State available to an [Item][crate::item::Item] render function
pub struct Render<'a, 'c> {
    pub canvas : &'a mut DrawTarget<&'c mut [u32]>,

    pub render_extents : (f32, f32, f32, f32),
    pub render_pos : f32,
    pub render_ypos : Option<f32>,
    pub render_flex : bool,

    pub font : &'a FontMapped,
    pub font_size : f32,
    pub font_color : (u16, u16, u16, u16),
    pub text_stroke : Option<(u16, u16, u16, u16)>,
    pub text_stroke_size : Option<f32>,

    pub align : Align,
    pub err_name : &'a str,
    pub runtime : &'a Runtime,
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Width {
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f32),
    /// Some number of pixels
    Pixels(f32),
}

impl Width {
    pub fn from_str(value : Cow<str>) -> Option<Self> {
        if value.ends_with('%') {
            let value = &value[..value.len() - 1];
            let pct = value.parse::<f32>().ok()?;
            return Some(Width::Fraction(pct / 100.0));
        }
        if value.contains('.') {
            value.parse().ok().map(Width::Fraction)
        } else {
            value.parse().ok().map(Width::Pixels)
        }
    }
}

pub const MIDDLE : f32 = 0.5;

#[derive(Default,Debug,Copy,Clone,PartialEq)]
pub struct Align {
    pub horiz : Option<f32>,
    pub vert : Option<f32>,
}

impl Align {
    pub fn bar_default() -> Self {
        Align {
            horiz : None,
            vert : Some(MIDDLE),
        }
    }

    pub fn parse_hv(value : Cow<str>) -> Option<f32> {
        if value.ends_with('%') {
            let value = &value[..value.len() - 1];
            let pct = value.parse::<f32>().ok()?;
            return Some(pct / 100.0);
        }
        value.parse().ok()
    }

    pub fn from_name(&mut self, value : Option<Cow<str>>) {
        match value.as_deref() {
            Some("north")  => *self = Align { horiz : Some(MIDDLE), vert : Some(0.0) },
            Some("south")  => *self = Align { horiz : Some(MIDDLE), vert : Some(1.0) },
            Some("east")   => *self = Align { horiz : Some(0.0), vert : Some(MIDDLE) },
            Some("west")   => *self = Align { horiz : Some(1.0), vert : Some(MIDDLE) },
            Some("center") => *self = Align { horiz : Some(MIDDLE), vert : Some(MIDDLE) },
            Some("") | None => {}
            Some(x) => {
                error!("Unknown alignment {}", x);
            }
        }
    }

    pub fn merge(&self, child : &Self) -> Self {
        Align {
            horiz : child.horiz.or(self.horiz),
            vert : child.vert.or(self.vert),
        }
    }
}
