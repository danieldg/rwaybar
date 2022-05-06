use crate::font::{FontMapped,RenderKey,TextImage};
use crate::state::Runtime;
use crate::wayland::{Globals,Surface};
use log::error;
use tiny_skia::PixmapMut;
use std::borrow::Cow;
use std::convert::TryInto;
use std::io;
use std::time;
use smithay_client_toolkit::environment::Environment;
use smithay_client_toolkit::shm::AutoMemPool;
use wayland_client::Attached;
use wayland_client::protocol::wl_pointer::WlPointer;
use wayland_client::protocol::wl_surface::WlSurface;

pub struct Renderer {
    shm : AutoMemPool,
    pub cursor : Cursor,
    has_be_rgba: bool,
}

impl Renderer {
    pub fn new(env : &Environment<Globals>) -> io::Result<Self> {
        use smithay_client_toolkit::shm::ShmHandling;
        let shm = env.create_auto_pool()?;

        let mut cursor_scale = 1;

        for output in env.get_all_outputs() {
            smithay_client_toolkit::output::with_output_info(&output, |oi| {
                if oi.scale_factor > cursor_scale {
                    cursor_scale = oi.scale_factor;
                }
            });
        }

        let has_be_rgba = env.with_inner(|i| i.sctk_shm.shm_formats())
            .contains(&smithay_client_toolkit::shm::Format::Abgr8888);
        let cursor = Cursor::new(&env, cursor_scale);

        Ok(Renderer {
            cursor,
            shm,
            has_be_rgba,
        })
    }

    pub fn render_be_rgba(&mut self, surface: &Surface) -> (&mut [u8], impl FnOnce(&mut [u8])) {
        let width = surface.pixel_width();
        let height = surface.pixel_height();
        let target = &surface.wl;

        let stride = width * 4;
        let has_be_rgba = self.has_be_rgba;
        let fmt = if has_be_rgba {
            smithay_client_toolkit::shm::Format::Abgr8888
        } else {
            // wayland always supports this format, so we convert to it as a fallback
            smithay_client_toolkit::shm::Format::Argb8888
        };
        let (canvas, wl_buf) = self.shm.buffer(width, height, stride, fmt).expect("OOM");

        target.attach(Some(&wl_buf), 0, 0);
        target.damage_buffer(0, 0, width, height);

        (canvas, move |buf| {
            if !has_be_rgba {
                for pixel in buf.chunks_mut(4) {
                    let [r,g,b,a] : [u8;4] = (&*pixel).try_into().expect("partial pixel");
                    pixel.copy_from_slice(&[b,g,r,a]);
                }
            }
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

#[derive(Debug)]
pub struct RenderCache {
    pub text: std::cell::RefCell<std::collections::HashMap<RenderKey, TextImage>>,
    last_expire: time::Instant,
}

impl RenderCache {
    pub fn new() -> Self {
        Self {
            text: Default::default(),
            last_expire: time::Instant::now(),
        }
    }
    
    pub fn prune(&mut self, as_of: time::Instant) {
        if self.last_expire > as_of - time::Duration::from_secs(300) {
            return;
        }
        if let Some(min) = as_of.checked_sub(time::Duration::from_secs(130)) {
            let had = self.text.get_mut().len();
            self.text.get_mut().retain(|_k,v| {
                v.last_used > min
            });
            log::debug!("Cache pruned from {} to {} entries", had, self.text.get_mut().len());
        }
        self.last_expire = as_of;
    }
}

/// State available to an [Item][crate::item::Item] render function
pub struct Render<'a, 'c> {
    pub canvas : &'a mut PixmapMut<'c>,

    pub cache: &'a RenderCache,

    pub render_xform : tiny_skia::Transform,
    pub render_extents : (tiny_skia::Point, tiny_skia::Point),
    pub render_pos : tiny_skia::Point,
    pub render_flex : bool,

    pub font : &'a FontMapped,
    pub font_size : f32,
    pub font_color : tiny_skia::Color,
    pub text_stroke : Option<tiny_skia::Color>,
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
