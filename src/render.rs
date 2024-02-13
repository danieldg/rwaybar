use crate::{
    font::{FontMapped, RenderKey, TextImage},
    state::Runtime,
    wayland::{SurfaceData, WaylandClient},
};
use log::error;
use smithay_client_toolkit::shm::slot::SlotPool;
use std::{borrow::Cow, convert::TryInto, sync::Arc, time};
use wayland_client::protocol::{wl_pointer::WlPointer, wl_shm::Format, wl_surface::WlSurface};

/// Global render state independent of any window's lifetime
#[derive(Debug)]
pub struct Renderer {
    shm: Option<SlotPool>,
    cursor_surf: Option<WlSurface>,
    cursor_spot: (i32, i32),
    has_be_rgba: Option<bool>,
    pub cache: RenderCache,
}

impl Renderer {
    pub fn new() -> Self {
        Renderer {
            shm: None,
            cursor_surf: None,
            cursor_spot: (0, 0),
            has_be_rgba: None,
            cache: RenderCache::new(),
        }
    }

    pub fn render_dummy<R>(rt: &mut Runtime, render: impl FnOnce(&mut Render) -> R) -> R {
        let font = &rt.fonts[0];

        let mut queue = Queue {
            rect: vec![],
            image: vec![],
            cache: None,
        };
        let mut ctx = Render {
            queue: &mut queue,
            damage: None,
            render_extents: (
                tiny_skia::Point::zero(),
                tiny_skia::Point { x: 1.0, y: 1.0 },
            ),
            render_pos: tiny_skia::Point::zero(),
            render_flex: false,
            bounds_only: true,
            scale: 1.0,

            font,
            font_size: 16.0,
            font_color: tiny_skia::Color::BLACK,
            align: Align::bar_default(),
            err_name: "dummy",
            text_stroke: None,
            text_stroke_size: None,
            runtime: rt,
        };
        render(&mut ctx)
    }

    pub fn render<R>(
        &mut self,
        rt: &mut Runtime,
        surface: &WlSurface,
        render: impl FnOnce(&mut Render) -> R,
    ) -> Option<R> {
        let surface_data = SurfaceData::from_wl(surface);
        let (canvas, finalize, cache) = self.render_be_rgba(&mut rt.wayland, surface);
        let mut canvas = match tiny_skia::PixmapMut::from_bytes(
            canvas,
            surface_data.pixel_width() as u32,
            surface_data.pixel_height() as u32,
        ) {
            Some(canvas) => canvas,
            None => return None,
        };
        canvas.fill(tiny_skia::Color::TRANSPARENT);
        let font = &rt.fonts[0];
        let mut damage = vec![[
            0,
            0,
            surface_data.pixel_width(),
            surface_data.pixel_height(),
        ]];

        let mut queue = Queue {
            rect: vec![],
            image: vec![],
            cache: Some(cache),
        };

        let scale = surface_data.scale_factor() as f32;
        let render_xform = surface_data.scale_transform();

        let mut ctx = Render {
            queue: &mut queue,
            damage: Some(&mut damage),
            render_extents: (
                tiny_skia::Point::zero(),
                tiny_skia::Point {
                    x: surface_data.width() as f32,
                    y: surface_data.height() as f32,
                },
            ),
            render_pos: tiny_skia::Point::zero(),
            render_flex: false,
            bounds_only: false,
            scale,

            font,
            font_size: 16.0,
            font_color: tiny_skia::Color::BLACK,
            align: Align::bar_default(),
            err_name: "bar",
            text_stroke: None,
            text_stroke_size: None,
            runtime: rt,
        };
        let rv = render(&mut ctx);

        for rect in queue.rect {
            if rect.bounds.width() <= 0.0 {
                continue;
            }
            let paint = tiny_skia::Paint {
                shader: tiny_skia::Shader::SolidColor(rect.color),
                anti_alias: true,
                ..Default::default()
            };
            canvas.fill_rect(rect.bounds, &paint, render_xform, None);
        }

        for img in queue.image {
            let x = img.top_left.x;
            let y = img.top_left.y;
            if img.crop[0].is_nan() {
                canvas.draw_pixmap(
                    x as i32,
                    y as i32,
                    img.pixels.as_ref().as_ref(),
                    &Default::default(),
                    Default::default(),
                    None,
                );
            } else {
                let mut crop = img.crop;
                for c in &mut crop {
                    *c *= scale;
                }
                let [l, t, r, b] = crop;

                canvas.fill_rect(
                    tiny_skia::Rect::from_ltrb(l, t, r, b).unwrap(),
                    &tiny_skia::Paint {
                        shader: tiny_skia::Pattern::new(
                            img.pixels.as_ref().as_ref(),
                            tiny_skia::SpreadMode::Pad,
                            tiny_skia::FilterQuality::Nearest,
                            1.0,
                            tiny_skia::Transform::from_translate(x, y),
                        ),
                        ..Default::default()
                    },
                    tiny_skia::Transform::identity(),
                    None,
                );
            }
        }
        finalize(canvas.data_mut());
        surface.frame(&rt.wayland.queue, surface.clone());
        for [x, y, w, h] in damage {
            surface.damage_buffer(x, y, w, h);
        }
        surface.commit();
        Some(rv)
    }

    pub fn render_be_rgba(
        &mut self,
        wl: &WaylandClient,
        target: &WlSurface,
    ) -> (&mut [u8], impl FnOnce(&mut [u8]), &mut RenderCache) {
        let data = SurfaceData::from_wl(target);
        let width = data.pixel_width();
        let height = data.pixel_height();
        let stride = width * 4;

        let len = height as usize * stride as usize;

        let shm = match &mut self.shm {
            Some(shm) => shm,
            v @ None => {
                let shm = SlotPool::new(len * 2, &wl.shm).unwrap();
                v.get_or_insert(shm)
            }
        };

        let has_be_rgba = *self
            .has_be_rgba
            .get_or_insert_with(|| wl.shm.formats().contains(&Format::Abgr8888));
        let fmt = if has_be_rgba {
            Format::Abgr8888
        } else {
            // wayland always supports this format, so we convert to it as a fallback
            Format::Argb8888
        };
        let (buffer, canvas) = shm.create_buffer(width, height, stride, fmt).expect("OOM");

        buffer
            .attach_to(&target)
            .expect("New buffers are not already attached");

        (
            canvas,
            move |buf| {
                if !has_be_rgba {
                    for pixel in buf.chunks_mut(4) {
                        let [r, g, b, a]: [u8; 4] = (&*pixel).try_into().expect("partial pixel");
                        pixel.copy_from_slice(&[b, g, r, a]);
                    }
                }
            },
            &mut self.cache,
        )
    }

    fn setup_cursor(&mut self, wl: &WaylandClient) {
        let mut scale = 1;

        for output in wl.output.outputs() {
            if let Some(oi) = wl.output.info(&output) {
                if oi.scale_factor > scale {
                    scale = oi.scale_factor;
                }
            }
        }

        let base_theme = std::env::var("XCURSOR_THEME").unwrap_or_else(|_| "default".into());
        let base_size = std::env::var("XCURSOR_SIZE")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(24u32);

        let mut cursor_theme = wayland_cursor::CursorTheme::load_from_name(
            &wl.conn,
            wl.shm.wl_shm().clone(),
            &base_theme,
            base_size * scale as u32,
        )
        .unwrap();
        let cursor = cursor_theme
            .get_cursor("default")
            .expect("Could not load cursor, check XCURSOR_THEME");

        let cursor_surf = wl.compositor.create_surface(&wl.queue);
        let cursor_img = &cursor[0];
        let (w, h) = cursor_img.dimensions();
        let (x, y) = cursor[0].hotspot();
        self.cursor_spot = (x as i32 / scale, y as i32 / scale);
        cursor_surf.set_buffer_scale(scale);
        cursor_surf.attach(Some(&cursor_img), 0, 0);
        cursor_surf.damage_buffer(0, 0, w as _, h as _);
        cursor_surf.commit();
        self.cursor_surf = Some(cursor_surf);
    }

    pub fn set_cursor(&mut self, wl: &WaylandClient, mouse: &WlPointer, serial: u32) {
        if self.cursor_surf.is_none() {
            self.setup_cursor(wl);
        }
        if self.cursor_surf.is_some() {
            let (x, y) = self.cursor_spot;
            mouse.set_cursor(serial, self.cursor_surf.as_ref(), x, y);
        }
    }
}

/// Render state bound to a bar
#[derive(Debug)]
pub struct RenderSurface {
    // TODO damage tracking goes here
}

impl RenderSurface {
    pub fn new() -> Self {
        Self {}
    }
}

/// TODO make private
#[derive(Debug)]
pub struct RenderCache {
    pub text: std::collections::HashMap<RenderKey, TextImage>,
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
            let had = self.text.len();
            self.text.retain(|_k, v| v.last_used > min);
            log::debug!("Cache pruned from {} to {} entries", had, self.text.len());
        }
        self.last_expire = as_of;
    }
}

#[derive(Debug)]
struct RenderRect {
    bounds: tiny_skia::Rect,
    color: tiny_skia::Color,
}

#[derive(Debug)]
struct RenderImage {
    top_left: tiny_skia::Point,
    pixels: Arc<tiny_skia::Pixmap>,
    crop: [f32; 4],
}

#[derive(Debug)]
pub struct Queue<'a> {
    rect: Vec<RenderRect>,
    image: Vec<RenderImage>,

    pub cache: Option<&'a mut RenderCache>,
}

#[derive(Debug)]
pub struct QueueMark {
    rect: usize,
    image: usize,
}

impl Queue<'_> {
    pub fn push_rect(&mut self, bounds: tiny_skia::Rect, color: tiny_skia::Color) {
        self.rect.push(RenderRect { bounds, color });
    }

    pub fn push_image(&mut self, top_left: tiny_skia::Point, pixels: Arc<tiny_skia::Pixmap>) {
        if top_left.x.fract() != 0.0 || top_left.y.fract() != 0.0 {
            log::debug!("Found fractional image coordinates ({top_left:?})");
        }
        self.image.push(RenderImage {
            top_left,
            pixels,
            crop: [f32::NAN; 4],
        });
    }

    pub fn start_group(&self) -> QueueMark {
        QueueMark {
            rect: self.rect.len(),
            image: self.image.len(),
        }
    }

    pub fn translate_group(&mut self, mark: &QueueMark, xlate: tiny_skia::Point) {
        if xlate.x.fract() != 0.0 || xlate.y.fract() != 0.0 {
            log::debug!("Found fractional-pixel translation ({xlate:?})");
        }
        let bb = tiny_skia::NonZeroRect::from_xywh(xlate.x, xlate.y, 1., 1.).unwrap();
        for rect in &mut self.rect[mark.rect..] {
            rect.bounds = rect.bounds.bbox_transform(bb);
        }
        for img in &mut self.image[mark.image..] {
            img.top_left += xlate;
            img.crop[0] += xlate.x;
            img.crop[1] += xlate.y;
            img.crop[2] += xlate.x;
            img.crop[3] += xlate.y;
        }
    }

    pub fn swap_rect_ordering(&mut self, a: &QueueMark, b: &QueueMark) {
        let len = self.rect.len() - b.rect;
        if a.rect == b.rect || len == 0 {
            return;
        }
        self.rect[a.rect..].rotate_right(len);
    }

    pub fn crop_range(&mut self, a: &QueueMark, b: &QueueMark, crop: [f32; 4]) {
        for rect in &mut self.rect[a.rect..b.rect] {
            let ltrb = [
                rect.bounds.left(),
                rect.bounds.top(),
                rect.bounds.right(),
                rect.bounds.bottom(),
            ];
            rect.bounds = tiny_skia::Rect::from_ltrb(
                ltrb[0].max(crop[0]),
                ltrb[1].max(crop[1]),
                ltrb[2].min(crop[2]),
                ltrb[3].min(crop[3]),
            )
            .or_else(|| tiny_skia::Rect::from_ltrb(0., 0., 0., 0.))
            .unwrap();
        }
        for img in &mut self.image[a.image..b.image] {
            if img.crop[0].is_nan() {
                img.crop = crop;
            } else {
                img.crop[0] = img.crop[0].max(crop[0]);
                img.crop[1] = img.crop[1].max(crop[1]);
                img.crop[2] = img.crop[2].min(crop[2]);
                img.crop[3] = img.crop[3].min(crop[3]);
            }
        }
    }
}

/// State available to an [Item][crate::item::Item] render function
pub struct Render<'a, 'c> {
    pub queue: &'a mut Queue<'c>,

    pub damage: Option<&'a mut Vec<[i32; 4]>>,

    pub scale: f32,

    /// Bounding box for the current item or group.  This is used to compute percentage-based
    /// widths, so it is constant for all items in a group.
    pub render_extents: (tiny_skia::Point, tiny_skia::Point),

    /// Position of the pen.  During any render call, this should move from the top-left of an item
    /// to the bottom-right of an item.
    pub render_pos: tiny_skia::Point,

    pub render_flex: bool,
    /// Skip expensive rendering steps, we just want bounds
    pub bounds_only: bool,

    pub font: &'a FontMapped,
    pub font_size: f32,
    pub font_color: tiny_skia::Color,
    pub text_stroke: Option<tiny_skia::Color>,
    pub text_stroke_size: Option<f32>,

    pub align: Align,
    pub err_name: &'a str,
    pub runtime: &'a Runtime,
}

#[derive(Debug)]
pub struct Group {
    pub origin: tiny_skia::Point,
    pub bounds: tiny_skia::Point,
}

impl Group {
    pub fn next_h(&mut self, ctx: &mut Render) {
        self.bounds.x = ctx.render_pos.x;
        if ctx.render_pos.y > self.bounds.y {
            self.bounds.y = ctx.render_pos.y;
        }
        ctx.render_pos.y = self.origin.y;
    }
    pub fn next_v(&mut self, ctx: &mut Render) {
        if ctx.render_pos.x > self.bounds.x {
            self.bounds.x = ctx.render_pos.x;
        }
        ctx.render_pos.x = self.origin.x;
        self.bounds.y = ctx.render_pos.y;
    }
}

impl Render<'_, '_> {
    pub fn group(&self) -> Group {
        Group {
            origin: self.render_pos,
            bounds: self.render_pos,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Width {
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f32),
    /// Some number of pixels
    Pixels(f32),
}

impl Width {
    pub fn from_str(value: Cow<str>) -> Option<Self> {
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

pub const MIDDLE: f32 = 0.5;

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct Align {
    pub horiz: Option<f32>,
    pub vert: Option<f32>,
}

impl Align {
    pub fn bar_default() -> Self {
        Align {
            horiz: None,
            vert: Some(MIDDLE),
        }
    }

    pub fn parse_hv(value: Cow<str>) -> Option<f32> {
        if value.ends_with('%') {
            let value = &value[..value.len() - 1];
            let pct = value.parse::<f32>().ok()?;
            return Some(pct / 100.0);
        }
        value.parse().ok()
    }

    pub fn from_name(&mut self, value: Option<Cow<str>>) {
        match value.as_deref() {
            Some("north") => {
                *self = Align {
                    horiz: Some(MIDDLE),
                    vert: Some(0.0),
                }
            }
            Some("south") => {
                *self = Align {
                    horiz: Some(MIDDLE),
                    vert: Some(1.0),
                }
            }
            Some("east") => {
                *self = Align {
                    horiz: Some(0.0),
                    vert: Some(MIDDLE),
                }
            }
            Some("west") => {
                *self = Align {
                    horiz: Some(1.0),
                    vert: Some(MIDDLE),
                }
            }
            Some("center") => {
                *self = Align {
                    horiz: Some(MIDDLE),
                    vert: Some(MIDDLE),
                }
            }
            Some("") | None => {}
            Some(x) => {
                error!("Unknown alignment {}", x);
            }
        }
    }

    pub fn merge(&self, child: &Self) -> Self {
        Align {
            horiz: child.horiz.or(self.horiz),
            vert: child.vert.or(self.vert),
        }
    }
}
