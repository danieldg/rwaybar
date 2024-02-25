use crate::{
    font::{FontMapped, RenderKey, TextImage},
    icon::OwnedImage,
    state::Runtime,
    wayland::{SurfaceData, WaylandClient},
};
use log::error;
use smithay_client_toolkit::shm::slot::{Slot, SlotPool};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    sync::Arc,
    time,
};
use wayland_client::protocol::{wl_pointer::WlPointer, wl_shm::Format, wl_surface::WlSurface};

/// Global render state independent of any window's lifetime
#[derive(Debug)]
pub struct Renderer {
    shm: Option<SlotPool>,
    cursor_surf: Option<WlSurface>,
    cursor_spot: (i32, i32),
    has_be_rgba: Option<bool>,
    shm_warned: Option<()>,
    pub cache: RenderCache,
}

impl Renderer {
    pub fn new() -> Self {
        Renderer {
            shm: None,
            cursor_surf: None,
            cursor_spot: (0, 0),
            has_be_rgba: None,
            shm_warned: None,
            cache: RenderCache::new(),
        }
    }

    pub fn render_dummy<R>(&mut self, rt: &Runtime, render: impl FnOnce(&mut Render) -> R) -> R {
        let font = &rt.fonts[0];

        let mut queue = Queue {
            rect: vec![],
            image: vec![],
        };
        let mut ctx = Render {
            queue: &mut queue,
            cache: &mut self.cache,
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

    pub fn render(
        &mut self,
        rt: &Runtime,
        surface: &WlSurface,
        data: &mut RenderSurface,
        render: impl FnOnce(&mut Render) -> bool,
    ) {
        let surface_data = SurfaceData::from_wl(surface);

        let pixel_width = surface_data.pixel_width() as u32;
        let pixel_height = surface_data.pixel_height() as u32;
        let scale = surface_data.scale_factor();
        let render_xform = surface_data.scale_transform();
        let no_xform = tiny_skia::Transform::identity();

        let mut queue = Queue {
            rect: vec![],
            image: vec![],
        };
        let font = &rt.fonts[0];

        let mut ctx = Render {
            queue: &mut queue,
            cache: &mut self.cache,
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

        if !render(&mut ctx) {
            return;
        }

        let mut damage = data.diff_contents(&queue, scale);

        let everything = tiny_skia::IntRect::from_xywh(0, 0, pixel_width, pixel_height).unwrap();

        damage.retain_mut(|dmg| match dmg.intersect(&everything) {
            Some(i) => {
                *dmg = i;
                true
            }
            None => false,
        });

        if damage.is_empty() {
            // This happens when a wakeup didn't actually cause the displayed data to change. For
            // example, a load or temperature value can easily remain constant.
            surface_data.undo_damage();
            return;
        }

        let Queue { rect, image } = queue;

        // Drawing is done directly to the SHM region.  If must_clear is false, that region still
        // contains the prior frame, so we can avoid redrawing undamaged areas.
        let (canvas, must_clear, finalize) = self.render_be_rgba(data, &rt.wayland, surface);
        let mut canvas = tiny_skia::PixmapMut::from_bytes(canvas, pixel_width, pixel_height)
            .expect("Bad canvas size?");

        let draw_damage = if must_clear {
            std::array::from_ref(&everything) as &[_]
        } else {
            &damage
        };

        if must_clear {
            canvas.fill(tiny_skia::Color::TRANSPARENT);
        } else {
            let paint = tiny_skia::Paint {
                blend_mode: tiny_skia::BlendMode::Clear,
                ..Default::default()
            };
            for rect in &damage {
                canvas.fill_rect(rect.to_rect(), &paint, no_xform, None);
            }
        }

        for rect in &rect {
            if rect.bounds.width() <= 0.0 {
                continue;
            }
            let paint = tiny_skia::Paint {
                shader: tiny_skia::Shader::SolidColor(rect.color),
                anti_alias: true,
                ..Default::default()
            };
            if must_clear {
                canvas.fill_rect(rect.bounds, &paint, render_xform, None);
            } else {
                rect.for_damage(&damage, scale, |rect| {
                    canvas.fill_rect(rect, &paint, no_xform, None)
                });
            }
        }

        for img in &image {
            let tiny_skia::Point { x, y } = img.top_left;
            let paint = tiny_skia::Paint {
                shader: tiny_skia::Pattern::new(
                    img.pixels.as_ref().as_ref(),
                    tiny_skia::SpreadMode::Pad,
                    tiny_skia::FilterQuality::Nearest,
                    1.0,
                    tiny_skia::Transform::from_translate(x, y),
                ),
                ..Default::default()
            };

            img.for_damage(&draw_damage, |rect| {
                canvas.fill_rect(rect, &paint, no_xform, None);
            });
        }
        finalize(canvas.data_mut());
        surface.frame(&rt.wayland.queue, surface.clone());

        for r in damage {
            surface.damage_buffer(r.x(), r.y(), r.width() as _, r.height() as _);
        }
        surface.commit();
    }

    fn render_be_rgba(
        &mut self,
        rs: &mut RenderSurface,
        wl: &WaylandClient,
        target: &WlSurface,
    ) -> (&mut [u8], bool, impl FnOnce(&mut [u8])) {
        let (must_clear, has_be_rgba, canvas) = rs.prep_slot(self, wl, target);

        if !has_be_rgba && !must_clear {
            for pixel in canvas.chunks_mut(4) {
                let [b, g, r, a]: [u8; 4] = (&*pixel).try_into().expect("partial pixel");
                pixel.copy_from_slice(&[r, g, b, a]);
            }
        }

        (canvas, must_clear, move |buf| {
            if !has_be_rgba {
                for pixel in buf.chunks_mut(4) {
                    let [r, g, b, a]: [u8; 4] = (&*pixel).try_into().expect("partial pixel");
                    pixel.copy_from_slice(&[b, g, r, a]);
                }
            }
        })
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
    rect: HashSet<RenderRect>,
    image: HashSet<RenderImage>,
    size: (i32, i32),

    slot: Option<Slot>,
}

impl RenderSurface {
    pub fn new() -> Self {
        Self {
            rect: HashSet::new(),
            image: HashSet::new(),
            size: (0, 0),
            slot: None,
        }
    }

    fn prep_slot<'r>(
        &mut self,
        renderer: &'r mut Renderer,
        wl: &WaylandClient,
        target: &WlSurface,
    ) -> (bool, bool, &'r mut [u8]) {
        let data = SurfaceData::from_wl(target);
        let width = data.pixel_width();
        let height = data.pixel_height();
        let stride = width * 4;
        let len = height as usize * stride as usize;

        if self.size != (width, height) {
            self.slot = None;
        }
        self.size = (width, height);

        let shm = match &mut renderer.shm {
            Some(shm) => shm,
            v @ None => {
                let shm = SlotPool::new(len * 2, &wl.shm).unwrap();
                v.get_or_insert(shm)
            }
        };

        let has_be_rgba = *renderer
            .has_be_rgba
            .get_or_insert_with(|| wl.shm.formats().contains(&Format::Abgr8888));
        let fmt = if has_be_rgba {
            Format::Abgr8888
        } else {
            // wayland always supports this format, so we convert to it as a fallback
            Format::Argb8888
        };

        if self.slot.as_ref().is_some_and(|s| shm.canvas(s).is_none()) {
            renderer.shm_warned.get_or_insert_with(|| {
                log::warn!("SHM buffers not released before next frame, disabling damage rendering")
            });
            self.slot = None;
        }

        let must_clear = self.slot.is_none();

        let slot = self
            .slot
            .get_or_insert_with(|| shm.new_slot(len).expect("OOM"));

        let buffer = shm
            .create_buffer_in(slot, width, height, stride, fmt)
            .unwrap();
        let canvas = shm.canvas(&buffer).expect("Checked above");

        buffer
            .attach_to(&target)
            .expect("New buffers are not already attached");

        (must_clear, has_be_rgba, canvas)
    }

    /// Take the current and previous frame's contents, and remove identical regions.  Mark any
    /// other region as a damage area.
    ///
    /// Technically this could miss a pair of regions or images that only change their relative
    /// Z-ordering, but that's pretty hard to make happen by accident.
    ///
    /// In order to avoid making a ton of small regions, merge any regions within 7 pixels of each
    /// other.  This also ignores the Y coordinate when determining what to merge, because most
    /// damage occupies the full height of the bar anyway.
    fn diff_contents(&mut self, queue: &Queue, scale: f32) -> Vec<tiny_skia::IntRect> {
        let mut dmg = Vec::new();

        for rect in &queue.rect {
            if !self.rect.remove(&rect) {
                if let Some(bbox) = rect.bbox(scale) {
                    dmg.push(bbox);
                }
            }
        }
        dmg.extend(self.rect.drain().filter_map(|e| e.bbox(scale)));

        for img in &queue.image {
            if !self.image.remove(&img) {
                if let Some(bbox) = img.bbox() {
                    dmg.push(bbox);
                }
            }
        }
        dmg.extend(self.image.drain().filter_map(|e| e.bbox()));

        self.rect.extend(queue.rect.iter().cloned());
        self.image.extend(queue.image.iter().cloned());

        // This could also consider y-coordinates, but that's harder
        dmg.sort_by_key(|r| r.x());
        dmg.dedup_by(|b, a| {
            let dup = a.right() + 7 > b.left();
            if dup {
                *a = tiny_skia::IntRect::from_ltrb(
                    a.left().min(b.left()),
                    a.top().min(b.top()),
                    a.right().max(b.right()),
                    a.bottom().max(b.bottom()),
                )
                .unwrap();
            }
            dup
        });

        dmg
    }
}

/// TODO make private
#[derive(Debug)]
pub struct RenderCache {
    pub text: HashMap<RenderKey, TextImage>,
    pub icon: HashMap<(Box<str>, u32), Option<OwnedImage>>,
    last_expire: time::Instant,
}

impl RenderCache {
    pub fn new() -> Self {
        Self {
            text: HashMap::new(),
            icon: HashMap::new(),
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

#[derive(Debug, PartialEq, Clone)]
struct RenderRect {
    bounds: tiny_skia::Rect,
    color: tiny_skia::Color,
}

impl Hash for RenderRect {
    fn hash<H: Hasher>(&self, h: &mut H) {
        h.write_u32(self.bounds.left().to_bits());
        h.write_u32(self.bounds.top().to_bits());
        h.write_u32(self.bounds.right().to_bits());
        h.write_u32(self.bounds.bottom().to_bits());
        h.write_u32(self.color.red().to_bits());
        h.write_u32(self.color.green().to_bits());
        h.write_u32(self.color.blue().to_bits());
        h.write_u32(self.color.alpha().to_bits());
    }
}

impl Eq for RenderRect {}

impl RenderRect {
    /// Return the pixel bounding box of this rect, or None if it's a 0-pixel rect
    fn bbox(&self, scale: f32) -> Option<tiny_skia::IntRect> {
        tiny_skia::IntRect::from_ltrb(
            (self.bounds.left() * scale).floor() as i32,
            (self.bounds.top() * scale).floor() as i32,
            (self.bounds.right() * scale).ceil() as i32,
            (self.bounds.bottom() * scale).ceil() as i32,
        )
    }

    fn for_damage(
        &self,
        damage: &[tiny_skia::IntRect],
        scale: f32,
        mut fill: impl FnMut(tiny_skia::Rect),
    ) {
        let l = self.bounds.left() * scale;
        let t = self.bounds.top() * scale;
        let r = self.bounds.right() * scale;
        let b = self.bounds.bottom() * scale;

        for dbox in damage {
            if r <= dbox.left() as f32 || dbox.right() as f32 <= l {
                continue;
            }
            let l = l.max(dbox.left() as f32);
            let t = t.max(dbox.top() as f32);
            let r = r.min(dbox.right() as f32);
            let b = b.min(dbox.bottom() as f32);
            if let Some(rect) = tiny_skia::Rect::from_ltrb(l, t, r, b) {
                fill(rect);
            }
        }
    }
}

#[derive(Debug, Clone)]
struct RenderImage {
    /// Pixel coordinates; should be integers to avoid blurring
    top_left: tiny_skia::Point,
    pixels: Arc<tiny_skia::Pixmap>,
    /// LTRB in pixel coordinates
    crop: [f32; 4],
}

impl Hash for RenderImage {
    fn hash<H: Hasher>(&self, h: &mut H) {
        h.write_usize(Arc::as_ptr(&self.pixels) as _);
        h.write_u32(self.top_left.x.to_bits());
        h.write_u32(self.top_left.y.to_bits());
        // it's very unlikely to have two images with the same pixmap, only differing in crop
    }
}

impl Eq for RenderImage {}

impl PartialEq for RenderImage {
    fn eq(&self, r: &Self) -> bool {
        Arc::as_ptr(&self.pixels) == Arc::as_ptr(&r.pixels)
            && self.top_left == r.top_left
            && ((self.crop[0].is_nan() && r.crop[0].is_nan()) || self.crop == r.crop)
    }
}

impl RenderImage {
    /// Return the pixel bounding box of this rect, or None if it has been cropped to nothing
    fn bbox(&self) -> Option<tiny_skia::IntRect> {
        let tiny_skia::Point { x, y } = self.top_left;
        let w = self.pixels.width();
        let h = self.pixels.height();
        let ibox = tiny_skia::IntRect::from_xywh(x.floor() as i32, y.floor() as i32, w, h)?;
        if self.crop[0].is_nan() {
            Some(ibox)
        } else {
            let cbox = tiny_skia::IntRect::from_ltrb(
                self.crop[0].floor() as i32,
                self.crop[1].floor() as i32,
                self.crop[2].ceil() as i32,
                self.crop[3].ceil() as i32,
            )?;
            cbox.intersect(&ibox)
        }
    }

    fn cbox(&self) -> Option<tiny_skia::Rect> {
        let tiny_skia::Point { x, y } = self.top_left;
        let w = self.pixels.width() as f32;
        let h = self.pixels.height() as f32;
        if self.crop[0].is_nan() {
            tiny_skia::Rect::from_xywh(x, y, w, h)
        } else {
            tiny_skia::Rect::from_ltrb(
                self.crop[0].max(x),
                self.crop[1].max(y),
                self.crop[2].min(x + w),
                self.crop[3].min(y + h),
            )
        }
    }

    fn for_damage(&self, damage: &[tiny_skia::IntRect], mut fill: impl FnMut(tiny_skia::Rect)) {
        let Some(cbox) = self.cbox() else {
            return;
        };

        for dbox in damage {
            let dbox = dbox.to_rect();
            // Faster check for non-overlap, which would make the rect None
            if cbox.right() <= dbox.left() || dbox.right() <= cbox.left() {
                continue;
            }
            let l = cbox.left().max(dbox.left());
            let t = cbox.top().max(dbox.top());
            let r = cbox.right().min(dbox.right());
            let b = cbox.bottom().min(dbox.bottom());
            if let Some(rect) = tiny_skia::Rect::from_ltrb(l, t, r, b) {
                fill(rect);
            }
        }
    }
}

#[derive(Debug)]
pub struct Queue {
    rect: Vec<RenderRect>,
    image: Vec<RenderImage>,
}

#[derive(Debug)]
pub struct QueueMark {
    rect: usize,
    image: usize,
}

impl Queue {
    pub fn push_rect(&mut self, bounds: tiny_skia::Rect, color: tiny_skia::Color) {
        self.rect.push(RenderRect { bounds, color });
    }

    pub fn push_image(&mut self, top_left: tiny_skia::Point, pixels: Arc<tiny_skia::Pixmap>) {
        self.push_image_clip(top_left, pixels, [f32::NAN; 4]);
    }

    pub fn push_image_clip(
        &mut self,
        top_left: tiny_skia::Point,
        pixels: Arc<tiny_skia::Pixmap>,
        crop: [f32; 4],
    ) {
        if top_left.x.fract() != 0.0 || top_left.y.fract() != 0.0 {
            log::debug!("Found fractional image coordinates ({top_left:?})");
        }
        self.image.push(RenderImage {
            top_left,
            pixels,
            crop,
        });
    }

    pub fn start_group(&self) -> QueueMark {
        QueueMark {
            rect: self.rect.len(),
            image: self.image.len(),
        }
    }

    pub fn swap_rect_ordering(&mut self, a: &QueueMark, b: &QueueMark) {
        let len = self.rect.len() - b.rect;
        if a.rect == b.rect || len == 0 {
            return;
        }
        self.rect[a.rect..].rotate_right(len);
    }
}

/// State available to an [Item][crate::item::Item] render function
pub struct Render<'a> {
    pub queue: &'a mut Queue,
    pub cache: &'a mut RenderCache,

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

impl<'a> Render<'a> {
    pub fn with_err_name<'b>(&'b mut self, name: &'b str) -> Render<'b> {
        Render {
            queue: &mut *self.queue,
            cache: &mut *self.cache,
            err_name: name,
            ..*self
        }
    }

    pub fn with_font<'b>(&'b mut self, font: Option<&'b FontMapped>) -> Render<'b> {
        Render {
            queue: &mut *self.queue,
            cache: &mut *self.cache,
            font: font.unwrap_or(&self.font),
            ..*self
        }
    }

    pub fn group(&self) -> Group {
        Group {
            origin: self.render_pos,
            bounds: self.render_pos,
        }
    }

    pub fn translate_group(&mut self, mark: &QueueMark, mut xlate: tiny_skia::Point) {
        let bb = tiny_skia::NonZeroRect::from_xywh(xlate.x, xlate.y, 1., 1.).unwrap();
        for rect in &mut self.queue.rect[mark.rect..] {
            rect.bounds = rect.bounds.bbox_transform(bb);
        }
        xlate.scale(self.scale);
        if xlate.x.fract() != 0.0 || xlate.y.fract() != 0.0 {
            log::debug!("Found fractional-pixel translation ({xlate:?})");
        }
        for img in &mut self.queue.image[mark.image..] {
            img.top_left += xlate;
            img.crop[0] += xlate.x;
            img.crop[1] += xlate.y;
            img.crop[2] += xlate.x;
            img.crop[3] += xlate.y;
        }
    }

    pub fn crop_range(&mut self, a: &QueueMark, b: &QueueMark, mut crop: [f32; 4]) {
        for rect in &mut self.queue.rect[a.rect..b.rect] {
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
        for x in &mut crop {
            *x *= self.scale;
        }
        for img in &mut self.queue.image[a.image..b.image] {
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
