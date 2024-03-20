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

        let mut queue = Queue { items: vec![] };
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
        let no_xform = tiny_skia::Transform::identity();

        let mut queue = Queue { items: vec![] };
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

        let mut damage = data.diff_contents(&queue);

        let everything = Rect::from_u32_wh(pixel_width, pixel_height);

        damage.retain_mut(|dmg| {
            *dmg = dmg.intersect(everything);
            dmg.is_valid()
        });

        if damage.is_empty() {
            // This happens when a wakeup didn't actually cause the displayed data to change. For
            // example, a load or temperature value can easily remain constant.
            surface_data.undo_damage();
            return;
        }

        // Drawing is done directly to the SHM region.  If must_clear is false, that region still
        // contains the prior frame, so we can avoid redrawing undamaged areas.
        let (canvas, must_clear, finalize) = self.render_be_rgba(data, &rt.wayland, surface);
        let mut canvas = tiny_skia::PixmapMut::from_bytes(canvas, pixel_width, pixel_height)
            .expect("Bad canvas size?");

        if must_clear {
            canvas.fill(tiny_skia::Color::TRANSPARENT);
        } else {
            let paint = tiny_skia::Paint {
                blend_mode: tiny_skia::BlendMode::Clear,
                ..Default::default()
            };
            for rect in &damage {
                if let Some(rect) = rect.to_skia() {
                    canvas.fill_rect(rect, &paint, no_xform, None);
                }
            }
        }

        for item in &queue.items {
            let paint = tiny_skia::Paint {
                shader: match &item.contents {
                    &RenderContents::Color(color) => tiny_skia::Shader::SolidColor(color),
                    RenderContents::Image {
                        top_left: tiny_skia::Point { x, y },
                        pixels,
                    } => tiny_skia::Pattern::new(
                        pixels.as_ref().as_ref(),
                        tiny_skia::SpreadMode::Pad,
                        tiny_skia::FilterQuality::Nearest,
                        1.0,
                        tiny_skia::Transform::from_translate(*x, *y),
                    ),
                },
                anti_alias: true,
                colorspace: tiny_skia::ColorSpace::Gamma2,
                ..Default::default()
            };
            if must_clear {
                if let Some(rect) = item.pixel_box.to_skia() {
                    canvas.fill_rect(rect, &paint, no_xform, None);
                }
            } else {
                item.for_damage(&damage, |rect| {
                    canvas.fill_rect(rect, &paint, no_xform, None)
                });
            }
        }

        finalize(canvas.data_mut());
        surface.frame(&rt.wayland.queue, surface.clone());

        for r in damage {
            surface.damage_buffer(r.left as _, r.top as _, r.width() as _, r.height() as _);
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
    rect: HashSet<RenderItem>,
    size: (i32, i32),

    slot: Option<Slot>,
}

impl RenderSurface {
    pub fn new() -> Self {
        Self {
            rect: HashSet::new(),
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
    fn diff_contents(&mut self, queue: &Queue) -> Vec<Rect> {
        let mut dmg = Vec::new();

        for item in &queue.items {
            if !self.rect.remove(&item) {
                if let Some(bbox) = item.bbox() {
                    dmg.push(bbox);
                }
            }
        }
        dmg.extend(self.rect.drain().filter_map(|e| e.bbox()));

        self.rect.extend(
            queue
                .items
                .iter()
                .filter(|i| i.pixel_box.is_valid())
                .cloned(),
        );

        // This could also consider y-coordinates, but that's harder
        dmg.sort_by(|a, b| a.left.total_cmp(&b.left));
        dmg.dedup_by(|b, a| {
            let dup = a.right + 7.0 > b.left;
            if dup {
                *a = a.join(b);
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
        if self.last_expire > as_of - time::Duration::from_secs(30) {
            return;
        }
        if let Some(min) = as_of.checked_sub(time::Duration::from_secs(13)) {
            let had = self.text.len();
            self.text.retain(|_k, v| v.last_used > min);
            log::debug!("Cache pruned from {} to {} entries", had, self.text.len());
        }
        self.last_expire = as_of;
    }
}

#[derive(Debug, Clone)]
enum RenderContents {
    Color(tiny_skia::Color),
    Image {
        top_left: tiny_skia::Point,
        pixels: Arc<tiny_skia::Pixmap>,
    },
}

#[derive(Debug, Clone, PartialEq)]
struct RenderItem {
    /// The bounding box on this item
    ///
    /// Note: this might be an invalid rect; if so, don't draw this item at all.
    pixel_box: Rect,
    contents: RenderContents,
}

impl Hash for RenderItem {
    fn hash<H: Hasher>(&self, h: &mut H) {
        h.write_u32(self.pixel_box.left.to_bits());
        h.write_u32(self.pixel_box.top.to_bits());
        h.write_u32(self.pixel_box.right.to_bits());
        h.write_u32(self.pixel_box.bottom.to_bits());
        match &self.contents {
            RenderContents::Color(color) => {
                h.write_u8(0);
                h.write_u32(color.red().to_bits());
                h.write_u32(color.green().to_bits());
                h.write_u32(color.blue().to_bits());
                h.write_u32(color.alpha().to_bits());
            }
            RenderContents::Image { top_left, pixels } => {
                h.write_u8(1);
                h.write_usize(Arc::as_ptr(pixels) as _);
                h.write_u32(top_left.x.to_bits());
                h.write_u32(top_left.y.to_bits());
            }
        }
    }
}

impl PartialEq for RenderContents {
    fn eq(&self, r: &Self) -> bool {
        use RenderContents::*;
        match (self, r) {
            (Color(a), Color(b)) => a == b,
            (
                Image {
                    pixels: a,
                    top_left: x,
                },
                Image {
                    pixels: b,
                    top_left: y,
                },
            ) => Arc::as_ptr(a) == Arc::as_ptr(b) && x == y,
            _ => false,
        }
    }
}

impl Eq for RenderItem {}

impl RenderItem {
    /// Return the pixel bounding box of this rect, or None if it's empty
    fn bbox(&self) -> Option<Rect> {
        if self.pixel_box.is_valid() {
            Some(Rect {
                left: self.pixel_box.left.floor(),
                top: self.pixel_box.top.floor(),
                right: self.pixel_box.right.ceil(),
                bottom: self.pixel_box.bottom.ceil(),
            })
        } else {
            None
        }
    }

    fn for_damage(&self, damage: &[Rect], mut fill: impl FnMut(tiny_skia::Rect)) {
        for &dbox in damage {
            if let Some(rect) = self.pixel_box.intersect(dbox).to_skia() {
                fill(rect);
            }
        }
    }
}

#[derive(Debug)]
struct Queue {
    items: Vec<RenderItem>,
}

#[derive(Debug)]
pub struct QueueMark {
    pos: usize,
}

/// State available to an [Item][crate::item::Item] render function
pub struct Render<'a> {
    queue: &'a mut Queue,
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

    pub fn item_group(&self) -> Group {
        Group {
            origin: self.render_pos,
            bounds: self.render_pos,
        }
    }

    /// bounds is in draw coordinates (pre-scale)
    pub fn push_rect(&mut self, bounds: Rect, color: tiny_skia::Color) {
        let pixel_box = bounds.scale(self.scale);
        self.queue.items.push(RenderItem {
            pixel_box,
            contents: RenderContents::Color(color),
        });
    }

    /// top_left is in pixel coordinates
    pub fn push_image(&mut self, top_left: tiny_skia::Point, pixels: Arc<tiny_skia::Pixmap>) {
        let bounds = Rect::from_xywh(
            top_left.x,
            top_left.y,
            pixels.width() as f32,
            pixels.height() as f32,
        );
        self.push_image_clip(top_left, pixels, bounds);
    }

    pub fn push_image_clip(
        &mut self,
        top_left: tiny_skia::Point,
        pixels: Arc<tiny_skia::Pixmap>,
        pixel_box: Rect,
    ) {
        if top_left.x.fract() != 0.0 || top_left.y.fract() != 0.0 {
            log::debug!("Found fractional image coordinates ({top_left:?})");
        }
        self.queue.items.push(RenderItem {
            contents: RenderContents::Image { top_left, pixels },
            pixel_box,
        });
    }

    pub fn start_group(&self) -> QueueMark {
        QueueMark {
            pos: self.queue.items.len(),
        }
    }

    /// This moves all items pushed after (b) behind all items pushed between (a) and (b)
    pub fn swap_after_marks(&mut self, a: &QueueMark, b: &QueueMark) {
        let len = self.queue.items.len() - b.pos;
        if a.pos == b.pos || len == 0 {
            return;
        }
        self.queue.items[a.pos..].rotate_right(len);
    }

    pub fn floor_to_pixel(&self, x: f32) -> f32 {
        (x * self.scale + 0.01).floor() / self.scale
    }

    pub fn ceil_to_pixel(&self, x: f32) -> f32 {
        (x * self.scale - 0.01).ceil() / self.scale
    }

    pub fn round_to_pixel(&self, x: f32) -> f32 {
        (x * self.scale).round() / self.scale
    }

    pub fn translate_group_x(&mut self, mark: &QueueMark, x: f32) {
        let x = x * self.scale;

        for item in &mut self.queue.items[mark.pos..] {
            item.pixel_box = item.pixel_box.translate(x, 0.0);
            if let RenderContents::Image { top_left, .. } = &mut item.contents {
                top_left.x += x;
            }
        }
    }

    pub fn crop_range(&mut self, a: &QueueMark, b: &QueueMark, crop: Rect) {
        let crop = crop.scale(self.scale);
        for item in &mut self.queue.items[a.pos..b.pos] {
            item.pixel_box = item.pixel_box.intersect(crop);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Rect {
    pub left: f32,
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
}

impl Rect {
    pub fn from_u32_wh(w: u32, h: u32) -> Self {
        Self::from_ltrb(0.0, 0.0, w as f32, h as f32)
    }

    pub fn from_xywh(x: f32, y: f32, w: f32, h: f32) -> Self {
        Self::from_ltrb(x, y, x + w, y + h)
    }

    pub fn from_ltrb(left: f32, top: f32, right: f32, bottom: f32) -> Self {
        Self {
            left,
            top,
            right,
            bottom,
        }
    }

    pub fn infinite() -> Self {
        Self {
            left: -f32::INFINITY,
            top: -f32::INFINITY,
            right: f32::INFINITY,
            bottom: f32::INFINITY,
        }
    }

    pub fn anti_plane() -> Self {
        Self {
            left: f32::INFINITY,
            top: f32::INFINITY,
            right: -f32::INFINITY,
            bottom: -f32::INFINITY,
        }
    }

    pub fn tl(self) -> tiny_skia::Point {
        tiny_skia::Point {
            x: self.left,
            y: self.top,
        }
    }

    #[allow(unused)]
    pub fn br(self) -> tiny_skia::Point {
        tiny_skia::Point {
            x: self.right,
            y: self.bottom,
        }
    }

    pub fn width(self) -> f32 {
        self.right - self.left
    }

    pub fn height(self) -> f32 {
        self.bottom - self.top
    }

    pub fn is_valid(self) -> bool {
        self.left <= self.right && self.top <= self.bottom
    }

    pub fn translate(mut self, x: f32, y: f32) -> Self {
        self.left += x;
        self.top += y;
        self.right += x;
        self.bottom += y;
        self
    }

    pub fn scale(mut self, scale: f32) -> Self {
        self.left *= scale;
        self.top *= scale;
        self.right *= scale;
        self.bottom *= scale;
        self
    }

    pub fn intersect(self, other: Self) -> Self {
        Self {
            left: self.left.max(other.left),
            top: self.top.max(other.top),
            right: self.right.min(other.right),
            bottom: self.bottom.min(other.bottom),
        }
    }

    pub fn join(&self, other: &Self) -> Self {
        Self {
            left: self.left.min(other.left),
            top: self.top.min(other.top),
            right: self.right.max(other.right),
            bottom: self.bottom.max(other.bottom),
        }
    }

    pub fn round_out(self) -> Self {
        Self {
            left: (self.left + 0.01).floor(),
            top: (self.top + 0.01).floor(),
            right: (self.right - 0.01).ceil(),
            bottom: (self.bottom - 0.01).ceil(),
        }
    }

    pub fn to_skia(self) -> Option<tiny_skia::Rect> {
        tiny_skia::Rect::from_ltrb(self.left, self.top, self.right, self.bottom)
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
