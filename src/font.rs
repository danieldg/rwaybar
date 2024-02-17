use crate::{icon::OwnedImage, item::Formatting, render::Render, state::Runtime, util::UID};
use log::info;
use std::{fs::File, io, path::PathBuf, sync::Arc, time::Instant};
use tiny_skia::{Color, Point, Transform};
use ttf_parser::{Face, GlyphId};

#[derive(Debug)]
pub struct FontMapped {
    // Note: lifetime is actually tied to mmap, not 'static
    parsed: Face<'static>,
    // this field must follow parsed for safety (drop order)
    #[allow(unused)]
    mmap: memmap2::Mmap,
    pub file: PathBuf,
    pub name: String,
    pub uid: UID,
}

impl FontMapped {
    pub fn new(name: String, path: PathBuf) -> io::Result<Self> {
        let file = File::open(&path)?;
        // rust's memory model requires that the backing file not be modified while in use; this is
        // generally not a concern for font files, but could in theory cause issues
        let mmap = unsafe { memmap2::Mmap::map(&file)? };
        // forge a static lifetime, safe if accessed via public API
        let buf = unsafe { &*(mmap.as_ref() as *const [u8]) };
        let parsed =
            Face::parse(&buf, 0).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let uid = UID::new();
        Ok(FontMapped {
            parsed,
            mmap,
            file: path,
            name,
            uid,
        })
    }

    pub fn as_ref<'a>(&'a self) -> &'a Face<'a> {
        &self.parsed
    }

    pub fn scale_from_pt(&self, pt: f32) -> f32 {
        pt * 1.33333333 / self.as_ref().units_per_em() as f32
    }
}

#[derive(Debug, Clone)]
pub struct CGlyph<'a> {
    pub id: GlyphId,
    /// For normal glyphs, scales from font units (integer) to render coordinates.
    /// If pixmap is Some, then scales from pixmap pixel to final pixel
    pub scale: f32,
    /// Relative position from the layout's origin point.
    ///
    /// If pixmap is Some, this is the top-left corner.
    /// For normal glyphs, this is the bottom-left (not a bbox)
    pub position: Point,
    pub font: &'a FontMapped,
    pub color: Color,
    pub pixmap: Option<OwnedImage>,
}

fn layout_font<'a>(
    font: &'a FontMapped,
    size_pt: f32,
    runtime: &'a Runtime,
    rgba: Color,
    text: &str,
    markup: bool,
) -> (Vec<CGlyph<'a>>, Point) {
    let scale = font.scale_from_pt(size_pt);
    let mut xpos = 0.0f32;
    let mut xmax = 0.0f32;
    let mut ypos = scale * (font.as_ref().line_gap() + font.as_ref().ascender()) as f32;
    let line_height = ypos - scale * font.as_ref().descender() as f32;
    let mut prev = None;
    let mut stack = Vec::new();
    let mut skip = 0;
    if false {
        stack.push((font, rgba));
    }

    let to_draw: Vec<_> = text
        .char_indices()
        .filter_map(|(i, mut c)| {
            if c == '\n' {
                xmax = xmax.max(xpos);
                xpos = 0.0;
                ypos += line_height as f32;
                return None;
            }
            if c == '\t' {
                c = ' ';
            }
            let mut fid = stack.last().map_or(font, |v| v.0);
            let color = stack.last().map_or(rgba, |v| v.1);
            if markup && c == '<' {
                if let Some(eot) = text[i..].find('>') {
                    let tag = &text[i..][..eot][1..];
                    skip = i + eot + 1;
                    if tag.starts_with('/') {
                        stack.pop();
                    } else {
                        let mut color = color;
                        for kv in tag.split(' ') {
                            if kv.starts_with("color='") || kv.starts_with("color=\"") {
                                let v = kv[7..].get(..kv.len() - 8);
                                color = Formatting::parse_rgba(v, None).unwrap_or(color);
                            } else if kv.starts_with("color=") {
                                color =
                                    Formatting::parse_rgba(Some(&kv[6..]), None).unwrap_or(color);
                            } else if kv.starts_with("font='") || kv.starts_with("font=\"") {
                                let v = kv[6..].get(..kv.len() - 7);
                                for font in &runtime.fonts {
                                    if v == Some(font.name.as_str()) {
                                        fid = font;
                                        break;
                                    }
                                }
                            }
                        }
                        stack.push((fid, color));
                    }
                }
            }
            if skip > i {
                return None;
            }
            let mut id = fid.as_ref().glyph_index(c).unwrap_or_default();
            if id.0 != 0 {
                let kern = ttf_parser::Tag::from_bytes(b"kern");
                if let Some(prev) = prev {
                    xpos += fid
                        .as_ref()
                        .raw_face()
                        .table(kern)
                        .and_then(ttf_parser::kern::Table::parse)
                        .map(|t| t.subtables)
                        .into_iter()
                        .flatten()
                        .filter(|st| st.horizontal && !st.variable)
                        .filter_map(|st| st.glyphs_kerning(prev, id))
                        .next()
                        .unwrap_or(0) as f32
                        * scale;
                }
                prev = Some(id);
            } else {
                let mut i = runtime.fonts.iter();
                loop {
                    let font = match i.next() {
                        Some(font) => font,
                        None => {
                            info!("Cannot find font for '{}'", c);
                            return None;
                        }
                    };
                    if let Some(gid) = font.as_ref().glyph_index(c) {
                        id = gid;
                        fid = font;
                        break;
                    }
                }
                prev = None;
            }
            let position = Point { x: xpos, y: ypos };
            let scale = fid.scale_from_pt(size_pt);
            let w = fid.as_ref().glyph_hor_advance(id).unwrap_or(0);
            xpos += w as f32 * scale;
            Some(CGlyph {
                id,
                position,
                scale,
                font: fid,
                color,
                pixmap: None,
            })
        })
        .collect();

    let text_size = Point {
        x: xpos.max(xmax) as f32,
        y: ypos - scale * font.as_ref().descender() as f32,
    };

    (to_draw, text_size)
}

/// Determine the bounding box of this series of glyphs
///
/// Output coordinates use the render scale and position as the origin.
fn bounding_box(to_draw: &mut [CGlyph], g_scale: f32, stroke: f32) -> (Point, Point) {
    let mut tl = Point {
        x: f32::MAX,
        y: f32::MAX,
    };
    let mut br = Point {
        x: f32::MIN,
        y: f32::MIN,
    };
    for &mut CGlyph {
        id,
        ref mut scale,
        ref mut position,
        font,
        ref mut pixmap,
        ..
    } in to_draw
    {
        if let Some(gbox) = font.as_ref().glyph_bounding_box(id) {
            let mut g_tl = Point {
                x: gbox.x_min as f32,
                y: -gbox.y_max as f32,
            };
            let mut g_br = Point {
                x: gbox.x_max as f32,
                y: -gbox.y_min as f32,
            };
            g_tl.scale(*scale);
            g_br.scale(*scale);
            g_tl += *position;
            g_br += *position;
            tl.x = tl.x.min(g_tl.x - stroke);
            tl.y = tl.y.min(g_tl.y - stroke);
            br.x = br.x.max(g_br.x + stroke);
            br.y = br.y.max(g_br.y + stroke);
            continue;
        }

        let target_ppem = *scale * font.as_ref().units_per_em() as f32 * g_scale;
        let target_h = *scale * font.as_ref().height() as f32 * g_scale;

        position.y -= font.as_ref().ascender() as f32 * *scale;
        if let Some(raster_img) = font.as_ref().glyph_raster_image(id, target_ppem as u16) {
            if let Some(img) = OwnedImage::from_data(raster_img.data, target_h as u32, false) {
                *pixmap = Some(img);

                *scale = target_ppem / raster_img.pixels_per_em as f32;

                position.x += raster_img.x as f32 * *scale;
                position.y += raster_img.y as f32 * *scale;
            }
        }
        if pixmap.is_none() {
            if let Some(svg) = font.as_ref().glyph_svg_image(id) {
                *pixmap = OwnedImage::from_svg(svg.data, target_h as u32);
                *scale = 1.;
            }
        }
        if let Some(img) = pixmap.as_ref() {
            let size = Point {
                x: img.pixmap.width() as f32 / g_scale,
                y: img.pixmap.height() as f32 / g_scale,
            };
            let g_br = *position + size;

            tl.x = tl.x.min(position.x);
            tl.y = tl.y.min(position.y);
            br.x = br.x.max(g_br.x);
            br.y = br.y.max(g_br.y);
        }
    }

    (tl, br)
}

fn draw_font_with<T>(
    target: &mut T,
    xform: Transform,
    to_draw: &[CGlyph],
    mut draw: impl FnMut(&mut T, &tiny_skia::Path, Color),
    mut draw_img: impl FnMut(&mut T, Transform, &OwnedImage),
) {
    for &CGlyph {
        id,
        scale,
        position,
        font,
        color,
        ref pixmap,
    } in to_draw
    {
        if let Some(img) = pixmap.as_ref() {
            let xform = xform.pre_translate(position.x, position.y);
            let xform = xform.pre_scale(scale, scale);
            draw_img(target, xform, img);
            continue;
        }

        struct Draw(tiny_skia::PathBuilder);
        let mut path = Draw(tiny_skia::PathBuilder::new());
        impl ttf_parser::OutlineBuilder for Draw {
            fn move_to(&mut self, x: f32, y: f32) {
                self.0.move_to(x, -y);
            }
            fn line_to(&mut self, x: f32, y: f32) {
                self.0.line_to(x, -y);
            }
            fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
                self.0.quad_to(x1, -y1, x, -y);
            }
            fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
                self.0.cubic_to(x1, -y1, x2, -y2, x, -y);
            }
            fn close(&mut self) {
                self.0.close();
            }
        }
        if let Some(_bounds) = font.as_ref().outline_glyph(id, &mut path) {
            let xform = xform.pre_translate(position.x, position.y);
            let xform = xform.pre_scale(scale, scale);
            if let Some(path) = path.0.finish().and_then(|p| p.transform(xform)) {
                draw(target, &path, color);
            }
            continue;
        }
    }
}

/// High quality pixmap scaling - used when a resize may happen
static HQ_PIXMAP_PAINT: tiny_skia::PixmapPaint = tiny_skia::PixmapPaint {
    opacity: 1.0,
    blend_mode: tiny_skia::BlendMode::SourceOver,
    quality: tiny_skia::FilterQuality::Bicubic,
};

const SUBPIXEL_KEYS: f32 = 8.0;
fn get_subpixel_key(x: f32) -> u8 {
    let frac = x - x.floor();
    let idx = frac * SUBPIXEL_KEYS;
    idx.floor() as u8
}

/// Align the given point to the pixel grid, returning true if the change would be unnoticeable
fn align_nearby_grid(Point { x, y }: &mut Point) -> bool {
    let margin = 1.0 / SUBPIXEL_KEYS;
    let xp = *x;
    let yp = *y;
    let xr = xp.round();
    let yr = yp.round();
    *x = xr;
    *y = yr;
    let dx = xp - xr;
    let dy = yp - yr;
    (dx.abs() <= margin) && (dy.abs() <= margin)
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct RenderKey {
    x_offset_subpix: u8,
    y_offset_subpix: u8,

    font: UID,
    font_size_millipt: u32,
    font_color: u32,
    text_stroke: u32,
    text_stroke_size_milli: u32,

    text: Box<str>,
}

#[derive(Debug)]
pub struct TextImage {
    /// Declared size of the text, in pixels.
    ///
    /// This controls the movement of render_pos, and is only vaguely related to the size of the
    /// bounding box of the text.
    text_size: Point,

    /// Pixel distance from the initial render_pos to the actual pixmap origin.
    ///
    /// This plus pixmap.(width, height) forms the actual bounding box for the text.
    origin_offset: Point,

    pixmap: Arc<tiny_skia::Pixmap>,
    pub last_used: Instant,
}

fn to_color_u32(color: Color) -> u32 {
    let c = color.to_color_u8();
    u32::from_ne_bytes([c.red(), c.green(), c.blue(), c.alpha()])
}

impl RenderKey {
    fn new(ctx: &Render, text: &str) -> Option<Self> {
        let pixel_x = ctx.render_pos.x * ctx.scale;
        let pixel_y = ctx.render_pos.y * ctx.scale;
        let xi = get_subpixel_key(pixel_x);
        let yi = get_subpixel_key(pixel_y);

        let text_stroke_size_milli = ctx.text_stroke.map_or(0, |_| {
            (ctx.text_stroke_size.unwrap_or(1.0) * ctx.scale * 1000.0).round() as u32
        });
        Some(RenderKey {
            x_offset_subpix: xi,
            y_offset_subpix: yi,
            font: ctx.font.uid,
            font_size_millipt: (ctx.scale * ctx.font_size * 1000.0).round() as u32,
            font_color: to_color_u32(ctx.font_color),

            text_stroke: ctx.text_stroke.map_or(0, to_color_u32),
            text_stroke_size_milli,

            text: text.into(),
        })
    }
}

pub fn render_font_item(ctx: &mut Render, text: &str, markup: bool) {
    if text.is_empty() {
        return;
    }

    let scale = ctx.scale;
    let mut render_pos = ctx.render_pos;

    let clip_w = ctx.render_extents.1.x - ctx.render_pos.x;
    let clip_h = ctx.render_extents.1.y - ctx.render_extents.0.y;

    let mut key = None;
    if ctx.queue.cache.is_some() {
        key = RenderKey::new(ctx, text);
    }

    if key
        .as_ref()
        .and_then(|k| {
            let ti = ctx.queue.cache.as_mut().unwrap().text.get_mut(k)?;
            let mut text_size = ti.text_size;
            text_size.scale(1. / scale);

            if text_size.x > clip_w {
                return None;
            }
            match ctx.align.vert {
                Some(f) if !ctx.render_flex => {
                    let extra = clip_h - text_size.y;
                    if extra >= 0.0 {
                        render_pos.y += extra * f;
                        ctx.render_pos.y += extra * f;
                    }
                }
                _ => {}
            }

            let mut pixel_pos = ctx.render_pos;
            pixel_pos.scale(scale);

            let mut pixmap_tl = pixel_pos - ti.origin_offset;
            if !align_nearby_grid(&mut pixmap_tl) {
                return None;
            }

            let img = ti.pixmap.clone();
            ti.last_used = Instant::now();
            ctx.render_pos += text_size;
            ctx.queue.push_image(pixmap_tl, img);

            Some(())
        })
        .is_some()
    {
        return;
    }

    let (mut to_draw, mut text_size) = layout_font(
        ctx.font,
        ctx.font_size,
        &ctx.runtime,
        ctx.font_color,
        &text,
        markup,
    );

    if text_size.y > clip_w {
        to_draw.retain(|glyph| glyph.position.x < clip_w);
        key = None;
    }

    ctx.render_pos += text_size;

    if !ctx.render_flex {
        match ctx.align.vert {
            Some(f) => {
                let extra = clip_h - text_size.y;
                if extra >= 0.0 {
                    render_pos.y += extra * f;
                    ctx.render_pos.y += extra * f;
                }
            }
            _ => {}
        }
    }

    if to_draw.is_empty() || ctx.bounds_only {
        return;
    }

    let stroke_width = if ctx.text_stroke.is_some() {
        ctx.text_stroke_size.unwrap_or(1.0)
    } else {
        0.0
    };

    // bounding box relative to render_pos
    let bbox = bounding_box(&mut to_draw, scale, 1.0 + stroke_width);

    if bbox.0.x >= bbox.1.x || bbox.0.y >= bbox.1.y {
        // empty bounding box
        return;
    }

    // our pixmap location, in render coordinates, not yet aligned to the pixel grid
    let bbox_tl = bbox.0 + render_pos;
    let bbox_br = bbox.1 + render_pos;

    // Expand the bbox to full pixels
    let pixel_l = (bbox_tl.x * scale).floor();
    let pixel_t = (bbox_tl.y * scale).floor();
    let pixel_r = (bbox_br.x * scale).ceil();
    let pixel_b = (bbox_br.y * scale).ceil();

    // pixmap location, in pixel coordinates
    let pixmap_tl = Point {
        x: pixel_l,
        y: pixel_t,
    };
    let xsize = pixel_r - pixel_l;
    let ysize = pixel_b - pixel_t;

    let origin_offset = Point {
        x: render_pos.x * scale - pixel_l,
        y: render_pos.y * scale - pixel_t,
    };

    // transform from render_pos-relative to our-pixmap-pixel
    let render_xform = tiny_skia::Transform {
        sx: scale,
        sy: scale,
        tx: origin_offset.x,
        ty: origin_offset.y,
        ..tiny_skia::Transform::identity()
    };

    let mut pixmap = match tiny_skia::Pixmap::new(xsize as u32, ysize as u32) {
        Some(pixmap) => pixmap,
        None => {
            log::debug!("Not rendering \"{text}\" ({xsize}, {ysize})");
            return;
        }
    };

    if let Some(rgba) = ctx.text_stroke {
        let stroke_paint = tiny_skia::Paint {
            shader: tiny_skia::Shader::SolidColor(rgba),
            anti_alias: true,
            ..tiny_skia::Paint::default()
        };
        let stroke = tiny_skia::Stroke {
            width: stroke_width,
            ..Default::default()
        };

        draw_font_with(
            &mut pixmap,
            render_xform,
            &to_draw,
            |canvas, path, color| {
                canvas.stroke_path(&path, &stroke_paint, &stroke, Transform::identity(), None);
                let paint = tiny_skia::Paint {
                    shader: tiny_skia::Shader::SolidColor(color),
                    anti_alias: true,
                    ..tiny_skia::Paint::default()
                };
                canvas.fill_path(
                    &path,
                    &paint,
                    tiny_skia::FillRule::EvenOdd,
                    Transform::identity(),
                    None,
                );
            },
            |canvas, xform, img| {
                canvas.draw_pixmap(0, 0, img.as_ref(), &HQ_PIXMAP_PAINT, xform, None);
            },
        );
    } else {
        draw_font_with(
            &mut pixmap,
            render_xform,
            &to_draw,
            |canvas, path, color| {
                let paint = tiny_skia::Paint {
                    shader: tiny_skia::Shader::SolidColor(color),
                    anti_alias: true,
                    ..tiny_skia::Paint::default()
                };
                canvas.fill_path(
                    &path,
                    &paint,
                    tiny_skia::FillRule::EvenOdd,
                    Transform::identity(),
                    None,
                );
            },
            |canvas, xform, img| {
                canvas.draw_pixmap(0, 0, img.as_ref(), &HQ_PIXMAP_PAINT, xform, None);
            },
        );
    }

    let pixmap = Arc::new(pixmap);

    ctx.queue.push_image(pixmap_tl, pixmap.clone());

    if let Some(key) = key {
        text_size.scale(scale);
        ctx.queue.cache.as_mut().unwrap().text.insert(
            key,
            TextImage {
                text_size,
                origin_offset,
                pixmap,
                last_used: Instant::now(),
            },
        );
    }
}
