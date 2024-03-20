use crate::{
    icon::OwnedImage,
    item::Formatting,
    render::{Rect, Render},
    state::Runtime,
    util::UID,
};
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

    let to_draw = text
        .char_indices()
        .filter_map(|(i, mut c)| {
            if skip > i {
                return None;
            }
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
                    return None;
                }
            }
            let mut id = GlyphId(0);
            if markup && c == '&' {
                if let Some(eot) = text[i..].find(';') {
                    let tag = &text[i..][..eot][1..];
                    skip = i + eot + 1;
                    if tag.starts_with("#0x") || tag.starts_with("#0X") {
                        match u32::from_str_radix(&tag[3..], 16)
                            .ok()
                            .and_then(char::from_u32)
                        {
                            Some(nc) => c = nc,
                            None => return None,
                        }
                    } else if tag.starts_with('#') {
                        match tag[1..].parse().ok().and_then(char::from_u32) {
                            Some(nc) => c = nc,
                            None => return None,
                        }
                    } else if tag.starts_with('@') {
                        if let Ok(gi) = tag[1..].parse::<u16>() {
                            if gi > 0 && gi < fid.as_ref().number_of_glyphs() {
                                id.0 = gi;
                            } else {
                                return None;
                            }
                        } else {
                            return None;
                        }
                    } else {
                        match tag {
                            "amp" => c = '&',
                            "lt" => c = '<',
                            "gt" => c = '>',
                            _ => return None,
                        }
                    }
                }
            }
            if id.0 == 0 {
                id = fid.as_ref().glyph_index(c).unwrap_or_default();
            }
            if id.0 != 0 {
                if let Some(prev) = prev {
                    let tables = fid.as_ref().tables();
                    let offset = tables
                        .kern
                        .map(|t| t.subtables)
                        .into_iter()
                        .flatten()
                        .filter(|st| st.horizontal && !st.variable)
                        .filter_map(|st| st.glyphs_kerning(prev, id))
                        .fold(0, |a, b| a + b);
                    let offset = tables
                        .kerx
                        .map(|t| t.subtables)
                        .into_iter()
                        .flatten()
                        .filter(|st| st.horizontal && !st.variable)
                        .filter_map(|st| st.glyphs_kerning(prev, id))
                        .fold(offset, |a, b| a + b);

                    xpos += offset as f32 * scale;
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

impl CGlyph<'_> {
    fn key(&mut self, stroke_width: f32, text_stroke: u32) -> RenderKey {
        fn sp_key(x: &mut f32) -> u8 {
            const HALF_MAX_EXACT_F32: f32 = (1 << f32::MANTISSA_DIGITS - 1) as f32;
            //debug_assert_eq!(HALF_MAX_EXACT_F32 + 1.0, HALF_MAX_EXACT_F32.next_up());
            const SUBPIXEL_KEYS: u8 = 8;
            const SUBPIXEL_KEYS_F: f32 = SUBPIXEL_KEYS as f32;
            const SUBPIXEL_KEYS_I: u32 = SUBPIXEL_KEYS as u32;

            let sp_int = (*x * SUBPIXEL_KEYS_F).round();
            *x = sp_int / SUBPIXEL_KEYS_F;
            // offset far into the positive numbers to avoid saturate-to-0
            let ipos = (sp_int + HALF_MAX_EXACT_F32) as u32;
            (ipos % SUBPIXEL_KEYS_I) as u8
        }

        let text_stroke_size_milli = (stroke_width * 1000.0).round() as u32;
        RenderKey {
            x_offset_subpix: sp_key(&mut self.position.x),
            y_offset_subpix: sp_key(&mut self.position.y),
            font: self.font.uid,
            scale: self.scale.to_bits(),
            font_color: to_color_u32(self.color),

            text_stroke,
            text_stroke_size_milli,

            glyph: self.id,
        }
    }

    fn scale(&mut self, scale: f32) {
        self.position.scale(scale);
        self.scale *= scale;
    }

    fn translate(&mut self, delta: Point) {
        self.position += delta;
    }

    fn bbox(&mut self, stroke: f32) -> Rect {
        let font = self.font.as_ref();
        if let Some(gbox) = font.glyph_bounding_box(self.id) {
            let mut g_tl = Point {
                x: gbox.x_min as f32,
                y: -gbox.y_max as f32,
            };
            let mut g_br = Point {
                x: gbox.x_max as f32,
                y: -gbox.y_min as f32,
            };
            g_tl.scale(self.scale);
            g_br.scale(self.scale);
            g_tl += self.position;
            g_br += self.position;
            return Rect::from_ltrb(
                g_tl.x - stroke,
                g_tl.y - stroke,
                g_br.x + stroke,
                g_br.y + stroke,
            );
        }

        let target_ppem = self.scale * font.units_per_em() as f32;
        let target_h = self.scale * font.height() as f32;
        let mut position = self.position;

        position.y -= font.ascender() as f32 * self.scale;
        if let Some(raster_img) = font.glyph_raster_image(self.id, target_ppem as u16) {
            let img_scale = target_ppem / raster_img.pixels_per_em as f32;
            if let Some(img) = OwnedImage::from_data(raster_img.data, target_h as u32, false) {
                let real_h = img.pixmap.height() as f32 * img_scale;
                let img = img.rescale_height(real_h as u32);
                self.pixmap = Some(img);

                position.x += raster_img.x as f32 * self.scale;
                position.y += raster_img.y as f32 * self.scale;
            }
        }
        if self.pixmap.is_none() {
            if let Some(svg) = font.glyph_svg_image(self.id) {
                self.pixmap = OwnedImage::from_svg(svg.data, target_h as u32);
            }
        }
        if let Some(img) = self.pixmap.as_ref() {
            Rect::from_xywh(
                position.x,
                position.y,
                img.pixmap.width() as f32,
                img.pixmap.height() as f32,
            )
        } else {
            Rect::anti_plane()
        }
    }

    fn path(&self) -> Option<tiny_skia::Path> {
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
        if let Some(_bounds) = self.font.as_ref().outline_glyph(self.id, &mut path) {
            let xform = Transform::from_translate(self.position.x, self.position.y);
            let xform = xform.pre_scale(self.scale, self.scale);
            path.0.finish().and_then(|p| p.transform(xform))
        } else {
            None
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct RenderKey {
    x_offset_subpix: u8,
    y_offset_subpix: u8,

    font: UID,
    scale: u32,
    font_color: u32,
    text_stroke: u32,
    text_stroke_size_milli: u32,

    glyph: GlyphId,
}

#[derive(Debug)]
pub struct TextImage {
    /// Pixel distance from the initial render_pos to the actual pixmap origin.
    ///
    /// This plus pixmap.(width, height) forms the actual bounding box for the text.
    origin_offset: Point,

    pub pixmap: Arc<tiny_skia::Pixmap>,
    pub last_used: Instant,
}

fn to_color_u32(color: Color) -> u32 {
    let c = color.to_color_u8();
    u32::from_ne_bytes([c.red(), c.green(), c.blue(), c.alpha()])
}

pub fn render_font_item(ctx: &mut Render, text: &str, markup: bool) {
    if text.is_empty() {
        return;
    }

    let scale = ctx.scale;
    let mut render_pos = ctx.render_pos;

    let clip_w = ctx.render_extents.1.x - ctx.render_pos.x;
    let clip_h = ctx.render_extents.1.y - ctx.render_extents.0.y;

    let (mut to_draw, text_size) = layout_font(
        ctx.font,
        ctx.font_size,
        &ctx.runtime,
        ctx.font_color,
        &text,
        markup,
    );

    if text_size.x > clip_w {
        to_draw.retain(|glyph| glyph.position.x < clip_w);
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
    let stroke_color_u32 = ctx.text_stroke.map_or(0, to_color_u32);
    let stroke = tiny_skia::Stroke {
        width: stroke_width,
        ..Default::default()
    };

    let stroke_paint = ctx.text_stroke.map(|rgba| tiny_skia::Paint {
        shader: tiny_skia::Shader::SolidColor(rgba),
        anti_alias: true,
        colorspace: tiny_skia::ColorSpace::Gamma2,
        ..tiny_skia::Paint::default()
    });

    for mut glyph in to_draw {
        glyph.translate(render_pos);
        glyph.scale(scale);

        // glyph is now positioned in pixel coordinates on the actual render target.  See if we
        // have a pixmap in the glyph cache that can be used - if we do, use it so that damage
        // tracking works (and also to save time stroking the path every frame).
        //
        // The key() function will move the character by at most (0.5/SUBPIXEL_KEYS) pixel in each
        // direction to align it to a sub-pixel grid.  This means rendering "aaaaaaaaaaaaaaaaaaaa"
        // will end up with at most SUBPIXEL_KEYS "a" images in the cache, one per subpixel offset.
        let key = glyph.key(stroke_width, stroke_color_u32);
        if let Some(ti) = ctx.cache.text.get_mut(&key) {
            let img = ti.pixmap.clone();
            let mut pos = glyph.position - ti.origin_offset;
            pos.x = pos.x.round();
            pos.y = pos.y.round();
            ti.last_used = Instant::now();
            ctx.push_image(pos, img);
            continue;
        }

        let bbox = glyph.bbox(1.0 + stroke_width);
        if !bbox.is_valid() {
            continue;
        }
        let pbox = bbox.round_out();
        glyph.position -= pbox.tl();

        if let Some(pixmap) = glyph.pixmap.take() {
            ctx.cache.text.insert(
                key,
                TextImage {
                    origin_offset: glyph.position,
                    pixmap: pixmap.pixmap.clone(),
                    last_used: Instant::now(),
                },
            );
            ctx.push_image(pbox.tl(), pixmap.pixmap);
            continue;
        }

        let Some(path) = glyph.path() else {
            continue;
        };

        let Some(mut pixmap) = tiny_skia::Pixmap::new(pbox.width() as u32, pbox.height() as u32)
        else {
            continue;
        };

        if let Some(stroke_paint) = stroke_paint.as_ref() {
            pixmap.stroke_path(&path, stroke_paint, &stroke, Transform::identity(), None);
        }
        let paint = tiny_skia::Paint {
            shader: tiny_skia::Shader::SolidColor(glyph.color),
            anti_alias: true,
            colorspace: tiny_skia::ColorSpace::Gamma2,
            ..tiny_skia::Paint::default()
        };
        pixmap.fill_path(
            &path,
            &paint,
            tiny_skia::FillRule::EvenOdd,
            Transform::identity(),
            None,
        );

        let pixmap = Arc::new(pixmap);
        ctx.cache.text.insert(
            key,
            TextImage {
                origin_offset: glyph.position,
                pixmap: pixmap.clone(),
                last_used: Instant::now(),
            },
        );
        ctx.push_image(pbox.tl(), pixmap);
    }
}
