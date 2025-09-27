use crate::{
    icon::OwnedImage,
    render::{Rect, Render, RenderCache},
    style::Formatting,
    util::Cell,
};
use log::info;
use std::{collections::HashMap, mem, sync::Arc, time::Instant};
use tiny_skia::{Color, Point, Transform};
use ttf_parser::{Face, GlyphId};

#[derive(Debug)]
pub struct FontDB {
    fontdb: Option<Arc<fontdb::Database>>,
    queries: Cell<HashMap<Box<str>, Option<fontdb::ID>>>,
    fallback_ids: Vec<fontdb::ID>,
}

impl FontDB {
    pub fn load() -> Self {
        let mut db = fontdb::Database::new();
        db.load_system_fonts();
        Self {
            fontdb: Some(Arc::new(db)),
            queries: Default::default(),
            fallback_ids: Vec::new(),
        }
    }

    pub fn set_fallback(&mut self, list: &[toml::Value]) {
        self.fallback_ids.clear();
        for item in list {
            let Some(name) = item.as_str() else { continue };
            if name.starts_with('/') {
                let db = Arc::get_mut(self.fontdb.as_mut().unwrap()).unwrap();
                let ids = db.load_font_source(fontdb::Source::File(name.into()));
                self.fallback_ids.extend(ids);
            } else if let Some(id) = self.query(name) {
                self.fallback_ids.push(id);
            } else {
                info!("Could not find font '{name}', ignoring");
            }
        }
        let db = Arc::get_mut(self.fontdb.as_mut().unwrap()).unwrap();
        for &id in &self.fallback_ids {
            unsafe { db.make_shared_face_data(id) };
        }
    }

    pub fn db_arc(&self) -> Arc<fontdb::Database> {
        self.fontdb.clone().unwrap()
    }

    pub fn query(&self, name: &str) -> Option<fontdb::ID> {
        if let Some(res) = self.queries.take_in(|q| q.get(name).cloned()) {
            return res;
        }
        let db = self.fontdb.as_ref().unwrap();
        if name.starts_with('/') {
            for face in db.faces() {
                match &face.source {
                    fontdb::Source::File(p) | fontdb::Source::SharedFile(p, _)
                        if p.as_os_str() == name =>
                    {
                        // Note: Path::eq is stupidly slow compared to OsStr::eq
                        self.queries
                            .take_in(|q| q.insert(name.into(), Some(face.id)));
                        return Some(face.id);
                    }
                    _ => {}
                }
            }
        }
        let rv = db.query(&fontdb::Query {
            families: &[fontdb::Family::Name(name)],
            ..Default::default()
        });
        self.queries.take_in(|q| q.insert(name.into(), rv));
        rv
    }

    pub fn face(&mut self, id: fontdb::ID) -> Face<'_> {
        let db = Arc::get_mut(self.fontdb.as_mut().unwrap()).unwrap();
        // Avoid continually mapping and unmapping fonts.  The comment in fontdb is incorrect: it's
        // either safe to keep it mapped forever, or it's unsafe to map it at all, so for truly
        // paranoid correctness you must disable the 'mmap' feature in fontdb.  That's bad for
        // performance, and font files aren't edited in place on a sane system anyway.
        unsafe { db.make_shared_face_data(id) };
        let info = db.face(id).expect("Invalid Font ID");
        let buf = match &info.source {
            fontdb::Source::SharedFile(_, buf) => buf,
            _ => unreachable!(),
        };
        Face::parse((**buf).as_ref(), info.index).unwrap()
    }

    fn face1(&mut self, id: fontdb::ID) {
        let db = Arc::get_mut(self.fontdb.as_mut().unwrap()).unwrap();
        // Avoid continually mapping and unmapping fonts.  The comment in fontdb is incorrect: it's
        // either safe to keep it mapped forever, or it's unsafe to map it at all, so for truly
        // paranoid correctness you must disable the 'mmap' feature in fontdb.  That's bad for
        // performance, and font files aren't edited in place on a sane system anyway.
        unsafe { db.make_shared_face_data(id) };
    }

    fn face2(&self, id: fontdb::ID) -> Face<'_> {
        let db = self.fontdb.as_ref().unwrap();
        let info = db.face(id).expect("Invalid Font ID");
        let buf = match &info.source {
            fontdb::Source::SharedFile(_, buf) => buf,
            _ => unreachable!(),
        };
        Face::parse((**buf).as_ref(), info.index).unwrap()
    }

    fn take(&mut self) -> Self {
        mem::replace(
            self,
            Self {
                fontdb: Default::default(),
                queries: Default::default(),
                fallback_ids: Default::default(),
            },
        )
    }
}

pub fn scale_from_pt(face: &Face, pt: f32) -> f32 {
    pt * 1.33333333 / face.units_per_em() as f32
}

#[derive(Debug, Clone)]
struct CGlyph {
    pub id: GlyphId,
    /// For normal glyphs, scales from font units (integer) to render coordinates.
    /// If pixmap is Some, then scales from pixmap pixel to final pixel
    pub scale: f32,
    /// Relative position from the layout's origin point.
    ///
    /// If pixmap is Some, this is the top-left corner.
    /// For normal glyphs, this is the bottom-left (not a bbox)
    pub position: Point,
    pub fid: fontdb::ID,
    pub color: Color,

    /// At most one of pixmap and path is populated by gen_path
    pub pixmap: Option<OwnedImage>,
    /// At most one of pixmap and path is populated by gen_path
    pub path: Option<tiny_skia::Path>,
}

#[derive(Clone)]
struct CharIndices<'a> {
    offset: usize,
    iter: std::str::Chars<'a>,
}

impl<'a> CharIndices<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            offset: 0,
            iter: s.chars(),
        }
    }

    fn new_at(s: &'a str, offset: usize) -> Self {
        Self {
            offset,
            iter: s[offset..].chars(),
        }
    }
}

impl<'a> Iterator for CharIndices<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<(usize, char)> {
        let pre = self.iter.as_str().len();
        let c = self.iter.next()?;
        let post = self.iter.as_str().len();
        let offset = self.offset;
        self.offset += pre - post;
        Some((offset, c))
    }
}

fn layout_font(
    fid: fontdb::ID,
    size_pt: f32,
    cache: &mut RenderCache,
    rgba: Color,
    text: &str,
    markup: bool,
    wrap: Option<f32>,
) -> (Vec<CGlyph>, Point, FontDB) {
    let mut db = cache.fontdb.take();
    db.face1(fid);
    let mut font = db.face2(fid);
    let mut font_fid = fid;
    let scale = scale_from_pt(&font, size_pt);
    let mut xpos = 0.0f32;
    let mut xmax = 0.0f32;
    let mut ypos = scale * (font.line_gap() + font.ascender()) as f32;
    let desc = scale * font.descender() as f32;
    let line_height = ypos - desc;
    let mut prev = None;
    let mut stack = Vec::new();
    if false {
        // Avoid creating a stack if not needed by falling back on unwrap
        stack.push((fid, size_pt, rgba));
    }

    let mut iter = CharIndices::new(text);
    let mut wrap_i = None;

    let mut to_draw = Vec::with_capacity(text.len());
    while let Some((i, mut c)) = iter.next() {
        if c == '\n' {
            xmax = xmax.max(xpos);
            xpos = 0.0;
            ypos += line_height as f32;
            continue;
        }
        if wrap.is_some_and(|wpos| xpos > wpos) {
            if let Some((wpos, i, draw_end)) = wrap_i.take() {
                iter = CharIndices::new_at(text, i);
                to_draw.truncate(draw_end);
                xmax = xmax.max(wpos);
                xpos = 0.0;
                ypos += line_height as f32;
                continue;
            }
        }
        if c == '\t' {
            c = ' ';
        }
        let &(mut fid, size_pt, color) = stack.last().unwrap_or(&(fid, size_pt, rgba));
        if markup && c == '<' {
            if let Some(eot) = text[i..].find('>') {
                let tag = &text[i..][..eot][1..];
                iter = CharIndices::new_at(text, i + eot + 1);
                if tag.starts_with('/') {
                    stack.pop();
                } else {
                    let mut color = color;
                    let mut size_pt = size_pt;
                    for kv in tag.split(' ') {
                        if kv.starts_with("color='") || kv.starts_with("color=\"") {
                            let v = kv[7..].get(..kv.len() - 8);
                            color = Formatting::parse_rgba(v, None).unwrap_or(color);
                        } else if kv.starts_with("color=") {
                            color = Formatting::parse_rgba(Some(&kv[6..]), None).unwrap_or(color);
                        } else if kv.starts_with("font='") || kv.starts_with("font=\"") {
                            if let Some(v) = kv[6..].get(..kv.len() - 7) {
                                if let Some(id) = db.query(v) {
                                    fid = id;
                                }
                            }
                        } else if kv.starts_with("size=") {
                            let mut v = &kv[5..];
                            if matches!(v.chars().next(), Some('"') | Some('\'')) {
                                v = &v[1..v.len() - 1];
                            }
                            if v.ends_with('%') {
                                size_pt *= v[..v.len() - 1].parse().unwrap_or(100.0) / 100.0;
                            } else if let Ok(v) = v.parse() {
                                size_pt = v;
                            }
                        }
                    }
                    stack.push((fid, size_pt, color));
                }
                continue;
            }
        }

        if c == ' ' && xpos != 0.0 {
            wrap_i = Some((xpos, iter.offset, to_draw.len()));
        }

        if fid != font_fid {
            font_fid = fid;
            drop(font);
            db.face1(fid);
            font = db.face2(fid);
        }
        let mut fid = fid;
        let mut fallback = None;

        let mut id = GlyphId(0);
        if markup && c == '&' {
            if let Some(eot) = text[i..].find(';') {
                let tag = &text[i..][..eot][1..];
                iter = CharIndices::new_at(text, i + eot + 1);
                if tag.starts_with("#0x") || tag.starts_with("#0X") {
                    match u32::from_str_radix(&tag[3..], 16)
                        .ok()
                        .and_then(char::from_u32)
                    {
                        Some(nc) => c = nc,
                        None => continue,
                    }
                } else if tag.starts_with('#') {
                    match tag[1..].parse().ok().and_then(char::from_u32) {
                        Some(nc) => c = nc,
                        None => continue,
                    }
                } else if tag.starts_with('@') {
                    if let Ok(gi) = tag[1..].parse::<u16>() {
                        if gi > 0 && gi < font.number_of_glyphs() {
                            id.0 = gi;
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                } else {
                    match tag {
                        "amp" => c = '&',
                        "lt" => c = '<',
                        "gt" => c = '>',
                        _ => continue,
                    }
                }
            }
        }
        if id.0 == 0 {
            id = font.glyph_index(c).unwrap_or_default();
        }
        if id.0 != 0 {
            if let Some(prev) = prev {
                let tables = font.tables();
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
        } else if cache.failed_char(c) {
            continue;
        } else {
            for &fid_c in &db.fallback_ids {
                // All fallback_ids have been mmap'd already, no need for face1 calls
                let font_c = db.face2(fid_c);
                if let Some(gid) = font_c.glyph_index(c) {
                    id = gid;
                    fid = fid_c;
                    fallback = Some(font_c);
                    break;
                }
            }
            if fallback.is_none() {
                cache.set_failed(c);
                info!("Cannot find font for '{c}'; consider adding a fallback font for it");
                continue;
            }
            prev = None;
        }
        let font = fallback.as_ref().unwrap_or(&font);
        let position = Point { x: xpos, y: ypos };
        let scale = scale_from_pt(&font, size_pt);
        let w = font.glyph_hor_advance(id).unwrap_or(0);
        xpos += w as f32 * scale;
        to_draw.push(CGlyph {
            id,
            position,
            scale,
            fid,
            color,
            pixmap: None,
            path: None,
        });
    }

    let text_size = Point {
        x: xpos.max(xmax) as f32,
        y: ypos - desc,
    };

    (to_draw, text_size, db)
}

impl CGlyph {
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
            font: self.fid,
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

    fn gen_path<'a>(
        &mut self,
        cache: &mut Option<(fontdb::ID, Face<'a>)>,
        db: &'a FontDB,
        stroke: f32,
    ) -> Rect {
        let font = match cache {
            &mut Some((cid, ref font)) if cid == self.fid => font,
            opt => {
                *opt = None;
                &opt.get_or_insert((self.fid, db.face2(self.fid))).1
            }
        };
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
        if let Some(gbox) = font.outline_glyph(self.id, &mut path) {
            let xform = Transform::from_translate(self.position.x, self.position.y);
            let xform = xform.pre_scale(self.scale, self.scale);
            self.path = path.0.finish().and_then(|p| p.transform(xform));

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
        // Note: there is also paint_color_glyph, but that's a lot more work and it doesn't
        // integrate well with the stroke setting

        let target_ppem = self.scale * font.units_per_em() as f32;
        let target_h = self.scale * font.height() as f32;
        let mut position = self.position;

        position.y -= font.ascender() as f32 * self.scale;
        if let Some(raster_img) = font.glyph_raster_image(self.id, target_ppem as u16) {
            let img_scale = target_ppem / raster_img.pixels_per_em as f32;
            if let Some(img) = OwnedImage::from_data(raster_img.data, target_h as u32, false, None)
            {
                let real_h = img.pixmap.height() as f32 * img_scale;
                let img = img.rescale_height(real_h as u32);
                self.pixmap = Some(img);

                position.x += raster_img.x as f32 * self.scale;
                position.y += raster_img.y as f32 * self.scale;
            }
        }
        if self.pixmap.is_none() {
            if let Some(svg) = font.glyph_svg_image(self.id) {
                self.pixmap = OwnedImage::from_svg(svg.data, target_h as u32, None);
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
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub struct RenderKey {
    x_offset_subpix: u8,
    y_offset_subpix: u8,

    font: fontdb::ID,
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

#[derive(Debug, Default, Copy, Clone)]
pub struct TextSettings {
    pub markup: bool,
    pub wrap: bool,
}

pub fn render_font_item(ctx: &mut Render, text: &str, TextSettings { markup, wrap }: TextSettings) {
    if text.is_empty() {
        return;
    }

    let scale = ctx.scale;
    let mut render_pos = ctx.render_pos;

    let clip_w = ctx.render_extents.right - ctx.render_pos.x;
    let clip_h = ctx.render_extents.height();

    let (mut to_draw, text_size, db) = layout_font(
        ctx.style.font,
        ctx.style.font_size,
        ctx.cache,
        ctx.style.font_color,
        &text,
        markup,
        wrap.then_some(clip_w),
    );

    if text_size.x > clip_w {
        to_draw.retain(|glyph| glyph.position.x < clip_w);
    }

    ctx.render_pos += text_size;

    if !ctx.render_flex {
        match ctx.style.align.vert {
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
        ctx.cache.fontdb = db;
        return;
    }

    let stroke_width = if ctx.style.text_stroke.is_some() {
        ctx.style.text_stroke_size.unwrap_or(1.0)
    } else {
        0.0
    };
    let stroke_color_u32 = ctx.style.text_stroke.map_or(0, to_color_u32);
    let stroke = tiny_skia::Stroke {
        width: stroke_width,
        ..Default::default()
    };

    let stroke_paint = ctx.style.text_stroke.map(|rgba| tiny_skia::Paint {
        shader: tiny_skia::Shader::SolidColor(rgba),
        anti_alias: true,
        colorspace: tiny_skia::ColorSpace::Gamma2,
        ..tiny_skia::Paint::default()
    });

    let mut face = None;
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
        if let Some(ti) = ctx.cache.get_glyph(&key) {
            let img = ti.pixmap.clone();
            let mut pos = glyph.position - ti.origin_offset;
            pos.x = pos.x.round();
            pos.y = pos.y.round();
            ti.last_used = Instant::now();
            ctx.push_image(pos, img);
            continue;
        }

        let bbox = glyph.gen_path(&mut face, &db, 1.0 + stroke_width);
        if !bbox.is_valid() {
            continue;
        }
        let pbox = bbox.round_out();
        glyph.position -= pbox.tl();

        if let Some(pixmap) = glyph.pixmap.take() {
            ctx.cache.add_glyph(
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

        let Some(path) = glyph.path else {
            continue;
        };
        let Some(path) = path.transform(Transform::from_translate(-pbox.left, -pbox.top)) else {
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
        ctx.cache.add_glyph(
            key,
            TextImage {
                origin_offset: glyph.position,
                pixmap: pixmap.clone(),
                last_used: Instant::now(),
            },
        );
        ctx.push_image(pbox.tl(), pixmap);
    }
    ctx.cache.fontdb = db;
}
