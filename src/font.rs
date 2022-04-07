use crate::icon::OwnedImage;
use crate::item::Formatting;
use crate::state::Runtime;
use crate::render::{Render,UID};
use log::info;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::time::Instant;
use tiny_skia::{Color,PixmapMut,Point,Transform};
use ttf_parser::{Face,GlyphId};

#[derive(Debug)]
pub struct FontMapped {
    // Note: lifetime is actually tied to mmap, not 'static
    parsed : Face<'static>,
    // this field must follow parsed for safety (drop order)
    #[allow(unused)]
    mmap : memmap2::Mmap,
    pub file : PathBuf,
    pub name : String,
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
        let parsed = Face::from_slice(&buf, 0)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        let uid = UID::default();
        Ok(FontMapped { parsed, mmap, file : path, name, uid })
    }

    pub fn as_ref<'a>(&'a self) -> &'a Face<'a> {
        &self.parsed
    }

    pub fn scale_from_pt(&self, pt : f32) -> f32 {
        pt * 1.33333333 / self.as_ref().units_per_em() as f32
    }
}

#[derive(Debug,Copy,Clone)]
pub struct CGlyph<'a> {
    pub id: GlyphId,
    pub scale : f32,
    pub position : (f32, f32),
    pub font : &'a FontMapped,
    pub color : Color,
}

pub fn layout_font<'a>(
    font: &'a FontMapped,
    size_pt: f32,
    runtime : &'a Runtime,
    rgba : Color,
    text : &str,
    markup: bool)
    -> (Vec<CGlyph<'a>>, (f32, f32))
{
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

    let to_draw : Vec<_> =
        text.char_indices()
        .filter_map(|(i,c)| {
            if c == '\n' {
                xmax = xmax.max(xpos);
                xpos = 0.0;
                ypos += line_height as f32;
                return None;
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
                            if kv.starts_with("color='") || kv.starts_with("color=\""){
                                let v = kv[7..].get(..kv.len() - 8);
                                color = Formatting::parse_rgba(v, None).unwrap_or(color);
                            } else if kv.starts_with("color=") {
                                color = Formatting::parse_rgba(Some(&kv[6..]), None).unwrap_or(color);
                            } else if kv.starts_with("font='") || kv.starts_with("font=\""){
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
                    xpos += fid.as_ref()
                        .table_data(kern)
                        .and_then(ttf_parser::kern::Table::parse)
                        .map(|t| t.subtables)
                        .into_iter()
                        .flatten()
                        .filter(|st| st.horizontal && !st.variable)
                        .filter_map(|st| st.glyphs_kerning(prev, id))
                        .next()
                        .unwrap_or(0) as f32 * scale;
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
            let position = (xpos, ypos);
            let scale = fid.scale_from_pt(size_pt);
            let w = fid.as_ref().glyph_hor_advance(id).unwrap_or(0);
            xpos += w as f32 * scale;
            Some(CGlyph { id, position, scale, font : fid, color })
        })
        .collect();


    let width = xpos.max(xmax) as f32;
    let height = ypos - scale * font.as_ref().descender() as f32;
    (to_draw, (width, height))
}

pub fn draw_font_with(target : &mut PixmapMut, xform: Transform, to_draw : &[CGlyph], mut draw : impl FnMut(&mut PixmapMut, &tiny_skia::Path, Color)) {
    for &CGlyph { id, scale, position, font, color } in to_draw {
        struct Draw(tiny_skia::PathBuilder);
        let mut path = Draw(tiny_skia::PathBuilder::new());
        impl ttf_parser::OutlineBuilder for Draw {
            fn move_to(&mut self, x: f32, y: f32) {
                self.0.move_to(x,-y);
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
            let xform = xform.pre_translate(position.0, position.1);
            let xform = xform.pre_scale(scale, scale);
            if let Some(path) = path.0.finish().and_then(|p| p.transform(xform)) {
                draw(target, &path, color);
            }
            continue;
        }
        let target_ppem = scale * font.as_ref().units_per_em() as f32;
        let target_h = scale * font.as_ref().height() as f32;
        if let Some(raster_img) = font.as_ref().glyph_raster_image(id, target_ppem as u16) {
            // This is a PNG glyph (color emoji); read it into a pixbuf and draw like an icon
            if let Some(img) = OwnedImage::from_data(raster_img.data, target_h as u32) {
                let ypos = position.1 - font.as_ref().ascender() as f32 * scale;
                let png_scale = target_ppem / raster_img.pixels_per_em as f32;
                let xform = xform.pre_translate(position.0, ypos);
                let xform = xform.pre_scale(png_scale, png_scale);
                let xform = xform.pre_translate(raster_img.x as f32, raster_img.y as f32);
                target.draw_pixmap(
                    0, 0,
                    img.0.as_ref(),
                    &tiny_skia::PixmapPaint::default(),
                    xform,
                    None);
                continue;
            }
        }
        if let Some(svg) = font.as_ref().glyph_svg_image(id) {
            if let Some(img) = OwnedImage::from_svg(svg, target_h as u32) {
                let ypos = position.1 - font.as_ref().ascender() as f32 * scale;
                let xform = xform.pre_translate(position.0, ypos);
                target.draw_pixmap(
                    0, 0,
                    img.0.as_ref(),
                    &tiny_skia::PixmapPaint::default(),
                    xform,
                    None);
                continue;
            }
        }
    }
}

pub fn render_font(ctx: &mut Render, start: (f32, f32), text: &str, markup: bool) -> (f32, f32) {
    let (mut to_draw, size) = layout_font(ctx.font, ctx.font_size, ctx.runtime, ctx.font_color, text, markup);
    let clip_w = ctx.render_extents.1.x - ctx.render_pos.x;
    if size.1 > clip_w {
        to_draw.retain(|glyph| glyph.position.0 < clip_w);
    }
    let xform = ctx.render_xform.pre_translate(start.0, start.1);
    draw_font_with(ctx.canvas, xform, &to_draw, |canvas,path,color| {
        let paint = tiny_skia::Paint {
            shader: tiny_skia::Shader::SolidColor(color),
            anti_alias: true,
            ..tiny_skia::Paint::default()
        };
        canvas.fill_path(&path, &paint, tiny_skia::FillRule::EvenOdd, Transform::identity(), None);
    });
    size
}

#[derive(Eq,Hash,PartialEq,Debug)]
pub struct RenderKey {
    x_offset_centipixel: u8,
    scale: u8,

    font : UID,
    font_size_millipt : u32,
    font_color : u32,
    text_stroke : Option<u32>,
    text_stroke_size_milli : Option<u32>,

    text: String,
}

#[derive(Debug)]
pub struct TextImage {
    width: f32,
    height: f32,
    y_offset_centipixel: u8,
    pixmap: tiny_skia::Pixmap,
    pub last_used: Instant,
}

impl RenderKey {
    fn new(ctx: &Render, xform: tiny_skia::Transform, text: &str) -> Option<Self> {
        let xi = (xform.tx * 100.0).round() as u64 % 100;
        let scale = xform.sx as u8;
        if scale as f32 != xform.sy || xform.sx != xform.sy {
            return None;
        }
        let text_stroke_size_milli = ctx.text_stroke.and_then(|_| ctx.text_stroke_size.map(|s| (s * 1000.0).round() as u32));
        Some(RenderKey {
            x_offset_centipixel: xi as u8,
            scale,
            font: ctx.font.uid,
            font_size_millipt: (ctx.font_size * 1000.0).round() as u32,
            font_color: ctx.font_color.to_color_u8().get(),

            text_stroke: ctx.text_stroke.map(|c| c.to_color_u8().get()),
            text_stroke_size_milli,

            text: text.into(),
        })
    }
}


pub fn render_font_item(ctx: &mut Render, text: &str, markup: bool) {
    if text.is_empty() {
        return;
    }

    let Point { x: xstart, y: ystart } = ctx.render_pos;

    let mut xform = ctx.render_xform.pre_translate(xstart, ystart);

    let clip_w = ctx.render_extents.1.x - ctx.render_pos.x;
    let clip_h = ctx.render_extents.1.y - ctx.render_extents.0.y;

    let mut key = RenderKey::new(ctx, xform, text);

    if key.as_ref().and_then(|k| {
        let mut xform = xform;
        let mut cache = ctx.cache.text.borrow_mut();
        let ti = cache.get_mut(k)?;
        if ti.width > clip_w {
            return None;
        }
        let mut xlate_y = xform.ty;
        match ctx.align.vert {
            Some(f) if !ctx.render_flex => {
                let extra = clip_h - ti.height;
                if extra >= 0.0 {
                    xform = xform.pre_translate(0.0, extra * f);
                    xlate_y = xform.ty;
                }
            }
            _ => {}
        }
        if ti.y_offset_centipixel as u64 != (xlate_y * 100.0).round() as u64 % 100 {
            return None;
        }

        let mut origin = [ Point {
            x: -(k.x_offset_centipixel as f32 / 100.0),
            y: -(ti.y_offset_centipixel as f32 / 100.0),
        } ];
        xform.map_points(&mut origin);
        let draw_x = origin[0].x.round() as i32;
        let draw_y = origin[0].y.round() as i32;

        ctx.canvas.draw_pixmap(draw_x, draw_y, ti.pixmap.as_ref(),
            &tiny_skia::PixmapPaint::default(),
            tiny_skia::Transform::identity(),
            None)?;

        ti.last_used = Instant::now();

        ctx.render_pos.x += ti.width;
        ctx.render_pos.y += ti.height;
        Some(())
    }).is_some() {
        return;
    }

    let (mut to_draw, (width, height)) = layout_font(ctx.font, ctx.font_size, &ctx.runtime, ctx.font_color, &text, markup);

    if width > clip_w {
        to_draw.retain(|glyph| glyph.position.0 < clip_w);
        key = None;
    }

    ctx.render_pos.x += width;
    ctx.render_pos.y += height;

    if to_draw.is_empty() {
        return;
    }

    if !ctx.render_flex {
        match ctx.align.vert {
            Some(f) => {
                let extra = clip_h - height;
                if extra >= 0.0 {
                    xform = xform.pre_translate(0.0, extra * f);
                    ctx.render_pos.y += extra * f;
                }
            }
            _ => {}
        }
    }
    let x_offset_centipixel = ((xform.tx * 100.0).round() as u64 % 100) as u8;
    let y_offset_centipixel = ((xform.ty * 100.0).round() as u64 % 100) as u8;

    let mut bounding = [
        Point::zero(),
        Point {
            x: width + (x_offset_centipixel as f32 / 100.0),
            y: height + (y_offset_centipixel as f32 / 100.0),
        },
        Point {
            x: -(x_offset_centipixel as f32 / 100.0),
            y: -(y_offset_centipixel as f32 / 100.0),
        },
    ];

    xform.map_points(&mut bounding);
    let xsize = bounding[1].x - bounding[0].x;
    let ysize = bounding[1].y - bounding[0].y;
    let draw_x = bounding[2].x.round() as i32;
    let draw_y = bounding[2].y.round() as i32;

    let render_xform = tiny_skia::Transform {
        sx: xform.sx,
        sy: xform.sy,
        tx: bounding[2].x - draw_x as f32,
        ty: bounding[2].y - draw_y as f32,
        ..tiny_skia::Transform::identity()
    };

    let mut pixmap = match tiny_skia::Pixmap::new(xsize.ceil() as u32, ysize.ceil() as u32) {
        Some(pixmap) => pixmap,
        None => { log::debug!("Not rendering \"{text}\" ({xsize}, {ysize})"); return }
    };
    let mut canvas = pixmap.as_mut();

    if let Some(rgba) = ctx.text_stroke {
        let stroke_paint = tiny_skia::Paint {
            shader: tiny_skia::Shader::SolidColor(rgba),
            anti_alias: true,
            ..tiny_skia::Paint::default()
        };
        let stroke = tiny_skia::Stroke {
            width : ctx.text_stroke_size.unwrap_or(1.0),
            ..Default::default()
        };

        draw_font_with(&mut canvas, render_xform, &to_draw, |canvas, path, color| {
            canvas.stroke_path(&path, &stroke_paint, &stroke, Transform::identity(), None);
            let paint = tiny_skia::Paint {
                shader: tiny_skia::Shader::SolidColor(color),
                anti_alias: true,
                ..tiny_skia::Paint::default()
            };
            canvas.fill_path(&path, &paint, tiny_skia::FillRule::EvenOdd, Transform::identity(), None);
        });
    } else {
        draw_font_with(&mut canvas, render_xform, &to_draw, |canvas, path, color| {
            let paint = tiny_skia::Paint {
                shader: tiny_skia::Shader::SolidColor(color),
                anti_alias: true,
                ..tiny_skia::Paint::default()
            };
            canvas.fill_path(&path, &paint, tiny_skia::FillRule::EvenOdd, Transform::identity(), None);
        });
    }

    ctx.canvas.draw_pixmap(draw_x, draw_y, canvas.as_ref(),
        &tiny_skia::PixmapPaint::default(),
        tiny_skia::Transform::identity(),
        None);

    if let Some(key) = key {
        ctx.cache.text.borrow_mut().insert(key, TextImage {
            width,
            height,
            y_offset_centipixel,
            pixmap,
            last_used: Instant::now(),
        });
    }
}
