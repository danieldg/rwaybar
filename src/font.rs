use crate::icon::OwnedImage;
use crate::item::Formatting;
use crate::state::Runtime;
use log::info;
use raqote::{DrawTarget,Point};
use std::fs::File;
use std::io;
use std::path::PathBuf;
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
        Ok(FontMapped { parsed, mmap, file : path, name })
    }

    pub fn as_ref<'a>(&'a self) -> &'a Face<'a> {
        &self.parsed
    }

    pub fn scale_from_pt(&self, pt : f32) -> f32 {
        pt * 1.33333333 / self.as_ref().units_per_em().unwrap_or(1) as f32
    }
}

#[derive(Debug,Copy,Clone)]
pub struct CGlyph<'a> {
    pub id: GlyphId,
    pub scale : f32,
    pub position : Point,
    pub font : &'a FontMapped,
    pub color : (u16, u16, u16, u16),
}

pub fn layout_font<'a>(
    font: &'a FontMapped,
    size_pt: f32,
    runtime : &'a Runtime,
    rgba : (u16, u16, u16, u16),
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
                if let Some(prev) = prev {
                    xpos += fid.as_ref()
                        .kerning_subtables()
                        .filter(|st| st.is_horizontal() && !st.is_variable())
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
            let position = Point::new(xpos, ypos);
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

pub fn draw_font_with(target : &mut DrawTarget<&mut [u32]>, start : (f32, f32), to_draw : &[CGlyph], mut draw : impl FnMut(&mut DrawTarget<&mut [u32]>, raqote::Path, (u16, u16, u16, u16))) {
    for &CGlyph { id, scale, position, font, color } in to_draw {
        struct Draw(raqote::Path);
        let mut path = Draw(raqote::Path {
            ops : Vec::new(),
            winding: raqote::Winding::EvenOdd,
        });
        impl ttf_parser::OutlineBuilder for Draw {
            fn move_to(&mut self, x: f32, y: f32) {
                self.0.ops.push(raqote::PathOp::MoveTo(Point::new(x,-y)));
            }
            fn line_to(&mut self, x: f32, y: f32) {
                self.0.ops.push(raqote::PathOp::LineTo(Point::new(x,-y)));
            }
            fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
                self.0.ops.push(raqote::PathOp::QuadTo(Point::new(x1, -y1), Point::new(x,-y)));
            }
            fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
                self.0.ops.push(raqote::PathOp::CubicTo(Point::new(x1, -y1), Point::new(x2, -y2), Point::new(x,-y)));
            }
            fn close(&mut self) {
                self.0.ops.push(raqote::PathOp::Close);
            }
        }
        if let Some(_bounds) = font.as_ref().outline_glyph(id, &mut path) {
            let xform = raqote::Transform::scale(scale, scale);
            let xform = xform.then_translate(position - Point::origin());
            let xform = xform.then_translate(raqote::Vector::new(start.0, start.1));
            let path = path.0.transform(&xform);
            draw(target, path, color);
            continue;
        }
        let target_ppem = scale * font.as_ref().units_per_em().unwrap_or(1) as f32;
        let target_h = scale * font.as_ref().height() as f32;
        if let Some(raster_img) = font.as_ref().glyph_raster_image(id, target_ppem as u16) {
            // This is a PNG glyph (color emoji); read it into a pixbuf and draw like an icon
            if let Some(img) = OwnedImage::from_data(raster_img.data, target_h as u32) {
                let ypos = position.y - font.as_ref().ascender() as f32 * scale;
                let png_scale = target_ppem / raster_img.pixels_per_em as f32;
                target.draw_image_with_size_at(
                    img.width as f32 * png_scale,
                    img.height as f32 * png_scale,
                    raster_img.x as f32 * png_scale + position.x + start.0,
                    raster_img.y as f32 * png_scale + ypos + start.1,
                    &img.as_ref(),
                    &Default::default());
                continue;
            }
        }
        if let Some(svg) = font.as_ref().glyph_svg_image(id) {
            if let Some(img) = OwnedImage::from_svg(svg, target_h as u32) {
                let ypos = position.y - font.as_ref().ascender() as f32 * scale;
                target.draw_image_with_size_at(
                    img.width as f32,
                    img.height as f32,
                    position.x + start.0,
                    ypos + start.1,
                    &img.as_ref(),
                    &Default::default());
                continue;
            }
        }
    }
}

pub fn render_font(target : &mut DrawTarget<&mut [u32]>,
    font: &FontMapped,
    scale : f32,
    rgba : (u16, u16, u16, u16),
    runtime: &Runtime,
    start: (f32, f32),
    text: &str,
    markup: bool)
    -> (f32, f32)
{
    let (to_draw, size) = layout_font(font, scale, runtime, rgba, text, markup);
    let opts = raqote::DrawOptions::default();
    draw_font_with(target, start, &to_draw, |canvas,path,color| {
        let src = raqote::Color::new(
            (color.3 / 256) as u8,
            (color.0 / 256) as u8,
            (color.1 / 256) as u8,
            (color.2 / 256) as u8,
        ).into();
        canvas.fill(&path, &src, &opts);
    });
    size
}
