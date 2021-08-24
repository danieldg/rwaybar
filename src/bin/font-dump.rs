use ttf_parser::Face;
use raqote::{DrawTarget,Point};
use std::io::Read;
use std::fs::File;
use std::io::BufWriter;

fn main() {
    let filename = std::env::args_os().nth(1).expect("Use <font filename> [<width>] [<size>]");
    let width = std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(80usize);
    let pt_size = std::env::args().nth(3).and_then(|s| s.parse().ok()).unwrap_or(20.0f32);
    let mut font = std::fs::File::open(&filename).expect("Font does not exist");
    let mut bits = Vec::new();
    font.read_to_end(&mut bits).expect("read");
    drop(font);
    let font = Face::from_slice(&bits, 0).expect("parse");
    let mut glyphs : Vec<_> = font.character_mapping_subtables()
        .filter(|t| dbg!(t.is_unicode()))
        .flat_map(|t| {
            let mut cps = Vec::new();
            t.codepoints(|c| {
                if let Some(id) = t.glyph_index(c) {
                    cps.push((id, c));
                }
            });
            cps
        }).collect();
    glyphs.sort_by_key(|&(_, c)| c);
    glyphs.dedup();

    let lines = glyphs.len() / width;
    let scale = pt_size / font.units_per_em().unwrap_or(1) as f32;
    let height = ((font.ascender() - font.descender() + font.line_gap()) as f32 * scale) as i32;
    let mut canvas = DrawTarget::new((width as i32 + 1) * pt_size as i32, (2 + lines) as i32 * height);
    canvas.clear(raqote::Color::new(255, 255, 255, 255).into());

    println!("Font has {} glyphs, height {}:", font.number_of_glyphs(), height);
    let h0 = font.ascender() as f32 * scale;
    let target_ppem = scale * font.units_per_em().unwrap_or(1) as f32;

    for (i, &(id,c)) in glyphs.iter().enumerate() {
        let xg = i % width;
        let yg = i / width;
        if xg == 0 && i != 0 {
            println!();
        }
        if c as u32 >= 0x2001 && (c as u32) < 0x2004 {
            continue;
        }
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
        if let Some(_bounds) = font.outline_glyph(id, &mut path) {
            let xform = raqote::Transform::scale(scale, scale);
            let xform = xform.then_translate(raqote::Vector::new(xg as f32 * pt_size, yg as f32 * height as f32 + h0));
            let path = path.0.transform(&xform);
            let opts = raqote::DrawOptions::default();
            let src = raqote::Color::new(255, 0, 0, 0).into();
            canvas.fill(&path, &src, &opts);
            print!(" {}", char::from_u32(c).unwrap_or('?'));
        } else if let Some(raster_img) = font.glyph_raster_image(id, target_ppem as u16) {
            let ypos = - font.ascender() as f32 * scale;
            let mut png = png::Decoder::new(std::io::Cursor::new(raster_img.data));
            png.set_transformations(png::Transformations::EXPAND | png::Transformations::STRIP_16);

            let mut png = png.read_info().unwrap();
            let mut image = vec![0; png.output_buffer_size()];
            let (color, _depth_is_8) = png.output_color_type();
            png.next_frame(&mut image).unwrap();

            let info = png.info();
            let mut tmp_canvas = DrawTarget::new(info.width as i32, info.height as i32);
            match color {
                png::ColorType::Rgba => {
                    tmp_canvas.get_data_u8_mut().copy_from_slice(&image);
                    for pixel in tmp_canvas.get_data_mut() {
                        let [r,g,b,a] = pixel.to_le_bytes();
                        let m = |v| ((v as u32) * (a as u32) / 255) as u8;
                        *pixel = u32::from_be_bytes([a,m(r), m(g), m(b)]);
                    }
                }
                _ => todo!(),
            }
            let tmp_data = tmp_canvas.into_vec();
            let img = raqote::Image {
                width : info.width as i32,
                height : info.height as i32,
                data : &tmp_data,
            };

            let png_scale = target_ppem / raster_img.pixels_per_em as f32;
            canvas.draw_image_with_size_at(
                img.width as f32 * png_scale,
                img.height as f32 * png_scale,
                raster_img.x as f32 * png_scale +  xg as f32 * pt_size,
                raster_img.y as f32 * png_scale + ypos + yg as f32 * height as f32 + h0,
                &img,
                &Default::default());
            print!(" {}", char::from_u32(c).unwrap_or('?'));
        } else {
            print!("-{}", c);
        }
    }
    println!();

    let w = BufWriter::new(File::create("out.png").unwrap());
    let mut encoder = png::Encoder::new(w, canvas.width() as u32, canvas.height() as u32);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header().unwrap();
    for pixel in canvas.get_data_mut() {
        let [a,r,g,b] = pixel.to_be_bytes();
        let m = |v| ((v as u32) * 255).checked_div(a as u32).unwrap_or(0) as u8;
        *pixel = u32::from_le_bytes([m(r), m(g), m(b), a]);
    }
    writer.write_image_data(canvas.get_data_u8()).unwrap()
}
