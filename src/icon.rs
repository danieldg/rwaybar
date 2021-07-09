use raqote::DrawTarget;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self,File};
use std::io;
use std::path::{PathBuf,Component};
use crate::item::Render;

thread_local! {
    static CACHE : RefCell<HashMap<(String, u32), Option<OwnedImage>>> = Default::default();
}

#[derive(Debug)]
pub struct OwnedImage {
    pub width : i32,
    pub height : i32,
    pub buf : Vec<u32>,
}

impl OwnedImage {
    pub fn as_ref(&self) -> raqote::Image {
        raqote::Image {
            width: self.width,
            height: self.height,
            data: &self.buf,
        }
    }

    pub fn from_canvas(canvas : DrawTarget) -> Self {
        let width = canvas.width();
        let height = canvas.height();
        Self { width, height, buf: canvas.into_vec() }
    }

    pub fn from_file<R : io::Read>(mut file : R, tsize : u32) -> Option<Self> {
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).ok()?;
        Self::from_data(&buf, tsize)
    }

    pub fn from_data(buf : &[u8], tsize : u32) -> Option<Self> {
        Self::from_png(buf).ok()
            .or_else(|| Self::from_svg(buf, tsize))
    }

    pub fn from_png(data : &[u8]) -> Result<Self, png::DecodingError> {
        let mut png = png::Decoder::new(std::io::Cursor::new(data));
        png.set_transformations(png::Transformations::EXPAND | png::Transformations::STRIP_16);
        let (info, mut png) = png.read_info()?;
        let mut image = vec![0; png.output_buffer_size()];
        let (color, _depth_is_8) = png.output_color_type();
        png.next_frame(&mut image)?;

        let mut tmp_canvas = DrawTarget::new(info.width as i32, info.height as i32);
        let step = match color {
            png::ColorType::Grayscale => 1,
            png::ColorType::GrayscaleAlpha => 2,
            png::ColorType::RGB => 3,
            png::ColorType::RGBA => 4,
            _ => unreachable!(),
        };
        if step == 4 {
            tmp_canvas.get_data_u8_mut().copy_from_slice(&image);
            for pixel in tmp_canvas.get_data_mut() {
                let [r,g,b,a] = pixel.to_le_bytes();
                let m = |v| ((v as u32) * (a as u32) / 255) as u8;
                *pixel = u32::from_be_bytes([a,m(r), m(g), m(b)]);
            }
        } else {
            for (src, pixel) in image.chunks(step).zip(tmp_canvas.get_data_mut()) {
                let argb = match src.len() {
                    1 => [255, src[0], src[0], src[0]],
                    2 => {
                        let a = src[1];
                        let g = ((src[0] as u32) * (a as u32) / 255) as u8;
                        [a, g,g,g]
                    }
                    3 => [255, src[0], src[1], src[2]],
                    _ => break,
                };
                *pixel = u32::from_be_bytes(argb);
            }
        }
        Ok(Self::from_canvas(tmp_canvas))
    }

    pub fn from_svg(data : &[u8], height : u32) -> Option<Self> {
        let tree = usvg::Tree::from_data(data, &usvg::Options::default()).ok()?;
        let svg_width = tree.svg_node().size.width();
        let svg_height = tree.svg_node().size.height();
        let width = (height as f64 * svg_width / svg_height).ceil() as u32;
        let mut pixmap = tiny_skia::Pixmap::new(width, height)?;
        resvg::render(&tree, usvg::FitTo::Height(height), pixmap.as_mut())?;
        let buf = pixmap.pixels().iter().map(|pixel| {
            u32::from_be_bytes([pixel.alpha(), pixel.red(), pixel.green(), pixel.blue()])
        }).collect();
        Some(Self {
            width: width as i32,
            height: height as i32,
            buf
        })
    }
}

fn open_icon(xdg : &xdg::BaseDirectories, name : &str, target_size : f32) -> io::Result<PathBuf> {
    if name.contains('/') {
        return Ok(PathBuf::from(name.to_owned()));
    }

    // return paths in order from highest to lowest priority, unlike how the xdg crate does it
    // (sadly that crate doesn't support DoubleEndedIterator yet)
    let find_data = |path : &str| {
        let dirs : Vec<_> = xdg.find_data_files(path).collect();
        dirs.into_iter().rev()
    };

    let f = |mut path: PathBuf| {
        path.push(name);
        path.set_extension("svg");
        match File::open(&path) {
            Ok(_) => {
                return Some(path);
            }
            Err(_) => {}
        }
        path.set_extension("png");
        match File::open(&path) {
            Ok(_) => {
                return Some(path);
            }
            Err(_) => {}
        }
        None
    };

    for path in find_data("pixmaps") {
        match f(path) {
            Some(rv) => return Ok(rv),
            None => {}
        }
    }

    // TODO take a theme (instead of "hicolor") as an argument
    for path in find_data("icons/hicolor") {
        match iter_icons(&path, target_size, f)? {
            Some(rv) => return Ok(rv),
            None => {}
        }
    }
    Err(io::ErrorKind::NotFound.into())
}

fn iter_icons<F,R>(base : &PathBuf, target_size : f32, mut f : F) -> io::Result<Option<R>>
    where F : FnMut(PathBuf) -> Option<R>
{
    let mut sorted_dirs = Vec::new();

    for size_dir in fs::read_dir(base)? {
        let cur_rank;
        let mut cur_size = 0;
        let size_dir = size_dir?;
        if !size_dir.file_type()?.is_dir() {
            continue;
        }
        let size_dir = size_dir.path();
        if let Some(Component::Normal(s)) = size_dir.components().last() {
            match s.to_str() {
                Some("scalable") => {
                    cur_rank = 4;
                }
                Some(s) => {
                    if let Some(size) = s.find('x').and_then(|p| s[..p].parse::<u32>().ok()) {
                        cur_size = size;
                        if target_size == size as f32 {
                            cur_rank = 5;
                        } else if target_size > size as f32 {
                            cur_rank = 2;
                        } else {
                            cur_rank = 1;
                        }
                    } else {
                        cur_rank = 3;
                    }
                }
                None => continue,
            }
        } else {
            continue;
        }
        sorted_dirs.push((cur_rank, cur_size, size_dir));
    }
    sorted_dirs.sort_unstable();

    for (_,_,size_dir) in sorted_dirs.into_iter().rev() {
        for theme_item in fs::read_dir(size_dir)? {
            let path = theme_item?.path();
            if let v @ Some(_) = f(path) {
                return Ok(v);
            }
        }
    }
    Ok(None)
}

pub fn render(ctx : &mut Render, name : &str) -> Result<(), ()> {
    let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.render_extents;
    let xform = ctx.canvas.get_transform();
    let raqote::Vector { x: xsize, y: ysize, .. } =
        xform.transform_vector(raqote::Vector::new(clip_x1 - clip_x0, clip_y1 - clip_y0)); 
    let pixel_size = f32::min(xsize, ysize);
    if pixel_size < 1.0 {
        return Err(());
    }

    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let tsize = pixel_size as u32;
        match cache.entry((name.into(), tsize)).or_insert_with(|| {
                open_icon(&ctx.runtime.xdg, name, pixel_size).ok()
                .and_then(|mut path| {
                    match File::open(&path) { Ok(file) => return Some(file), _ => {} }
                    path.set_extension("png");
                    match File::open(&path) { Ok(file) => return Some(file), _ => {} }
                    path.set_extension("svg");
                    match File::open(&path) { Ok(file) => return Some(file), _ => {} }
                    None
                })
                .and_then(|file| OwnedImage::from_file(file, tsize))
            })
        {
            Some(img) => {
                let xscale = (clip_x1 - clip_x0) / img.width as f32;
                let yscale = (clip_y1 - clip_y0) / img.height as f32;
                let scale = f32::min(xscale, yscale);
                ctx.canvas.draw_image_with_size_at(
                    img.width as f32 * scale,
                    img.height as f32 * scale,
                    ctx.render_pos,
                    clip_y0,
                    &img.as_ref(),
                    &Default::default());
                ctx.render_pos += img.width as f32 * scale;
                Ok(())
            }
            None => Err(()),
        }
    })
}
