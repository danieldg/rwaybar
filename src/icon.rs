use crate::render::Render;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self, File};
use std::io;
use std::path::{Component, PathBuf};
use tiny_skia::Transform;

thread_local! {
    static CACHE : RefCell<HashMap<(String, u32), Option<OwnedImage>>> = Default::default();
}

#[derive(Debug)]
pub struct OwnedImage(pub tiny_skia::Pixmap);

impl OwnedImage {
    pub fn as_ref(&self) -> tiny_skia::PixmapRef {
        self.0.as_ref()
    }

    pub fn from_file<R: io::Read>(mut file: R, tsize: u32, rescale: bool) -> Option<Self> {
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).ok()?;
        Self::from_data(&buf, tsize, rescale)
    }

    pub fn from_data(buf: &[u8], tsize: u32, rescale: bool) -> Option<Self> {
        Self::from_png(buf)
            .map(|img| {
                if rescale {
                    img.rescale_height(tsize)
                } else {
                    img
                }
            })
            .or_else(|| Self::from_svg(buf, tsize))
    }

    pub fn from_png(data: &[u8]) -> Option<Self> {
        let mut png = png::Decoder::new(std::io::Cursor::new(data));
        png.set_transformations(png::Transformations::EXPAND | png::Transformations::STRIP_16);
        let mut png = png.read_info().ok()?;
        let color = png.output_color_type().0;
        let mut image = vec![0; png.output_buffer_size()];
        png.next_frame(&mut image).ok()?;

        let info = png.info();
        let mut pixmap = tiny_skia::Pixmap::new(info.width as u32, info.height as u32)?;
        let step = match color {
            png::ColorType::Grayscale => 1,
            png::ColorType::GrayscaleAlpha => 2,
            png::ColorType::Rgb => 3,
            png::ColorType::Rgba => 4,
            _ => unreachable!(),
        };
        for (src, pixel) in image.chunks(step).zip(pixmap.pixels_mut()) {
            let c = match src.len() {
                1 => tiny_skia::ColorU8::from_rgba(src[0], src[0], src[0], 255),
                2 => tiny_skia::ColorU8::from_rgba(src[0], src[0], src[0], src[1]),
                3 => tiny_skia::ColorU8::from_rgba(src[0], src[1], src[2], 255),
                4 => tiny_skia::ColorU8::from_rgba(src[0], src[1], src[2], src[3]),
                _ => break,
            };
            *pixel = c.premultiply();
        }
        Some(Self(pixmap))
    }

    pub fn rescale_height(self, height: u32) -> Self {
        if self.0.height() == height {
            return self;
        }
        let scale = height as f32 / self.0.height() as f32;
        let xform = Transform::from_scale(scale, scale);
        let px_width = (self.0.width() as f32 * scale).ceil() as u32;
        let mut pixmap = tiny_skia::Pixmap::new(px_width, height).unwrap();

        pixmap.draw_pixmap(
            0,
            0,
            self.as_ref(),
            &tiny_skia::PixmapPaint {
                opacity: 1.0,
                blend_mode: tiny_skia::BlendMode::Source,
                quality: tiny_skia::FilterQuality::Bicubic,
            },
            xform,
            None,
        );

        Self(pixmap)
    }

    pub fn from_svg(data: &[u8], height: u32) -> Option<Self> {
        let tree = usvg::Tree::from_data(data, &usvg::Options::default().to_ref()).ok()?;
        let svg_width = tree.svg_node().size.width();
        let svg_height = tree.svg_node().size.height();
        let width = (height as f64 * svg_width / svg_height).ceil() as u32;
        let mut pixmap = tiny_skia::Pixmap::new(width, height)?;
        resvg::render(
            &tree,
            usvg::FitTo::Height(height),
            tiny_skia::Transform::identity(),
            pixmap.as_mut(),
        )?;
        Some(Self(pixmap))
    }
}

fn open_icon(xdg: &xdg::BaseDirectories, name: &str, target_size: u32) -> io::Result<PathBuf> {
    if name.contains('/') {
        return Ok(PathBuf::from(name.to_owned()));
    }

    // return paths in order from highest to lowest priority, unlike how the xdg crate does it
    // (sadly that crate doesn't support DoubleEndedIterator yet)
    let find_data = |path: &str| {
        let dirs: Vec<_> = xdg.find_data_files(path).collect();
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

fn iter_icons<F, R>(base: &PathBuf, target_size: u32, mut f: F) -> io::Result<Option<R>>
where
    F: FnMut(PathBuf) -> Option<R>,
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
                        if target_size == size {
                            cur_rank = 5;
                        } else if target_size > size {
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

    for (_, _, size_dir) in sorted_dirs.into_iter().rev() {
        for theme_item in fs::read_dir(size_dir)? {
            let path = theme_item?.path();
            if let v @ Some(_) = f(path) {
                return Ok(v);
            }
        }
    }
    Ok(None)
}

pub fn render(ctx: &mut Render, name: &str) -> Result<(), ()> {
    let xform = ctx.render_xform;
    let mut extent_points = [ctx.render_pos, ctx.render_extents.1];
    xform.map_points(&mut extent_points);
    let xsize = extent_points[1].x - extent_points[0].x;
    let ysize = extent_points[1].y - extent_points[0].y;
    let tsize = ysize as u32;
    if f32::min(xsize, ysize) < 1.0 {
        return Err(());
    }

    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        match cache.entry((name.into(), tsize)).or_insert_with(|| {
            open_icon(&ctx.runtime.xdg, name, tsize)
                .ok()
                .and_then(|mut path| {
                    match File::open(&path) {
                        Ok(file) => return Some(file),
                        _ => {}
                    }
                    path.set_extension("png");
                    match File::open(&path) {
                        Ok(file) => return Some(file),
                        _ => {}
                    }
                    path.set_extension("svg");
                    match File::open(&path) {
                        Ok(file) => return Some(file),
                        _ => {}
                    }
                    None
                })
                .and_then(|file| OwnedImage::from_file(file, tsize, true))
        }) {
            Some(img) => {
                ctx.canvas.draw_pixmap(
                    extent_points[0].x.round() as i32,
                    extent_points[0].y.round() as i32,
                    img.as_ref(),
                    &Default::default(),
                    Transform::identity(),
                    None,
                );
                // convert the sizes back to virtual pixels (inverse xform)
                ctx.render_pos.x += img.0.width() as f32 / xform.sx;
                ctx.render_pos.y += img.0.height() as f32 / xform.sy;
                Ok(())
            }
            None => Err(()),
        }
    })
}
