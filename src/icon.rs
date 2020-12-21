use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self,File};
use std::io;
use std::path::{PathBuf,Component};
use crate::item::Render;

thread_local! {
    static CACHE : RefCell<HashMap<String, Option<gdk_pixbuf::Pixbuf>>> = Default::default();
}

fn open_icon(name : &str, target_size : f64) -> io::Result<PathBuf> {
    if name.contains('/') {
        return Ok(PathBuf::from(name.to_owned()));
    }

    let base = "/usr/share/icons";
    let theme = "hicolor";

    let mut best = None;
    let mut best_size = 0.0;
    let mut best_rank = 0;

    let size_dirs : Vec<_> = fs::read_dir(format!("{}/{}", base, theme))?.collect();
    // TODO sort here, not later

    'sizes: for size_dir in size_dirs {
        let cur_rank;
        let mut cur_size = 0.0;
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
                    if let Some(size) = s.find('x').and_then(|p| s[..p].parse::<f64>().ok()) {
                        cur_size = size;
                        if size == target_size {
                            cur_rank = 5;
                        } else if size < target_size {
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
        if best_rank > cur_rank {
            continue;
        }
        if best_rank == cur_rank && best_size > cur_size {
            continue;
        }
        for theme_item in fs::read_dir(size_dir)? {
            let mut path = theme_item?.path();
            path.push(name);
            path.set_extension("svg");
            match File::open(&path) {
                Ok(_) => {
                    best = Some(path);
                    best_rank = cur_rank;
                    best_size = cur_size;
                    continue 'sizes;
                }
                Err(_) => {}
            }
            path.set_extension("png");
            match File::open(&path) {
                Ok(_) => {
                    best = Some(path);
                    best_rank = cur_rank;
                    best_size = cur_size;
                    continue 'sizes;
                }
                Err(_) => {}
            }
        }
    }
    best.ok_or(io::ErrorKind::NotFound.into())
}

pub fn render(ctx : &Render, name : &str) -> Result<(), ()> {
    let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.cairo.clip_extents();
    let size = f64::min(clip_y1 - clip_y0, clip_x1 - clip_x0);

    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let pixbuf = match cache.entry(name.into()).or_insert_with(|| {
                open_icon(name, size).ok()
                .and_then(|path| gdk_pixbuf::Pixbuf::from_file(path).ok())
                .and_then(|pixbuf| pixbuf.add_alpha(false, 0,0,0))
            })
        {
            Some(pixbuf) => pixbuf,
            None => Err(())?,
        };
        let format = match pixbuf.get_n_channels() {
            4 => cairo::Format::ARgb32,
            _ => Err(())?,
        };
        let w = pixbuf.get_width();
        let h = pixbuf.get_height();
        let s = pixbuf.get_rowstride();
        let pixels = unsafe { &mut *(pixbuf.get_pixels() as *mut [u8]) };
        let surf = match cairo::ImageSurface::create_for_data(pixels, format, w, h, s) {
            Ok(i) => i,
            Err(_) => Err(())?,
        };
        let pattern = cairo::SurfacePattern::create(&surf);
        let point = ctx.cairo.get_current_point();
        let mut m = cairo::Matrix::identity();
        m.scale((w as f64) / size, (h as f64) / size);
        m.translate(-point.0, 0.0);
        pattern.set_matrix(m);
        ctx.cairo.save();
        ctx.cairo.set_source(&pattern);
        ctx.cairo.paint();
        ctx.cairo.restore();
        ctx.cairo.rel_move_to(size, 0.0);
        Ok(())
    })
}
