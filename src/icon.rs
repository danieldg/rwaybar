use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self,File};
use std::io;
use std::path::{PathBuf,Component};
use crate::item::Render;

thread_local! {
    static CACHE : RefCell<HashMap<(String, i32), Option<CairoStylePixbuf>>> = Default::default();
}

#[derive(Debug)]
struct CairoStylePixbuf {
    w : i32,
    h : i32,
    s : i32,
    buf : Box<[u8]>,
}

impl CairoStylePixbuf {
    fn parse(pixbuf : gdk_pixbuf::Pixbuf) -> Option<Self> {
        if pixbuf.get_n_channels() != 4 {
            return None;
        }
        let w = pixbuf.get_width() as i32;
        let h = pixbuf.get_height() as i32;
        let src_s = pixbuf.get_rowstride() as i32;
        let s = cairo::Format::ARgb32.stride_for_width(w as u32).ok()?;
        let mut buf = vec![0;(h*s) as usize].into_boxed_slice();

        let pixels = unsafe { &*pixbuf.get_pixels() }; // convert to non-mut and it's safe
        let idx_map = u32::to_ne_bytes(u32::from_be_bytes([0,1,2,3]));
        for r in 0..h {
            let src_i = r * src_s;
            let dst_i = r * s;
            for c in 0..w {
                let src_i = (src_i + c * 4) as usize;
                let dst_i = (dst_i + c * 4) as usize;
                let alpha = pixels[src_i + 3] as u32;
                for k in 0..3 {
                    let pix = pixels[src_i + k] as u32;
                    let pix = ((pix * alpha) / 255) as u8;
                    buf[dst_i + idx_map[k + 1] as usize] = pix;
                }
                buf[src_i + idx_map[0] as usize] = alpha as u8;
            }
        }

        Some(CairoStylePixbuf { w, h, s, buf })
    }
}

fn open_icon(name : &str, target_size : f64) -> io::Result<PathBuf> {
    if name.contains('/') {
        return Ok(PathBuf::from(name.to_owned()));
    }

    let base = "/usr/share/icons"; // TODO other paths?
    let theme = "hicolor"; // TODO configurable as list

    let mut sorted_dirs = Vec::new();
    for size_dir in fs::read_dir(format!("{}/{}", base, theme))? {
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
                        if target_size == size as f64 {
                            cur_rank = 5;
                        } else if target_size > size as f64 {
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
            let mut path = theme_item?.path();
            path.push(name);
            path.set_extension("svg");
            match File::open(&path) {
                Ok(_) => {
                    return Ok(path);
                }
                Err(_) => {}
            }
            path.set_extension("png");
            match File::open(&path) {
                Ok(_) => {
                    return Ok(path);
                }
                Err(_) => {}
            }
        }
    }
    Err(io::ErrorKind::NotFound.into())
}

pub fn render(ctx : &Render, name : &str) -> Result<(), ()> {
    let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.cairo.clip_extents();
    let m = ctx.cairo.get_matrix();
    let (xsize, ysize) = m.transform_distance(clip_y1 - clip_y0, clip_x1 - clip_x0);
    let logic_size = f64::min(clip_y1 - clip_y0, clip_x1 - clip_x0);
    let pixel_size = f64::min(xsize, ysize);

    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let tsize = pixel_size as i32;
        match cache.entry((name.into(), tsize)).or_insert_with(|| {
                open_icon(name, pixel_size).ok()
                .and_then(|path| gdk_pixbuf::Pixbuf::from_file_at_size(path, tsize, tsize).ok())
                .and_then(|pixbuf| pixbuf.add_alpha(false, 0,0,0))
                .and_then(CairoStylePixbuf::parse)
            })
        {
            &mut Some(CairoStylePixbuf { w, h, s, ref mut buf }) => {
                let pixels = unsafe { &mut *(&mut **buf as *mut [u8]) }; // could just switch to Box::leak really
                let surf = match cairo::ImageSurface::create_for_data(pixels, cairo::Format::ARgb32, w, h, s) {
                    Ok(i) => i,
                    Err(_) => Err(())?,
                };
                let pattern = cairo::SurfacePattern::create(&surf);
                let point = ctx.cairo.get_current_point();
                let mut m = ctx.cairo.get_matrix();
                if w != tsize && h != tsize {
                    m.scale((w as f64 - 0.5) / pixel_size, (h as f64 - 0.5) / pixel_size);
                }
                m.translate(-point.0, 0.0);
                pattern.set_matrix(m);
                ctx.cairo.save();
                ctx.cairo.set_source(&pattern);
                ctx.cairo.paint();
                ctx.cairo.restore();
                ctx.cairo.rel_move_to(logic_size, 0.0);
                Ok(())
            }
            None => Err(()),
        }
    })
}
