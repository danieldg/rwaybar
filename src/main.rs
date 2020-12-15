use smithay_client_toolkit::environment::{Environment,SimpleGlobal};
use smithay_client_toolkit::output::OutputInfo;
use smithay_client_toolkit::shm::MemPool;
use smithay_client_toolkit::{new_default_environment,default_environment};
use std::collections::HashMap;
use std::error::Error;
use wayland_client::Attached;
use wayland_client::protocol::wl_output::WlOutput;
use wayland_client::protocol::wl_surface::WlSurface;
use wayland_protocols::wlr::unstable::layer_shell::v1::client as layer_shell;

use layer_shell::zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer};
use layer_shell::zwlr_layer_surface_v1::Anchor;
use layer_shell::zwlr_layer_surface_v1::Event as LayerSurfaceEvent;

default_environment!(MyEnv,
    fields = [
        layer_shell : SimpleGlobal<ZwlrLayerShellV1>,
    ],
    singles = [
        ZwlrLayerShellV1 => layer_shell,
    ]
);

/// XXX hack for working around pango bugs
struct Nop;
impl<T> glib::boxed::BoxedMemoryManager<T> for Nop {
    unsafe fn copy(ptr: *const T) -> *mut T { ptr as _ }
    unsafe fn free(_: *mut T) {}
    unsafe fn init(_: *mut T) {}
    unsafe fn clear(_: *mut T) {}
}

enum Item {
    Text {
        text : String,
        markup : bool
    },
    Group {
        item : Box<Item>,
        bg_rgba : (f64, f64, f64, f64),
        fg_alpha : f64,
    },
    List(Vec<Item>),
}

impl Item {
    fn render(&self, ctx : &Render) {
        match self {
            Item::Text { text, markup : false } => {
                let text = strfmt::strfmt(&text, ctx.data).unwrap();
                let attrs = pango::AttrList::new();
                for item in pango::itemize(&ctx.pango, &text, 0, text.len() as i32, &attrs, None) {
                    let mut glyphs = pango::GlyphString::new();
                    let pa = item.analysis();
                    let ps = &text[item.offset() as usize .. (item.offset() + item.length()) as usize];
                    pango::shape(&ps, &pa, &mut glyphs);
                    pangocairo::show_glyph_string(&ctx.cairo, &pa.font(), &mut glyphs);
                    let x = (glyphs.get_width() as f64) / 1000.0;
                    ctx.cairo.rel_move_to(x, 0.0);
                }
            }
            Item::Text { text, markup : true } => {
                let text = strfmt::strfmt(&text, ctx.data).unwrap();
                let (attrs, s, _) = pango::parse_markup(&text, '\x01').unwrap();
                let mut i = attrs.get_iterator().unwrap();

                for item in pango::itemize(&ctx.pango, &s, 0, s.len() as i32, &attrs, None) {
                    ctx.cairo.save();
                    let mut glyphs = pango::GlyphString::new();
                    let pa = item.analysis();
                    let ps = &s[item.offset() as usize .. (item.offset() + item.length()) as usize];
                    // XXX this avoids a double-free bug in pango
                    //
                    // pango_attr_iterator_get is "transfer none" but the rust get() uses from_glib_full
                    // https://developer.gnome.org/pango/unstable/pango-Text-Attributes.html#pango-attr-iterator-get
                    //
                    // Downcast the value properly while we're at it.  This needs unsafe/transmute anyway.
                    // https://developer.gnome.org/pango/unstable/pango-Text-Attributes.html#PangoAttrType
                    let c : Option<glib::boxed::Boxed<pango_sys::PangoAttrColor, Nop>> = i.get(pango::AttrType::Foreground).map(|a| unsafe { std::mem::transmute(a) });
                    if let Some(c) = c.map(|e| e.color) {
                        ctx.cairo.set_source_rgb(c.red as f64 / 65535.0, c.green as f64 / 65535.0, c.blue as f64 / 65535.0);
                    }
                    pango::shape(&ps, &pa, &mut glyphs);
                    let x = (glyphs.get_width() as f64) / 1000.0;
                    pangocairo::show_glyph_string(&ctx.cairo, &pa.font(), &mut glyphs);

                    ctx.cairo.restore();
                    ctx.cairo.rel_move_to(x, 0.0);
                    i.next();
                }
            }
            Item::Group { item, bg_rgba, fg_alpha } => {
                ctx.cairo.push_group();
                ctx.cairo.set_source_rgba(bg_rgba.0, bg_rgba.1, bg_rgba.2, bg_rgba.3);
                ctx.cairo.paint();
                ctx.cairo.set_source_rgb(1.0, 1.0, 1.0);
                item.render(ctx);
                ctx.cairo.pop_group_to_source();
                ctx.cairo.paint_with_alpha(*fg_alpha);
            }
            Item::List(list) => {
                for item in list {
                    item.render(ctx);
                }
            }
        }
    }
}

struct Render<'a> {
    cairo : cairo::Context,
    pango : pango::Context,
    data : &'a HashMap<String, String>,
}

struct Bar {
    surf : Attached<WlSurface>,
    width : i32,
    height : i32,
    dirty : bool,
    left : Item,
}

impl Bar {
    fn render(&self, surf : &cairo::ImageSurface, data : &HashMap<String, String>) {
        let ctx = cairo::Context::new(surf);
        let font = cairo::FontFace::toy_create("Liberation Sans", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        ctx.set_font_face(&font);
        ctx.set_font_size(16.0);
        ctx.move_to(8.0, 16.0);

        let render = Render {
            pango : pangocairo::create_context(&ctx).unwrap(),
            cairo : ctx,
            data,
        };

        self.left.render(&render);
    }
}

struct State {
    eloop : calloop::LoopHandle<State>,
    refresh_timer : calloop::timer::TimerHandle<()>,
    env : Environment<MyEnv>,
    ls : Attached<ZwlrLayerShellV1>,
    shm : MemPool,
    bars : Vec<Bar>,
    display : wayland_client::Display,
    data : HashMap<String, String>,
}

impl State {
    fn config(&mut self, id : usize, width : i32, height : i32) {
        self.bars[id].width = width;
        self.bars[id].height = height;
    }

    fn draw(&mut self) -> Result<(), Box<dyn Error>> {
        let mut shm_size = 0;
        let shm_pos : Vec<_> = self.bars.iter().map(|bar| {
            let pos = shm_size;
            let len = if bar.dirty {
                let stride = cairo::Format::ARgb32.stride_for_width(bar.width as u32).unwrap();
                (bar.height as usize) * (stride as usize)
            } else { 
                0
            };
            shm_size += len;
            (pos, len)
        }).collect();

        if shm_size == 0 {
            return Ok(());
        }

        self.shm.resize(shm_size).expect("OOM");

        for (bar, (pos, len)) in self.bars.iter_mut().zip(shm_pos) {
            if !bar.dirty {
                continue;
            }
            let stride = cairo::Format::ARgb32.stride_for_width(bar.width as u32).unwrap();
            let buf : &mut [u8] = &mut self.shm.mmap().as_mut()[pos..][..len];
            unsafe {
                // Start with a fully transparent buffer (assumes Argb8888)
                // TODO there must be a way to do this with cairo.
                std::ptr::write_bytes(buf.as_mut_ptr(), 0, buf.len());

                // cairo::ImageSurface::create_for_data requires a 'static type, so give it that
                // (this could be done safely by using a wrapper object and mem::replace on the shm object)
                let buf : &'static mut [u8] = &mut *(buf as *mut [u8]);
                let surf = cairo::ImageSurface::create_for_data(buf, cairo::Format::ARgb32, bar.width, bar.height, stride)?;
                // safety: ImageSurface never gives out direct access to D
                bar.render(&surf, &self.data);
                // safety: we must finish the cairo surface to end the 'static borrow
                surf.finish();
                drop(surf);
            }
            let buf = self.shm.buffer(pos as i32, bar.width, bar.height, stride, smithay_client_toolkit::shm::Format::Argb8888);
            bar.surf.attach(Some(&buf), 0, 0);
            bar.surf.damage_buffer(0, 0, bar.width, bar.height);
            bar.surf.commit();
            bar.dirty = false;
        }
        self.display.flush()?;
        Ok(())
    }

    fn add_output(&mut self, output : &WlOutput, oi : &OutputInfo) {
        let i = self.bars.len();
        let surf = self.env.create_surface();
        let ls_surf = self.ls.get_layer_surface(&surf, Some(output), Layer::Top, "bar".to_owned());
        dbg!(oi);

        ls_surf.set_size(0, 20);
        ls_surf.set_anchor(Anchor::Top | Anchor::Left | Anchor::Right);
        ls_surf.set_exclusive_zone(20);
        ls_surf.quick_assign(move |ls_surf, event, mut data| {
            let state : &mut State = data.get().unwrap();
            match event {
                LayerSurfaceEvent::Configure { serial, width, height } => {
                    state.config(i, width as i32, height as i32);
                    ls_surf.ack_configure(serial);
                    if !state.bars[i].dirty {
                        state.bars[i].dirty = true;
                        state.eloop.insert_idle(|state| state.draw().unwrap());
                    }
                }
                LayerSurfaceEvent::Closed => {
                    todo!();
                },
                _ => () 
            }
        });

        surf.commit();

        // TODO draw from config
        let left = Item::Group {
            item : Box::new(Item::List(vec![
                Item::Text {
                    text : "{time} [   , , ,  , , , ,    ]".into(),
                    markup : false,
                },
                Item::Text {
                    text : "A <span color='red'>Red R<span color='orange'>e</span>d <b>Bold</b> <span color='blue'>blue</span> word</span>!".into(),
                    markup : true,
                }
            ])),
            bg_rgba : (0.0, 0.0, 1.0, 0.3),
            fg_alpha : 0.7,
        };

        self.bars.push(Bar {
            surf,
            left,
            width : 0,
            height : 0,
            dirty : false,
        });
    }

    fn set_data(&mut self) {
        let now = chrono::Local::now();

        // Set a timer to expire when the subsecond offset will be zero
        let subsec = chrono::Timelike::nanosecond(&now) as u64;
        let delay = 1_000_000_000u64.checked_sub(subsec);
        let delay = delay.map_or(std::time::Duration::from_secs(1), std::time::Duration::from_nanos);
        self.refresh_timer.add_timeout(delay, ());

        // TODO user-defined data sources
        self.data.insert("time".into(), format!("{}", now.format("%H:%M:%S")));

        // TODO maybe don't refresh all bars all the time?  Needs real dirty tracking.
        for bar in &mut self.bars {
            bar.dirty = true;
        }
    }

    fn init_data(&mut self) {
        self.set_data();
    }

    fn tick(&mut self) {
        self.set_data();
        self.draw().unwrap();
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut eloop = calloop::EventLoop::new()?;
    let (env, display, wl_queue) = new_default_environment!(MyEnv,
        fields=[
            layer_shell : SimpleGlobal::new(),
        ]
    )?;

    let shm = env.create_simple_pool(|_| ())?;
    let ls = env.require_global();

    let timer = calloop::timer::Timer::new().unwrap();
    let mut state = State {
        env,
        ls,
        eloop : eloop.handle(),
        refresh_timer : timer.handle(),
        shm,
        bars : Vec::new(),
        display,
        data : HashMap::new(),
    };

    state.eloop.insert_source(timer, |_data, _timer_handle, state : &mut State| {
        state.tick()
    })?;

    // TODO config file
    state.init_data();

    let _out_watcher = state.env.listen_for_outputs(|out, oi, mut data| {
        let state : &mut State = data.get().unwrap();
        state.add_output(&out, oi);
    });

    for (n, output) in state.env.get_all_outputs().into_iter().enumerate() {
        if n != 1 { continue; }
        smithay_client_toolkit::output::with_output_info(&output, |oi| state.add_output(&output, oi));
    }

    // kick off the initial configuration events
    state.display.flush()?;

    smithay_client_toolkit::WaylandSource::new(wl_queue).quick_insert(eloop.handle())?;

    loop {
        eloop.dispatch(None, &mut state)?;
    }
}
