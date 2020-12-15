use smithay_client_toolkit::environment::{Environment,SimpleGlobal};
use smithay_client_toolkit::output::OutputInfo;
use smithay_client_toolkit::shm::MemPool;
use smithay_client_toolkit::{new_default_environment,default_environment};
use std::error::Error;
use wayland_client::Attached;
use wayland_client::protocol::wl_output::WlOutput;
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

struct Bar {
    surf : Attached<wayland_client::protocol::wl_surface::WlSurface>,
    width : i32,
    height : i32,
    dirty : bool,
}

impl Bar {
    fn render(&self, surf : cairo::ImageSurface) {
        let ctx = cairo::Context::new(&surf);
        let font = cairo::FontFace::toy_create("Liberation Sans", cairo::FontSlant::Normal, cairo::FontWeight::Normal);
        ctx.set_source_rgba(0.0, 0.0, 0.0, 0.3);
        ctx.paint();
        ctx.set_source_rgba(1.0, 1.0, 1.0, 0.7);
        ctx.set_font_face(&font);
        ctx.set_font_size(16.0);
        ctx.move_to(4.0,16.0);
        ctx.show_text("Test of symbols used in the bar: [   , , ,  , , , ,    ]");

        let sf = ctx.get_scaled_font();
        let s = "!Test! [   , , ,  , , , ,    ]";
        let (glyph, cluster) = sf.text_to_glyphs(800.0, 16.0, s);
        dbg!(&glyph);
        ctx.show_text_glyphs(s, &glyph, &cluster, cairo::TextClusterFlags::None);
        surf.finish();
    }
}

struct State {
    eloop : calloop::LoopHandle<State>,
    env : Environment<MyEnv>,
    ls : Attached<ZwlrLayerShellV1>,
    shm : MemPool,
    bars : Vec<Bar>,
    display : wayland_client::Display,
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
                (bar.width as usize) * (bar.height as usize) * 4
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
            let buf : &mut [u8] = &mut self.shm.mmap().as_mut()[pos..][..len];
            unsafe {
                let buf : &'static mut [u8] = &mut *(buf as *mut [u8]);
                std::ptr::write_bytes(buf.as_mut_ptr(), 0, buf.len());
                let surf = cairo::ImageSurface::create_for_data(buf, cairo::Format::ARgb32, bar.width, bar.height, bar.width*4)?;
                bar.render(surf);
            }
            let buf = self.shm.buffer(pos as i32, bar.width, bar.height, bar.width*4, smithay_client_toolkit::shm::Format::Argb8888);
            bar.surf.attach(Some(&buf), 0, 0);
            bar.surf.damage(0, 0, bar.width, bar.height);
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

        self.bars.push(Bar {
            surf,
            width : 0,
            height : 0,
            dirty : false,
        });
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

    let mut state = State {
        env,
        ls,
        eloop : eloop.handle(),
        shm,
        bars : Vec::new(),
        display,
    };

    // TODO config

    let _out_watcher = state.env.listen_for_outputs(|out, oi, mut data| {
        let state : &mut State = data.get().unwrap();
        state.add_output(&out, oi);
    });

    for (n, output) in state.env.get_all_outputs().into_iter().enumerate() {
        if n != 1 { continue; }
        smithay_client_toolkit::output::with_output_info(&output, |oi| state.add_output(&output, oi));
    }

    // kick off the initial configuration
    state.display.flush()?;

    smithay_client_toolkit::WaylandSource::new(wl_queue).quick_insert(eloop.handle())?;

if false {
    let mut delay = 1;
    let timer = calloop::timer::Timer::new()?;
    let timer_h = timer.handle();
    timer_h.add_timeout(std::time::Duration::from_secs(1), ());
    eloop.handle().insert_source(timer, move |_, _, state : &mut State| {
        dbg!();
        delay *= 2;
        timer_h.add_timeout(std::time::Duration::from_secs(delay), ());

        state.draw().unwrap();
    })?;
}

    loop {
        eloop.dispatch(None, &mut state)?;
    }
}
