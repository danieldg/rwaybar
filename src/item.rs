use crate::data::{Action,Module,ModuleContext,ItemReference,IterationItem,Value};
use crate::state::Runtime;
use crate::font::{FontMapped,draw_font_with,layout_font,render_font};
use crate::icon;
#[cfg(feature="dbus")]
use crate::tray;
use log::{debug,warn,error};
use raqote::DrawTarget;
use std::borrow::Cow;
use std::rc::Rc;

/// State available to an [Item] render function
pub struct Render<'a, 'c> {
    pub canvas : &'a mut DrawTarget<&'c mut [u32]>,

    pub render_extents : (f32, f32, f32, f32),
    pub render_pos : f32,
    pub render_ypos : Option<f32>,
    pub render_flex : bool,

    pub font : &'a FontMapped,
    pub font_size : f32,
    pub font_color : (u16, u16, u16, u16),
    pub text_stroke : Option<(u16, u16, u16, u16)>,
    pub text_stroke_size : Option<f32>,

    pub align : Align,
    pub err_name : &'a str,
    pub runtime : &'a Runtime,
}

#[derive(Debug,Clone,Copy,PartialEq)]
enum Width {
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f32),
    /// Some number of pixels
    Pixels(f32),
}

impl Width {
    pub fn from_str(value : Cow<str>) -> Option<Self> {
        if value.ends_with('%') {
            let value = &value[..value.len() - 1];
            let pct = value.parse::<f32>().ok()?;
            return Some(Width::Fraction(pct / 100.0));
        }
        if value.contains('.') {
            value.parse().ok().map(Width::Fraction)
        } else {
            value.parse().ok().map(Width::Pixels)
        }
    }
}

pub const MIDDLE : f32 = 0.5;

#[derive(Default,Debug,Copy,Clone,PartialEq)]
pub struct Align {
    pub horiz : Option<f32>,
    pub vert : Option<f32>,
}

impl Align {
    pub fn bar_default() -> Self {
        Align {
            horiz : None,
            vert : Some(MIDDLE),
        }
    }

    pub fn parse_hv(value : Cow<str>) -> Option<f32> {
        if value.ends_with('%') {
            let value = &value[..value.len() - 1];
            let pct = value.parse::<f32>().ok()?;
            return Some(pct / 100.0);
        }
        value.parse().ok()
    }

    pub fn from_name(&mut self, value : Option<Cow<str>>) {
        match value.as_deref() {
            Some("north")  => *self = Align { horiz : Some(MIDDLE), vert : Some(0.0) },
            Some("south")  => *self = Align { horiz : Some(MIDDLE), vert : Some(1.0) },
            Some("east")   => *self = Align { horiz : Some(0.0), vert : Some(MIDDLE) },
            Some("west")   => *self = Align { horiz : Some(1.0), vert : Some(MIDDLE) },
            Some("center") => *self = Align { horiz : Some(MIDDLE), vert : Some(MIDDLE) },
            Some("") | None => {}
            Some(x) => {
                error!("Unknown alignment {}", x);
            }
        }
    }

    pub fn merge(&self, child : &Self) -> Self {
        Align {
            horiz : child.horiz.or(self.horiz),
            vert : child.vert.or(self.vert),
        }
    }
}

/// A visible item in a bar
#[derive(Debug)]
pub struct Item {
    pub config : Option<toml::Value>,
    pub data : Module,
    events : EventSink,
}

/// Formatting information (colors, width, etc) for an [Item]
#[derive(Debug,Clone,Default)]
pub struct Formatting<'a> {
    font : Option<&'a FontMapped>,
    font_size : Option<f32>,
    fg_rgba : Option<(u16, u16, u16, u16)>,
    bg_rgba : Option<(u16, u16, u16, u16)>,
    stroke_rgba : Option<(u16, u16, u16, u16)>,
    stroke_size : Option<f32>,
    border : Option<(f32, f32, f32, f32)>,
    border_rgba : Option<(u16, u16, u16, u16)>,
    min_width : Option<Width>,
    max_width : Option<Width>,
    align : Align,
    margin : Option<(f32, f32, f32, f32)>,
    padding : Option<(f32, f32, f32, f32)>,
    alpha : Option<u16>,
}

impl<'a> Formatting<'a> {
    fn expand(config : &toml::Value, runtime : &'a Runtime) -> Self {
        let get = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime.format(&fmt).or_else(|e| {
                    warn!("Error expanding '{}' when rendering: {}", fmt, e);
                    Err(())
                }).ok().map(Value::into_text),
                None => Some(v.to_string().into())
            })
        };

        let get_f32 = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime.format(&fmt).or_else(|e| {
                    warn!("Error expanding '{}' when rendering: {}", fmt, e);
                    Err(())
                }).ok().and_then(|v| v.parse_f32()),
                None => v.as_float().map(|v| v as f32).or_else(|| v.as_integer().map(|i| i as f32)),
            })
        };
        let mut align = Align {
            horiz : get("halign").and_then(Align::parse_hv),
            vert : get("valign").and_then(Align::parse_hv),
        };
        align.from_name(get("align"));

        let (font, font_size) = get("font").map_or((None, None), |font| {
            let mut size = None::<f32>;
            let font = match font.rsplit_once(' ') {
                Some((name, ssize)) if {
                    size = ssize.parse().ok();
                    size.is_some()
                } => name,
                _ => &*font,
            };
            let font = runtime.fonts.iter().find(|f| f.name == font);
            (font, size)
        });
        let stroke_rgba = Formatting::parse_rgba(get("text-outline"), get_f32("text-outline-alpha"));
        let stroke_size = get_f32("text-outline-width");
        let min_width = get("min-width").and_then(Width::from_str);
        let max_width = get("max-width").and_then(Width::from_str);
        let alpha = get_f32("alpha").map(|f| {
            f32::min(65535.0, f32::max(0.0, f * 65535.0)) as u16
        });

        let margin = get("margin").and_then(Formatting::parse_trbl);
        let border = get("border").and_then(Formatting::parse_trbl);
        let padding = get("padding").and_then(Formatting::parse_trbl);

        let bg_rgba = Formatting::parse_rgba(get("bg"), get_f32("bg-alpha"));
        let fg_rgba = Formatting::parse_rgba(get("fg"), get_f32("fg-alpha"));
        let border_rgba = Formatting::parse_rgba(get("border-color"), get_f32("border-alpha"));

        Self {
            font,
            font_size,
            fg_rgba,
            bg_rgba,
            stroke_rgba,
            stroke_size,
            border,
            border_rgba,
            min_width,
            max_width,
            align,
            margin,
            padding,
            alpha,
        }
    }

    fn parse_trbl(v : Cow<str>) -> Option<(f32, f32, f32, f32)> {
        let mut rv = (0.0, 0.0, 0.0, 0.0);
        for (i,x) in v.split_whitespace().enumerate() {
            match (i, x.parse()) {
                (0, Ok(v)) => {
                    rv = (v, v, v, v);
                }
                (1, Ok(v)) => {
                    rv.1 = v; rv.3 = v;
                }
                (2, Ok(v)) => {
                    rv.2 = v;
                }
                (3, Ok(v)) => {
                    rv.3 = v;
                }
                _ => return None,
            }
        }
        Some(rv)
    }

    pub fn parse_rgba(color : Option<impl AsRef<str>>, alpha : Option<f32>) -> Option<(u16, u16, u16, u16)> {
        if color.is_none() && alpha.is_none() {
            return None;
        }
        let color = color.as_ref().map_or("black", |v| v.as_ref());
        let (r,g,b,mut a);
        let alpha_f = alpha.unwrap_or(1.0) * 65535.0;
        a = f32::min(65535.0, f32::max(0.0, alpha_f)) as u64;
        if color.starts_with('#') {
            let v = u64::from_str_radix(&color[1..], 16);
            match (v, color.len()) {
                (Ok(v), 4) => {
                    r = ((v >> 8) & 0xF) * 0x1111;
                    g = ((v >> 4) & 0xF) * 0x1111;
                    b = ((v >> 0) & 0xF) * 0x1111;
                }
                (Ok(v), 5) => {
                    r = ((v >> 12) & 0xF) * 0x1111;
                    g = ((v >> 8) & 0xF) * 0x1111;
                    b = ((v >> 4) & 0xF) * 0x1111;
                    a = ((v >> 0) & 0xF) * 0x1111;
                }
                (Ok(v), 7) => {
                    r = ((v >> 16) & 0xFF) * 0x101;
                    g = ((v >> 8) & 0xFF) * 0x101;
                    b = ((v >> 0) & 0xFF) * 0x101;
                }
                (Ok(v), 9) => {
                    r = ((v >> 24) & 0xFF) * 0x101;
                    g = ((v >> 16) & 0xFF) * 0x101;
                    b = ((v >> 8) & 0xFF) * 0x101;
                    a = ((v >> 0) & 0xFF) * 0x101;
                }
                (Ok(v), 13) => {
                    r = (v >> 32) & 0xFFFF;
                    g = (v >> 16) & 0xFFFF;
                    b = (v >> 0) & 0xFFFF;
                }
                (Ok(v), 17) => {
                    r = (v >> 48) & 0xFFFF;
                    g = (v >> 32) & 0xFFFF;
                    b = (v >> 16) & 0xFFFF;
                    a = (v >> 0) & 0xFFFF;
                }
                _ => {
                    debug!("Could not parse color '{}'", color);
                    r = 0; g = 0; b = 0;
                }
            }
        } else {
            match color {
                "black" => { r = 0; g = 0; b = 0; }
                "red" => { r = 0xFFFF; g = 0; b = 0; }
                "yellow" => { r = 0xFFFF; g = 0xFFFF; b = 0; }
                "green" => { r = 0; g = 0xFFFF; b = 0; }
                "blue" => { r = 0; g = 0; b = 0xFFFF; }
                "gray" => { r = 0x7FFF; g = 0x7FFF; b = 0x7FFF; }
                "white" => { r = 0xFFFF; g = 0xFFFF; b = 0xFFFF; }
                _ => {
                    debug!("Unknown color '{}'", color);
                    r = 0; g = 0; b = 0;
                }
            }
        }
        Some((r as u16, g as u16, b as u16, a as u16))
    }

    fn get_shrink(&self) -> Option<(f32, f32, f32, f32)> {
        let mut rv = (0.0, 0.0, 0.0, 0.0);
        if self.padding == None && self.margin == None {
            return None;
        }
        for &i in &[self.padding, self.margin, self.border] {
            if let Some((t,r,b,l)) = i {
                rv.0 += t;
                rv.1 += r;
                rv.2 += b;
                rv.3 += l;
            }
        }
        Some(rv)
    }

    fn is_boring(&self) -> bool {
        self.fg_rgba.is_none() &&
        self.bg_rgba.is_none() &&
        self.border.is_none() &&
        self.border_rgba.is_none() &&
        self.min_width.is_none() &&
        self.max_width.is_none() &&
        self.margin.is_none() &&
        self.padding.is_none() &&
        self.alpha.is_none()
    }

    fn setup_ctx<'p : 'a, 'c>(&'a self, ctx : &'a mut Render<'p, 'c>) -> Render<'a, 'c> {
        Render {
            canvas : &mut *ctx.canvas,
            align : ctx.align.merge(&self.align),
            font : self.font.unwrap_or(&ctx.font),
            font_size : self.font_size.unwrap_or(ctx.font_size),
            font_color : self.fg_rgba.unwrap_or(ctx.font_color),
            text_stroke : self.stroke_rgba.or(ctx.text_stroke),
            text_stroke_size : self.stroke_size.or(ctx.text_stroke_size),
            ..*ctx
        }
    }
}

/// A single click action associated with the area that activates it
#[derive(Debug,Clone)]
struct EventListener {
    x_min : f32,
    x_max : f32,
    buttons : u32,
    item : Option<IterationItem>,
    target : Action,
}

/// A list of [EventListener]s
#[derive(Debug,Default,Clone)]
pub struct EventSink {
    handlers : Vec<EventListener>,
    hovers : Vec<(f32, f32, PopupDesc)>,
}

impl EventSink {
    fn from_toml(value : &toml::Value) -> Self {
        let mut sink = EventSink::default();
        sink.add_click(value.get("on-click"), 1 << 0);
        sink.add_click(value.get("on-click-right"), 1 << 1);
        sink.add_click(value.get("on-click-middle"), 1 << 2);
        sink.add_click(value.get("on-click-back"), 1 << 3);
        sink.add_click(value.get("on-click-backward"), 1 << 3);
        sink.add_click(value.get("on-click-forward"), 1 << 4);
        sink.add_click(value.get("on-scroll-up"), 1 << 5);
        sink.add_click(value.get("on-scroll-down"), 1 << 6);
        sink.add_click(value.get("on-vscroll"), 3 << 5);
        sink.add_click(value.get("on-scroll-left"), 1 << 7);
        sink.add_click(value.get("on-scroll-right"), 1 << 8);
        sink.add_click(value.get("on-hscroll"), 3 << 7);
        sink.add_click(value.get("on-scroll"), 15 << 5);
        sink
    }

    fn add_click(&mut self, value : Option<&toml::Value>, buttons : u32) {
        if let Some(value) = value {
            self.handlers.push(EventListener {
                x_min : 0.0,
                x_max : 1e20,
                buttons,
                item : None,
                target : Action::from_toml(value)
            })
        }
    }

    #[cfg(feature="dbus")]
    pub fn from_tray(owner : Rc<str>, path : Rc<str>) -> Self {
        let mut sink = EventSink::default();
        sink.handlers.push(EventListener {
            x_min : -1e20,
            x_max : 1e20,
            buttons : 7 | (15 << 5),
            item : None,
            target : Action::from_tray(owner, path),
        });
        sink
    }

    pub fn merge(&mut self, sink : Self) {
        self.handlers.extend(sink.handlers);
        self.hovers.extend(sink.hovers);
    }

    pub fn offset_clamp(&mut self, offset : f32, min : f32, max : f32) {
        for h in &mut self.handlers {
            h.x_min += offset;
            h.x_max += offset;
            if h.x_min < min {
                h.x_min = min;
            } else if h.x_min > max {
                h.x_min = max;
            }
            if h.x_max < min {
                h.x_max = min;
            } else if h.x_max > max {
                h.x_max = max;
            }
        }
        for (x_min, x_max, _) in &mut self.hovers {
            *x_min += offset;
            *x_max += offset;
            if *x_min < min {
                *x_min = min;
            } else if *x_min > max {
                *x_min = max;
            }
            if *x_max < min {
                *x_max = min;
            } else if *x_max > max {
                *x_max = max;
            }
        }
    }

    pub fn button(&mut self, x : f32, y : f32, button : u32, runtime : &mut Runtime) {
        let _ = y;
        for h in &mut self.handlers {
            if x < h.x_min || x > h.x_max {
                continue;
            }
            if (h.buttons & (1 << button)) == 0 {
                continue;
            }
            if h.item.is_none() {
                h.target.invoke(runtime, button);
            } else {
                let item_var = runtime.get_item_var();
                item_var.set(h.item.clone());
                h.target.invoke(runtime, button);
                item_var.set(None);
            }
        }
    }

    #[cfg_attr(not(feature="dbus"), allow(unused))]
    pub fn add_hover(&mut self, min : f32, max : f32, desc : PopupDesc) {
        self.hovers.push((min, max, desc));
    }

    pub fn get_hover(&mut self, x : f32, y : f32) -> Option<(f32, f32, &mut PopupDesc)> {
        let _ = y;
        for &mut (min, max, ref mut text) in &mut self.hovers {
            if x >= min && x < max {
                return Some((min, max, text));
            }
        }
        None
    }

    pub fn for_active_regions(&self, mut f : impl FnMut(f32, f32)) {
        let mut ha = self.handlers.iter().peekable();
        let mut ho = self.hovers.iter().peekable();
        loop {
            let a_min = ha.peek().map_or(f32::NAN, |e| e.x_min);
            let o_min = ho.peek().map_or(f32::NAN, |e| e.0);
            let min = f32::min(a_min, o_min);
            let mut max;
            if min == a_min {
                max = ha.next().unwrap().x_max;
            } else if min == o_min {
                max = ho.next().unwrap().1;
            } else {
                // NaN
                return;
            }
            loop {
                if ha.peek().map_or(f32::NAN, |e| e.x_min) <= max + 1.0 {
                    max = f32::max(max, ha.next().map_or(f32::NAN, |e| e.x_max));
                } else if ho.peek().map_or(f32::NAN, |e| e.0) <= max + 1.0 {
                    max = f32::max(max, ho.next().map_or(f32::NAN, |e| e.1));
                } else {
                    break;
                }
            }
            f(min, max);
        }
    }
}

impl From<Module> for Item {
    fn from(data : Module) -> Self {
        Self {
            config : None,
            events : EventSink::default(),
            data
        }
    }
}

impl Item {
    pub fn none() -> Self {
        Self {
            config : None,
            events : EventSink::default(),
            data : Module::none(),
        }
    }

    pub fn new_bar(cfg : toml::Value) -> Self {
        let left = Rc::new(cfg.get("left").map_or_else(Item::none, Item::from_toml_ref));
        let right = Rc::new(cfg.get("right").map_or_else(Item::none, Item::from_toml_ref));
        let center = Rc::new(cfg.get("center").map_or_else(Item::none, Item::from_toml_ref));

        Item {
            data : Module::Bar {
                left, center, right,
                config : cfg.clone(),
            },
            events : EventSink::from_toml(&cfg),
            config : Some(cfg),
        }
    }

    pub fn from_toml_ref(value : &toml::Value) -> Self {
        if value.as_str().is_some() {
            return Module::from_toml_in(value, ModuleContext::Source).into();
        }

        Self::from_item_list("<ref>", value)
    }

    pub fn from_toml_format(value : &toml::Value) -> Self {
        Self::from_item_list("<ref>", value)
    }

    pub fn from_item_list(key : &str, value : &toml::Value) -> Self {
        if let Some(array) = value.as_array() {
            return Module::Group {
                items : array.iter().map(Item::from_toml_ref).map(Rc::new).collect(),
                tooltip : None,
                spacing : "".into(),
            }.into();
        }

        let data = Module::from_toml_in(value, ModuleContext::Item);
        if data.is_none() {
            match value.get("type").and_then(|v| v.as_str()) {
                Some(tipe) => error!("Unknown item type '{}' in '{}'", tipe, key),
                None => error!("Type required for item '{}'", key),
            }
        }
        Item {
            config : Some(value.clone()),
            events : EventSink::from_toml(value),
            data,
        }
    }

    pub fn render(self : &Rc<Self>, parent_ctx : &mut Render) -> EventSink {
        let mut rv = self.events.clone();

        let format = match self.config.as_ref().map(|cfg| Formatting::expand(cfg, &parent_ctx.runtime)) {
            Some(f) => f,
            None => {
                self.render_inner(parent_ctx, &mut rv);
                return rv;
            }
        };
        let mut ctx = format.setup_ctx(parent_ctx);
        if format.is_boring() {
            self.render_inner(&mut ctx, &mut rv);
            let (x,y) = (ctx.render_pos, ctx.render_ypos);
            parent_ctx.render_pos = x;
            parent_ctx.render_ypos = y;
            return rv;
        }

        let mut outer_clip = ctx.render_extents;
        let start_pos = ctx.render_pos;

        let mut inner_clip = outer_clip;
        inner_clip.0 = start_pos;

        let shrink = format.get_shrink();
        if (shrink, format.max_width) != (None, None) {
            match shrink {
                Some((t, r, b, l)) => {
                    inner_clip.0 += l;
                    inner_clip.1 += t;
                    inner_clip.2 -= r;
                    inner_clip.3 -= b;
                }
                None => {}
            }
            match format.max_width {
                Some(Width::Pixels(n)) => {
                    inner_clip.2 = inner_clip.2.min(inner_clip.0 + n);
                }
                Some(Width::Fraction(f)) => {
                    let parent_width = outer_clip.2 - outer_clip.0;
                    inner_clip.2 = inner_clip.2.min(inner_clip.0 + parent_width * f);
                }
                None => {}
            }
        }

        ctx.render_pos = inner_clip.0;

        ctx.render_extents = inner_clip;
        self.render_inner(&mut ctx, &mut rv);
        let mut inner_clip = inner_clip;

        let inner_end = ctx.render_pos;

        let child_render_width = inner_end - inner_clip.0;
        let mut min_width = match format.min_width {
            None => 0.0,
            Some(Width::Pixels(n)) => n,
            Some(Width::Fraction(f)) => f * (outer_clip.2 - outer_clip.0),
        };
        if inner_clip.0 + min_width > inner_clip.2 {
            // clamp the minimum to only the available region
            min_width = inner_clip.2 - inner_clip.0;
        }

        let inner_x_offset;
        if child_render_width < min_width {
            // child is smaller than the box; align it
            let expand = min_width - child_render_width;
            match format.align.horiz.or(ctx.align.horiz) {
                Some(f) => {
                    inner_x_offset = expand * f;
                    let x0 = inner_clip.0.floor() as usize;
                    let x1 = inner_end.ceil() as usize;
                    let ilen = x1 - x0;
                    let wlen = min_width.ceil() as usize;
                    if wlen > ilen {
                        // Align by rotating the pixels of the inner clip region to the right.  The
                        // right part of the clip region should just be blank pixels at this point,
                        // which is what we want to put on the left.
                        let rlen = wlen - ilen;
                        let stride = ctx.canvas.width() as usize;
                        let h = ctx.canvas.height() as usize;
                        for y in 0..h {
                            let x0 = y * stride + x0;
                            let x2 = y * stride + x0 + wlen;
                            if let Some(buf) = ctx.canvas.get_data_mut().get_mut(x0..x2) {
                                buf.rotate_right(rlen);
                            }
                        }
                    }
                }
                _ => { // defaults to left align
                    inner_x_offset = 0.0;
                }
            }
        } else {
            inner_x_offset = 0.0;
        }

        let shrink_r_width = outer_clip.2 - inner_clip.2;
        if ctx.render_flex {
            // flex the right side of the clip regions to the actual dimensions
            inner_clip.2 = inner_clip.0 + child_render_width;
            outer_clip.2 = inner_clip.2 + shrink_r_width;
        } else {
            // move the right side of the clip regions leftwards to the actual dimensions
            if inner_clip.2 > inner_clip.0 + child_render_width {
                inner_clip.2 = inner_clip.0 + child_render_width;
            }
            if outer_clip.2 > inner_clip.2 + shrink_r_width {
                outer_clip.2 = inner_clip.2 + shrink_r_width;
            }
        }

        rv.offset_clamp(inner_x_offset, inner_clip.0, inner_clip.2);

        if format.bg_rgba.is_some() || format.border.is_some() {
            let mut bg_clip = inner_clip;
            if let Some((t, r, b, l)) = format.padding {
                bg_clip.0 -= l;
                bg_clip.1 -= t;
                bg_clip.2 += r;
                bg_clip.3 += b;
            }

            if let Some(rgba) = format.bg_rgba {
                ctx.canvas.fill_rect(bg_clip.0, bg_clip.1, bg_clip.2 - bg_clip.0, bg_clip.3 - bg_clip.1,
                    &raqote::Color::new((rgba.3 / 256) as u8, (rgba.0 / 256) as u8, (rgba.1 / 256) as u8, (rgba.2 / 256) as u8).into(),
                    &raqote::DrawOptions {
                        blend_mode : raqote::BlendMode::DstOver,
                        ..Default::default()
                    });
            }

            if let Some(border) = format.border {
                let rgba = format.border_rgba.unwrap_or(ctx.font_color);
                let src = raqote::Color::new((rgba.3 / 256) as u8, (rgba.0 / 256) as u8, (rgba.1 / 256) as u8, (rgba.2 / 256) as u8).into();

                if border.0 > 0.0 {
                    ctx.canvas.fill_rect(bg_clip.0, bg_clip.1, bg_clip.2 - bg_clip.0, border.0, &src, &Default::default());
                }
                if border.1 > 0.0 {
                    ctx.canvas.fill_rect(bg_clip.2, bg_clip.1, border.1, bg_clip.3 - bg_clip.1, &src, &Default::default());
                }
                if border.2 > 0.0 {
                    ctx.canvas.fill_rect(bg_clip.0, bg_clip.3, bg_clip.2 - bg_clip.0, border.2, &src, &Default::default());
                }
                if border.3 > 0.0 {
                    ctx.canvas.fill_rect(bg_clip.0, bg_clip.1, border.3, bg_clip.3 - bg_clip.1, &src, &Default::default());
                }
            }
        }

        let yp = ctx.render_ypos;
        parent_ctx.render_pos = outer_clip.2;
        parent_ctx.render_ypos = yp;

        rv
    }

    pub fn render_clamped(self : &Rc<Self>, ctx : &mut Render, ev : &mut EventSink) {
        let x0 = ctx.render_pos;
        let mut rv = self.render(ctx);
        let x1 = ctx.render_pos;
        rv.offset_clamp(0.0, x0, x1);
        ev.merge(rv);
    }

    pub fn render_clamped_item(self : &Rc<Self>, ctx : &mut Render, ev : &mut EventSink, item : &IterationItem) {
        let item_var = ctx.runtime.get_item_var();
        let prev = item_var.replace(Some(item.clone()));
        let x0 = ctx.render_pos;
        let mut rv = self.render(ctx);
        let x1 = ctx.render_pos;
        rv.offset_clamp(0.0, x0, x1);
        for h in &mut rv.handlers {
            h.item.get_or_insert_with(|| item.clone());
        }
        ev.merge(rv);
        item_var.set(prev);
    }

    /// Render the block contents to the given context.
    ///
    /// Your item starts at the context's current point.  When you are done rendering, you should
    /// adjust the point to be offset by the size of your rendered item.
    ///
    /// You may use the current clip area to determine sizes.  By default, the clip area is set to
    /// the size of the entire bar; however, any max_width specifiers in a parent item will reduce
    /// this.
    ///
    /// Note that the coordinates you use to render may not match the final coordinates in the
    /// buffer; if your item is not left-aligned, it will likely be shifted right before the final
    /// render.
    fn render_inner(self : &Rc<Self>, ctx : &mut Render, rv : &mut EventSink) {
        match &self.data {
            Module::ItemReference { value } => {
                ItemReference::with(value, &ctx.runtime, |item| match item {
                    Some(item) => rv.merge(item.render(ctx)),
                    None => {}
                });
            }
            Module::Group { items, tooltip, spacing } => {
                if let Some(cond) = self.config.as_ref()
                    .and_then(|c| c.get("condition"))
                    .and_then(|v| v.as_str())
                {
                    if !cond.is_empty() {
                        match ctx.runtime.format(cond) {
                            Ok(v) if v.as_bool() => {},
                            Ok(_) => return,
                            Err(e) => {
                                warn!("Error evaluating condition '{}': {}", cond, e);
                            }
                        }
                    }
                }
                let mut ypos = ctx.render_ypos.map(|p| (p, p));
                let spacing = ctx.runtime.format(spacing).ok().and_then(|s| s.parse_f32()).unwrap_or(0.0);
                let xstart = ctx.render_pos;
                ctx.render_pos += spacing;
                for item in items {
                    item.render_clamped(ctx, rv);
                    if spacing >= 1.0 {
                        ctx.render_pos = ctx.render_pos.ceil() + spacing;
                    }
                    if let Some((min,max)) = &mut ypos {
                        let now = ctx.render_ypos.unwrap();
                        ctx.render_ypos = Some(*min);
                        if now > *max {
                            *max = now;
                        }
                    }
                }
                ctx.render_ypos.as_mut().map(|p| *p = ypos.unwrap().1);
                if let Some(item) = tooltip {
                    let xend = ctx.render_pos;
                    rv.hovers.push((xstart, xend, PopupDesc::RenderItem {
                        item : item.clone(),
                        iter : ctx.runtime.copy_item_var(),
                    }));
                }
            }
            Module::FocusList { source, others, focused, spacing } => {
                let spacing = ctx.runtime.format(spacing).ok().and_then(|s| s.parse_f32()).unwrap_or(0.0);
                let item_var = ctx.runtime.get_item_var();
                ctx.render_pos += spacing;
                let prev = item_var.replace(None);
                source.read_focus_list(ctx.runtime, |focus, item| {
                    item_var.set(Some(item.clone()));
                    let x0 = ctx.render_pos;
                    let mut ev = if focus {
                        focused.render(ctx)
                    } else {
                        others.render(ctx)
                    };
                    let x1 = ctx.render_pos;
                    ev.offset_clamp(0.0, x0, x1);
                    for h in &mut ev.handlers {
                        h.item.get_or_insert_with(|| item.clone());
                    }
                    rv.merge(ev);
                    ctx.render_pos += spacing;
                });
                item_var.set(prev);
            }
            Module::Bar { left, center, right, .. } => {
                let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.render_extents;
                let xform = *ctx.canvas.get_transform();
                let width = clip_x1 - clip_x0;
                let height = clip_y1.ceil();
                let render_extents = (0.0, clip_y0, width, clip_y1);
                let canvas_size = xform.transform_point(raqote::Point::new(width, height)).to_i32();
                let mut canvas = DrawTarget::new(canvas_size.x, canvas_size.y).into_vec();
                let mut canvas = DrawTarget::from_backing(canvas_size.x, canvas_size.y, &mut canvas[..]);
                canvas.set_transform(&xform);

                let mut left_ev = left.render(ctx);
                let left_size = ctx.render_pos;
                left_ev.offset_clamp(0.0, 0.0, left_size);
                rv.merge(left_ev);

                let mut group = Render {
                    canvas : &mut canvas, 
                    render_extents,
                    render_pos : 0.0,
                    render_ypos : None,
                    render_flex : ctx.render_flex,

                    font : ctx.font,
                    font_size : ctx.font_size,
                    font_color : ctx.font_color,
                    text_stroke : ctx.text_stroke,
                    text_stroke_size : ctx.text_stroke_size,

                    align : ctx.align,
                    err_name : "bar",
                    runtime : ctx.runtime,
                };

                let mut right_ev = right.render(&mut group);
                let right_width = group.render_pos.ceil();

                let right_offset = clip_x1 - right_width;
                ctx.canvas.copy_surface(&group.canvas,
                    raqote::IntRect::new(raqote::IntPoint::origin(), xform.transform_point(raqote::Point::new(right_width, height)).to_i32()),
                    xform.transform_point(raqote::Point::new(right_offset, 0.0)).to_i32());

                right_ev.offset_clamp(right_offset, right_offset, clip_x1);
                rv.merge(right_ev);

                group.canvas.clear(raqote::SolidSource { a: 0, r: 0, g: 0, b: 0 });
                group.render_pos = 0.0;

                let mut cent_ev = center.render(&mut group);
                let cent_size = group.render_pos;

                let max_side = (width - cent_size) / 2.0;
                let total_room = width - (left_size + right_width + cent_size);
                let cent_offset;
                if total_room < 0.0 {
                    // TODO maybe we should have cropped it?
                    return;
                } else if left_size > max_side {
                    // left side is too long to properly center; put it just to the right of that
                    cent_offset = left_size;
                } else if right_width > max_side {
                    // right side is too long to properly center; put it just to the left of that
                    cent_offset = clip_x1 - right_width - cent_size;
                } else {
                    // Actually center the center module
                    cent_offset = max_side;
                }
                ctx.canvas.copy_surface(&group.canvas,
                    raqote::IntRect::new(raqote::IntPoint::origin(), xform.transform_point(raqote::Point::new(cent_size, height)).to_i32()),
                    xform.transform_point(raqote::Point::new(cent_offset, 0.0)).to_i32());
                cent_ev.offset_clamp(cent_offset, cent_offset, cent_offset + cent_size);
                rv.merge(cent_ev);

                ctx.render_pos = clip_x1;
            }
            Module::Icon { name, fallback, tooltip } => {
                let start_pos = ctx.render_pos;
                let markup = self.config.as_ref().and_then(|c| c.get("markup")).and_then(|v| v.as_bool()).unwrap_or(false);
                let name = ctx.runtime.format_or(name, ctx.err_name).into_text();
                match icon::render(ctx, &name) {
                    Ok(()) => {},
                    Err(()) => {
                        let value = ctx.runtime.format_or(fallback, ctx.err_name).into_owned();
                        let mut item : Item = Module::new_value(value).into();
                        if markup {
                            item.config = self.config.clone();
                        }
                        Rc::new(item).render(ctx);
                    }
                }
                if !tooltip.is_empty() {
                    let end_pos = ctx.render_pos;
                    rv.hovers.push((start_pos, end_pos, PopupDesc::TextItem {
                        source : self.clone(),
                        iter : ctx.runtime.copy_item_var(),
                    }));
                }
            },
            Module::SwayTree(tree) => {
                tree.render(ctx, rv);
            }
            #[cfg(feature="dbus")]
            Module::Tray { spacing, show_passive, show_active, show_urgent } => {
                let spacing = ctx.runtime.format(spacing).ok().and_then(|s| s.parse_f32()).unwrap_or(0.0);
                let show_passive = ctx.runtime.format(show_passive).ok().and_then(|s| s.parse_bool()).unwrap_or(false);
                let show_active = ctx.runtime.format(show_active).ok().and_then(|s| s.parse_bool()).unwrap_or(true);
                let show_urgent = ctx.runtime.format(show_urgent).ok().and_then(|s| s.parse_bool()).unwrap_or(true);
                tray::show(ctx, rv, spacing, (show_passive, show_active, show_urgent))
            }
            Module::None => {}

            // All other modules are rendered as text
            _ => {
                let markup = self.config.as_ref().and_then(|c| c.get("markup")).and_then(|v| v.as_bool()).unwrap_or(false);
                let text = self.data.read_to_owned(ctx.err_name, "text", &ctx.runtime).into_text();
                let main_scale = ctx.font.scale_from_pt(ctx.font_size);
                let main_font = ctx.font.as_ref();

                let xstart = ctx.render_pos;
                let ystart = ctx.render_ypos.unwrap_or_else(|| {
                    let yoff = match ctx.align.vert {
                        Some(f) => {
                            let (_x0, clip_y0, _x1, clip_y1) = ctx.render_extents;
                            let extra = clip_y1 - clip_y0 - main_scale * (main_font.ascender() - main_font.descender()) as f32;
                            if extra >= 0.0 {
                                extra * f
                            } else {
                                0.0
                            }
                        }
                        _ => 0.0,
                    };
                    yoff + ctx.render_extents.1
                });

                let (width, height) = if let Some(rgba) = ctx.text_stroke {
                    let (to_draw, (width, height)) = layout_font(ctx.font, ctx.font_size, &ctx.runtime, ctx.font_color, &text, markup);

                    let src = raqote::Color::new(
                        (rgba.3 / 256) as u8,
                        (rgba.0 / 256) as u8,
                        (rgba.1 / 256) as u8,
                        (rgba.2 / 256) as u8,
                    ).into();
                    let style = raqote::StrokeStyle {
                        width : ctx.text_stroke_size.unwrap_or(1.0),
                        cap: raqote::LineCap::Round,
                        join: raqote::LineJoin::Round,
                        ..Default::default()
                    };
                    let opts = raqote::DrawOptions {
                        ..Default::default()
                    };

                    draw_font_with(ctx.canvas, (xstart, ystart), &to_draw, |canvas, path, color| {
                        canvas.stroke(&path, &src, &style, &opts);
                        let src = raqote::Color::new(
                            (color.3 / 256) as u8,
                            (color.0 / 256) as u8,
                            (color.1 / 256) as u8,
                            (color.2 / 256) as u8,
                        ).into();
                        canvas.fill(&path, &src, &opts);
                    });

                    (width, height)
                } else {
                    render_font(ctx.canvas, ctx.font, ctx.font_size, ctx.font_color, &ctx.runtime, (xstart, ystart), &text, markup)
                };

                ctx.render_pos = xstart + width;
                if let Some(yrec) = ctx.render_ypos.as_mut() {
                    *yrec = ystart + height - main_scale * main_font.descender() as f32;
                }

                let xpos = xstart;
                match &self.data {
                    Module::Formatted { tooltip : Some(item), .. } => {
                        rv.hovers.push((xpos, xpos + width, PopupDesc::RenderItem {
                            item : item.clone(),
                            iter : ctx.runtime.copy_item_var(),
                        }));
                    }
                    Module::Formatted { tooltip : None, .. } => {}
                    _ => {
                        let tt = self.data.read_to_owned(ctx.err_name, "tooltip", &ctx.runtime).into_text();
                        if !tt.is_empty() {
                            rv.hovers.push((xpos, xpos + width, PopupDesc::TextItem {
                                source : self.clone(),
                                iter : ctx.runtime.copy_item_var(),
                            }));
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug,Clone)]
pub enum PopupDesc {
    RenderItem {
        item : Rc<Item>,
        iter : Option<IterationItem>,
    },
    TextItem {
        source : Rc<Item>,
        iter : Option<IterationItem>,
    },
    #[cfg(feature="dbus")]
    Tray(tray::TrayPopup),
}

impl PartialEq for PopupDesc {
    fn eq(&self, rhs : &Self) -> bool {
        match (self, rhs) {
            (PopupDesc::RenderItem { item : a, iter : ai }, PopupDesc::RenderItem { item : b, iter : bi }) => {
                Rc::ptr_eq(a,b) && ai == bi
            }
            (PopupDesc::TextItem { source : a, iter : ai }, PopupDesc::TextItem { source : b, iter : bi }) => {
                Rc::ptr_eq(a,b) && ai == bi
            }
            #[cfg(feature="dbus")]
            (PopupDesc::Tray(a), PopupDesc::Tray(b)) => a == b,
            _ => false,
        }
    }
}

impl PopupDesc {
    pub fn render_popup(&mut self, runtime : &Runtime, target : &mut DrawTarget<&mut [u32]>) -> (i32, i32) {
        target.clear(raqote::SolidSource { a: 255, r: 0, g: 0, b: 0 });
        let font = &runtime.fonts[0];
        let render_extents = (0.0, 0.0, target.width() as f32, target.height() as f32);

        let mut ctx = Render {
            canvas : target,
            font,
            font_size : 16.0,
            font_color : (0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF),
            align : Align::bar_default(),
            render_extents,
            render_pos : 2.0,
            render_ypos : Some(2.0),
            render_flex : true,
            err_name: "popup",
            text_stroke : None,
            text_stroke_size : None,
            runtime,
        };

        self.render(&mut ctx);
        (ctx.render_pos as i32, ctx.render_ypos.unwrap() as i32)
    }

    fn render(&mut self, ctx : &mut Render) {
        match self {
            PopupDesc::RenderItem { item, iter } => {
                let item_var = ctx.runtime.get_item_var();
                item_var.set(iter.clone());
                item.render(ctx);
                item_var.set(None);
            }
            PopupDesc::TextItem { source, iter } => {
                let item_var = ctx.runtime.get_item_var();
                item_var.set(iter.clone());
                let value = source.data.read_to_owned("tooltip", "tooltip", ctx.runtime).into_text();
                item_var.set(None);

                if value.is_empty() {
                    return;
                }

                let markup = source.config.as_ref().and_then(|c| c.get("markup")).and_then(|v| v.as_bool()).unwrap_or(false);

                let (width, height) = render_font(ctx.canvas, ctx.font, ctx.font_size, ctx.font_color, &ctx.runtime, (2.0, 2.0), &value, markup);
                ctx.render_pos = width + 4.0;
                ctx.render_ypos.as_mut().map(|p| *p = height + 4.0);
            }
            #[cfg(feature="dbus")]
            PopupDesc::Tray(tray) => tray.render(ctx),
        }
    }

    #[cfg_attr(not(feature="dbus"), allow(unused))]
    pub fn button(&mut self, x : f64, y : f64, button : u32, runtime : &mut Runtime) {
        match self {
            PopupDesc::RenderItem { .. } => { }
            PopupDesc::TextItem { .. } => { }
            #[cfg(feature="dbus")]
            PopupDesc::Tray(tray) => tray.button(x, y, button, runtime),
        }
    }
}
