use crate::data::{Action,Module,IterationItem};
use crate::state::Runtime;
use crate::tray;
use log::{debug,warn,error};
use std::rc::Rc;

/// State available to an [Item] render function
#[derive(Clone,Copy)]
pub struct Render<'a> {
    pub cairo : &'a cairo::Context,
    pub font : &'a pango::FontDescription,
    pub text_stroke : Option<(u16, u16, u16, u16)>,
    pub align : Align,
    pub err_name : &'a str,
    pub runtime : &'a Runtime,
}

#[derive(Debug,Clone,Copy,PartialEq)]
enum Width {
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f64),
    /// Some number of pixels
    Pixels(f64),
}

impl Width {
    pub fn from_str(mut value : String) -> Option<Self> {
        if value.ends_with('%') {
            value.pop();
            let pct = value.parse::<f64>().ok()?;
            return Some(Width::Fraction(pct / 100.0));
        }
        if value.contains('.') {
            value.parse().ok().map(Width::Fraction)
        } else {
            value.parse().ok().map(Width::Pixels)
        }
    }
}

pub const MIDDLE : f64 = 0.5;

#[derive(Default,Debug,Copy,Clone,PartialEq)]
pub struct Align {
    pub horiz : Option<f64>,
    pub vert : Option<f64>,
}

impl Align {
    pub fn bar_default() -> Self {
        Align {
            horiz : None,
            vert : Some(MIDDLE),
        }
    }

    pub fn parse_hv(mut value : String) -> Option<f64> {
        if value.ends_with('%') {
            value.pop();
            let pct = value.parse::<f64>().ok()?;
            return Some(pct / 100.0);
        }
        value.parse().ok()
    }

    pub fn from_name(&mut self, value : Option<String>) {
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
#[derive(Debug,Clone,Default,PartialEq)]
struct Formatting {
    font : Option<pango::FontDescription>,
    fg_rgba : Option<(u16, u16, u16, u16)>,
    bg_rgba : Option<(u16, u16, u16, u16)>,
    stroke_rgba : Option<(u16, u16, u16, u16)>,
    border : Option<(f64, f64, f64, f64)>,
    border_rgba : Option<(u16, u16, u16, u16)>,
    min_width : Option<Width>,
    max_width : Option<Width>,
    align : Align,
    margin : Option<(f64, f64, f64, f64)>,
    padding : Option<(f64, f64, f64, f64)>,
    alpha : Option<u16>,
}

impl Formatting {
    fn expand(config : &toml::Value, runtime : &Runtime) -> Self {
        let get = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime.format(&fmt).or_else(|e| {
                    warn!("Error expanding '{}' when rendering: {}", fmt, e);
                    Err(())
                }).ok(),
                None => Some(v.to_string())
            })
        };

        let get_f64 = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime.format(&fmt).or_else(|e| {
                    warn!("Error expanding '{}' when rendering: {}", fmt, e);
                    Err(())
                }).ok().and_then(|v| v.parse().ok()),
                None => v.as_float().or_else(|| v.as_integer().map(|i| i as f64)),
            })
        };
        let mut align = Align {
            horiz : get("halign").and_then(Align::parse_hv),
            vert : get("valign").and_then(Align::parse_hv),
        };
        align.from_name(get("align"));

        let font = get("font").map(|font| pango::FontDescription::from_string(&font));
        let stroke_rgba = Formatting::parse_rgba(get("text-outline"), get_f64("text-outline-alpha"));
        let min_width = get("min-width").and_then(Width::from_str);
        let max_width = get("max-width").and_then(Width::from_str);
        let alpha = get_f64("alpha").map(|f| {
            f64::min(65535.0, f64::max(0.0, f * 65535.0)) as u16
        });

        let margin = get("margin").and_then(Formatting::parse_trbl);
        let border = get("border").and_then(Formatting::parse_trbl);
        let padding = get("padding").and_then(Formatting::parse_trbl);

        let bg_rgba = Formatting::parse_rgba(get("bg"), get_f64("bg-alpha"));
        let fg_rgba = Formatting::parse_rgba(get("fg"), get_f64("fg-alpha"));
        let border_rgba = Formatting::parse_rgba(get("border-color"), get_f64("border-alpha"));

        Self {
            font,
            fg_rgba,
            bg_rgba,
            stroke_rgba,
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

    fn parse_trbl(v : String) -> Option<(f64, f64, f64, f64)> {
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

    fn parse_rgba(color : Option<String>, alpha : Option<f64>) -> Option<(u16, u16, u16, u16)> {
        if color.is_none() && alpha.is_none() {
            return None;
        }
        let mut pc = pango_sys::PangoColor {
            red : 0, blue : 0, green : 0
        };
        if let Some(color) = color {
            let cstr = std::ffi::CString::new(color).unwrap();
            unsafe {
                pango_sys::pango_color_parse(&mut pc, cstr.as_ptr());
            }
        }
        let alpha_f = alpha.unwrap_or(1.0) * 65535.0;
        let alpha_i = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;
        Some((pc.red, pc.green, pc.blue, alpha_i))
    }

    fn get_shrink(&self) -> Option<(f64, f64, f64, f64)> {
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

    fn is_boring(&mut self) -> bool {
        // this might be more efficient if we enumerated all fields
        let mut b = Self::default();
        b.align = self.align;
        b.stroke_rgba = self.stroke_rgba;
        let font = self.font.take();
        let rv = *self == b;
        self.font = font;
        rv
    }

    fn setup_ctx<'a, 'p : 'a>(&'a self, ctx : &Render<'p>) -> Render<'a> {
        Render {
            align : ctx.align.merge(&self.align),
            font : self.font.as_ref().unwrap_or(ctx.font),
            text_stroke : self.stroke_rgba.or(ctx.text_stroke),
            ..*ctx
        }
    }
}

/// A single click action associated with the area that activates it
#[derive(Debug,Clone)]
struct EventListener {
    x_min : f64,
    x_max : f64,
    buttons : u32,
    item : Option<Rc<IterationItem>>,
    target : Action,
}

/// A list of [EventListener]s
#[derive(Debug,Default,Clone)]
pub struct EventSink {
    handlers : Vec<EventListener>,
    hovers : Vec<(f64, f64, PopupDesc)>,
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

    pub fn from_tray(owner : String, path : String) -> Self {
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

    pub fn offset_clamp(&mut self, offset : f64, min : f64, max : f64) {
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

    pub fn button(&mut self, x : f64, y : f64, button : u32, runtime : &mut Runtime) {
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
                let item_var = match runtime.items.get("item") {
                    Some(&Item { data : Module::Item { ref value }, .. }) => value,
                    _ => {
                        error!("The 'item' variable was not assignable");
                        return;
                    }
                };
                item_var.set(h.item.clone());
                h.target.invoke(runtime, button);
                item_var.set(None);
            }
        }
    }

    pub fn add_hover(&mut self, min : f64, max : f64, desc : PopupDesc) {
        self.hovers.push((min, max, desc));
    }

    pub fn get_hover(&mut self, x : f64, y : f64) -> Option<(f64, f64, &mut PopupDesc)> {
        let _ = y;
        for &mut (min, max, ref mut text) in &mut self.hovers {
            if x >= min && x <= max {
                return Some((min, max, text));
            }
        }
        None
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
        let left = cfg.get("left").map_or_else(Item::none, Item::from_toml_ref);
        let right = cfg.get("right").map_or_else(Item::none, Item::from_toml_ref);
        let center = cfg.get("center").map_or_else(Item::none, Item::from_toml_ref);

        Item {
            data : Module::Bar {
                items : Box::new([left, center, right]),
                config : cfg.clone(),
            },
            events : EventSink::from_toml(&cfg),
            config : Some(cfg),
        }
    }

    pub fn from_toml_ref(value : &toml::Value) -> Self {
        if let Some(text) = value.as_str() {
            let name = text.to_owned();
            return Module::ItemReference { name }.into();
        }

        if let Some(array) = value.as_array() {
            return Module::Group {
                items : array.iter().map(Item::from_toml_ref).collect(),
                spacing : String::new(),
            }.into();
        }

        Self::from_item_list("<ref>", value)
    }

    pub fn from_item_list(key : &str, value : &toml::Value) -> Self {
        let data = Module::from_toml(value);
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

    pub fn render(&self, parent_ctx : &Render) -> EventSink {
        let mut rv = self.events.clone();

        let format;

        if let Some(mut f) = self.config.as_ref().map(|cfg| Formatting::expand(cfg, &parent_ctx.runtime)) {
            if f.is_boring() {
                let ctx = f.setup_ctx(parent_ctx);
                self.render_inner(&ctx, &mut rv);
                return rv;
            }
            format = f;
        } else {
            self.render_inner(parent_ctx, &mut rv);
            return rv;
        }
        let ctx = format.setup_ctx(parent_ctx);

        let mut outer_clip = ctx.cairo.clip_extents();
        let start_pos = ctx.cairo.get_current_point();
        ctx.cairo.push_group();

        let mut inner_clip = outer_clip;
        inner_clip.0 = start_pos.0;

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
            ctx.cairo.rectangle(inner_clip.0, inner_clip.1, inner_clip.2, inner_clip.3);
            ctx.cairo.clip();
            ctx.cairo.move_to(inner_clip.0, inner_clip.1);
        }

        if let Some(rgba) = format.fg_rgba {
            ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
        }

        self.render_inner(&ctx, &mut rv);

        let inner_end = ctx.cairo.get_current_point();
        let group = ctx.cairo.pop_group();
        ctx.cairo.save();

        let child_render_width = inner_end.0 - inner_clip.0;
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

                    let mut m = ctx.cairo.get_matrix();
                    m.translate(-inner_x_offset, 0.0);
                    group.set_matrix(m);
                }
                _ => { // defaults to left align
                    inner_x_offset = 0.0;
                }
            }
        } else {
            inner_x_offset = 0.0;
        }

        // move the right side of the clip regions leftwards to the actual dimensions
        let shrink_r_width = outer_clip.2 - inner_clip.2;
        if inner_clip.2 > inner_clip.0 + child_render_width {
            inner_clip.2 = inner_clip.0 + child_render_width;
        }
        if outer_clip.2 > inner_clip.2 + shrink_r_width {
            outer_clip.2 = inner_clip.2 + shrink_r_width;
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
            ctx.cairo.set_operator(cairo::Operator::Source);

            if let Some(rgba) = format.bg_rgba {
                ctx.cairo.save();
                ctx.cairo.rectangle(bg_clip.0, bg_clip.1, bg_clip.2 - bg_clip.0, bg_clip.3 - bg_clip.1);
                ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
                ctx.cairo.fill();
                ctx.cairo.restore();
            }

            if let Some(rgba) = format.border_rgba {
                ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
            }

            if let Some(border) = format.border {
                if border.0 == border.1 && border.0 == border.2 && border.0 == border.3 {
                    ctx.cairo.set_line_width(border.0);
                    ctx.cairo.rectangle(bg_clip.0, bg_clip.1, bg_clip.2 - bg_clip.0, bg_clip.3 - bg_clip.1);
                    ctx.cairo.stroke();
                } else {
                    if border.0 > 0.0 {
                        ctx.cairo.set_line_width(border.0);
                        ctx.cairo.move_to(bg_clip.0, bg_clip.1);
                        ctx.cairo.line_to(bg_clip.2, bg_clip.1);
                        ctx.cairo.stroke();
                    }
                    if border.1 > 0.0 {
                        ctx.cairo.set_line_width(border.1);
                        ctx.cairo.move_to(bg_clip.2, bg_clip.1);
                        ctx.cairo.line_to(bg_clip.2, bg_clip.3);
                        ctx.cairo.stroke();
                    }
                    if border.2 > 0.0 {
                        ctx.cairo.set_line_width(border.2);
                        ctx.cairo.move_to(bg_clip.0, bg_clip.3);
                        ctx.cairo.line_to(bg_clip.2, bg_clip.3);
                        ctx.cairo.stroke();
                    }
                    if border.3 > 0.0 {
                        ctx.cairo.set_line_width(border.3);
                        ctx.cairo.move_to(bg_clip.0, bg_clip.1);
                        ctx.cairo.line_to(bg_clip.0, bg_clip.3);
                        ctx.cairo.stroke();
                    }
                }
            }

            ctx.cairo.set_operator(cairo::Operator::Over);
        }
        ctx.cairo.set_source(&group);
        if let Some(alpha) = format.alpha {
            ctx.cairo.paint_with_alpha(alpha as f64 / 65535.0);
        } else {
            ctx.cairo.paint();
        }
        ctx.cairo.restore();
        ctx.cairo.move_to(outer_clip.2, outer_clip.1);

        rv
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
    fn render_inner(&self, ctx : &Render, rv : &mut EventSink) {
        match &self.data {
            Module::ItemReference { name } => {
                match ctx.runtime.items.get(name) {
                    Some(item) => {
                        let ctx = Render {
                            err_name: name,
                            ..*ctx
                        };
                        rv.merge(item.render(&ctx));
                    }
                    None => {
                        error!("Unresolved reference to item {}", name);
                    }
                }
            }
            Module::Group { items, spacing } => {
                if let Some(cond) = self.config.as_ref()
                    .and_then(|c| c.get("condition"))
                    .and_then(|v| v.as_str())
                {
                    if !cond.is_empty() {
                        match ctx.runtime.format(cond) {
                            Ok(v) if v.is_empty() => return,
                            Ok(_) => {}
                            Err(e) => {
                                warn!("Error evaluating condition '{}': {}", cond, e);
                            }
                        }
                    }
                }
                let spacing = ctx.runtime.format(spacing).ok().and_then(|s| s.parse().ok()).unwrap_or(0.0);
                ctx.cairo.rel_move_to(spacing, 0.0);
                for item in items {
                    let x0 = ctx.cairo.get_current_point().0;
                    let mut ev = item.render(ctx);
                    let x1 = ctx.cairo.get_current_point().0;
                    ev.offset_clamp(0.0, x0, x1);
                    rv.merge(ev);
                    ctx.cairo.rel_move_to(spacing, 0.0);
                }
            }
            Module::FocusList { source, items, spacing } => {
                let spacing = ctx.runtime.format(spacing).ok().and_then(|s| s.parse().ok()).unwrap_or(0.0);
                let source = match ctx.runtime.items.get(source) {
                    Some(var) => var,
                    None => return,
                };
                let item_var = match ctx.runtime.items.get("item") {
                    Some(&Item { data : Module::Item { ref value }, .. }) => value,
                    _ => {
                        error!("The 'item' variable was not assignable");
                        return;
                    }
                };
                ctx.cairo.rel_move_to(spacing, 0.0);
                source.data.read_focus_list(ctx.runtime, |focus, item| {
                    item_var.set(Some(item.clone()));
                    let x0 = ctx.cairo.get_current_point().0;
                    let mut ev = if focus {
                        items[1].render(ctx)
                    } else {
                        items[0].render(ctx)
                    };
                    let x1 = ctx.cairo.get_current_point().0;
                    ev.offset_clamp(0.0, x0, x1);
                    for h in &mut ev.handlers {
                        h.item = Some(item.clone());
                    }
                    rv.merge(ev);
                    ctx.cairo.rel_move_to(spacing, 0.0);
                });
                item_var.set(None);
            }
            Module::Bar { items, .. } => {
                let start = ctx.cairo.get_current_point();
                let (clip_x0, clip_y0, clip_x1, _y1) = ctx.cairo.clip_extents();
                let width = clip_x1 - clip_x0;

                let left = &items[0];
                let center = &items[1];
                let right = &items[2];

                ctx.cairo.push_group();
                let mut left_ev = left.render(&ctx);
                let left_size = ctx.cairo.get_current_point();
                let left = ctx.cairo.pop_group();
                left_ev.offset_clamp(0.0, 0.0, left_size.0);
                rv.merge(left_ev);

                ctx.cairo.push_group();
                ctx.cairo.move_to(start.0, start.1);
                let mut cent_ev = center.render(&ctx);
                let cent_size = ctx.cairo.get_current_point();
                let cent = ctx.cairo.pop_group();

                ctx.cairo.push_group();
                ctx.cairo.move_to(start.0, start.1);
                let mut right_ev = right.render(&ctx);
                let right_size = ctx.cairo.get_current_point();
                let right = ctx.cairo.pop_group();

                ctx.cairo.set_source(&left);
                ctx.cairo.paint();

                let mut m = ctx.cairo.get_matrix();
                let right_offset = clip_x1 - right_size.0; // this is negative
                m.translate(-right_offset, 0.0);
                right_ev.offset_clamp(right_offset, right_offset, clip_x1);
                rv.merge(right_ev);
                right.set_matrix(m);
                ctx.cairo.set_source(&right);
                ctx.cairo.paint();

                let max_side = (width - cent_size.0) / 2.0;
                let total_room = width - (left_size.0 + right_size.0 + cent_size.0);
                let cent_offset;
                if total_room < 0.0 {
                    // TODO maybe we should have cropped it?
                    return;
                } else if left_size.0 > max_side {
                    // left side is too long to properly center; put it just to the right of that
                    cent_offset = left_size.0;
                } else if right_size.0 > max_side {
                    // right side is too long to properly center; put it just to the left of that
                    cent_offset = clip_x1 - right_size.0 - cent_size.0;
                } else {
                    // Actually center the center module
                    cent_offset = max_side;
                }
                m = ctx.cairo.get_matrix();
                m.translate(-cent_offset, 0.0);
                cent.set_matrix(m);
                cent_ev.offset_clamp(cent_offset, cent_offset, cent_offset + cent_size.0);
                rv.merge(cent_ev);
                ctx.cairo.set_source(&cent);
                ctx.cairo.paint();

                ctx.cairo.move_to(clip_x1, clip_y0);
            }
            Module::Tray { spacing } => {
                let spacing = ctx.runtime.format(spacing).ok().and_then(|s| s.parse().ok()).unwrap_or(0.0);
                tray::show(ctx, rv, spacing);
            }
            Module::None => {}

            // All other modules are rendered as text
            _ => {
                let markup = self.config.as_ref().and_then(|c| c.get("markup")).and_then(|v| v.as_bool()).unwrap_or(false);
                let text = self.data.read_in(ctx.err_name, "text", &ctx.runtime, |s| s.to_string());
                let tooltip = self.data.read_in(ctx.err_name, "tooltip", &ctx.runtime, |s| s.to_string());
                let layout = pangocairo::create_layout(ctx.cairo).unwrap();
                layout.set_font_description(Some(ctx.font));
                if markup {
                    layout.set_markup(&text);
                } else {
                    layout.set_text(&text);
                }
                let size = layout.get_size();
                let yoff = match ctx.align.vert {
                    Some(f) => {
                        let (_x0, clip_y0, _x1, clip_y1) = ctx.cairo.clip_extents();
                        let extra = clip_y1 - clip_y0 - pango::units_to_double(size.1);
                        if extra >= 0.0 {
                            extra * f
                        } else {
                            debug!("Cannot align text '{}' which exceeds clip bounds", text);
                            0.0
                        }
                    }
                    _ => 0.0,
                };
                let start_pos = ctx.cairo.get_current_point();
                ctx.cairo.rel_move_to(0.0, yoff);
                if let Some(rgba) = ctx.text_stroke {
                    ctx.cairo.save();
                    pangocairo::layout_path(ctx.cairo, &layout);
                    ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
                    ctx.cairo.stroke();
                    ctx.cairo.restore();
                }

                ctx.cairo.move_to(start_pos.0, start_pos.1 + yoff);
                pangocairo::show_layout(ctx.cairo, &layout);
                let width = pango::units_to_double(size.0);
                ctx.cairo.move_to(start_pos.0 + width, start_pos.1);
                if !tooltip.is_empty() {
                    rv.hovers.push((start_pos.0, start_pos.0 + width, PopupDesc::Text {
                        value : tooltip,
                        markup,
                    }));
                }
            }
        }
    }
}

#[derive(Debug,Clone)]
pub enum PopupDesc {
    Text { value : String, markup : bool },
    Tray(tray::TrayPopup),
}

impl PopupDesc {
    pub fn get_size(&mut self) -> (i32, i32) {
        match self {
            PopupDesc::Text { value, markup } => {
                let tmp = cairo::RecordingSurface::create(cairo::Content::ColorAlpha, None).unwrap();
                let ctx = cairo::Context::new(&tmp);
                let layout = pangocairo::create_layout(&ctx).unwrap();
                if *markup {
                    layout.set_markup(value);
                } else {
                    layout.set_text(value);
                }
                let size = layout.get_size();
                (pango::units_to_double(size.0) as i32 + 4, pango::units_to_double(size.1) as i32 + 4)
            }
            PopupDesc::Tray(tray) => tray.get_size(),
        }
    }
    pub fn render(&mut self, ctx : &cairo::Context, runtime : &Runtime) -> (i32, i32) {
        match self {
            PopupDesc::Text { value, markup } => {
                ctx.move_to(2.0, 2.0);
                let layout = pangocairo::create_layout(&ctx).unwrap();
                if *markup {
                    layout.set_markup(value);
                } else {
                    layout.set_text(value);
                }
                pangocairo::show_layout(&ctx, &layout);
                let size = layout.get_size();
                (pango::units_to_double(size.0) as i32 + 4, pango::units_to_double(size.1) as i32 + 4)
            }
            PopupDesc::Tray(tray) => tray.render(ctx, runtime),
        }
    }

    pub fn button(&mut self, x : f64, y : f64, button : u32, runtime : &mut Runtime) {
        match self {
            PopupDesc::Text { .. } => {
            }
            PopupDesc::Tray(tray) => tray.button(x, y, button, runtime),
        }
    }
}
