use json::JsonValue;
use crate::data::Action;
use crate::state::Runtime;
use crate::tray;
use log::{debug,warn,error};

/// State available to an [Item] render function
#[derive(Clone,Copy)]
pub struct Render<'a> {
    pub cairo : &'a cairo::Context,
    pub font : &'a pango::FontDescription,
    pub align : Align,
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
    fmt_config : json::object::Object,
    contents : Contents,
    events : EventSink,
}

/// Formatting information (colors, width, etc) for an [Item]
#[derive(Debug,Clone,Default,PartialEq)]
struct Formatting {
    font : Option<String>,
    fg_rgba : Option<(u16, u16, u16, u16)>,
    bg_rgba : Option<(u16, u16, u16, u16)>,
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
    fn filter_json(value : &JsonValue) -> json::object::Object {
        let mut rv = json::object::Object::new();
        if let JsonValue::Object(src) = value {
            for key in &["font", "min-width", "max-width", "align", "halign", "valign", "margin", "padding", "alpha", "bg", "bg-alpha", "fg", "fg-alpha", "border", "border-color", "border-alpha"] {
                if let Some(v) = src.get(key) {
                    rv.insert(key, v.clone());
                }
            }
        }
        rv
    }

    fn expand(obj : &json::object::Object, runtime : &Runtime) -> Self {
        let get = |key| {
            obj.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime.format(&fmt).or_else(|e| {
                    warn!("Error expanding '{}' when rendering: {}", fmt, e);
                    Err(())
                }).ok(),
                None => Some(v.dump())
            })
        };

        let get_f64 = |key| {
            obj.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime.format(&fmt).or_else(|e| {
                    warn!("Error expanding '{}' when rendering: {}", fmt, e);
                    Err(())
                }).ok().and_then(|v| v.parse().ok()),
                None => v.as_f64(),
            })
        };
        let mut align = Align {
            horiz : get("halign").and_then(Align::parse_hv),
            vert : get("valign").and_then(Align::parse_hv),
        };
        align.from_name(get("align"));

        let font = get("font");
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

    fn is_boring(&self) -> bool {
        *self == Self::default()
    }
}

/// Type-specific part of an [Item]
#[derive(Debug)]
enum Contents {
    Reference { name : String },
    Text {
        text : String,
        markup : bool
    },
    Group {
        items : Vec<Item>,
        spacing : f64,
        // TODO crop ordering: allow specific items to be cropped first
        // TODO use min-width to force earlier cropping
    },
    FocusList {
        source : String,
        // always two items: non-focused, focused
        items : Box<[Item;2]>,
        spacing : f64,
    },
    Bar {
        // always three items: left, center, right
        items : Box<[Item;3]>,
    },
    Tray {
        spacing : f64,
    },
    Null,
}
impl Contents {
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
    fn render(&self, ctx : &Render, rv : &mut EventSink) {
        match self {
            Contents::Reference { name } => {
                match ctx.runtime.items.get(name) {
                    Some(item) => {
                        rv.merge(item.render(ctx));
                    }
                    None => {
                        error!("Unresolved reference to item {}", name);
                    }
                }
            }
            Contents::Text { text, markup } => {
                let text = ctx.runtime.format(&text)
                    .unwrap_or_else(|e| {
                        warn!("Error formatting text: {}", e);
                        "Error".into()
                    });
                let layout = pangocairo::create_layout(ctx.cairo).unwrap();
                layout.set_font_description(Some(ctx.font));
                if *markup {
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
                ctx.cairo.rel_move_to(0.0, yoff);
                pangocairo::show_layout(ctx.cairo, &layout);
                ctx.cairo.rel_move_to(pango::units_to_double(size.0), -yoff);
            }
            Contents::Group { items, spacing } => {
                ctx.cairo.rel_move_to(*spacing, 0.0);
                for item in items {
                    let x0 = ctx.cairo.get_current_point().0;
                    let mut ev = item.render(ctx);
                    let x1 = ctx.cairo.get_current_point().0;
                    ev.offset_clamp(0.0, x0, x1);
                    rv.merge(ev);
                    ctx.cairo.rel_move_to(*spacing, 0.0);
                }
            }
            Contents::FocusList { source, items, spacing } => {
                let source = match ctx.runtime.vars.get(source) {
                    Some(var) => var,
                    None => return,
                };
                let item_var = ctx.runtime.vars.get("item").unwrap();
                ctx.cairo.rel_move_to(*spacing, 0.0);
                source.read_focus_list(|item, focus| {
                    item_var.set_current_item(Some(item.to_owned()));
                    let x0 = ctx.cairo.get_current_point().0;
                    let mut ev = if focus {
                        items[1].render(ctx)
                    } else {
                        items[0].render(ctx)
                    };
                    let x1 = ctx.cairo.get_current_point().0;
                    ev.offset_clamp(0.0, x0, x1);
                    for h in &mut ev.handlers {
                        h.item = item.to_owned();
                    }
                    rv.merge(ev);
                    ctx.cairo.rel_move_to(*spacing, 0.0);
                });
                item_var.set_current_item(None);
            }
            Contents::Bar { items } => {
                let start = ctx.cairo.get_current_point();
                let (clip_x0, _y0, clip_x1, _y1) = ctx.cairo.clip_extents();
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

                let mut m = cairo::Matrix::identity();
                m.x0 = right_size.0 - clip_x1;
                right_ev.offset_clamp(-m.x0, -m.x0, clip_x1);
                rv.merge(right_ev);
                right.set_matrix(m);
                ctx.cairo.set_source(&right);
                ctx.cairo.paint();

                let max_side = (width - cent_size.0) / 2.0;
                let total_room = width - (left_size.0 + right_size.0 + cent_size.0);
                if total_room < 0.0 {
                    // TODO maybe we should have cropped it?
                    return;
                } else if left_size.0 > max_side {
                    // left side is too long to properly center; put it just to the right of that
                    m.x0 = -left_size.0;
                } else if right_size.0 > max_side {
                    // right side is too long to properly center; put it just to the left of that
                    m.x0 = right_size.0 + cent_size.0 - clip_x1;
                } else {
                    // Actually center the center module
                    m.x0 = -max_side;
                }
                cent.set_matrix(m);
                cent_ev.offset_clamp(-m.x0, -m.x0, cent_size.0 - m.x0);
                rv.merge(cent_ev);
                ctx.cairo.set_source(&cent);
                ctx.cairo.paint();
            }
            Contents::Tray { spacing } => {
                tray::show(ctx, rv, *spacing);
            }
            Contents::Null => {}
        }
    }
}

/// A single click action associated with the area that activates it
#[derive(Debug,Clone)]
struct EventListener {
    x_min : f64,
    x_max : f64,
    buttons : u32,
    item : String,
    target : Action,
}

/// A list of [EventListener]s
#[derive(Debug,Default,Clone)]
pub struct EventSink {
    handlers : Vec<EventListener>
}

impl EventSink {
    fn from_json(value : &JsonValue) -> Self {
        let mut sink = EventSink::default();
        sink.add_click(&value["on-click"], 1 << 0);
        sink.add_click(&value["on-click-right"], 1 << 1);
        sink.add_click(&value["on-click-middle"], 1 << 2);
        sink.add_click(&value["on-click-back"], 1 << 3);
        sink.add_click(&value["on-click-backward"], 1 << 3);
        sink.add_click(&value["on-click-forward"], 1 << 4);
        sink.add_click(&value["on-scroll-up"], 1 << 5);
        sink.add_click(&value["on-scroll-down"], 1 << 6);
        sink.add_click(&value["on-vscroll"], 3 << 5);
        sink.add_click(&value["on-scroll-left"], 1 << 7);
        sink.add_click(&value["on-scroll-right"], 1 << 8);
        sink.add_click(&value["on-hscroll"], 3 << 7);
        sink.add_click(&value["on-scroll"], 15 << 5);
        sink
    }

    fn add_click(&mut self, value : &JsonValue, buttons : u32) {
        if value.is_null() {
            return;
        }
        self.handlers.push(EventListener {
            x_min : 0.0,
            x_max : 1e20,
            buttons,
            item : String::new(),
            target : Action::from_json(value)
        })
    }

    pub fn from_tray(owner : String, path : String) -> Self {
        let mut sink = EventSink::default();
        sink.handlers.push(EventListener {
            x_min : -1e20,
            x_max : 1e20,
            buttons : 7 | (15 << 5),
            item : String::new(),
            target : Action::from_tray(owner, path),
        });
        sink
    }

    pub fn merge(&mut self, sink : Self) {
        self.handlers.extend(sink.handlers);
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
            if h.item.is_empty() {
                h.target.invoke(runtime, button);
            } else {
                runtime.vars.get("item").unwrap().set_current_item(Some(h.item.clone()));
                h.target.invoke(runtime, button);
                runtime.vars.get("item").unwrap().set_current_item(None);
            }
        }
    }
}

impl From<Contents> for Item {
    fn from(contents : Contents) -> Self {
        Self {
            fmt_config : json::object::Object::new(),
            events : EventSink::default(),
            contents
        }
    }
}

impl Item {
    pub fn new_bar(cfg : &JsonValue) -> Self {
        let left = Item::from_json_ref(&cfg["left"]);
        let right = Item::from_json_ref(&cfg["right"]);
        let center = Item::from_json_ref(&cfg["center"]);

        Item {
            fmt_config : Formatting::filter_json(cfg),
            contents : Contents::Bar { items : Box::new([left, center, right]) },
            events : EventSink::from_json(cfg),
        }
    }

    pub fn from_json_ref(value : &JsonValue) -> Self {
        if let Some(text) = value.as_str() {
            let name = text.to_owned();
            return Contents::Reference { name }.into();
        }

        if value.is_null() {
            return Contents::Null.into();
        }

        if value.is_array() {
            return Contents::Group {
                items : value.members().map(Item::from_json_ref).collect(),
                spacing : 0.0
            }.into();
        }

        Self::from_json_i(value)
    }

    pub fn from_json_txt(value : &JsonValue) -> Self {
        if let Some(text) = value.as_str() {
            let text = text.to_owned();
            return Contents::Text { text, markup : false }.into();
        }

        Self::from_json_i(value)
    }

    fn from_json_i(value : &JsonValue) -> Self {
        if !value.is_object() {
            error!("Items must be JSON objects: {}", value);
            return Contents::Null.into();
        }
        match value["type"].as_str() {
            Some("group") => {
                let spacing = value["spacing"].as_f64().unwrap_or(0.0);

                let items = value["items"].members().map(Item::from_json_ref).collect();

                Item {
                    fmt_config : Formatting::filter_json(value),
                    contents : Contents::Group {
                        items,
                        spacing,
                    },
                    events : EventSink::from_json(value),
                }
            }
            Some("focus-list") => {
                let source = match value["source"].as_str() {
                    Some(s) => s.into(),
                    None => {
                        error!("A source is required for focus-list");
                        return Contents::Null.into();
                    }
                };
                let spacing = value["spacing"].as_f64().unwrap_or(0.0);
                let item = Item::from_json_ref(&value["item"]);
                let fitem = if value["focused-item"].is_null() {
                    Item::from_json_ref(&value["item"])
                } else {
                    Item::from_json_ref(&value["focused-item"])
                };

                let items = Box::new([item, fitem]);
                Item {
                    fmt_config : Formatting::filter_json(value),
                    contents : Contents::FocusList {
                        source,
                        items,
                        spacing,
                    },
                    events : EventSink::from_json(value),
                }
            }
            Some("tray") => {
                let spacing = value["spacing"].as_f64().unwrap_or(0.0);
                Item {
                    fmt_config : Formatting::filter_json(value),
                    contents : Contents::Tray {
                        spacing,
                    },
                    events : EventSink::from_json(value),
                }
            }
            Some("text") |
            None => {
                let text = value["format"].as_str()
                    .unwrap_or_else(|| {
                        error!("Text items require a 'format' value");
                        ""
                    }).to_owned();
                let markup = value["markup"].as_bool().unwrap_or(false);
                Item {
                    fmt_config : Formatting::filter_json(value),
                    contents : Contents::Text { text, markup },
                    events : EventSink::from_json(value),
                }
            }
            Some(tipe) => {
                error!("Unknown item type: {}", tipe);
                Contents::Null.into()
            }
        }
    }

    pub fn render(&self, ctx : &Render) -> EventSink {
        let mut rv = self.events.clone();

        let format = Formatting::expand(&self.fmt_config, ctx.runtime);

        if format.is_boring() {
            self.contents.render(ctx, &mut rv);
            return rv;
        }

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

        {
            let desc;
            let mut child_ctx = Render {
                cairo : &*ctx.cairo,
                runtime : &*ctx.runtime,
                align : ctx.align.merge(&format.align),
                font : &*ctx.font,
            };

            if let Some(font) = format.font {
                desc = pango::FontDescription::from_string(&font);
                child_ctx.font = &desc;
            }
            self.contents.render(&child_ctx, &mut rv);
        }

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
                    let mut m = cairo::Matrix::identity();
                    m.x0 = -inner_x_offset;
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
}
