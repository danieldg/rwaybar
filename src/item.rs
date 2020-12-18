use json::JsonValue;
use crate::data::Action;
use crate::state::Runtime;
use log::{warn,error};

/// State available to an [Item] render function
#[derive(Clone,Copy)]
pub struct Render<'a> {
    pub cairo : &'a cairo::Context,
    pub font : &'a pango::FontDescription,
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

#[derive(Debug,Copy,Clone,PartialEq,Eq)]
pub enum Align {
    Left,
    Right,
    Center
}

impl Align {
    pub fn from_str(value : String) -> Option<Self> {
        match value.as_str() {
            "left" => Some(Align::Left),
            "right" => Some(Align::Right),
            "center" => Some(Align::Center),
            x => {
                error!("Unknown alignment {}", x);
                None
            }
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
    min_width : Option<Width>,
    max_width : Option<Width>,
    align : Option<Align>,
    alpha : Option<u16>,
}

impl Formatting {
    fn filter_json(value : &JsonValue) -> json::object::Object {
        let mut rv = json::object::Object::new();
        if let JsonValue::Object(src) = value {
            for key in &["font", "min_width", "max_width", "align", "alpha", "bg", "bg_alpha", "fg", "fg_alpha"] {
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

        let font = get("font");
        let min_width = get("min_width").and_then(Width::from_str);
        let max_width = get("max_width").and_then(Width::from_str);
        let align = get("align").and_then(Align::from_str);
        let alpha = get_f64("alpha").map(|f| {
            f64::min(65535.0, f64::max(0.0, f * 65535.0)) as u16
        });

        let bg_rgba = if let Some(bg_str) = get("bg") {
            let mut bg = pango_sys::PangoColor {
                red : 0, blue : 0, green : 0
            };
            let cstr = std::ffi::CString::new(bg_str).unwrap();
            unsafe {
                pango_sys::pango_color_parse(&mut bg, cstr.as_ptr());
            }
            let alpha_f = get_f64("bg_alpha").unwrap_or(1.0) * 65535.0;
            let bga = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;
            Some((bg.red, bg.green, bg.blue, bga))
        } else {
            None
        };

        let fg_rgba = if let Some(fg_str) = get("fg") {
            let mut fg = pango_sys::PangoColor {
                red : 0, blue : 0, green : 0
            };
            let cstr = std::ffi::CString::new(fg_str).unwrap();
            unsafe {
                pango_sys::pango_color_parse(&mut fg, cstr.as_ptr());
            }
            let alpha_f = get_f64("fg_alpha").unwrap_or(1.0) * 65535.0;
            let fga = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;
            Some((fg.red, fg.green, fg.blue, fga))
        } else {
            None
        };

        Self {
            font,
            fg_rgba,
            bg_rgba,
            min_width,
            max_width,
            align,
            alpha,
        }
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
    Bar {
        // always three items: left, center, right
        items : Box<[Item;3]>,
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
                pangocairo::show_layout(ctx.cairo, &layout);
                ctx.cairo.rel_move_to(pango::units_to_double(size.0), 0.0);
            }
            Contents::Group { items, spacing } => {
                ctx.cairo.rel_move_to(*spacing, 0.0);
                for item in items {
                    let x0 = ctx.cairo.get_current_point().0;
                    let mut ev = item.render(ctx);
                    let x1 = ctx.cairo.get_current_point().0;
                    ev.offset_clamp(x0, x1);
                    rv.merge(ev);
                    ctx.cairo.rel_move_to(*spacing, 0.0);
                }
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
                left_ev.offset_clamp(0.0, left_size.0);
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
                right_ev.offset_clamp(-m.x0, clip_x1);
                rv.merge(right_ev);
                right.set_matrix(m);
                ctx.cairo.set_source(&right);
                ctx.cairo.paint();

                let max_side = (width - cent_size.0) / 2.0;
                let total_room = width - (left_size.0 + right_size.0 + cent_size.0);
                if left_size.0 < max_side && right_size.0 < max_side {
                    // Actually center the center module
                    m.x0 = -max_side;
                } else if total_room >= 0.0 {
                    // At least it will fit somewhere
                    m.x0 = -(left_size.0 + total_room / 2.0);
                } else {
                    // TODO maybe we should have cropped it?
                    return;
                }
                cent.set_matrix(m);
                cent_ev.offset_clamp(-m.x0, cent_size.0 - m.x0);
                rv.merge(cent_ev);
                ctx.cairo.set_source(&cent);
                ctx.cairo.paint();
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
            target : Action::from_json(value)
        })
    }

    pub fn merge(&mut self, sink : Self) {
        self.handlers.extend(sink.handlers);
    }

    pub fn offset_clamp(&mut self, offset : f64, max : f64) {
        for h in &mut self.handlers {
            h.x_min += offset;
            h.x_max += offset;
            if h.x_min > max {
                h.x_min = max;
            }
            if h.x_max > max {
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
            h.target.invoke(runtime);
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

        let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.cairo.clip_extents();
        let start_pos = ctx.cairo.get_current_point();
        ctx.cairo.push_group();
        match format.max_width {
            None => {}
            Some(Width::Pixels(n)) => {
                ctx.cairo.rectangle(start_pos.0, clip_y0, start_pos.0 + n, clip_y1);
                ctx.cairo.clip();
                ctx.cairo.move_to(start_pos.0, start_pos.1);
            }
            Some(Width::Fraction(f)) => {
                let parent_width = clip_x1 - clip_x0;
                ctx.cairo.rectangle(start_pos.0, clip_y0, start_pos.0 + parent_width * f, clip_y1);
                ctx.cairo.move_to(start_pos.0, start_pos.1);
            }
        }

        if let Some(rgba) = format.fg_rgba {
            ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
        }

        if let Some(font) = format.font {
            let font = pango::FontDescription::from_string(&font);
            let ctx = Render {
                cairo : &*ctx.cairo,
                runtime : &*ctx.runtime,
                font : &font,
            };
            self.contents.render(&ctx, &mut rv);
        } else {
            self.contents.render(ctx, &mut rv);
        }

        let mut end_pos = ctx.cairo.get_current_point();
        let group = ctx.cairo.pop_group();
        ctx.cairo.save();

        let base_width = end_pos.0 - start_pos.0;
        let mut min_width = match format.min_width {
            None => 0.0,
            Some(Width::Pixels(n)) => n,
            Some(Width::Fraction(f)) => f * (clip_x1 - clip_x0),
        };
        if min_width > clip_x1 - start_pos.0 {
            min_width = clip_x1 - start_pos.0;
        }

        if base_width < min_width {
            let expand = min_width - base_width;
            match format.align.unwrap_or(Align::Left) {
                Align::Left => {}
                Align::Right => {
                    let mut m = cairo::Matrix::identity();
                    m.x0 = -expand;
                    group.set_matrix(m);
                }
                Align::Center => {
                    let mut m = cairo::Matrix::identity();
                    m.x0 = -expand / 2.0;
                    group.set_matrix(m);
                }
            }
            end_pos.0 = start_pos.0 + min_width;
        }

        if let Some(rgba) = format.bg_rgba {
            ctx.cairo.set_operator(cairo::Operator::Source);
            ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
            ctx.cairo.rectangle(start_pos.0, clip_y0, end_pos.0 - start_pos.0, clip_y1);
            ctx.cairo.fill();
            ctx.cairo.set_operator(cairo::Operator::Over);
        }
        ctx.cairo.set_source(&group);
        if let Some(alpha) = format.alpha {
            ctx.cairo.paint_with_alpha(alpha as f64 / 65535.0);
        } else {
            ctx.cairo.paint();
        }
        ctx.cairo.restore();
        ctx.cairo.move_to(end_pos.0, end_pos.1);

        rv
    }
}
