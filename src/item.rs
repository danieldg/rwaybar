use json::JsonValue;
use crate::data::Action;
use crate::state::Runtime;
use log::{warn,error};

#[derive(Clone,Copy)]
pub struct Render<'a> {
    pub cairo : &'a cairo::Context,
    pub font : &'a pango::FontDescription,
    pub runtime : &'a Runtime,
}

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum Width {
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f64),
    /// Some number of pixels
    Pixels(f64),
}

impl Width {
    pub fn from_json(value : &JsonValue) -> Option<Self> {
        if let Some(w) = value.as_f64() {
            if w > 1.0 {
                return Some(Width::Pixels(w));
            } else {
                return Some(Width::Fraction(w));
            }
        }

        if let Some(s) = value.as_str() {
            if let Some(pos) = s.find('%') {
                if let Ok(w) = s[..pos].parse::<f64>() {
                    return Some(Width::Fraction(w / 100.0));
                }
            }
        }

        None
    }
}

#[derive(Debug,Copy,Clone,PartialEq,Eq)]
pub enum Align {
    Left,
    Right,
    Center
}

impl Align {
    pub fn from_json(value : &JsonValue) -> Option<Self> {
        match value.as_str() {
            Some("left") => Some(Align::Left),
            Some("right") => Some(Align::Right),
            Some("center") => Some(Align::Center),
            Some(x) => {
                error!("Unknown alignment {}", x);
                None
            }
            None => None
        }
    }
}

#[derive(Debug)]
pub struct Item {
    format : Formatting,
    contents : Contents,
    events : EventSink,
}

#[derive(Debug,Clone,Default,PartialEq)]
struct Formatting {
    fg_rgba : Option<(u16, u16, u16, u16)>,
    bg_rgba : Option<(u16, u16, u16, u16)>,
    min_width : Option<Width>,
    max_width : Option<Width>,
    align : Option<Align>,
    alpha : Option<u16>,
}

impl Formatting {
    fn from_json(value : &JsonValue) -> Self {
        let mut bg_rgba = None;
        let mut fg_rgba = None;
        let min_width = Width::from_json(&value["min_width"]);
        let max_width = Width::from_json(&value["max_width"]);
        let align = Align::from_json(&value["align"]);
        let alpha = value["alpha"].as_f64().map(|f| {
            f64::min(65535.0, f64::max(0.0, f * 65535.0)) as u16
        });

        if let Some(bg_str) = value["bg"].as_str() {
            let mut bg = pango_sys::PangoColor {
                red : 0, blue : 0, green : 0
            };
            let cstr = std::ffi::CString::new(bg_str).unwrap();
            unsafe {
                pango_sys::pango_color_parse(&mut bg, cstr.as_ptr());
            }
            let alpha_f = value["bg_alpha"].as_f64().unwrap_or(1.0) * 65535.0;
            let bga = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;
            bg_rgba = Some((bg.red, bg.green, bg.blue, bga));
        }

        if let Some(fg_str) = value["fg"].as_str() {
            let mut fg = pango_sys::PangoColor {
                red : 0, blue : 0, green : 0
            };
            let cstr = std::ffi::CString::new(fg_str).unwrap();
            unsafe {
                pango_sys::pango_color_parse(&mut fg, cstr.as_ptr());
            }
            let alpha_f = value["fg_alpha"].as_f64().unwrap_or(1.0) * 65535.0;
            let fga = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;
            fg_rgba = Some((fg.red, fg.green, fg.blue, fga))
        }


        Self {
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
                let text = strfmt::strfmt(&text, &ctx.runtime.vars)
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

#[derive(Debug,Clone)]
struct EventListener {
    x_min : f64,
    x_max : f64,
    buttons : u32,
    target : Action,
}

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
            format : Formatting::default(),
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
            format : Formatting::from_json(cfg),
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
                    format : Formatting::from_json(value),
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
                    format : Formatting::from_json(value),
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

        if self.format.is_boring() {
            self.contents.render(ctx, &mut rv);
            return rv;
        }

        let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.cairo.clip_extents();
        let start_pos = ctx.cairo.get_current_point();
        ctx.cairo.push_group();
        match self.format.max_width {
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

        if let Some(rgba) = self.format.fg_rgba {
            ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
        }

        self.contents.render(ctx, &mut rv);

        let mut end_pos = ctx.cairo.get_current_point();
        let group = ctx.cairo.pop_group();
        ctx.cairo.save();

        let base_width = end_pos.0 - start_pos.0;
        let mut min_width = match self.format.min_width {
            None => 0.0,
            Some(Width::Pixels(n)) => n,
            Some(Width::Fraction(f)) => f * (clip_x1 - clip_x0),
        };
        if min_width > clip_x1 - start_pos.0 {
            min_width = clip_x1 - start_pos.0;
        }

        if base_width < min_width {
            let expand = min_width - base_width;
            match self.format.align.unwrap_or(Align::Left) {
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

        if let Some(rgba) = self.format.bg_rgba {
            ctx.cairo.set_operator(cairo::Operator::Source);
            ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
            ctx.cairo.rectangle(start_pos.0, clip_y0, end_pos.0 - start_pos.0, clip_y1);
            ctx.cairo.fill();
            ctx.cairo.set_operator(cairo::Operator::Over);
        }
        ctx.cairo.set_source(&group);
        if let Some(alpha) = self.format.alpha {
            ctx.cairo.paint_with_alpha(alpha as f64 / 65535.0);
        } else {
            ctx.cairo.paint();
        }
        ctx.cairo.restore();
        ctx.cairo.move_to(end_pos.0, end_pos.1);

        rv
    }
}
