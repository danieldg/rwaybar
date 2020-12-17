use json::JsonValue;
use crate::data::Action;
use crate::state::Runtime;
use log::error;

/// XXX hack for working around pango bugs
struct Nop;
impl<T> glib::boxed::BoxedMemoryManager<T> for Nop {
    unsafe fn copy(ptr: *const T) -> *mut T { ptr as _ }
    unsafe fn free(_: *mut T) {}
    unsafe fn init(_: *mut T) {}
    unsafe fn clear(_: *mut T) {}
}

pub struct Render<'a> {
    pub cairo : cairo::Context,
    pub pango : pango::Context,
    pub runtime : &'a Runtime,
}

#[derive(Debug)]
pub enum Width {
    /// No width restrictions
    None,
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f64),
    /// Some number of pixels
    Pixels(f64),
}

impl Width {
    pub fn from_json(value : &JsonValue) -> Self {
        if let Some(w) = value.as_f64() {
            if w > 1.0 {
                return Width::Pixels(w);
            } else {
                return Width::Fraction(w);
            }
        }

        if let Some(s) = value.as_str() {
            if let Some(pos) = s.find('%') {
                if let Ok(w) = s[..pos].parse::<f64>() {
                    return Width::Fraction(w / 100.0);
                }
            }
        }

        Width::None
    }
}

#[derive(Debug)]
pub enum Align {
    Left,
    Right,
    Center
}

impl Align {
    pub fn from_json(value : &JsonValue) -> Self {
        match value.as_str() {
            None |
            Some("left") => Align::Left,
            Some("right") => Align::Right,
            Some("center") => Align::Center,
            Some(x) => {
                error!("Unknown alignment {}, defaulting to left", x);
                Align::Left
            }
        }
    }
}

#[derive(Debug)]
pub struct Item {
    format : Formatting,
    contents : Contents,
    events : EventSink,
}

#[derive(Debug,Default,Clone)]
struct Formatting {
    fg_rgba : Option<(u16, u16, u16, u16)>,
    bg_rgba : Option<(u16, u16, u16, u16)>,
}

impl Formatting {
    fn from_json(value : &JsonValue) -> Self {
        let mut bg_rgba = None;
        let mut fg_rgba = None;

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
        }
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
        min_width : Width,
        max_width : Width,
        spacing : f64,
        align : Align,
        alpha : u16,
    },
    Null,
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
    pub fn from_json_ref(value : &JsonValue) -> Self {
        if let Some(text) = value.as_str() {
            let name = text.to_owned();
            return Contents::Reference { name }.into();
        }

        if value.is_null() {
            return Contents::Null.into();
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
        match value["type"].as_str() {
            Some("group") => {
                let spacing = value["spacing"].as_f64().unwrap_or(0.0);
                let min_width = Width::from_json(&value["min_width"]);
                let max_width = Width::from_json(&value["max_width"]);
                let align = Align::from_json(&value["align"]);

                let alpha_f = value["alpha"].as_f64().unwrap_or(1.0) * 65535.0;
                let alpha = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;

                let items = value["items"].members().map(Item::from_json_ref).collect();

                Item {
                    format : Formatting::from_json(value),
                    contents : Contents::Group {
                        items,
                        spacing,
                        align,
                        min_width,
                        max_width,
                        alpha,
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
        let mut did_save = false;
        if let Some(rgba) = self.format.fg_rgba {
            ctx.cairo.save();
            did_save = true;
            ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
        }

        let mut rv = self.events.clone();

        match &self.contents {
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
            Contents::Text { text, markup : false } => {
                let text = strfmt::strfmt(&text, &ctx.runtime.vars)
                    .unwrap_or_else(|e| {
                        error!("Error formatting text: {}", e);
                        "Error".into()
                    });
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
            Contents::Text { text, markup : true } => {
                let text = strfmt::strfmt(&text, &ctx.runtime.vars)
                    .unwrap_or_else(|e| {
                        error!("Error formatting text: {}", e);
                        "Error".into()
                    });
                let (attrs, s) = match pango::parse_markup(&text, '\x01') {
                    Ok((a,s,_)) => (a,s),
                    Err(e) => {
                        error!("Error formatting text (markup): {}", e);
                        (pango::AttrList::new(), "Error".into())
                    }
                };
                let mut i = attrs.get_iterator().unwrap();
                // Note: GString vs String is why these don't combine

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
            Contents::Group { items, spacing, align, min_width, max_width, alpha } => {
                let (clip_x0, clip_y0, clip_x1, clip_y1) = ctx.cairo.clip_extents();
                let start_pos = ctx.cairo.get_current_point();
                ctx.cairo.push_group();
                match max_width {
                    Width::None => {}
                    &Width::Pixels(n) => {
                        ctx.cairo.rectangle(start_pos.0, clip_y0, start_pos.0 + n, clip_y1);
                        ctx.cairo.clip();
                        ctx.cairo.move_to(start_pos.0, start_pos.1);
                    }
                    &Width::Fraction(f) => {
                        let parent_width = clip_x1 - clip_x0;
                        ctx.cairo.rectangle(start_pos.0, clip_y0, start_pos.0 + parent_width * f, clip_y1);
                        ctx.cairo.move_to(start_pos.0, start_pos.1);
                    }
                }

                ctx.cairo.rel_move_to(*spacing, 0.0);
                for item in items {
                    let x0 = ctx.cairo.get_current_point().0;
                    let mut ev = item.render(ctx);
                    let x1 = ctx.cairo.get_current_point().0;
                    ev.offset_clamp(x0, x1);
                    rv.merge(ev);
                    ctx.cairo.rel_move_to(*spacing, 0.0);
                }
                let mut end_pos = ctx.cairo.get_current_point();
                let group = ctx.cairo.pop_group();
                ctx.cairo.save();

                let base_width = end_pos.0 - start_pos.0;
                let mut min_width = match min_width {
                    Width::None => 0.0,
                    &Width::Pixels(n) => n,
                    &Width::Fraction(f) => f * (clip_x1 - clip_x0),
                };
                if min_width > clip_x1 - start_pos.0 {
                    min_width = clip_x1 - start_pos.0;
                }

                if base_width < min_width {
                    let expand = min_width - base_width;
                    match align {
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
                    ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
                    ctx.cairo.rectangle(start_pos.0, clip_y0, end_pos.0 - start_pos.0, clip_y1);
                    ctx.cairo.fill();
                }
                ctx.cairo.set_source(&group);
                ctx.cairo.paint_with_alpha(*alpha as f64 / 65535.0);
                ctx.cairo.restore();
                ctx.cairo.move_to(end_pos.0, end_pos.1);
            }
            Contents::Null => {}
        }
        if did_save {
            let pos = ctx.cairo.get_current_point();
            ctx.cairo.restore();
            ctx.cairo.move_to(pos.0, pos.1);
        }
        rv
    }
}
