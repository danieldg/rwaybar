use std::collections::HashMap;
use json::JsonValue;

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
    pub data : &'a HashMap<String, String>,
    pub items : &'a HashMap<String, Item>,
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
            _ => panic!("Unknown alignment"),
        }
    }
}

#[derive(Debug)]
pub enum Item {
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
        fg_rgba : Option<(u16, u16, u16, u16)>,
        bg_rgba : Option<(u16, u16, u16, u16)>,
        alpha : u16,
    },
    List(Vec<Item>),
    Null,
}

impl Item {
    pub fn from_json_ref(value : &JsonValue) -> Self {
        if let Some(text) = value.as_str() {
            let name = text.to_owned();
            return Item::Reference { name };
        }

        if value.is_null() {
            return Item::Null;
        }

        Self::from_json_i(value)
    }

    pub fn from_json_txt(value : &JsonValue) -> Self {
        if let Some(text) = value.as_str() {
            let text = text.to_owned();
            return Item::Text { text, markup : false };
        }

        Self::from_json_i(value)
    }

    fn from_json_i(value : &JsonValue) -> Self {
        if value.is_array() {
            let items = value.members().map(Item::from_json_ref).collect();
            return Item::List(items);
        }

        match value["type"].as_str() {
            Some("group") => {
                let mut bg_rgba = None;
                let mut fg_rgba = None;

                let spacing = value["spacing"].as_f64().unwrap_or(0.0);

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

                let min_width = Width::from_json(&value["min_width"]);
                let max_width = Width::from_json(&value["max_width"]);
                let align = Align::from_json(&value["align"]);

                let alpha_f = value["alpha"].as_f64().unwrap_or(1.0) * 65535.0;
                let alpha = f64::min(65535.0, f64::max(0.0, alpha_f)) as u16;

                let items = value["items"].members().map(Item::from_json_ref).collect();

                Item::Group {
                    items,
                    spacing,
                    align,
                    min_width,
                    max_width,
                    fg_rgba,
                    bg_rgba,
                    alpha,
                }
            }
            Some("text") |
            None => {
                if let Some(text) = value["format"].as_str() {
                    let text = text.to_owned();
                    let markup = value["markup"].as_bool().unwrap_or(false);
                    // TODO more attributes?
                    Item::Text { text, markup }
                } else {
                    panic!("Text nodes require format");
                }
            }
            Some(tipe) => {
                panic!("Unknown item type: {}", tipe);
            }
        }
    }

    pub fn render(&self, ctx : &Render) {
        match self {
            Item::Reference { name } => {
                match ctx.items.get(name) {
                    Some(item) => item.render(ctx),
                    None => panic!("Unresolved reference to item {}", name),
                }
            }
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
            Item::Group { items, spacing, align, min_width, max_width, fg_rgba, bg_rgba, alpha } => {
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

                if let Some(rgba) = fg_rgba {
                    ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
                }
                ctx.cairo.rel_move_to(*spacing, 0.0);
                for item in items {
                    item.render(ctx);
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
                            m.x0 = dbg!(-expand);
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

                if let Some(rgba) = bg_rgba {
                    ctx.cairo.set_source_rgba(rgba.0 as f64 / 65535.0, rgba.1 as f64 / 65535.0, rgba.2 as f64 / 65535.0, rgba.3 as f64 / 65535.0);
                    ctx.cairo.rectangle(start_pos.0, clip_y0, end_pos.0 - start_pos.0, clip_y1);
                    ctx.cairo.fill();
                }
                ctx.cairo.set_source(&group);
                ctx.cairo.paint_with_alpha(*alpha as f64 / 65535.0);
                ctx.cairo.restore();
                ctx.cairo.move_to(end_pos.0, end_pos.1);
            }
            Item::List(list) => {
                for item in list {
                    item.render(ctx);
                }
            }
            Item::Null => {}
        }
    }
}
