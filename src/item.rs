//! Graphical rendering of an [Item]
#[cfg(feature = "dbus")]
use crate::tray;
use crate::{
    data::{BarData, ItemReference, IterationItem, Module, ModuleContext, Value},
    event::EventSink,
    font::render_font_item,
    icon,
    render::{Align, Render, Width},
    state::Runtime,
    wayland::Button,
};
use log::{debug, error, warn};
use std::{borrow::Cow, rc::Rc};
use tiny_skia::{Color, Point};

/// A visible item in a bar
#[derive(Debug)]
pub struct Item {
    pub format: ItemFormat,
    pub data: Module,
    events: EventSink,
}

/// Formatting information for a visible bar item
#[derive(Debug, Default)]
pub struct ItemFormat {
    markup: bool,
    oneline: bool,
    cfg: Option<toml::Value>,
}

impl ItemFormat {
    pub fn from_toml(config: &toml::Value) -> Self {
        let mut rv = Self::default();
        rv.markup = config
            .get("markup")
            .and_then(|v| v.as_bool())
            .unwrap_or(false);
        rv.oneline = config
            .get("oneline")
            .and_then(|v| v.as_bool())
            .unwrap_or(false);

        rv.cfg = config
            .as_table()
            .map(|t| {
                t.iter()
                    .filter(|(k, _)| match &***k {
                        "align" | "bg" | "bg-alpha" | "border" | "border-alpha"
                        | "border-color" | "fg" | "fg-alpha" | "font" | "halign" | "margin"
                        | "max-width" | "min-width" | "padding" | "text-outline"
                        | "text-outline-alpha" | "text-outline-width" | "valign" => true,
                        _ => false,
                    })
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<toml::map::Map<_, _>>()
            })
            .filter(|m| !m.is_empty())
            .map(toml::Value::Table);

        rv
    }

    pub fn is_trivial(&self) -> bool {
        self.cfg.is_none()
    }

    pub fn setup_ctx<'a, 'p: 'a, 'c>(
        &self,
        ctx: &'a mut Render<'p, 'c>,
    ) -> (Formatting, Render<'a, 'c>) {
        let z = toml::Value::Integer(0);
        let config = self.cfg.as_ref().unwrap_or(&z);
        let fmt = Formatting::expand(config, ctx.runtime);
        let runtime = &ctx.runtime;
        let get = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime
                    .format(&fmt)
                    .or_else(|e| {
                        warn!("Error expanding '{}' when rendering: {}", fmt, e);
                        Err(())
                    })
                    .ok()
                    .map(Value::into_text),
                None => Some(v.to_string().into()),
            })
        };

        let get_f32 = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime
                    .format(&fmt)
                    .or_else(|e| {
                        warn!("Error expanding '{}' when rendering: {}", fmt, e);
                        Err(())
                    })
                    .ok()
                    .and_then(|v| v.parse_f32()),
                None => v
                    .as_float()
                    .map(|v| v as f32)
                    .or_else(|| v.as_integer().map(|i| i as f32)),
            })
        };

        let mut align = Align {
            horiz: get("halign").and_then(Align::parse_hv),
            vert: get("valign").and_then(Align::parse_hv),
        };
        align.from_name(get("align"));

        let (font, font_size) = get("font").map_or((None, None), |font| {
            let mut size = None::<f32>;
            let font = match font.rsplit_once(' ') {
                Some((name, ssize))
                    if {
                        size = ssize.parse().ok();
                        size.is_some()
                    } =>
                {
                    name
                }
                _ => &*font,
            };
            let font = runtime.fonts.iter().find(|f| f.name == font);
            (font, size)
        });

        let fg_rgba = Formatting::parse_rgba(get("fg"), get_f32("fg-alpha"));
        let stroke_rgba =
            Formatting::parse_rgba(get("text-outline"), get_f32("text-outline-alpha"));
        let stroke_size = get_f32("text-outline-width");

        let render = Render {
            queue: &mut *ctx.queue,
            damage: ctx.damage.as_deref_mut(),
            align: ctx.align.merge(&align),
            font: font.unwrap_or(&ctx.font),
            font_size: font_size.unwrap_or(ctx.font_size),
            font_color: fg_rgba.unwrap_or(ctx.font_color),
            text_stroke: stroke_rgba.or(ctx.text_stroke),
            text_stroke_size: stroke_size.or(ctx.text_stroke_size),
            ..*ctx
        };
        (fmt, render)
    }
}

/// Formatting that must be applied after rendering an item
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Formatting {
    bg_rgba: Option<Color>,
    border: Option<(f32, f32, f32, f32)>,
    border_rgba: Option<Color>,
    min_width: Option<Width>,
    max_width: Option<Width>,
    margin: Option<(f32, f32, f32, f32)>,
    padding: Option<(f32, f32, f32, f32)>,
}

impl Formatting {
    fn expand(config: &toml::Value, runtime: &Runtime) -> Self {
        let get = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime
                    .format(&fmt)
                    .or_else(|e| {
                        warn!("Error expanding '{}' when rendering: {}", fmt, e);
                        Err(())
                    })
                    .ok()
                    .map(Value::into_text),
                None => Some(v.to_string().into()),
            })
        };

        let get_f32 = |key| {
            config.get(key).and_then(|v| match v.as_str() {
                Some(fmt) => runtime
                    .format(&fmt)
                    .or_else(|e| {
                        warn!("Error expanding '{}' when rendering: {}", fmt, e);
                        Err(())
                    })
                    .ok()
                    .and_then(|v| v.parse_f32()),
                None => v
                    .as_float()
                    .map(|v| v as f32)
                    .or_else(|| v.as_integer().map(|i| i as f32)),
            })
        };
        let min_width = get("min-width").and_then(Width::from_str);
        let max_width = get("max-width").and_then(Width::from_str);

        let margin = get("margin").and_then(Formatting::parse_trbl);
        let border = get("border").and_then(Formatting::parse_trbl);
        let padding = get("padding").and_then(Formatting::parse_trbl);

        let bg_rgba = Formatting::parse_rgba(get("bg"), get_f32("bg-alpha"));
        let border_rgba = Formatting::parse_rgba(get("border-color"), get_f32("border-alpha"));

        Self {
            bg_rgba,
            border,
            border_rgba,
            min_width,
            max_width,
            margin,
            padding,
        }
    }

    fn parse_trbl(v: Cow<str>) -> Option<(f32, f32, f32, f32)> {
        let mut rv = (0.0, 0.0, 0.0, 0.0);
        for (i, x) in v.split_whitespace().enumerate() {
            match (i, x.parse()) {
                (0, Ok(v)) => {
                    rv = (v, v, v, v);
                }
                (1, Ok(v)) => {
                    rv.1 = v;
                    rv.3 = v;
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

    pub fn parse_rgba(color: Option<impl AsRef<str>>, alpha: Option<f32>) -> Option<Color> {
        if color.is_none() && alpha.is_none() {
            return None;
        }
        let color = color.as_ref().map_or("black", |v| v.as_ref());
        let (r, g, b, mut a);
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
                    r = 0;
                    g = 0;
                    b = 0;
                }
            }
        } else {
            match color {
                "black" => {
                    r = 0;
                    g = 0;
                    b = 0;
                }
                "red" => {
                    r = 0xFFFF;
                    g = 0;
                    b = 0;
                }
                "yellow" => {
                    r = 0xFFFF;
                    g = 0xFFFF;
                    b = 0;
                }
                "green" => {
                    r = 0;
                    g = 0xFFFF;
                    b = 0;
                }
                "blue" => {
                    r = 0;
                    g = 0;
                    b = 0xFFFF;
                }
                "gray" => {
                    r = 0x7FFF;
                    g = 0x7FFF;
                    b = 0x7FFF;
                }
                "white" => {
                    r = 0xFFFF;
                    g = 0xFFFF;
                    b = 0xFFFF;
                }
                _ => {
                    debug!("Unknown color '{}'", color);
                    r = 0;
                    g = 0;
                    b = 0;
                }
            }
        }
        Color::from_rgba(
            r as f32 / 65535.0,
            g as f32 / 65535.0,
            b as f32 / 65535.0,
            a as f32 / 65535.0,
        )
    }

    fn get_shrink(&self) -> Option<(f32, f32, f32, f32)> {
        let mut rv = (0.0, 0.0, 0.0, 0.0);
        if self.padding == None && self.margin == None {
            return None;
        }
        for &i in &[self.padding, self.margin, self.border] {
            if let Some((t, r, b, l)) = i {
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

    fn render(&self, ctx: &mut Render, inner: impl FnOnce(&mut Render)) -> (Point, f32, f32, f32) {
        let format = self;
        let outer_clip = ctx.render_extents;
        let mut start_pos = ctx.render_pos;
        let mut inner_clip = outer_clip;

        let shrink = format.get_shrink();
        if (shrink, format.max_width) != (None, None) {
            match shrink {
                Some((t, r, b, l)) => {
                    inner_clip.0.x += l;
                    inner_clip.0.y += t;
                    start_pos.x += l;
                    start_pos.y += t;
                    inner_clip.1.x -= r;
                    inner_clip.1.y -= b;
                }
                None => {}
            }
            match format.max_width {
                Some(Width::Pixels(n)) => {
                    let clip_at = start_pos.x + n;
                    if inner_clip.1.x > clip_at {
                        ctx.render_flex = false;
                        inner_clip.1.x = clip_at;
                    }
                }
                Some(Width::Fraction(f)) => {
                    let parent_width = outer_clip.1.x - outer_clip.0.x;
                    inner_clip.1.x = inner_clip.1.x.min(start_pos.x + parent_width * f);
                }
                None => {}
            }
        }

        let mark = ctx.queue.start_group();
        ctx.render_pos = start_pos;
        ctx.render_extents = inner_clip;

        inner(ctx);

        let mut end_pos = ctx.render_pos;

        let child_render_width = end_pos.x - start_pos.x;
        let mut min_width = match format.min_width {
            None => 0.0,
            Some(Width::Pixels(n)) => n,
            Some(Width::Fraction(f)) => f * (outer_clip.1.x - outer_clip.0.x),
        };
        if min_width > inner_clip.1.x - start_pos.x {
            // clamp the minimum to only the available region
            min_width = inner_clip.1.x - start_pos.x;
        }

        let inner_x_offset;
        if child_render_width < min_width {
            // child is smaller than the box; align it
            let expand = min_width - child_render_width;
            match ctx.align.horiz {
                Some(f) => {
                    inner_x_offset = expand * f;

                    ctx.queue.translate_group(
                        &mark,
                        Point {
                            x: inner_x_offset,
                            y: 0.,
                        },
                    );
                }
                _ => {
                    // defaults to left align
                    inner_x_offset = 0.0;
                }
            }
        } else {
            inner_x_offset = 0.0;
        }

        let shrink_r_width = shrink.map_or(0.0, |s| s.1);
        let shrink_b_height = shrink.map_or(0.0, |s| s.2);
        if !ctx.render_flex {
            // clip to the allowed size
            end_pos.x = end_pos.x.min(inner_clip.1.x);
        }
        let outer_pos = end_pos
            + Point {
                x: shrink_r_width,
                y: shrink_b_height,
            };

        if format.bg_rgba.is_some() || format.border.is_some() {
            let end_mark = ctx.queue.start_group();

            use tiny_skia::Rect;
            let mut bg_clip = (start_pos, end_pos);
            if let Some((t, r, b, l)) = format.padding {
                bg_clip.0.x -= l;
                bg_clip.0.y -= t;
                bg_clip.1.x += r;
                bg_clip.1.y += b;
            }

            if let Some(rgba) = format.bg_rgba {
                if let Some(rect) =
                    Rect::from_ltrb(bg_clip.0.x, bg_clip.0.y, bg_clip.1.x, bg_clip.1.y)
                {
                    ctx.queue.push_rect(rect, rgba);
                }
            }

            if let Some((t, r, b, l)) = format.border {
                let rgba = format.border_rgba.unwrap_or(ctx.font_color);

                bg_clip.0.y -= t;
                if let Some(rect) =
                    Rect::from_xywh(bg_clip.0.x, bg_clip.0.y, bg_clip.1.x - bg_clip.0.x, t)
                {
                    // top edge, no corners
                    ctx.queue.push_rect(rect, rgba);
                }

                bg_clip.0.x -= l;
                if let Some(rect) =
                    Rect::from_xywh(bg_clip.0.x, bg_clip.0.y, l, bg_clip.1.y - bg_clip.0.y)
                {
                    // left edge + top-left corner
                    ctx.queue.push_rect(rect, rgba);
                }

                if let Some(rect) =
                    Rect::from_xywh(bg_clip.1.x, bg_clip.0.y, r, bg_clip.1.y - bg_clip.0.y)
                {
                    // right edge + top-right corner
                    ctx.queue.push_rect(rect, rgba);
                }

                bg_clip.1.x += r;
                if let Some(rect) =
                    Rect::from_xywh(bg_clip.0.x, bg_clip.1.y, bg_clip.1.x - bg_clip.0.x, b)
                {
                    // bottom edge + both corners
                    ctx.queue.push_rect(rect, rgba);
                }
            }

            ctx.queue.swap_rect_ordering(&mark, &end_mark);
        }

        (outer_pos, inner_x_offset, start_pos.x, end_pos.x)
    }
}

impl From<Module> for Item {
    fn from(data: Module) -> Self {
        Self {
            format: ItemFormat::default(),
            events: EventSink::default(),
            data,
        }
    }
}

impl Item {
    pub fn none() -> Self {
        Self {
            format: ItemFormat::default(),
            events: EventSink::default(),
            data: Module::parse_error(""),
        }
    }

    pub fn new_bar(cfg: toml::Value) -> Self {
        let left = Rc::new(cfg.get("left").map_or_else(Item::none, Item::from_toml_ref));
        let right = Rc::new(
            cfg.get("right")
                .map_or_else(Item::none, Item::from_toml_ref),
        );
        let center = Rc::new(
            cfg.get("center")
                .map_or_else(Item::none, Item::from_toml_ref),
        );
        let mut tooltips = cfg
            .get("tooltips")
            .map_or_else(ItemFormat::default, ItemFormat::from_toml);

        if let Some(table) = tooltips.cfg.as_mut().and_then(|c| c.as_table_mut()) {
            if !table.contains_key("bg") {
                table.insert("bg".into(), "black".into());
            }
            if !table.contains_key("padding") {
                table.insert("padding".into(), "2".into());
            }
        } else {
            tooltips.cfg = Some(toml::Value::Table(toml::toml! {
                bg = "black"
                padding = "2"
            }));
        }

        Item {
            events: EventSink::from_toml(&cfg),
            format: ItemFormat::from_toml(&cfg),
            data: Module::Bar {
                left,
                center,
                right,
                data: BarData::new(tooltips, cfg),
            },
        }
    }

    pub fn from_toml_ref(value: &toml::Value) -> Self {
        if value.as_str().is_some() {
            return Module::from_toml_in(value, ModuleContext::Source).into();
        }

        Self::from_item_list("<ref>", value)
    }

    pub fn from_toml_format(value: &toml::Value) -> Self {
        Self::from_item_list("<ref>", value)
    }

    pub fn from_item_list(key: &str, value: &toml::Value) -> Self {
        if let Some(array) = value.as_array() {
            return Module::Group {
                items: array.iter().map(Item::from_toml_ref).map(Rc::new).collect(),
                condition: None,
                tooltip: None,
                spacing: "".into(),
                vertical: false,
            }
            .into();
        }

        let data = Module::from_toml_in(value, ModuleContext::Item);
        if let Module::ParseError { msg } = &data {
            error!("Error parsing {key}: {msg}");
        }
        Item {
            events: EventSink::from_toml(value),
            format: ItemFormat::from_toml(value),
            data,
        }
    }

    pub fn render(self: &Rc<Self>, parent_ctx: &mut Render) -> EventSink {
        // skip rendering if we are outside the clip bounds
        if !parent_ctx.render_flex && parent_ctx.render_pos.x > parent_ctx.render_extents.1.x {
            return EventSink::default();
        }

        let mut rv = self.events.clone();

        if self.format.is_trivial() {
            self.render_inner(parent_ctx, &mut rv);
            return rv;
        }

        let (format, mut ctx) = self.format.setup_ctx(parent_ctx);
        if format.is_boring() {
            self.render_inner(&mut ctx, &mut rv);
            let pos = ctx.render_pos;
            parent_ctx.render_pos = pos;
            return rv;
        }

        let (pos, offset, min, max) = format.render(&mut ctx, |ctx| {
            self.render_inner(ctx, &mut rv);
        });

        rv.offset_clamp(offset, min, max);
        parent_ctx.render_pos = pos;

        rv
    }

    pub fn render_clamped(self: &Rc<Self>, ctx: &mut Render, ev: &mut EventSink) {
        let x0 = ctx.render_pos.x;
        let mut rv = self.render(ctx);
        let x1 = ctx.render_pos.x;
        rv.offset_clamp(0.0, x0, x1);
        ev.merge(rv);
    }

    pub fn render_clamped_item(
        self: &Rc<Self>,
        ctx: &mut Render,
        ev: &mut EventSink,
        item: &IterationItem,
    ) {
        let item_var = ctx.runtime.get_item_var();
        let prev = item_var.replace(Some(item.clone()));
        let origin = ctx.render_pos;
        let mut rv = self.render(ctx);
        let x1 = ctx.render_pos.x;
        rv.offset_clamp(0.0, origin.x, x1);
        rv.set_item(item);
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
    fn render_inner(self: &Rc<Self>, ctx: &mut Render, rv: &mut EventSink) {
        match &self.data {
            Module::ItemReference { value } => {
                ItemReference::with(value, &ctx.runtime, |item| match item {
                    Some(item) => rv.merge(item.render(ctx)),
                    None => {}
                });
            }
            Module::Group {
                condition,
                items,
                tooltip,
                spacing,
                vertical,
            } => {
                if let Some(cond) = condition {
                    if !cond.is_empty() {
                        match ctx.runtime.format(cond) {
                            Ok(v) if v.as_bool() => {}
                            Ok(_) => return,
                            Err(e) => {
                                warn!("Error evaluating condition '{}': {}", cond, e);
                            }
                        }
                    }
                }
                let mut group = ctx.group();
                let spacing = ctx
                    .runtime
                    .format(spacing)
                    .ok()
                    .and_then(|s| s.parse_f32())
                    .unwrap_or(0.0);
                for item in items {
                    item.render_clamped(ctx, rv);

                    if *vertical {
                        group.next_v(ctx);
                        if spacing > 0.0 {
                            ctx.render_pos.y = (ctx.render_pos.y + spacing).ceil();
                        }
                    } else {
                        group.next_h(ctx);
                        if spacing > 0.0 {
                            ctx.render_pos.x = (ctx.render_pos.x + spacing).ceil();
                        }
                    }
                }
                ctx.render_pos = group.bounds;
                if let Some(item) = tooltip {
                    rv.add_tooltip(PopupDesc::RenderItem {
                        item: item.clone(),
                        iter: ctx.runtime.copy_item_var(),
                    });
                }
            }
            Module::FocusList {
                source,
                others,
                focused,
                spacing,
            } => {
                let spacing = ctx
                    .runtime
                    .format(spacing)
                    .ok()
                    .and_then(|s| s.parse_f32())
                    .unwrap_or(0.0);
                let item_var = ctx.runtime.get_item_var();
                let mut group = ctx.group();
                let prev = item_var.replace(None);
                source.read_focus_list(ctx.runtime, |focus, item| {
                    item_var.set(Some(item.clone()));
                    let x0 = ctx.render_pos.x;
                    let mut ev = if focus {
                        focused.render(ctx)
                    } else {
                        others.render(ctx)
                    };
                    let x1 = ctx.render_pos.x;
                    ev.offset_clamp(0.0, x0, x1);
                    ev.set_item(&item);
                    rv.merge(ev);
                    group.next_h(ctx);
                    ctx.render_pos.x += spacing;
                });
                ctx.render_pos = group.bounds;
                item_var.set(prev);
            }
            Module::Bar {
                left,
                center,
                right,
                ..
            } => {
                let scale = ctx.scale;
                // Region 0 (bit 0x1) is "entire bar" - set for the outer render of this item and
                // catches things like bar background changes.
                //
                // Region 1 is "left"
                // Region 2 is "right"
                // Region 3 is "center"
                //
                // A damage rectangle for each region is pushed to the ctx, and the bar rendering
                // code will filter that list using Bar::damage_regions
                let interest_region = ctx.runtime.divide_region(4);
                let clip = ctx.render_extents;
                let width = clip.1.x - ctx.render_pos.x;
                let left_size;
                assert!(interest_region.is_split());

                {
                    interest_region.set(1);
                    let mut left_ev = left.render(ctx);
                    left_size = ctx.render_pos.x.ceil();
                    left_ev.offset_clamp(0.0, 0.0, left_size);

                    rv.merge(left_ev);

                    let damage = ctx.damage.as_mut().unwrap();
                    let pos = ctx.render_pos
                        * Point {
                            x: ctx.scale,
                            y: ctx.scale,
                        };
                    damage[1] = [0, 0, pos.x.ceil() as _, pos.y.ceil() as _];
                }

                let right_size;
                {
                    ctx.render_pos = clip.0;
                    interest_region.set(2);

                    let mark = ctx.queue.start_group();
                    let mut right_ev = right.render(ctx);

                    right_size = ctx.render_pos.x.ceil();

                    let right_offset = clip.1.x - right_size;

                    let damage = ctx.damage.as_mut().unwrap();
                    let tl = Point {
                        x: right_offset,
                        y: 0.,
                    };
                    let br = ctx.render_pos;
                    ctx.queue.translate_group(&mark, tl);

                    damage[2] = [
                        (tl.x * scale) as i32,
                        (tl.y * scale) as i32,
                        (br.x.ceil() * scale) as _,
                        (br.y.ceil() * scale) as _,
                    ];

                    right_ev.offset_clamp(right_offset, right_offset, clip.1.x);
                    rv.merge(right_ev);
                }

                let max_center_width = width - left_size - right_size;

                if max_center_width <= 0.0 {
                    // don't render the center if there's no room at all
                    return;
                }

                // Given the size of the center item, find where to put the left edge
                let calc_center = |cent_size| {
                    let max_side = (width - cent_size) / 2.0;
                    let total_room = width - (left_size + right_size + cent_size);
                    if total_room <= 0.0 {
                        // no gaps at all; just put it at the start of the middle region
                        left_size
                    } else if left_size > max_side {
                        // left side is too long to properly center; put it just to the right of that
                        left_size
                    } else if right_size > max_side {
                        // right side is too long to properly center; put it just to the left of that
                        width - right_size - cent_size
                    } else {
                        // Actually center the center module
                        max_side
                    }
                };

                interest_region.set(3);
                let mark = ctx.queue.start_group();
                let x0 = clip.1.x - max_center_width;
                ctx.render_pos.x = x0;
                ctx.render_pos.y = clip.0.y;

                let mut cent_ev = center.render(ctx);
                let cent_size = (ctx.render_pos.x - x0).ceil();

                let cent_offset = calc_center(cent_size).floor();

                ctx.queue.translate_group(
                    &mark,
                    Point {
                        x: cent_offset - x0,
                        y: 0.,
                    },
                );

                let damage = ctx.damage.as_mut().unwrap();
                let br = Point {
                    x: cent_offset + cent_size,
                    y: ctx.render_pos.y,
                };
                damage[3] = [
                    (cent_offset * scale) as i32,
                    0,
                    (br.x.ceil() * scale) as _,
                    (br.y.ceil() * scale) as _,
                ];

                cent_ev.offset_clamp(cent_offset, cent_offset, cent_offset + cent_size);
                rv.merge(cent_ev);
            }
            Module::Fade {
                items,
                value,
                dir,
                tooltip,
            } => {
                let value = value.read_in(ctx.err_name, "", &ctx.runtime, |v| {
                    v.parse_f32().unwrap_or(0.0)
                });
                if let Some(item) = tooltip {
                    rv.add_tooltip(PopupDesc::RenderItem {
                        item: item.clone(),
                        iter: ctx.runtime.copy_item_var(),
                    });
                }
                if value <= 0.0 {
                    let ev = items[0].render(ctx);
                    rv.merge(ev);
                    return;
                }
                let last = items.len() - 1;
                if value >= last as f32 {
                    let ev = items[last].render(ctx);
                    rv.merge(ev);
                    return;
                }
                let base = value.floor() as usize;
                if value == base as f32 {
                    let ev = items[base].render(ctx);
                    rv.merge(ev);
                    return;
                }
                let value = value.fract();

                let origin = ctx.render_pos;

                let mark1 = ctx.queue.start_group();
                let mut ev1 = items[base].render(ctx);
                let end1 = ctx.render_pos;

                ctx.render_pos = origin;

                let mark2 = ctx.queue.start_group();
                let mut ev2 = items[base + 1].render(ctx);
                let end2 = ctx.render_pos;

                let mark3 = ctx.queue.start_group();

                let bb_l = origin.x;
                let bb_r = end1.x.max(end2.x);
                let hoff = (bb_r - bb_l) * value;

                let rb_t = origin.y;
                let rb_b = end1.y.max(end2.y);
                let voff = (rb_b - rb_t) * value;

                let bb_t = ctx.render_extents.0.y;
                let bb_b = ctx.render_extents.1.y;

                ctx.render_pos.x = bb_r;
                ctx.render_pos.y = rb_b;

                let (mut bb1, mut bb2) = ([bb_l, bb_t, bb_r, bb_b], [bb_l, bb_t, bb_r, bb_b]);

                match dir {
                    b'r' => {
                        ev1.offset_clamp(0.0, bb_l + hoff, bb_r);
                        rv.merge(ev1);
                        ev2.offset_clamp(0.0, bb_l, bb_l + hoff);
                        rv.merge(ev2);

                        bb1[0] = bb_l + hoff;
                        bb2[2] = bb_l + hoff;
                    }
                    b'l' => {
                        ev2.offset_clamp(0.0, bb_r - hoff, bb_r);
                        rv.merge(ev2);
                        ev1.offset_clamp(0.0, bb_l, bb_r - hoff);
                        rv.merge(ev1);
                        bb1[2] = bb_r - hoff;
                        bb2[0] = bb_r - hoff;
                    }
                    b'd' => {
                        rv.merge(ev1);
                        bb1[1] = rb_t + voff;
                        bb2[3] = rb_t + voff;
                    }
                    b'u' => {
                        rv.merge(ev1);
                        bb1[3] = rb_b - voff;
                        bb2[1] = rb_b - voff;
                    }
                    _ => unreachable!(),
                }

                ctx.queue.crop_range(&mark1, &mark2, bb1);
                ctx.queue.crop_range(&mark2, &mark3, bb2);
            }
            Module::Icon {
                name,
                fallback,
                tooltip,
            } => {
                let markup = self.format.markup;
                let name = ctx.runtime.format_or(name, ctx.err_name).into_text();
                match icon::render(ctx, &name) {
                    Ok(()) => {}
                    Err(()) => {
                        let value = ctx.runtime.format_or(fallback, ctx.err_name).into_owned();
                        let mut item: Item = Module::new_value(value).into();
                        item.format.markup = markup;
                        Rc::new(item).render(ctx);
                    }
                }
                if !tooltip.is_empty() {
                    rv.add_tooltip(PopupDesc::TextItem {
                        source: self.clone(),
                        iter: ctx.runtime.copy_item_var(),
                    });
                }
            }
            Module::SwayTree(tree) => {
                tree.render(ctx, rv);
            }
            #[cfg(feature = "dbus")]
            Module::Tray {
                passive,
                active,
                urgent,
            } => tray::show(ctx, rv, [passive, active, urgent]),

            // All other modules are rendered as text
            _ => {
                let markup = self.format.markup;
                let oneline = self.format.oneline;
                let mut text = self
                    .data
                    .read_to_owned(ctx.err_name, "text", &ctx.runtime)
                    .into_text();
                if oneline && text.contains('\n') {
                    text = text.replace('\n', " ").into();
                }

                render_font_item(ctx, &text, markup);

                match &self.data {
                    Module::Formatted {
                        tooltip: Some(item),
                        ..
                    } => {
                        rv.add_tooltip(PopupDesc::RenderItem {
                            item: item.clone(),
                            iter: ctx.runtime.copy_item_var(),
                        });
                    }
                    Module::Formatted { tooltip: None, .. } => {}
                    _ => {
                        let tt = self
                            .data
                            .read_to_owned(ctx.err_name, "tooltip", &ctx.runtime)
                            .into_text();
                        if !tt.is_empty() {
                            rv.add_tooltip(PopupDesc::TextItem {
                                source: self.clone(),
                                iter: ctx.runtime.copy_item_var(),
                            });
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PopupDesc {
    RenderItem {
        item: Rc<Item>,
        iter: Option<IterationItem>,
    },
    TextItem {
        source: Rc<Item>,
        iter: Option<IterationItem>,
    },
    #[cfg(feature = "dbus")]
    Tray(tray::TrayPopup),
}

impl PartialEq for PopupDesc {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (
                PopupDesc::RenderItem { item: a, iter: ai },
                PopupDesc::RenderItem { item: b, iter: bi },
            ) => Rc::ptr_eq(a, b) && ai == bi,
            (
                PopupDesc::TextItem {
                    source: a,
                    iter: ai,
                },
                PopupDesc::TextItem {
                    source: b,
                    iter: bi,
                },
            ) => Rc::ptr_eq(a, b) && ai == bi,
            #[cfg(feature = "dbus")]
            (PopupDesc::Tray(a), PopupDesc::Tray(b)) => a == b,
            _ => false,
        }
    }
}

impl PopupDesc {
    pub fn lazy_refresh(&mut self) {
        #[cfg(feature = "dbus")]
        if let PopupDesc::Tray(t) = self {
            t.lazy_refresh();
        }
    }

    pub fn render_popup(&mut self, ctx: &mut Render) -> (i32, i32) {
        ctx.font_color = tiny_skia::Color::WHITE;
        ctx.render_pos = tiny_skia::Point::zero();
        ctx.render_flex = true;
        ctx.err_name = "tooltip";

        let format = match &ctx.runtime.items["bar"].data {
            Module::Bar { data, .. } => &data.tooltips,
            _ => return (0, 0),
        };

        let (format, mut ctx) = format.setup_ctx(ctx);
        let (pos, _, _, _) = format.render(&mut ctx, |ctx| {
            self.render(ctx);
        });

        (pos.x as i32, pos.y as i32)
    }

    fn render(&mut self, ctx: &mut Render) {
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
                let value = source
                    .data
                    .read_to_owned("tooltip", "tooltip", ctx.runtime)
                    .into_text();
                item_var.set(None);

                if value.is_empty() {
                    return;
                }

                let markup = source.format.markup;

                render_font_item(ctx, &value, markup);
                ctx.render_pos.x += 2.0;
                ctx.render_pos.y += 2.0;
            }
            #[cfg(feature = "dbus")]
            PopupDesc::Tray(tray) => tray.render(ctx),
        }
    }

    pub fn button(&mut self, x: f64, y: f64, button: Button, runtime: &mut Runtime) {
        match self {
            PopupDesc::RenderItem { item, iter } => {
                if let Some(ii) = iter.as_ref() {
                    let mut events = item.events.clone();
                    events.set_item(ii);
                    events.button(x as f32, y as f32, button, runtime);
                } else {
                    item.events.button(x as f32, y as f32, button, runtime);
                }
            }
            PopupDesc::TextItem { .. } => {}
            #[cfg(feature = "dbus")]
            PopupDesc::Tray(tray) => tray.button(x, y, button, runtime),
        }
    }
}
