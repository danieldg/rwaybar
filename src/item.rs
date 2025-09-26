//! Graphical rendering of an [Item]
#[cfg(feature = "dbus")]
use crate::tray;
use crate::{
    data::{BarData, ItemReference, IterationItem, Module, Orientation},
    event::EventSink,
    font::render_font_item,
    icon,
    render::{Rect, Render},
    state::Runtime,
    style::ItemFormat,
    wayland::Button,
};
use log::{error, warn};
use std::rc::Rc;

/// A visible item in a bar
#[derive(Debug)]
pub struct Item {
    pub name: Rc<str>,
    pub format: ItemFormat,
    pub data: Module,
    events: EventSink,
}

/// The context of a parsed item, used to disambiguate strings
#[derive(Debug, Clone)]
pub struct ModuleContext<'a> {
    pub parent: &'a Rc<str>,
    pub prefer_ref: bool,
}

impl ModuleContext<'_> {
    pub fn with_ref_preferred(&self) -> Self {
        // Source
        ModuleContext {
            parent: &self.parent,
            prefer_ref: true,
        }
    }

    pub fn with_text_preferred(&self) -> Self {
        ModuleContext {
            parent: &self.parent,
            prefer_ref: false,
        }
    }

    pub fn item_from(&self, data: Module) -> Item {
        Item {
            name: self.parent.clone(),
            format: ItemFormat::default(),
            events: EventSink::default(),
            data,
        }
    }

    pub fn item_from_value(&self, value: &toml::Value) -> Item {
        if value.as_str().is_some() {
            return Item {
                data: Module::from_toml_in(value, self.clone()),
                format: ItemFormat::default(),
                events: EventSink::default(),
                name: self.parent.clone(),
            };
        }

        Item::from_item_list(&self.parent, value)
    }

    pub fn opt_item_from_key(&self, cfg: &toml::Value, key: &'static str) -> Option<Item> {
        cfg.get(key).map(|value| {
            let r: Rc<str> = format!("{}.{key}", self.parent).into();
            if value.as_str().is_some() {
                return Item {
                    data: Module::from_toml_in(
                        value,
                        ModuleContext {
                            parent: &r,
                            prefer_ref: true,
                        },
                    ),
                    format: ItemFormat::default(),
                    events: EventSink::default(),
                    name: r,
                };
            }

            Item::from_item_list(&r, value)
        })
    }

    pub fn item_from_key(&self, cfg: &toml::Value, key: &'static str) -> Item {
        self.opt_item_from_key(cfg, key).unwrap_or_else(|| Item {
            name: format!("{}.{key}", self.parent).into(),
            format: ItemFormat::default(),
            events: EventSink::default(),
            data: Module::parse_error(format!("Value '{}.{key}' not found", self.parent)),
        })
    }
}

impl Item {
    pub fn new_bar(cfg: toml::Value) -> Self {
        let bar = "bar".into();
        let ctx = ModuleContext {
            parent: &bar,
            prefer_ref: true,
        };
        let left = Rc::new(ctx.item_from_key(&cfg, "left"));
        let right = Rc::new(ctx.item_from_key(&cfg, "right"));
        let center = Rc::new(ctx.item_from_key(&cfg, "center"));
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
            name: "bar".into(),
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

    pub fn new_unresolved(text: &Rc<str>) -> Rc<Self> {
        Rc::new(Item {
            name: text.clone(),
            format: ItemFormat::default(),
            events: EventSink::default(),
            data: Module::new_format(&**text),
        })
    }

    pub fn new_current_item() -> Rc<Self> {
        Rc::new(Item {
            name: "item".into(),
            format: ItemFormat::default(),
            events: EventSink::default(),
            data: Module::new_current_item(),
        })
    }

    pub fn new_child_item(&self, name: &str, data: Module) -> Self {
        Item {
            name: format!("{}.{name}", self.name).into(),
            format: ItemFormat::default(),
            events: EventSink::default(),
            data,
        }
    }

    pub fn from_item_list(key: &Rc<str>, value: &toml::Value) -> Self {
        if let Some(array) = value.as_array() {
            return Item {
                name: key.clone(),
                format: ItemFormat::default(),
                events: EventSink::default(),

                data: Module::Group {
                    items: array
                        .iter()
                        .enumerate()
                        .map(|(i, v)| {
                            ModuleContext {
                                parent: &format!("{key}[{i}]").into(),
                                prefer_ref: true,
                            }
                            .item_from_value(v)
                        })
                        .map(Rc::new)
                        .collect(),
                    condition: None,
                    tooltip: None,
                    spacing: "".into(),
                    orientation: Orientation::Horizontal,
                },
            };
        }

        let data = Module::from_toml_in(
            value,
            ModuleContext {
                parent: key,
                prefer_ref: false,
            },
        );
        if let Module::ParseError { msg } = &data {
            if let Some(msg) = msg.take() {
                error!("Error parsing {key}: {msg}");
            }
        }
        Item {
            name: key.clone(),
            events: EventSink::from_toml(value),
            format: ItemFormat::from_toml(value),
            data,
        }
    }

    pub fn render(self: &Rc<Self>, parent_ctx: &mut Render) -> EventSink {
        // skip rendering if we are outside the clip bounds
        if !parent_ctx.render_flex && parent_ctx.render_pos.x > parent_ctx.render_extents.right {
            return EventSink::default();
        }

        let mut rv = self.events.clone();

        if self.format.is_trivial() {
            let mut ctx = parent_ctx.with_err_name(&self.name);
            self.render_inner(&mut ctx, &mut rv);
            let pos = ctx.render_pos;
            parent_ctx.render_pos = pos;
            return rv;
        }

        let (format, mut ctx) = self.format.setup_ctx(parent_ctx);
        ctx.err_name = &self.name;
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
                orientation,
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
                let mut group = ctx.item_group();
                let spacing = ctx
                    .runtime
                    .format(spacing)
                    .ok()
                    .and_then(|s| s.parse_f32())
                    .unwrap_or(0.0);
                for item in items {
                    item.render_clamped(ctx, rv);

                    match orientation {
                        Orientation::Vertical => {
                            group.next_v(ctx);
                            if spacing > 0.0 {
                                ctx.render_pos.y = (ctx.render_pos.y + spacing).ceil();
                            }
                        }
                        Orientation::Horizontal => {
                            group.next_h(ctx);
                            if spacing > 0.0 {
                                ctx.render_pos.x = (ctx.render_pos.x + spacing).ceil();
                            }
                        }
                        Orientation::Stacked => group.next_s(ctx),
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
                let mut group = ctx.item_group();
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
                let clip = ctx.render_extents;
                let width = clip.right - ctx.render_pos.x;

                let mut left_ev = left.render(ctx);
                let left_size = ctx.render_pos.x;
                left_ev.offset_clamp(0.0, 0.0, left_size);
                rv.merge(left_ev);

                ctx.render_pos = clip.tl();
                let mark = ctx.start_group();
                let mut right_ev = right.render(ctx);
                let right_size = ctx.ceil_to_pixel(ctx.render_pos.x);

                let right_offset = clip.right - right_size;
                ctx.translate_group_x(&mark, right_offset);
                right_ev.offset_clamp(right_offset, right_offset, clip.right);
                rv.merge(right_ev);

                let max_center_width = width - left_size - right_size;

                if max_center_width < 1.0 {
                    ctx.render_pos = clip.br();
                    // don't render the center if there's no room at all
                    return;
                }

                let mark = ctx.start_group();
                let x0 = ctx.round_to_pixel(clip.right - max_center_width);
                ctx.render_pos.x = x0;
                ctx.render_pos.y = clip.top;

                let mut cent_ev = center.render(ctx);
                let cent_size = ctx.ceil_to_pixel(ctx.render_pos.x - x0);

                // If the center region was actually centered, how large could each of the two sides be?
                let max_side = (width - cent_size) / 2.0;
                let total_room = width - (left_size + right_size + cent_size);

                let cent_offset = if total_room <= 0.0 {
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
                };
                let cent_offset = ctx.round_to_pixel(cent_offset);

                ctx.translate_group_x(&mark, cent_offset - x0);

                cent_ev.offset_clamp(cent_offset - x0, cent_offset, cent_offset + cent_size);
                rv.merge(cent_ev);
                ctx.render_pos = clip.br();
            }
            Module::Fade {
                items,
                value,
                dir,
                tooltip,
            } => {
                let value = value
                    .read_to_owned(ctx.err_name, "", &ctx.runtime)
                    .parse_f32()
                    .unwrap_or(0.0);
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

                let mark1 = ctx.start_group();
                let mut ev1 = items[base].render(ctx);
                let end1 = ctx.render_pos;

                ctx.render_pos = origin;

                let mark2 = ctx.start_group();
                let mut ev2 = items[base + 1].render(ctx);
                let end2 = ctx.render_pos;

                let mark3 = ctx.start_group();

                let bb_l = origin.x;
                let bb_r = end1.x.max(end2.x);
                let hoff = (bb_r - bb_l) * value;

                let rb_t = origin.y;
                let rb_b = end1.y.max(end2.y);
                let voff = (rb_b - rb_t) * value;

                ctx.render_pos.x = bb_r;
                ctx.render_pos.y = rb_b;

                let inf = f32::INFINITY;
                let mut bb1 = Rect::infinite();
                let mut bb2 = Rect::infinite();

                // Ideally, for the border pixel, if the fractional length of the border is
                // (0 < x < 1), then the value should be:
                //
                //   s1 = p1 * (1 - x)
                //   sa1 = p1a * (1 - x)
                //   s2 = p2 * x
                //   sa2 = p2a * x
                //
                //   s = s1 + s2
                //     = p1 * (1 - x) + p2 * x
                //   sa = sa1 + sa2
                //      = p1a * (1 - x) + p2a * x
                //   r = d * (1 - sa) + s
                //     = d * (1 - sa1 - sa2) + s1 + s2
                //   ra = da * (1 - sa) + sa
                //      = da * (1 - sa1 - sa2) + sa1 + sa2
                //
                // If we just draw the pixels blindly by cropping both, then:
                //   step 1: draw (p1) at (1 - x) * p1a opacity
                //   step 2: draw (p2) at (x) * p2a opacity
                //
                //   r1 = d * (1 - sa1) + s1
                //   ra1 = da * (1 - sa1) + sa1
                //   r2 = r1 * (1 - sa2) + s2
                //      = d * (1 - sa1) * (1 - sa2) + s1 * (1 - sa2) + s2
                //
                //   ra2 = ra1 * (1 - sa2) + sa2
                //       = da * (1 - sa1) * (1 - sa2) + sa1 * (1 - sa2) + sa2
                //
                // This loses a lot of the color contribution of the lower image.  The color
                // difference is most noticeable in fully opaque white regions, which become 75%
                // white when x = 0.5.
                //
                // If we draw the lower image across the whole border pixel, we get:
                //
                //   r1 = d * (1 - p1a) + p1
                //   ra1 = da * (1 - p1a) + p1a
                //   r2 = r1 * (1 - sa2) + s2
                //      = d * (1 - p1a) * (1 - sa2) + p1 * (1 - sa2) + s2
                //      = d * (1 - p1a) * (1 - sa2) + s1 * (1 - p2a * x) / (1 - x) + s2
                //
                //   ra2 = ra1 * (1 - sa2) + sa2
                //       = da * (1 - p1a) * (1 - sa2) + p1a * (1 - sa2) + sa2
                //
                // For fully opaque pixels (p1a = p2a = 1), this simplifies to the correct:
                //   r2 = s1 + s2
                //   ra2 = 1
                // regardless of the value of x. The edge (p2a = 1, p1a = 0) is also correct:
                //   r2 = d * (1 - sa2) + s2
                //   ra2 = da * (1 - sa2) + sa2
                //
                // Other opacities are still incorrect, however these two are the common case for a
                // 'meter' that fills from transparency as the value increases.
                match dir {
                    b'r' => {
                        ev1.offset_clamp(0.0, bb_l + hoff, inf);
                        rv.merge(ev1);
                        ev2.offset_clamp(0.0, -inf, bb_l + hoff);
                        rv.merge(ev2);

                        bb1.left = ctx.floor_to_pixel(bb_l + hoff);
                        bb2.right = bb_l + hoff;
                    }
                    b'l' => {
                        ev2.offset_clamp(0.0, bb_r - hoff, inf);
                        rv.merge(ev2);
                        ev1.offset_clamp(0.0, -inf, bb_r - hoff);
                        rv.merge(ev1);
                        bb1.right = ctx.ceil_to_pixel(bb_r - hoff);
                        bb2.left = bb_r - hoff;
                    }
                    b'd' => {
                        rv.merge(ev1);
                        bb1.top = ctx.floor_to_pixel(rb_t + voff);
                        bb2.bottom = rb_t + voff;
                    }
                    b'u' => {
                        rv.merge(ev1);
                        bb1.bottom = ctx.ceil_to_pixel(rb_b - voff);
                        bb2.top = rb_b - voff;
                    }
                    _ => unreachable!(),
                }

                ctx.crop_range(&mark1, &mark2, bb1);
                ctx.crop_range(&mark2, &mark3, bb2);
            }
            Module::FontTest { offset, interested } => {
                use std::fmt::Write;
                interested.add(&ctx.runtime);
                let font = ctx.cache.fontdb.face(ctx.style.font);
                let glyphs = font.number_of_glyphs();
                let offset = offset.get();
                let mut glyph_to_char = vec![0u32; glyphs as usize];
                if let Some(cmap) = font.tables().cmap {
                    for st in cmap.subtables {
                        if !st.is_unicode() {
                            continue;
                        }
                        st.codepoints(|c| {
                            if let Some(i) = st.glyph_index(c) {
                                if let Some(e) = glyph_to_char.get_mut(i.0 as usize) {
                                    *e = c;
                                }
                            }
                        });
                    }
                }
                let end = glyphs.min(offset.saturating_add(64));
                let mut text = String::new();
                _ = write!(text, "Showing glyph {offset}-{end}/{glyphs}:\n");
                for (i, &c) in glyph_to_char.iter().enumerate().skip(offset as _).take(64) {
                    if c > 0 && char::from_u32(c).is_some() {
                        _ = write!(text, "#{c:04} (&@{i};) ");
                    } else {
                        _ = write!(text, "@{i:04} (&@{i};) ");
                    }
                    if i & 0x7 == 0x7 {
                        text.pop();
                        text.push('\n');
                    }
                }
                text.pop();
                render_font_item(ctx, &text, true);
            }
            Module::Icon {
                name,
                fallback,
                tooltip,
            } => {
                let markup = self.format.markup;
                let name = ctx.runtime.format_or(name, ctx.err_name).into_text();
                match icon::render(ctx, name.into()) {
                    Ok(()) => {}
                    Err(()) => {
                        let value = ctx.runtime.format_or(fallback, ctx.err_name).into_owned();
                        let mut item = self.new_child_item("icon", Module::new_value(value));
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
        ctx.style.font_color = tiny_skia::Color::WHITE;
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
