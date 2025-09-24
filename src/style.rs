use crate::{
    render::{Rect, Render},
    state::Runtime,
    value::Value,
};
use log::{debug, error, warn};
use std::borrow::Cow;
use tiny_skia::{Color, Point};

/// Style information that is inherited from the parent item
#[derive(Debug, Copy, Clone)]
pub struct Computed {
    pub font: fontdb::ID,
    pub font_size: f32,
    pub font_color: Color,
    pub text_stroke: Option<Color>,
    pub text_stroke_size: Option<f32>,
    pub align: Align,
}

/// Formatting information (as configured) for an Item.
#[derive(Debug, Default)]
pub struct ItemFormat {
    pub markup: bool,
    pub oneline: bool,
    pub cfg: Option<toml::Value>,
}

impl ItemFormat {
    /// Copy formatting-relevant keys from the given configuration table
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
                        | "border-color" | "fg" | "fg-alpha" | "font" | "font-size" | "halign"
                        | "margin" | "max-width" | "min-width" | "padding" | "text-outline"
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

    /// Create a new rendering context with an updated [Computed] style for this item and its
    /// children, and a [Formatting] helper to handle more complex rendering.
    pub fn setup_ctx<'a, 'p: 'a>(&self, ctx: &'a mut Render<'p>) -> (Formatting, Render<'a>) {
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
            let font = ctx.cache.fontdb.query(font);

            (font, size)
        });

        let font_size = font_size.or_else(|| get("font-size")?.parse().ok());

        let mut ctx = ctx.as_mut();
        if let Some(font) = font {
            ctx.style.font = font;
        }
        ctx.style.font_size = font_size.unwrap_or(ctx.style.font_size);

        ctx.style.align = ctx.style.align.merge(&align);

        ctx.style.font_color =
            Formatting::parse_rgba(get("fg"), get_f32("fg-alpha")).unwrap_or(ctx.style.font_color);

        ctx.style.text_stroke =
            Formatting::parse_rgba(get("text-outline"), get_f32("text-outline-alpha"))
                .or(ctx.style.text_stroke);
        ctx.style.text_stroke_size = get_f32("text-outline-width").or(ctx.style.text_stroke_size);

        (fmt, ctx)
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
    /// Compute the current formatting of an item
    pub fn expand(config: &toml::Value, runtime: &Runtime) -> Self {
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

    /// If this returns true, [Self::render] does not need to be called to render this item.
    pub fn is_boring(&self) -> bool {
        *self == Self::default()
    }

    /// Render a styled item.
    ///
    /// The provided closure should render the contents of the item.  This function handles
    /// margins, padding, borders, and background.
    ///
    /// It does not handle changes to the [Computed] style.
    pub fn render(
        &self,
        ctx: &mut Render,
        inner: impl FnOnce(&mut Render),
    ) -> (Point, f32, f32, f32) {
        let format = self;
        let outer_clip = ctx.render_extents;
        let mut start_pos = ctx.render_pos;
        let mut inner_clip = outer_clip;

        let shrink = format.get_shrink();
        if (shrink, format.max_width) != (None, None) {
            match shrink {
                Some((t, r, b, l)) => {
                    inner_clip.left += l;
                    inner_clip.top += t;
                    start_pos.x += l;
                    start_pos.y += t;
                    inner_clip.right -= r;
                    inner_clip.bottom -= b;
                }
                None => {}
            }
            match format.max_width {
                Some(Width::Pixels(n)) => {
                    let clip_at = start_pos.x + n;
                    if inner_clip.right > clip_at {
                        ctx.render_flex = false;
                        inner_clip.right = clip_at;
                    }
                }
                Some(Width::Fraction(f)) => {
                    let parent_width = outer_clip.width();
                    inner_clip.right = inner_clip.right.min(start_pos.x + parent_width * f);
                }
                None => {}
            }
        }

        let mark = ctx.start_group();
        ctx.render_pos = start_pos;
        ctx.render_extents = inner_clip;

        inner(ctx);

        let mut end_pos = ctx.render_pos;

        let child_render_width = end_pos.x - start_pos.x;
        let mut min_width = match format.min_width {
            None => 0.0,
            Some(Width::Pixels(n)) => n,
            Some(Width::Fraction(f)) => f * outer_clip.width(),
        };
        if min_width > inner_clip.right - start_pos.x {
            // clamp the minimum to only the available region
            min_width = inner_clip.right - start_pos.x;
        }

        let inner_x_offset;
        if child_render_width < min_width {
            // child is smaller than the box; align it
            let expand = min_width - child_render_width;
            match ctx.style.align.horiz {
                Some(f) => {
                    inner_x_offset = ctx.round_to_pixel(expand * f);

                    ctx.translate_group_x(&mark, inner_x_offset);
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
            end_pos.x = end_pos.x.min(inner_clip.right);
        }
        let outer_pos = end_pos
            + Point {
                x: shrink_r_width,
                y: shrink_b_height,
            };

        if format.bg_rgba.is_some() || format.border.is_some() {
            let end_mark = ctx.start_group();

            let mut bg_clip = (start_pos, end_pos);
            if let Some((t, r, b, l)) = format.padding {
                bg_clip.0.x -= l;
                bg_clip.0.y -= t;
                bg_clip.1.x += r;
                bg_clip.1.y += b;
            }

            if let Some(rgba) = format.bg_rgba {
                let rect = Rect::from_ltrb(bg_clip.0.x, bg_clip.0.y, bg_clip.1.x, bg_clip.1.y);
                ctx.push_rect(rect, rgba);
            }

            if let Some((t, r, b, l)) = format.border {
                let rgba = format.border_rgba.unwrap_or(ctx.style.font_color);

                bg_clip.0.y -= t;
                let rect = Rect::from_xywh(bg_clip.0.x, bg_clip.0.y, bg_clip.1.x - bg_clip.0.x, t);
                // top edge, no corners
                ctx.push_rect(rect, rgba);

                bg_clip.0.x -= l;
                let rect = Rect::from_xywh(bg_clip.0.x, bg_clip.0.y, l, bg_clip.1.y - bg_clip.0.y);
                // left edge + top-left corner
                ctx.push_rect(rect, rgba);

                let rect = Rect::from_xywh(bg_clip.1.x, bg_clip.0.y, r, bg_clip.1.y - bg_clip.0.y);
                // right edge + top-right corner
                ctx.push_rect(rect, rgba);

                bg_clip.1.x += r;
                let rect = Rect::from_xywh(bg_clip.0.x, bg_clip.1.y, bg_clip.1.x - bg_clip.0.x, b);
                // bottom edge + both corners
                ctx.push_rect(rect, rgba);
            }

            // The background and borders go *behind* the item
            ctx.swap_after_marks(mark, end_mark);
        }

        (outer_pos, inner_x_offset, start_pos.x, end_pos.x)
    }
}

pub const MIDDLE: f32 = 0.5;

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct Align {
    pub horiz: Option<f32>,
    pub vert: Option<f32>,
}

impl Align {
    pub fn bar_default() -> Self {
        Align {
            horiz: None,
            vert: Some(MIDDLE),
        }
    }

    pub fn parse_hv(value: Cow<str>) -> Option<f32> {
        if value.ends_with('%') {
            let value = &value[..value.len() - 1];
            let pct = value.parse::<f32>().ok()?;
            return Some(pct / 100.0);
        }
        value.parse().ok()
    }

    pub fn from_name(&mut self, value: Option<Cow<str>>) {
        match value.as_deref() {
            Some("north") => {
                *self = Align {
                    horiz: Some(MIDDLE),
                    vert: Some(0.0),
                }
            }
            Some("south") => {
                *self = Align {
                    horiz: Some(MIDDLE),
                    vert: Some(1.0),
                }
            }
            Some("east") => {
                *self = Align {
                    horiz: Some(0.0),
                    vert: Some(MIDDLE),
                }
            }
            Some("west") => {
                *self = Align {
                    horiz: Some(1.0),
                    vert: Some(MIDDLE),
                }
            }
            Some("center") => {
                *self = Align {
                    horiz: Some(MIDDLE),
                    vert: Some(MIDDLE),
                }
            }
            Some("") | None => {}
            Some(x) => {
                error!("Unknown alignment {}", x);
            }
        }
    }

    pub fn merge(&self, child: &Self) -> Self {
        Align {
            horiz: child.horiz.or(self.horiz),
            vert: child.vert.or(self.vert),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Width {
    /// Some fraction (0.0-1.0) of the total width
    Fraction(f32),
    /// Some number of pixels
    Pixels(f32),
}

impl Width {
    pub fn from_str(value: Cow<str>) -> Option<Self> {
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
