# Bar definition

It is possible to define multiple bars (usually you use this to define one per
output).  A bar is defined as follows:

```toml
[[bar]]
name = "DP-1"
size = 40
side = "top"
left = ["clock", "mode"]
right = "dp-right"

```

Key | Value | Expanded | Default
----|-------|----------|--------
`name` | The output name (connector) for this bar. | No | Display on all outputs matching make, model, and description.
`make` | A regex that must match the make of the monitor | No | Display on all monitors
`model` | A regex that must match the model of the monitor | No | Display on all monitors
`description` | A regex that must match the description of the monitor | No | Display on all monitors
`size` | The size of the bar in pixels | No | `20`
`side` | `top` or `bottom` | No | `bottom`
`left` | Block or list of blocks | No | None
`center` | Block or list of blocks | No | None
`right` | Block or list of blocks | No | None

You can view the name/make/model/description for your monitors by running
`RUST_LOG=info rwaybar`; they are also displayed by default if the
configuration does not produce any matching bars.

Note: the bar configuration may also include [formatting rules](#formatting)
and other arbitrary text values accessible in [text expansions](#text-expansion).

# Common attributes

With a few exceptions where it is inferred, every block in the configuration
requires a `type` field declaring which module provides the contents of the
block.

## Text Expansion

Most values accept text expansion using `{block-name.key:format}` similar to python
`f""` or rust `format!` strings.  The `:format` part is optional; if present,
it allows formatting the string (adding padding, restricting width, etc).  The
`block-name` part is mandatory and defines which block (as defined in your
configuration) to consult for the item.  The `.key` part is optional and allows
modules to provide multiple values; see the module-specific documentation for
details.

## Formatting

Any block may contain one or more of the following keys, which influence the
display of the item.  While the names were chosen to be similar to CSS when
possible, not all features of the corresponding CSS property are present.

All formatting values are subject to [text expansion](#text-expansion).

Key | Value | Details
----|-------|---------
`align` | `north`, `south`, `east`, `west`, `center` | Simple alignment of the item.  See the `halign` and `valign` properties for more control.
`alpha` | `0.7` (70% opaque) | Applies transparency to the block as a whole - text, images, border, and background.  This is most useful on either the `bar` as a whole or on items like `tray` that don't have their own alpha settings.
`bg` | `red` or `#ff0000` | Background color (without transparency)
`bg-alpha` | 0.2 (20% opaque) | Background opacity
`border` | `1 2 3 4` (pixels) | Border width for the top, right, bottom, and left sides.  Like CSS, you can omit some of the values if they are the same.
`border-alpha` | 0.7 (70% opaque) | Border opacity
`border-color` | `red` or `#ff0000` | Border color (without transparency)
`fg` | `red` or `#ff0000` | Foreground color (without transparency)
`fg-alpha` | 0.7 (70% opaque) | Foreground opacity
`font` | A font name and size | 
`halign` | `20%` | Horizontal alignment (only used when min-width is present)
`margin` | `1 2 3 4` (pixels) | Margin width for the top, right, bottom, and left sides.  Like CSS, you can omit some of the values if they are the same.
`max-width` | `30%` or `40` (pixels) | Minimum width for this block.  If the contents are larger, they will be cropped.
`min-width` | `30%` or `40` (pixels) | Minimum width for this block.  If the contents are smaller, blank space is added and the contents are positioned according to `halign`
`padding` | `1 2 3 4` (pixels) | Padding width for the top, right, bottom, and left sides.  Like CSS, you can omit some of the values if they are the same.
`text-outline` | `red` or `#ff0000` | Color for text outline
`text-outline-alpha` | `0.5` | Opacity of the outline
`valign` | `20%` | Vertical alignment (of text)

## Actions

Any block may contain one of the following keys that define actions to take
when the block is clicked.

Key | Details
----|--------
`on-click` | Left (primary) button
`on-click-right` | Right button
`on-click-middle` |
`on-click-backward` | May also be known as "side"
`on-click-forward` | May also be known as "extra"
`on-scroll-up` |
`on-scroll-down` |
`on-vscroll` | A combination of up and down
`on-scroll-left` |
`on-scroll-right` |
`on-hscroll` | A combination of left and right
`on-scroll` | A scroll in any of the 4 directions

Actions can either be a direct program execution, for example:

```toml
on-click = { exec = "firefox" }
```

Or it can be used to write a value to an existing block, for modules that support this:

```toml
on-click = { send = "mpris-block", msg = "PlayPause" }
```

Either `msg` or `format` are valid; both are text-expanded before sending to the module.

## Text Module

Any module that does not declare otherwise is displayed as text, controlled by the following keys:

Key | Expanded | Value | Details
----|----------|-------|---------
`markup` | No | true/false | True if the value contains Pango (HTML-style) markup

The actual text displayed is `{`modulename`.text}` with a tooltip of `{`modulename`.tooltip}`.

# Modules

## clock

Key | Expanded | Default | Details
----|----------|---------|--------
`format` | Yes | `%H:%M` | Time format using the strftime format specifiers
`timezone` | Yes | | Time zone to display (blank uses the system local time zone)

## disk

#### Configuration

Key | Expanded | Default | Details
----|----------|---------|--------
`path` | No | -- | Path to the disk
`poll` | No | 60 | Number of seconds to wait between reads

#### Available Keys

## eval

Key | Expanded | Default | Details
----|----------|---------|--------
`expr` | No | -- | Expression to evaluate
\* | Yes | -- | Variables usable in the expression

Evaluates the given expression.  Basic math and logic operators are supported,
but not variable assignment, conditionals, looping, or recursion.  All other
keys in this block are expanded and can be read in the expression.

## exec-json

Key | Expanded | Default | Details
----|----------|---------|--------
`command` | No | -- | Shell command to execute

The output of the shell command should be a stream of JSON values, one per
line.  The text expansion of this module will consult the most recent command
output for a matching key and return its value.

The command will not be restarted if it exits; use a wrapper script that calls
it in a loop if you want to do this.

## focus-list

Key | Expanded | Default | Details
----|----------|---------|--------
`source` | No | -- | A module name that exposes a list of values 
`item` | N/A | | A block (or block name) to display for each item in the list
`focused-item` | N/A | Same as item | A block to display for items marked as "focused" in the list

When inside a focus-list block, the `item` block refers to the current item (so
`{item.title}` would refer to the title key).

## formatted

*Note*: The `type = formatted` key is optional for this module as long as you
specify the format.

Key | Expanded | Default | Details
----|----------|---------|--------
`format` | Yes | -- | The string to display
`tooltip` | Yes | "" | The tooltip to display when hovering over the text

## group

Key | Expanded | Value | Details
----|----------|-------|--------
`condition` | Yes | empty or non-empty | If this value is set but empty, the group will not be displayed
`spacing` | Yes | number of pixels | Spacing between each item in the group.  May be negative.

## meter

Key | Expanded | Default | Details
----|----------|---------|--------
`src` | Yes | -- | Source value (must expand to a floating-point number for a working meter)
`min` | Yes | 0 | Minimum value for the "valid" range of the meter
`max` | Yes | 100 | Maximum value for the "valid" range of the meter
`values` | Yes | -- | List of format values, such as `["", "", "", "", "", ""]`
`below` | Yes | (first value) | Format to use when the value is below `min`
`above` | Yes | (last value) | Format to use when the value is above `max`

## mpris

#### Configuration

Key | Expanded | Default | Details
----|----------|---------|--------
`name` | No | "" | Name of the default player for this item; if empty, the first "playing" player will be used.

#### Values

All string (and string list) values defined by the [mpris metadata spec](http://www.freedesktop.org/wiki/Specifications/mpris-spec/metadata)
are available, in addition to `length` which is the track length in seconds,
and `player.name` which is the mpris endpoint name (which may be something like
`firefox.instance1234567`).

#### Actions

For all actions, the target of the action is either the player specified in the block or the first "playing" player.

`Next` | `Previous` | `Pause` | `PlayPause` | `Stop` | `Play` | `Raise` | `Quit`

## pulse

Key | Expanded | Default | Details
----|----------|---------|--------
`target` | No | `"sink"` | Either `sink:` or `source:` followed by the name of the particular sink.  Names can be obtained from `pactl list` and look like `alsa_output.pci-0000_00_1f.3.analog-stereo`.

## regex

Key | Expanded | Default | Details
----|----------|---------|--------
`text` | Yes | -- | The text to run the regular expression against
`regex` | No | -- | The regular expression ([syntax](https://docs.rs/regex/#syntax) details)
`replace` | No\* | -- | A replacement string for all matches of the regular expression.  `$1`, `${1}`, or `$name` refer to capture groups.

This block's value either the replaced string when called with a blank key or the group identified by the key.

## read-file

Key | Expanded | Default | Details
----|----------|---------|--------
`file` | No | -- | File name to read
`poll` | No | 60 | Number of seconds to wait between reads

Note: this is intended for reading files like `/proc/loadavg` where there is no mechanism to watch for changes to the file.

## sway-mode

Expands to the current keybinding mode in sway

## sway-workspace

The currently selected workspace

This module is valid as a target for format-list; when used there, it shows all available workspaces.

## switch

Key | Expanded | Default | Details
----|----------|---------|--------
`format` | Yes | -- | Value to match against the possible cases
`default` | Yes | "" | Value to expand if no case matches
`cases` | Yes\* | -- | Table of strings

The switch block first expands the format value, then matches the resulting
string against the table of keys listed in `cases`.  If a match is found, the
resulting value is then expanded and used as the result of the module.

```toml
[mic-s]
type = 'switch'
format = '{mic-r.mute}'
default = "<span color='#ff8888'></span>"
cases = { 0 = "<span color='#88ff88'></span>", 1 = "" }
```

If you have more cases, you might prefer the alternate syntax for tables:

```toml
[mic-s]
type = 'switch'
format = '{mic-r.mute}'
default = "<span color='#ff8888'></span>"

[mic-s.cases]
0 = "<span color='#88ff88'></span>"
1 = ""
```

## tray

Key | Expanded | Default | Details
----|----------|---------|--------
`spacing` | Yes | 0 | Spacing between items in the tray

## value

*Note*: The `type = value` key is optional for this module as long as you
specify the value.

Key | Expanded | Default | Details
----|----------|---------|--------
`value` | No | "" | A string value that can be changed by actions

The value module accepts value sent to it by [actions](#actions), which you can
use to have some blocks control the contents of others.
