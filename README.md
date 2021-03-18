# rwaybar

A taskbar for Wayland written in Rust.  Works on any compositor supporting
the layer\_shell protocol (sway, most wlroots window managers, kwin).

## Available Modules

- Clock
- Custom scripts
- Disk (filesystem) free
- File reader (for showing battery, temperature, load average, etc.)
- MPRIS-compliant media player support
- PulseAudio volume controller
- Sway (workspaces, binding mode)
- Tray

See the [configuration documentation](doc/config.md) for details.

## Other Features

- Clicks can execute custom scripts or provide input to existing ones
- Support for showing meters () and alerts.
- Regular expressions
- Simple expression evaluation
- Config reload on SIGHUP

## Building

```bash
cargo build --release
cp doc/rwaybar.toml ~/.config/
./target/release/rwaybar
```

## Samples

![sample bar](doc/sample-black.png "Bar with black background")

![sample bar](doc/sample-white.png "Same bar with white background")

These two samples are using the same configuration, only the background color
differs.  I like a transparent background on my taskbar, but I also configure
my desktop background to be a slideshow.  This means that I need the bar to be
readable regardless of the color of the background, which was done in this
example by using text-outline.  The tray needs a solid background because some
icons (kdeconnect, steam) aren't otherwise visible on light backgrounds.

Note: these images were captured on a scaled (HiDPI) output, which is why they
appear double the size defined in the sample bar configuration.
