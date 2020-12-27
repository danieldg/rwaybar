# rwaybar

A bar for Wayland (for wlroots compositors like sway) written in Rust.

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

## Building

```bash
cargo build --release
cp doc/rwaybar.toml .
./target/release/rwaybar
```
