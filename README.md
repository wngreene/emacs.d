# emacs.d

W. Nicholas Greene's emacs configuration. Install some dependencies then
checkout this repo to your home directory as `.emacs.d` and launch emacs. It
should grab the required packages from the package manager:

```
# Install dependencies (Linux).
sudo apt-get install clangd
pip install --user pylint cpplint

# Install dependencies (macOS).
brew install coreutils  # Provides gls for dired --group-directories-first support.

# Clone and run.
git clone https://github.com/wngreene/emacs.d.git ~/.emacs.d
emacs -nw .
```

### Requires:
- emacs 27.1
- macOS: GNU coreutils (`brew install coreutils`)

### macOS: Caps Lock as Meta in Terminal Emacs (iTerm2)

Caps Lock is in the same physical position on every keyboard, making it a
consistent Meta key when running Emacs in a terminal.

**Karabiner-Elements** (`brew install --cask karabiner-elements`):
- Simple Modifications → All Devices: `caps_lock → right_option`
- Simple Modifications → External PC keyboard: `left_option → left_command`
  (makes the physical Alt key act as Command system-wide)

**System Settings → Keyboard → Modifier Keys → Karabiner DriverKit Virtual HID Keyboard**:
- Leave at defaults (Option → Option, Command → Command)

**iTerm2 → Settings → Profiles → Keys**:
- Left Option Key: `Normal`
- Right Option Key: `Esc+`

Result: `Caps Lock + x/i/j/k/l/...` → Meta in Emacs on any keyboard.
Window resize bindings use `C-c w b/f/p/n` (terminal-compatible alternative
to `C-s-b/f/p/n` which don't work in terminal Emacs).
