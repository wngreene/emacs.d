# emacs.d

W. Nicholas Greene's emacs configuration. Install some dependencies then
checkout this repo to your home directory as `.emacs.d` and launch emacs. It
should grab the required packages from the package manager:

```
# Install dependencies (Linux).
sudo apt-get install clangd
pip install --user cpplint ruff

# Install dependencies (macOS).
brew install coreutils  # Provides gls for dired --group-directories-first support.
brew install ripgrep    # Required for consult-ripgrep (project-wide search).
pip install --user cpplint ruff

# Clone and run.
git clone https://github.com/wngreene/emacs.d.git ~/.emacs.d
emacs -nw .
```

### Requires:
- emacs 28.1+

### Completion stack (Vertico + Consult)

Completion is powered by the [minad stack](https://github.com/minad/vertico):
- **Vertico** — vertical completion UI for all minibuffer commands
- **Orderless** — space-separated fuzzy matching in any order
- **Marginalia** — rich annotations next to candidates
- **Consult** — enhanced search/navigation commands
- **Embark** — contextual actions on minibuffer candidates

Key bindings:
| Key | Command | Description |
|-----|---------|-------------|
| `M-x` | (built-in) | Enhanced by Vertico automatically |
| `C-x C-f` | `find-file` | Enhanced by Vertico automatically |
| `C-x C-b` | `consult-buffer` | Buffers + recent files + bookmarks |
| `C-c h o` | `consult-line` | Search lines in current buffer |
| `C-c h g` | `consult-ripgrep` | Ripgrep (prompts for directory) |
| `C-c h i` | `consult-imenu` | Jump to symbol |
| `C-.` | `embark-act` | Contextual actions on candidate |

Project navigation uses built-in `project.el` (git root detection):
| Key | Command |
|-----|---------|
| `C-x p f` | Find file in project |
| `C-x p g` | Grep in project (scoped to git root) |
| `C-x p b` | Switch to project buffer |
| `C-x p p` | Switch project |
| `C-x p d` | Dired at project root |

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
