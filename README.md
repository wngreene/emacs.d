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
