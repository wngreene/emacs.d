# emacs.d

W. Nicholas Greene's emacs configuration. Install some dependencies then
checkout this repo to your home directory as `.emacs.d` and launch emacs. It
should grab the required packages from the package manager:

```
# Install dependencies.
pip install --user pylint cpplint

# Install language servers.
sudo apt-get install clangd
pip install --user pyright

# Clone and run.
git clone https://github.com/wngreene/emacs.d.git ~/.emacs.d
emacs -nw .
```

Based on guides here:
- https://ianyepan.github.io/posts/emacs-ide
- https://www.youtube.com/watch?v=jPXIP46BnNA

### Requires:
- emacs 27.1
