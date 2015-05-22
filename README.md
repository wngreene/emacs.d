# emacs.d

W. Nicholas Greene's emacs configuration. Checkout this repo to your home directory
(including submodules) as `.emacs.d` and launch emacs. It should grab
the required packages from MELPA:

```
cd
git clone git@github.mit.edu:USERNAME/emacs.d.git .emacs.d
cd .emacs.d
git submodule update --init Highlight-Indentation-for-Emacs
emacs . &
```

### Requires:
- emacs24
- cpplint.py

