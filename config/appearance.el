;;; Configuration for theme, colors, etc.

;; Turn off menu-bar/tool-bar.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; No theme — use the terminal's native ANSI color palette (iTerm2).
(unless (display-graphic-p)
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-foreground 'default "unspecified-fg" (selected-frame)))


;; Column number mode.
(column-number-mode 1)

;; Set line numbers.
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Set fill column and auto fill.
(setq-default fill-column 80)

;; Highlight indents.
(use-package highlight-indent-guides
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
