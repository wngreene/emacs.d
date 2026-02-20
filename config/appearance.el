;;; Configuration for theme, colors, etc.

;; Turn off menu-bar/tool-bar.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; No theme — use the terminal's native ANSI color palette (iTerm2).
(unless (display-graphic-p)
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-foreground 'default "unspecified-fg" (selected-frame)))

;; ansi-color.
;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :config (progn
            (defun my/ansi-colorize-buffer ()
              (let ((buffer-read-only nil))
                (ansi-color-apply-on-region (point-min) (point-max))))
            (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))


;; Column number mode.
(column-number-mode 1)

;; Set line numbers.
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Set fill column and auto fill.
(setq-default fill-column 80)
;; (setq-default auto-fill-function 'do-auto-fill)

;; Highlight indents.
(use-package highlight-indent-guides
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
