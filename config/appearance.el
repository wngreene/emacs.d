;;; Configuration for theme, colors, etc.

;; Monokai theme.
(use-package monokai-theme
  :ensure t
  ;; :if window-system
  :config (load-theme 'monokai t))

;; ansi-color.
;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :config (progn
            (defun my/ansi-colorize-buffer ()
              (let ((buffer-read-only nil))
                (ansi-color-apply-on-region (point-min) (point-max))))
            (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))

;; Set background to be transparent.
(defun transparent-background ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'transparent-background)

;; Column number mode.
(column-number-mode 1)

;; Set line numbers.
(use-package linum
  :ensure t
  :config (progn (add-hook 'text-mode-hook 'linum-mode)
                 (add-hook 'prog-mode-hook 'linum-mode)
                 (setq linum-format "%4d \u2502 ")))

;; Set fill column and auto fill.
(setq-default fill-column 120)
;; (setq-default auto-fill-function 'do-auto-fill)

;; Highlight indents.
(use-package highlight-indent-guides
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Fill column indicator.
(use-package fill-column-indicator
  :ensure t
  :init (add-hook `prog-mode-hook 'fci-mode))
