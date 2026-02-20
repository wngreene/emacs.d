;;; Configuration for navigation.

;; Scrolling buffer.
(bind-key* "M-p" 'scroll-down-line)
(bind-key* "M-n" 'scroll-up-line)

;; Set scrolling to 1 line at a time.
(setq scroll-step 1)

;; Navigation ijkl.
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-j") 'left-char)
(global-set-key (kbd "M-l") 'right-char)

(global-set-key (kbd "M-I") 'backward-paragraph)
(global-set-key (kbd "M-K") 'forward-paragraph)
(global-set-key (kbd "M-J") 'left-word)
(global-set-key (kbd "M-L") 'right-word)

;; Window resizing (C-c w prefix for terminal compatibility).
(global-set-key (kbd "C-c w b") 'shrink-window-horizontally)
(global-set-key (kbd "C-c w f") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c w p") 'shrink-window)
(global-set-key (kbd "C-c w n") 'enlarge-window)

;; winner-mode.
(winner-mode 1)
(global-set-key (kbd "C-c j") 'winner-undo)
(global-set-key (kbd "C-c l") 'winner-redo)

;; Use windmove to move cursor around split panes.
;; shift + arrow keys
(use-package windmove
  :bind* (("C-M-i" . windmove-up)
          ("C-M-k" . windmove-down)
          ("C-M-j" . windmove-left)
          ("C-M-l" . windmove-right)))
