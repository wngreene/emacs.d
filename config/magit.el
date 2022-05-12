;;; Configuration for magit.

;; Magit.
(use-package magit
  :ensure t
  :config (progn (setq magit-diff-refine-hunk t))
  :custom-face
  (magit-branch-local ((t (:foreground "green"))))
  (magit-branch-remote ((t (:foreground "cyan"))))
  (magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
  (magit-diff-file-heading ((((type tty)) nil)))
  (magit-diff-hunk-heading ((t (:background "grey50" :foreground "grey30"))))
  (magit-diff-hunk-heading-highlight ((t (:background "grey80" :foreground "grey30"))))
  (magit-log-author ((t (:foreground "magenta"))))
  (magit-section-heading ((t (:foreground "red" :weight bold))))
  (magit-section-highlight ((((type tty)) nil)))
  (magit-tag ((t (:foreground "orange"))))
  :bind (("C-x g" . magit-status)))
