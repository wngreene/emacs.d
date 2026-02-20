;;; Configuration for magit.

;; Magit.
(use-package magit
  :ensure t
  :config (progn (setq magit-diff-refine-hunk t)))
