;;; Configuration for magit.

;; Magit.
(use-package magit
  :ensure t
  :config (progn (setq magit-diff-refine-hunk t))
  :bind (("C-x g" . magit-status)))
