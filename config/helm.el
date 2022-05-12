;;; Configuration for helm.

(use-package helm
  :ensure t
  :config (progn (helm-autoresize-mode 1)
                 (setq helm-buffer-max-length nil))
  :bind (("M-x"     . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-c h i" . helm-semantic-or-imenu)
         ("C-c h o" . helm-occur)))

;; helm-projectile.
(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

;; Needed for helm-projectile-grep.
(use-package grep)

;; helm-ag
(use-package helm-ag
  :ensure t)
