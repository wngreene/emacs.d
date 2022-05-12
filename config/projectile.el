;;; Configuration for projectile.

(use-package projectile
  :ensure t
  :init (progn (projectile-global-mode))
  :config (progn (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
                 (add-to-list 'projectile-other-file-alist '("h" "cc"))
                 (add-to-list 'projectile-other-file-alist '("cc" "h"))
                 (add-to-list 'projectile-globally-ignored-directories ".git")
                 (setq projectile-find-dir-includes-top-level t)
                 (setq projectile-completion-system 'helm)
                 (setq projectile-switch-project-action 'helm-projectile)
                 (setq projectile-enable-caching t)
                 (setq projectile-indexing-method 'alien)))
