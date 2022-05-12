;;; Configuration for navigation.

;; Scrolling buffer.
(global-set-key (kbd "M-s-p") "\C-u1\M-v")
(global-set-key (kbd "M-s-n") "\C-u1\C-v")

(global-set-key (kbd "M-p") "\C-u1\M-v")
(global-set-key (kbd "M-n") "\C-u1\C-v")

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

;; Window resizing.
(global-set-key (kbd "C-s-b") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-f") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-p") 'shrink-window)
(global-set-key (kbd "C-s-n") 'enlarge-window)

;; winner-mode.
(winner-mode 1)
(global-set-key (kbd "C-c j") 'winner-undo)
(global-set-key (kbd "C-c l") 'winner-redo)

;; Use windmove to move cursor around split panes.
;; shift + arrow keys
(use-package windmove
  :config (windmove-default-keybindings 'meta)
  :init
  :bind* (("C-M-i" . windmove-up)
          ("C-M-k" . windmove-down)
          ("C-M-j" . windmove-left)
          ("C-M-l" . windmove-right)))

;; Dired settings.
(setq delete-by-moving-to-trash t) ;; Move to trash instead of deleting.
(setq dired-listing-switches "-alh --group-directories-first") ;; Set dired switches.
(setq dired-recursive-deletes 'always) ;; Allow for recursive trashing.
(setq dired-auto-revert-buffer t) ;; Updates dired buffers. automatically.

;; Helm.
(use-package helm
  :ensure t
  :config (progn (helm-autoresize-mode 1)
                 (setq helm-buffer-max-length nil))
  :bind (("M-x"     . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-c h i" . helm-semantic-or-imenu)
         ("C-c h o" . helm-occur)))

;; Projectile.
(use-package projectile
  :ensure t
  :init (progn (projectile-global-mode))
  :config (progn (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
                 (add-to-list 'projectile-other-file-alist '("h" "cc"))
                 (add-to-list 'projectile-other-file-alist '("cc" "h"))
                 (add-to-list 'projectile-globally-ignored-directories ".git")
                 (add-to-list 'projectile-globally-ignored-files "GPATH")
                 (add-to-list 'projectile-globally-ignored-files "GTAGS")
                 (add-to-list 'projectile-globally-ignored-files "GRTAGS")
                 (setq projectile-find-dir-includes-top-level t)
                 (setq projectile-completion-system 'helm)
                 (setq projectile-switch-project-action 'helm-projectile)
                 (setq projectile-enable-caching t)
                 (setq projectile-indexing-method 'alien)))

;; helm-projectile.
(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

;; Needed for helm-projectile-grep.
(use-package grep)

;; helm-ag
(use-package helm-ag
  :ensure t)
