;;; .emacs.s/init.el --- wng init file.

;;; Code:

;; Set up use-package.
(require 'package)
(setq package-enable-at-startup nil)

;; Add repos.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(image-dired-external-viewer "/usr/bin/geeqie")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (magit auctex gnu-elpa-keyring-update company-irony android-mode langtool expand-region monokai-theme git-gutter-fringe company irony helm-flycheck helm-flyspell helm-ag helm-projectile projectile helm ws-butler graphviz-dot-mode jedi google-c-style flycheck yasnippet fill-column-indicator highlight-indent-guides pandoc-mode markdown-mode rainbow-delimiters smartparens pdf-tools dockerfile-mode julia-shell julia-mode groovy-mode yaml-mode cmake-mode use-package)))
 '(use-file-dialog nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-branch-local ((t (:foreground "green"))))
 '(magit-branch-remote ((t (:foreground "cyan"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-hunk-heading ((t (:background "grey50" :foreground "grey30"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "grey80" :foreground "grey30"))))
 '(magit-log-author ((t (:foreground "magenta"))))
 '(magit-section-heading ((t (:foreground "red" :weight bold))))
 '(magit-section-highlight ((((type tty)) nil)))
 '(magit-tag ((t (:foreground "orange")))))

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

;; Navigation.
(global-set-key (kbd "M-s-p") "\C-u1\M-v")
(global-set-key (kbd "M-s-n") "\C-u1\C-v")

(global-set-key (kbd "M-p") "\C-u1\M-v")
(global-set-key (kbd "M-n") "\C-u1\C-v")

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

;; Auto refresh.
(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)

;; Tab width
(setq-default tab-width 2)
(setq-default sh-indentation 2)
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Column number mode.
(column-number-mode 1)

;; Set line numbers.
(use-package linum
  :ensure t
  :config (progn (add-hook 'text-mode-hook 'linum-mode)
                 (add-hook 'prog-mode-hook 'linum-mode)
                 (setq linum-format "%4d \u2502 ")))

;; Dired settings.
(setq delete-by-moving-to-trash t) ;; Move to trash instead of deleting.
(setq dired-listing-switches "-alh --group-directories-first") ;; Set dired switches.
(setq dired-recursive-deletes 'always) ;; Allow for recursive trashing.
(setq dired-auto-revert-buffer t) ;; Updates dired buffers. automatically.

;; Set fill column and auto fill.
(setq-default fill-column 120)
;; (setq-default auto-fill-function 'do-auto-fill)

;; Set some options for compilation mode.
(setq compilation-scroll-output 'first-error)

;; Set scrolling to 1 line at a time.
(setq scroll-step 1)

;; Fill column indicator.
(use-package fill-column-indicator
  :ensure t
  :init (add-hook `prog-mode-hook 'fci-mode))

;; Functions to undo fill-paragraph.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Highlight indents.
(use-package highlight-indent-guides
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Use the ipython interpreter.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))
  
;; gdb.
(setq gdb-many-windows t)

;; gud.
(use-package gud
  :ensure t
  :bind (([f5] . gud-cont)
         ([f6] . gud-finish)
         ([f7] . gud-tbreak)
         ([f9] . gud-break)
         ([f10] . gud-next)
         ([f11] . gud-step)))

;; Highlight matching pairs of parentheses.
(require 'paren)
(use-package paren
  :ensure t
  :config (show-paren-mode t))

;; Pair completion.
(use-package smartparens
  :ensure t
  :config (progn (smartparens-global-mode t)
                 (sp-local-pair `LaTeX-mode "$" "$")))

;; Rainbow delimiters.
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Flyspell modes.
(add-hook 'c-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'c++-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'xml-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'java-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'cmake-mode-hook (lambda () (flyspell-prog-mode)))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Magit.
(use-package magit
  :ensure t
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn (setq magit-diff-refine-hunk t))
  :bind (("C-x g" . magit-status)))

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

;; helm-flyspell
(use-package helm-flyspell
  :ensure t
  :bind (("C-c '" . helm-flyspell-correct)))

;; helm-flycheck
(use-package helm-flycheck
  :ensure t
  :bind (("C-c ! h" . helm-flycheck)))

;; Yasnippet.
(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode t))

;; Whitespace butler.
(use-package ws-butler
  :ensure t
  :commands ws-butler-mode
  :init (progn (add-hook 'text-mode-hook 'ws-butler-mode)
               (add-hook 'prog-mode-hook 'ws-butler-mode)))

;; Flycheck.
(use-package flycheck
  :ensure t
  :config (progn (add-hook 'after-init-hook #'global-flycheck-mode)
                 (add-hook 'python-mode-hook (lambda ()
                                               (flycheck-select-checker 'python-pylint)))
                 (setq flycheck-python-pylint-executable "pylint")
                 (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))))

;; c++.
(use-package c++-mode
  :config (c-set-offset 'innamespace 0)
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cu\\'" . c++-mode)
         ("\\.cuh\\'" . c++-mode)
         ("\\.cl\\'" . c++-mode)
         ("\\.clh\\'" . c++-mode)))

;; Flycheck Google cpplint
(use-package flycheck-google-cpplint
  :load-path "~/.emacs.d/flycheck-google-cpplint"
  :config (progn (setq flycheck-googlelint-verbose "3")
                 (setq flycheck-c/c++-googlelint-executable "cpplint")
                 ;; (setq flycheck-googlelint-root "src")
                 (setq flycheck-googlelint-linelength "120")
                 (setq flycheck-googlelint-filter "-build/c++11,-build/header_guard,-build/include_order")
                 (flycheck-add-next-checker 'c/c++-cppcheck
                                            '(warning . c/c++-googlelint))))

;; google-c-style
(use-package google-c-style
  :ensure t
  :init (progn (add-hook 'c-mode-common-hook 'google-set-c-style)
               (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

;; CMake
(use-package cmake-mode
  :ensure t)

;; ;; Jedi.
;; (use-package jedi
;;   :ensure t
;;   :init (setq jedi:complete-on-dot t)
;;   :config (add-hook 'python-mode-hook 'jedi:setup))

;; yaml-mode.
(use-package yaml-mode
  :ensure t)

;; Markdown mode.
(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; xml.
(use-package xml
  :mode (("\\.launch\\'" . xml-mode)))

;; Docker.
(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; latex.
(use-package tex-site
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config (progn (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
                 (setq TeX-auto-save t)
                 (setq TeX-parse-self t)
                 (setq-default TeX-master nil)
                 (setq latex-run-command "pdflatex")
                 (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
                 ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                 (setq TeX-view-program-selection '((output-pdf "Atril"))
                       TeX-source-correlate-start-server t)
                 (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
                           #'TeX-revert-document-buffer)))
