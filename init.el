;;; .emacs.s/init.el --- wng init file.

;;; Code:

;; Set up use-package.
(require 'package)
(setq package-enable-at-startup nil)

;; Add repos.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
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
 '(inhibit-startup-screen t)
 '(use-file-dialog nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background nil :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 85 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; Auto refresh.
(global-auto-revert-mode t)

;; Tab width
(setq-default tab-width 2)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Column number mode.
(column-number-mode 1)

;; Navigation.
(global-set-key (kbd "<s-up>") "\C-u1\M-v")
(global-set-key (kbd "<s-down>") "\C-u1\C-v")

(global-set-key (kbd "M-s-p") "\C-u1\M-v")
(global-set-key (kbd "M-s-n") "\C-u1\C-v")

(global-set-key (kbd "M-j") "\C-u1\M-v")
(global-set-key (kbd "M-k") "\C-u1\C-v")

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Window resizing.
(global-set-key (kbd "C-s-b") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-f") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-p") 'shrink-window)
(global-set-key (kbd "C-s-n") 'enlarge-window)

;; Commenting.
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-v") 'uncomment-region)

;; Set line numbers.
(use-package linum
  :ensure t
  :config (global-linum-mode t))

;; Show parentheses.
(require 'paren)
(use-package paren
  :ensure t
  :config (show-paren-mode t))

;; Use windmove to move cursor around split panes.
;; shift + arrow keys
(use-package windmove
  :config (windmove-default-keybindings 'meta))

;; CMake
(use-package cmake-mode)

;; xml.
(use-package xml
  :mode (("\\.launch\\'" . xml-mode)))

;; latex.
(use-package tex-mode
  :mode (("\\.tex\\'" . latex-mode)))

;; c++.
(use-package c++-mode
  :config (c-set-offset 'innamespace 0)
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cu\\'" . c++-mode)
         ("\\.cl\\'" . c++-mode)))

;; Pair completion.
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode t))

;; Rainbow delimiters.
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Markdown mode.
(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; Indent highlighting.
(use-package highlight-indentation
  :load-path "~/.emacs.d/Highlight-Indentation-for-Emacs"
  :init (progn (add-hook 'c-mode-hook 'highlight-indentation-mode)
               (add-hook 'c++-mode-hook 'highlight-indentation-mode)
               (add-hook 'python-mode-hook 'highlight-indentation-mode)
               (add-hook 'xml-mode-hook 'highlight-indentation-mode)
               (add-hook 'java-mode-hook 'highlight-indentation-mode)
               (add-hook 'cmake-mode-hook 'highlight-indentation-mode))
  :config (highlight-indentation-mode t))

;; Yasnippet.
(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode t))

;; Flycheck.
(use-package flycheck
  :ensure t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;; Flycheck Google cpplint
(use-package flycheck-google-cpplint
  :ensure t
  :init (progn '(flycheck-googlelint-verbose "3")
               '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
               '(flycheck-googlelint-root "project/src")
               '(flycheck-googlelint-linelength "80"))
  :config (progn (add-hook 'c-mode-hook (lambda ()
                                          (flycheck-select-checker 'c/c++-googlelint))
                 (add-hook 'c++-mode-hook (lambda ()
                                            (flycheck-select-checker 'c/c++-googlelint))))))

;; Magit.
(use-package magit
  :ensure t
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-x g" . magit-status)))

;; Jedi.
(use-package jedi
  :ensure t
  :init (setq jedi:complete-on-dot t)
  :config (add-hook 'python-mode-hook 'jedi:setup))

;; Graphiviz mode.
(use-package graphviz-dot-mode
  :ensure t)

;; Whitespace butler.
(use-package ws-butler
  :ensure t
  :commands ws-butler-mode
  :init (progn (add-hook 'c-mode-common-hook 'ws-butler-mode)
               (add-hook 'cc-mode-common-hook 'ws-butler-mode)
               (add-hook 'c++-mode-common-hook 'ws-butler-mode)
               (add-hook 'python-mode-hook 'ws-butler-mode)
               (add-hook 'cython-mode-hook 'ws-butler-mode)))

;; Helm.
(use-package helm
  :ensure t
  :config (progn (helm-autoresize-mode 1))
  :bind (("M-x"     . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)))

;; Projectile.
(use-package projectile
  :ensure t
  :init (progn (projectile-global-mode))
  :config (progn (setq projectile-completion-system 'helm)
                 ;; (setq projectile-switch-project-action 'helm-projectile)
                 (setq projectile-enable-caching t)
                 (setq projectile-indexing-method 'native)))

;; auto-complete.
(use-package auto-complete
  :ensure t
  :config (global-auto-complete-mode t)
  :bind ("<C-tab>" . auto-complete))

;; Git gutter fringe.
(use-package git-gutter-fringe
  :ensure t
  :if window-system
  :config (global-git-gutter-mode t))

;; Monokai theme.
(use-package monokai-theme
  :ensure t
  ;; :if window-system
  :config (load-theme 'monokai t))

;; expand-region.
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; dired-details.
(use-package dired-details
  :ensure t
  :init (setq dired-details-hidden-string "")
  :bind (("C-c C-s" . dired-details-toggle)))
