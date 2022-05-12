;;; Configuration for various languages.

;; Tab width
(setq-default tab-width 2)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Set some options for compilation mode.
(setq compilation-scroll-output 'first-error)

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

;; Yasnippet.
(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :config (yas-global-mode t))

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

(load-file (concat user-emacs-directory "config/languages/cpp.el"))
(load-file (concat user-emacs-directory "config/languages/python.el"))
(load-file (concat user-emacs-directory "config/languages/latex.el"))
(load-file (concat user-emacs-directory "config/languages/misc.el"))
