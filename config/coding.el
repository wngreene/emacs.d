;;; Configuration for various languages.

;; Tab width
(setq-default tab-width 2)
(setq-default sh-indentation 2)
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)

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

;; Bazel.
(use-package python-mode
  :mode (("\\.bazel\\'" . python-mode)
         ("\\.bzl\\'" . python-mode)
         ("\\BUILD\\'" . python-mode)
         ("\\WORKSPACE\\'" . python-mode)
         ))

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

;; Protobuf.
(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode)
         ("\\.pbtxt\\'" . protobuf-mode)))

;; Typescript.
(use-package typescript-mode
  :ensure t
  :config (progn (setq typescript-indent-level 2))
  :mode (("\\.ts\\'" . typescript-mode)))

(load-file (concat user-emacs-directory "config/languages/cpp.el"))
(load-file (concat user-emacs-directory "config/languages/python.el"))
(load-file (concat user-emacs-directory "config/languages/latex.el"))
