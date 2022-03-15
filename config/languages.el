;;; Configuration for various languages.

;; Tab width
(setq-default tab-width 2)
(setq-default sh-indentation 2)
(setq-default css-indent-offset 2)
(setq-default js-indent-level 2)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

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

;; C/C++
(use-package c++-mode
  :config (c-set-offset 'innamespace 0)
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cu\\'" . c++-mode)
         ("\\.cuh\\'" . c++-mode)
         ("\\.cl\\'" . c++-mode)
         ("\\.clh\\'" . c++-mode)))

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

;; Use the ipython interpreter.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))

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
