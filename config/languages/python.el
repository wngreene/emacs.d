;;; Configuration for python.

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

;; pyright language server.
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; Black formatter.
(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Use the ipython interpreter.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))

;; flycheck config.
(flycheck-add-next-checker 'lsp 'python-pyright)
(flycheck-add-next-checker 'python-pyright 'python-mypy)
;;                (add-hook 'python-mode-hook (lambda ()
;;                                              (flycheck-select-checker 'python-pylint)))
