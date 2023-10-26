;;; Configuration for lsp-mode.

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c ;")  ;; Or 'C-l', 's-l'
  :init
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  :config
  (lsp-enable-which-key-integration t))
