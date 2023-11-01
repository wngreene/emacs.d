;;; Configuration for lsp-mode.

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c ;")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))
