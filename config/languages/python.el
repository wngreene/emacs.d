;;; Configuration for python.

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

;; ;; Jedi.
;; (use-package jedi
;;   :ensure t
;;   :init (setq jedi:complete-on-dot t)
;;   :config (add-hook 'python-mode-hook 'jedi:setup))

;; Use the ipython interpreter.
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))
