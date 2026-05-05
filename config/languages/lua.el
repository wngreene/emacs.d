;;; Configuration for Lua and Luau.

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)
         ("\\.luau\\'" . lua-mode))
  :config (setq lua-indent-level 2))
