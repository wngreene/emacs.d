;;; .emacs.s/init.el --- wng init file.

;;; Code:

;; Set up use-package.
(require 'package)
(setq package-enable-at-startup nil)

;; Add repos.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Load indidvual configuration files.
(load-file (concat user-emacs-directory "config/misc.el"))
(load-file (concat user-emacs-directory "config/appearance.el"))
(load-file (concat user-emacs-directory "config/navigation.el"))
(load-file (concat user-emacs-directory "config/helm.el"))
(load-file (concat user-emacs-directory "config/projectile.el"))
(load-file (concat user-emacs-directory "config/magit.el"))
(load-file (concat user-emacs-directory "config/coding.el"))

;; "Custom" file stuff for things that emacs automatically saves.
(setq custom-file (concat user-emacs-directory "config/custom.el"))
(load custom-file)
