;;; .emacs.s/init.el --- wng init file.

;;; Code:

;; Set up use-package.
(require 'package)
(setq package-enable-at-startup nil)

;; Add repos.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Misc stuff.
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)

;; Auto refresh.
(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)

;; desktop-save
;; Code below allows desktop-save to restore buffer/window layout when in
;; terminal mode.
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
          (lambda ()
            (frameset-restore
             desktop-saved-frameset
             :reuse-frames (eq desktop-restore-reuses-frames t)
             :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
             :force-display desktop-restore-in-current-display
                 :force-onscreen desktop-restore-forces-onscreen)))

;; Functions to undo fill-paragraph.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Yasnippet.
(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode t))

;; Load indidvual configuration files.
(load-file (concat user-emacs-directory "config/appearance.el"))
(load-file (concat user-emacs-directory "config/navigation.el"))
(load-file (concat user-emacs-directory "config/magit.el"))
(load-file (concat user-emacs-directory "config/languages.el"))
(load-file (concat user-emacs-directory "config/linting.el"))

;; "Custom" file stuff for things that emacs automatically saves.
(setq custom-file (concat user-emacs-directory "config/custom.el"))
(load custom-file)
