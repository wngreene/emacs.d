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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(image-dired-external-viewer "/usr/bin/geeqie")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (magit auctex gnu-elpa-keyring-update company-irony android-mode langtool expand-region monokai-theme git-gutter-fringe company irony helm-flycheck helm-flyspell helm-ag helm-projectile projectile helm ws-butler graphviz-dot-mode jedi google-c-style flycheck yasnippet fill-column-indicator highlight-indent-guides pandoc-mode markdown-mode rainbow-delimiters smartparens pdf-tools dockerfile-mode julia-shell julia-mode groovy-mode yaml-mode cmake-mode use-package)))
 '(use-file-dialog nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-branch-local ((t (:foreground "green"))))
 '(magit-branch-remote ((t (:foreground "cyan"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-hunk-heading ((t (:background "grey50" :foreground "grey30"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "grey80" :foreground "grey30"))))
 '(magit-log-author ((t (:foreground "magenta"))))
 '(magit-section-heading ((t (:foreground "red" :weight bold))))
 '(magit-section-highlight ((((type tty)) nil)))
 '(magit-tag ((t (:foreground "orange")))))

;; Auto refresh.
(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)

;; Dired settings.
(setq delete-by-moving-to-trash t) ;; Move to trash instead of deleting.
(setq dired-listing-switches "-alh --group-directories-first") ;; Set dired switches.
(setq dired-recursive-deletes 'always) ;; Allow for recursive trashing.
(setq dired-auto-revert-buffer t) ;; Updates dired buffers. automatically.

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

;; Magit.
(use-package magit
  :ensure t
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn (setq magit-diff-refine-hunk t))
  :bind (("C-x g" . magit-status)))

;; Yasnippet.
(use-package yasnippet
  :ensure t
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode t))

;; Load indidvual configuration files.
(load-file (concat user-emacs-directory "config/appearance.el"))
(load-file (concat user-emacs-directory "config/navigation.el"))
(load-file (concat user-emacs-directory "config/languages.el"))
(load-file (concat user-emacs-directory "config/linting.el"))
