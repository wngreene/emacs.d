;;; .emacs.s/init.el --- wng init file.

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Add the Melpa repo.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Add the Marmalade repo.
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(minimap-always-recenter t)
 '(minimap-recenter-type (quote relative))
 '(minimap-window-location (quote right))
 '(safe-local-variable-values (quote ((company-clang-arguments "-I/home/wng/Projects/rrg/install/include" "-I/home/wng/Projects/rrg/camera_flycap2_driver/src/"))))
 '(use-file-dialog nil))

;; Auto refresh.
(global-auto-revert-mode t)

;; Navigation.
(global-set-key (kbd "<s-up>") "\C-u1\M-v")
(global-set-key (kbd "<s-down>") "\C-u1\C-v")

(global-set-key (kbd "M-s-p") "\C-u1\M-v")
(global-set-key (kbd "M-s-n") "\C-u1\C-v")

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Window resizing.
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; Commenting.
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-S-c C-S-c") 'uncomment-region)

;; Pair completion.
(electric-pair-mode 1)
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\"" 'electric-pair)
            (define-key python-mode-map "\'" 'electric-pair)
            (define-key python-mode-map "(" 'electric-pair)
            (define-key python-mode-map "[" 'electric-pair)
            (define-key python-mode-map "{" 'electric-pair)))
(add-hook 'cc-mode-hook
          (lambda ()
            (define-key cc-mode-map "\"" 'electric-pair)
            (define-key cc-mode-map "\'" 'electric-pair)
            (define-key cc-mode-map "(" 'electric-pair)
            (define-key cc-mode-map "[" 'electric-pair)
            (define-key cc-mode-map "{" 'electric-pair)))
(add-hook 'latex-mode-hook
          (lambda ()
            (define-key latex-mode-map "\"" 'electric-pair)
            (define-key latex-mode-map "\'" 'electric-pair)
            (define-key latex-mode-map "(" 'electric-pair)
            (define-key latex-mode-map "[" 'electric-pair)
            (define-key latex-mode-map "{" 'electric-pair)))


;; Tab width
(setq-default tab-width 2)

;; Don't indent in namespaces.
(c-set-offset 'innamespace 0)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Set line numbers.
(require 'linum)
(global-linum-mode)

;; Show parentheses.
(require 'paren)
(show-paren-mode 1)

;; Use windmove to move cursor around split panes.
;; shift + arrow keys
(require 'windmove)
(windmove-default-keybindings 'meta)

;; CMake
(require 'cmake-mode) 

; Fill column indicator.
;;(require 'fill-column-indicator)
;;(add-hook 'after-change-major-mode-hook 'fci-mode)
;;(setq fci-rule-column 80)

;; Column number mode.
(column-number-mode 1)

;; File associations.
;;(require 'xml)
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . c++-mode))

;; Markdown mode.
(add-to-list 'load-path "~/.emacs.d/markdown-mode/")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Indent highlighting.
(add-to-list 'load-path "~/.emacs.d/Highlight-Indentation-for-Emacs/")
(require 'highlight-indentation)
(highlight-indentation-mode 1)
(add-hook 'c-mode-hook 'highlight-indentation-mode)
(add-hook 'c++-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'xml-mode-hook 'highlight-indentation-mode)
(add-hook 'java-mode-hook 'highlight-indentation-mode)
(add-hook 'cmake-mode-hook 'highlight-indentation-mode)

;; Yasnippet.
;;(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20141223.303/")
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))

;; Flycheck.
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flycheck Google cpplint
(require 'flycheck)
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)))
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     ;; (flycheck-add-next-checker 'c/c++-cppcheck
     ;;                            'c/c++-googlelint 'append)))
(add-hook 'c-mode-hook (lambda ()
                          (flycheck-select-checker 'c/c++-googlelint)
                          ))
(add-hook 'c++-mode-hook (lambda ()
                          (flycheck-select-checker 'c/c++-googlelint)
                          ))
'(flycheck-googlelint-verbose "3")
'(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
'(flycheck-googlelint-root "project/src")
'(flycheck-googlelint-linelength "80")

;; GGtags.
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'python-mode)
              (ggtags-mode 1))))
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g d") 'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; Company.
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "<C-tab>") 'company-complete)
;;(define-key company-active-map (kbd "<C-e>") #'company-other-backend)
(setq company-idle-delay 0.1)

(setq company-backends (delete 'company-semantic company-backends))
;;(define-key c-mode-map  (kbd "<C-tab>") 'company-complete)
;;(define-key c++-mode-map (kbd "<C-tab>") 'company-complete)

;; Company C/C++ Headers.
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")

;; Magit.
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Jedi.
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Graphiviz mode.
(add-to-list 'load-path "~/.emacs.d/graphviz-dot-mode/")
(require 'graphviz-dot-mode)

;; Stuff to run when a window is present.
(when window-system 
;; Set theme.
(require 'monokai-theme)
(load-theme 'monokai t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(minimap-always-recenter t)
 '(minimap-recenter-type (quote relative))
 '(minimap-window-location (quote right))
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 85 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
)

;; speedbar
;; ========
;; (when window-system
;;   (speedbar t))

;; ;; jump to speedbar frame
;; (global-set-key (kbd "<f4>") 'speedbar-get-focus)

;; sr-speedbar
;; (add-to-list 'load-path "~/.emacs.d/sr-speedbar/")
;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; (sr-speedbar-open)
;; (with-current-buffer sr-speedbar-buffer-name
;;   (setq window-size-fixed 'width))
