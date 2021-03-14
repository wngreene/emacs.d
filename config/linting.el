;;; Configuration for linting.

;; Whitespace butler.
(use-package ws-butler
  :ensure t
  :commands ws-butler-mode
  :init (progn (add-hook 'text-mode-hook 'ws-butler-mode)
               (add-hook 'prog-mode-hook 'ws-butler-mode)))

;; Flycheck.
(use-package flycheck
  :ensure t
  :config (progn (add-hook 'after-init-hook #'global-flycheck-mode)
                 (add-hook 'python-mode-hook (lambda ()
                                               (flycheck-select-checker 'python-pylint)))
                 (setq flycheck-python-pylint-executable "pylint")
                 (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))))

;; Flycheck Google cpplint
(use-package flycheck-google-cpplint
  :load-path "~/.emacs.d/flycheck-google-cpplint"
  :config (progn (setq flycheck-googlelint-verbose "3")
                 (setq flycheck-c/c++-googlelint-executable "cpplint")
                 ;; (setq flycheck-googlelint-root "src")
                 (setq flycheck-googlelint-linelength "120")
                 (setq flycheck-googlelint-filter "-build/c++11,-build/header_guard,-build/include_order")
                 (flycheck-add-next-checker 'c/c++-cppcheck
                                            '(warning . c/c++-googlelint))))

;; Flyspell modes.
(add-hook 'c-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'c++-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'xml-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'java-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'cmake-mode-hook (lambda () (flyspell-prog-mode)))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
