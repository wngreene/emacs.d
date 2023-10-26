;;; Configuration to C/C++.

;; C/C++
(use-package c++-mode
  :config (c-set-offset 'innamespace 0)
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cu\\'" . c++-mode)
         ("\\.cuh\\'" . c++-mode)
         ("\\.cl\\'" . c++-mode)
         ("\\.clh\\'" . c++-mode)))

;; google-c-style
(use-package google-c-style
  :ensure t
  :init (progn (add-hook 'c-mode-common-hook 'google-set-c-style)
               (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

;; CMake
(use-package cmake-mode
  :ensure t)

;; ;; Flycheck Google cpplint
;; (use-package flycheck-google-cpplint
;;   :ensure t
;;   :config (progn (setq flycheck-googlelint-verbose "3")
;;                  (setq flycheck-c/c++-googlelint-executable "cpplint")
;;                  ;; (setq flycheck-googlelint-root "src")
;;                  (setq flycheck-googlelint-linelength "120")
;;                  (setq flycheck-googlelint-filter "-build/c++11,-build/header_guard,-build/include_order")
;;                  (flycheck-add-next-checker 'c/c++-cppcheck
;;                                             '(warning . c/c++-googlelint))))
