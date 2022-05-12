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
