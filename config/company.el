;;; Configuration for company.

(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  ;; :bind (:map company-active-map
  ;;        ("<tab>" . company-complete-selection))
  ;;       (:map lsp-mode-map
  ;;        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-minimum-width 70)
  (company-tooltip-maximum-width 80))
