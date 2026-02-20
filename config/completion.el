;;; Configuration for completion (vertico + consult stack).

;; Vertico — vertical completion UI.
(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Orderless — space-separated matching in any order.
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

;; Marginalia — rich annotations next to candidates.
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; Consult — enhanced search/navigation commands.
(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-c h i" . consult-imenu)
         ("C-c h o" . consult-line)
         ("C-c h g" . consult-ripgrep)))

;; Embark — contextual actions on minibuffer candidates.
(use-package embark
  :ensure t
  :bind ("C-." . embark-act))

;; Embark-consult — integration between embark and consult.
(use-package embark-consult
  :ensure t
  :demand t
  :after (embark consult))
