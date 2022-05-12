;;; Configuration to latex.

(use-package tex-site
  :ensure auctex
  :mode (("\\.tex\\'" . LaTeX-mode))
  :config (progn (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
                 (setq TeX-auto-save t)
                 (setq TeX-parse-self t)
                 (setq-default TeX-master nil)
                 (setq latex-run-command "pdflatex")
                 (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
                 ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                 (setq TeX-view-program-selection '((output-pdf "Atril"))
                       TeX-source-correlate-start-server t)
                 (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
                           #'TeX-revert-document-buffer)))
