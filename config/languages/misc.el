;;; Misc languages.

;; Bazel.
(use-package bazel
  :mode (("\\.bazel\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode)
         ("\\BUILD\\'" . bazel-mode)
         ("\\WORKSPACE\\'" . bazel-mode)
         ))

;; yaml-mode.
(use-package yaml-mode
  :ensure t)

;; Markdown mode.
(use-package markdown-mode
  :ensure t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; xml.
(use-package xml
  :mode (("\\.launch\\'" . xml-mode)))

;; Docker.
(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; Protobuf.
(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode)
         ("\\.pbtxt\\'" . protobuf-mode)))

;; Typescript.
(use-package typescript-mode
  :ensure t
  :config (progn (setq typescript-indent-level 2))
  :mode (("\\.ts\\'" . typescript-mode)))
