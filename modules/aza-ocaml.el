(use-package tuareg
  :mode ("\\.ml[ily]?$"
         "\\.topml$")
  :hook ((merlin-mode. tuareg-mode)
         (flycheck-ocaml . tuareg-mode)
         (utop-minor-mode . tuareg-mode))
  :bind (:map tuareg-mode-map ("C-c C-s" . 'utop))
  :config
  (setq compile-command "opam config exec corebuild "))

(use-package utop
  :after tuareg
  :config
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t))

(use-package merlin
  :after tuareg
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  (add-hook 'merlin-mode-hook 'company-mode)
  (setq merlin-error-after-save nil))

(use-package flycheck-ocaml
  :after tuareg
  :config
  (flycheck-ocaml-setup))

(provide 'aza-ocaml)
