(use-package js2-mode
  :hook
  (js2-mode . js2-imenu-extras-mode)
  :mode "\\.js\\'"
  :custom (js-indent-level 2))

(use-package xref-js2
  :after js2-mode)

(provide 'aza-js)
