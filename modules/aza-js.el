(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'add-node-modules-path))

(use-package add-node-modules-path
  :straight (add-node-modules-path
             :type git :flavor melpa :host github :repo "codesuki/add-node-modules-path"))

(use-package xref-js2
  :after js2-mode
  :mode ("\\.js\\'" . js2-mode))

(provide 'aza-js)
