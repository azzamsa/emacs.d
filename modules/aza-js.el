(use-package js2-mode
  :mode ("\\.js\\'" "\\.cjs\\'")
  :config
  (setq js-indent-level 2)

  ;;(add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'add-node-modules-path))

(use-package add-node-modules-path
  :straight (add-node-modules-path
             :type git :flavor melpa :host github :repo "codesuki/add-node-modules-path"))

(use-package xref-js2
  :after js2-mode
  :mode (("\\.js\\'" . js2-mode)))

(use-package prettier-js
  :delight " Pr")

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook ((vue-mode . prettier-js-mode))
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq prettier-js-args '("--parser vue"))

  (add-hook 'vue-mode-hook
            (lambda ()
              (emmet-mode +1)
              (subword-mode +1)
              (smartparens-mode -1))))

(use-package svelte-mode
  :mode "\\.svelte\\'"
  :hook ((svelte-mode . prettier-js-mode))
  :config
  (add-hook 'svelte-mode-hook #'lsp)
  (setq prettier-js-args '("--plugin-search-dir=."))

  (add-hook 'svelte-mode-hook
            (lambda ()
              (emmet-mode +1)
              (subword-mode +1))))

(use-package typescript-mode
  :config
  (add-hook 'python-mode-hook #'lsp)
  (setq js-indent-level 2))

(provide 'aza-js)
