(use-package rust-ts-mode
  :mode "\\.rs$"
  :config
  (add-hook 'before-save-hook #'eglot-format-buffer t t))

(use-package rustic
  :disabled
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot))

(use-package rust-mode
  :disabled
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t))

(provide '+rust)
