(use-package rustic
  :defer 0.3
  :hook (rustic-mode . lsp)
  :config
  ;; (setq rustic-format-on-save t)
  ;; it's very distracting, Now I'm using Makefile

  (add-hook 'rust-mode-hook #'subword-mode))

(provide 'aza-rust)
