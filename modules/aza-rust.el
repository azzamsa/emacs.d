(use-package rustic
  :defer 0.3
  :hook (rustic-mode . lsp)
  :config
  (aza-rust-path)
  ;;(setq rustic-format-on-save t)

  (add-hook 'rust-mode-hook #'subword-mode))

(provide 'aza-rust)
