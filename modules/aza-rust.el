(use-package rustic
  :defer 0.3
  :hook (rustic-mode . lsp)
  :config
  (add-hook 'rust-mode-hook #'subword-mode)

  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer")))

(provide 'aza-rust)
