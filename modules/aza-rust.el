(use-package rustic
  :defer 0.3
  :hook (rustic-mode . lsp)
  :config
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer"))

  (add-hook 'rust-mode-hook (lambda ()
                              (subword-mode +1)
                              (run-hooks 'aza-prog-mode-hook))))

(provide 'aza-rust)
