(use-package rust-mode
  :defer t
  :config
  (aza-rust-path)
  (setq rust-format-on-save t)

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'subword-mode))

(use-package cargo
  :after rust)

(use-package racer
  :after rust
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path (expand-file-name (getenv "RUST_SRC_PATH")))
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :after rust
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'aza-rust)
