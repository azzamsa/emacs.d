(use-package rust-mode
  :ensure t
  :defer t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package racer
  :after rust
  :ensure t
  :defer t
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path (expand-file-name (getenv "RUST_SRC_PATH")))
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(setq rust-format-on-save t)

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'aza-rust)
