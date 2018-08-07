(use-package rust-mode
  :ensure t
  :defer t)

(use-package racer
  :ensure t
  :defer t
  :config
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path (expand-file-name (getenv "RUST_SRC_PATH")))
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'aza-rust)
