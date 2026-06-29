;; -*- lexical-binding: t; -*-

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("\\.env.*\\'" . sh-mode)))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :config
  (setq go-ts-mode-indent-offset 2) ; default: 8
  (add-hook 'before-save-hook #'eglot-format-buffer t t))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup))

(use-package lua-ts-mode
  :mode "\\.lua$")

(use-package python-ts-mode
  :mode "\\.py\\'"
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pylsp"))))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package beancount
  :ensure t
  :config
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)
  (add-hook 'beancount-mode-hook #'outline-minor-mode))

(use-package hurl-mode
  :ensure t
  :vc (:url "https://github.com/jaszhe/hurl-mode"))

(use-package fish-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package json-mode :ensure t)
(use-package nushell-mode :ensure t)
(use-package kdl-mode :ensure t)
