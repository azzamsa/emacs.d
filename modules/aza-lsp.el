(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  ;; always breaks
  ;; :hook ((before-save . lsp-format-buffer)
  ;;        (before-save . lsp-organize-imports))
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-document-sync-method 'incremental)
  (setq lsp-log-io nil)
  (setq lsp-trace nil)
  (setq lsp-print-performance nil)

  (setq lsp-vetur-format-default-formatter-css "none")
  (setq lsp-vetur-format-default-formatter-html "none")
  (setq lsp-vetur-format-default-formatter-js "none")
  (setq lsp-vetur-validation-template nil)

  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer")))

(use-package company-lsp
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (push 'company-lsp company-backends))

(use-package helm-lsp
  :after lsp-mode
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t))

(provide 'aza-lsp)
