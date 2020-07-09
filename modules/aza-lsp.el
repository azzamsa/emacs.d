(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook (((before-save . lsp-format-buffer)
          (before-save . lsp-organize-imports)))
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-document-sync-method 'incremental)
  (setq lsp-log-io nil)
  (setq lsp-trace nil)
  (setq lsp-print-performance nil)
  :custom
  (lsp-vetur-format-default-formatter-css "none")
  (lsp-vetur-format-default-formatter-html "none")
  (lsp-vetur-format-default-formatter-js "none")
  (lsp-vetur-validation-template nil))

(use-package company-lsp
  :after lsp-mode
  :commands lsp-ui-mode
  :config (push 'company-lsp company-backends))

(use-package helm-lsp
  :after lsp-mode
  :commands helm-lsp-workspace-symbol)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c C-l .") 'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map (kbd "C-c C-l ?") 'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c C-l r") 'lsp-rename)
  (define-key lsp-ui-mode-map (kbd "C-c C-l x") 'lsp-restart-workspace)
  (define-key lsp-ui-mode-map (kbd "C-c C-l w") 'lsp-ui-peek-find-workspace-symbol)
  (define-key lsp-ui-mode-map (kbd "C-c C-l i") 'lsp-ui-peek-find-implementation)
  (define-key lsp-ui-mode-map (kbd "C-c C-l d") 'lsp-describe-thing-at-point)
  (define-key lsp-ui-mode-map (kbd "C-c C-l e") 'lsp-execute-code-action)

  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t))

(provide 'aza-lsp)
