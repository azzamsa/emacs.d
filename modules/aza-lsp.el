(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  ;; vue
  (setq lsp-vetur-format-default-formatter-css "none")
  (setq lsp-vetur-format-default-formatter-html "none")
  (setq lsp-vetur-format-default-formatter-js "none")
  (setq lsp-vetur-validation-template nil)

  ;; python
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_black.enabled" nil t)
     ))
  )

(use-package company-lsp
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t))

(provide 'aza-lsp)
