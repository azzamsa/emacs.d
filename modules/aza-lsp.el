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
  (defcustom lsp-pyls-plugins-black-enabled t
    "Enable or disable the plugin."
    :type 'boolean
    :group 'lsp-pyls)

  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_black.enabled" lsp-pyls-plugins-black-enabled t)))

  (add-hook 'hack-local-variables-hook
            (lambda () (when (derived-mode-p 'python-mode) (lsp)))))

(use-package company-lsp
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))

(provide 'aza-lsp)
