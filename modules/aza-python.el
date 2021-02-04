(use-package python
  :straight (:type built-in)
  :config
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'python-mode-hook (lambda ()
                                (yas-minor-mode)))

  (setq lsp-pyls-plugins-flake8-enabled t)
  ;; https://github.com/emacs-lsp/lsp-mode/issues/746#issuecomment-480565612
  ;; https://github.com/palantir/python-language-server/issues/190
  (setq lsp-pyls-configuration-sources ["flake8"]))

(provide 'aza-python)
