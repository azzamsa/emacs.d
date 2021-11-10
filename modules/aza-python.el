(use-package python
  :straight (:type built-in)
  :config
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'python-mode-hook (lambda ()
                                (yas-minor-mode)
                                (highlight-indentation-mode +1)))

  (setq lsp-pyls-plugins-flake8-enabled t)
  ;; https://github.com/emacs-lsp/lsp-mode/issues/746#issuecomment-480565612
  ;; https://github.com/palantir/python-language-server/issues/190
  (setq lsp-pyls-configuration-sources ["flake8"]))

(use-package pyvenv
  :after python
  :config
   (setq pyvenv-mode-line-indicator
         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
   (setq pyvenv-workon "global3")
   (pyvenv-tracking-mode 1)
   (pyvenv-mode +1))

(provide 'aza-python)
