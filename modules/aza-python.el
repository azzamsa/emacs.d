(use-package virtualenvwrapper
  :ensure t
  ;; demanded by eshell prompt
  ;; for venv-current-name symbols
  :demand t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package elpy
  :ensure t
  :defer 4
  :bind (:map elpy-mode-map
              ("C-c C-y l" . elpy-shell-clear-shell))
  :config
  (defun elpy-shell-clear-shell ()
    "Clear the current shell buffer."
    (interactive)
    (with-current-buffer (process-buffer (elpy-shell-get-or-create-process))
      (comint-clear-buffer)))

  ;; Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (use-package company-jedi
    :ensure t)

  (elpy-enable))

(provide 'aza-python)
