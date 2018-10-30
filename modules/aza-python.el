(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable))
  :commands elpy-enable
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

  (setq elpy-rpc-backend "jedi"))

(use-package virtualenvwrapper
  :ensure t
  ;; demanded by eshell prompt
  ;; for venv-current-name symbols
  :after eshell
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
  :ensure t
  :after elpy
  :init
  (add-hook 'python-mode-hook 'my/python-mode-hook))


(provide 'aza-python)
