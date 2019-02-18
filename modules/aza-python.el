(use-package elpy
  :delight "Ep"
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

  (setq elpy-rpc-backend "jedi")
  (add-hook 'elpy-mode-hook (lambda ()
                              (subword-mode +1))))

(use-package pyvenv
  :after elpy
  :config
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (revert-buffer t t))))

(provide 'aza-python)
