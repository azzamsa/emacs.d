(use-package elpy
  :delight " Ep"
  :init (with-eval-after-load 'python (elpy-enable))
  :commands elpy-enable
  :bind ((:map elpy-refactor-map
               ("f" . elpy-yapf-fix-code))
         (:map inferior-python-mode-map
               ("C-c C-l" . helm-comint-input-ring)))
  :config
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
