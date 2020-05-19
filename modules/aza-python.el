(use-package elpy
  :delight " Ep"
  :init (with-eval-after-load 'python (elpy-enable))
  :commands elpy-enable
  :bind ((:map elpy-refactor-map
               ("f" . elpy-black-fix-code))
         (:map inferior-python-mode-map
               ("C-c C-l" . helm-comint-input-ring)))
  :config
  ;; Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (setq elpy-rpc-backend "jedi")
  (setq whitespace-line-column 90)
  (setq elpy-rpc-virtualenv-path "~/.virtualenvs/global37")

  (delight 'python-mode " Py")
  (delight 'highlight-indentation-mode "")
  (delight 'subword-mode "")
  (pyvenv-workon "global37")
  (add-hook 'elpy-mode-hook (lambda ()
                              (subword-mode +1))))

(use-package pyvenv
  :after elpy
  :config
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (revert-buffer t t))))

(use-package blacken
  :after elpy
  :delight " Bl"
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package isortify
  :after elpy
  :delight " Is"
  :config
  (add-hook 'python-mode-hook 'isortify-mode))

(add-hook 'inferior-python-mode-hook (lambda ()
                                       (smartparens-mode 1)))
(provide 'aza-python)
