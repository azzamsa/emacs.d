(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (use-package php-beautifier
  :defer t
  :load-path "elisp/php-beautifier/")
  (use-package company-php
    :ensure t)
  (add-hook 'php-mode-hook
            '(lambda ()
               (company-mode t)
               (ac-php-core-eldoc-setup) ;; enable eldoc
               (make-local-variable 'company-backends)
               (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.css\\'"
         "\\.php\\'")
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode +1)
                    (smartparens-mode -1)))
  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)))

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
              ("M-e" . emmet-expand-line))
  :config (add-hook 'web-mode-hook 'emmet-mode))

(provide 'aza-web)
