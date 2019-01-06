(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (company-mode t)
               (ac-php-core-eldoc-setup) ;; enable eldoc
               (make-local-variable 'company-backends)
               (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package php-beautifier
  :defer t
  :load-path "elisp/php-beautifier/")

(use-package company-php
  :after php)

(provide 'aza-php)
