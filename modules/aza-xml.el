(use-package nxml-mode
  :defer t
  :ensure nil
  :config
  (setq nxml-child-indent 4)
  (setq nxml-attribute-indent 4)
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t)
  (add-hook 'nxml-mode-hook (lambda () (rng-validate-mode 0)) 'append)
  (add-hook 'dix-mode-hook (lambda () (flycheck-mode 0)) 'append))

(provide 'aza-xml)
