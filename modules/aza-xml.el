(use-package nxml-mode
  :defer t
  :ensure nil
  :config
  (setq nxml-child-indent 4)
  (setq nxml-attribute-indent 4)
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t))

(provide 'aza-xml)
