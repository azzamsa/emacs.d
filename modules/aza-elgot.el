(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  )

(provide 'aza-elgot)
