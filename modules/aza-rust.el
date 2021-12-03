(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook (lambda ()
                              (eglot-ensure)
                              (subword-mode +1)
                              (run-hooks 'aza-prog-mode-hook))))


(provide 'aza-rust)
