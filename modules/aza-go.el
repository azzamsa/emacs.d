(use-package go-mode
  :bind ((:map go-mode-map
               ("C-c a" . go-test-current-project)
               ("C-c m" . go-test-current-file)
               ("C-c ." . go-test-current-test)
               ("C-c b" . go-run)
               ("C-h f" . godoc-at-point)))
  :config
  (aza-go-path)

  (add-to-list 'completion-ignored-extensions ".test")
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook (lambda ()
                            (subword-mode +1))))

(provide 'aza-go)
