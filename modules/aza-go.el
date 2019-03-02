(use-package go-mode
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c ." . go-test-current-test)
              ("C-c b" . go-run)
              ("C-h f" . godoc-at-point))
  :preface
  (defun go-mode-config ()
    ;; Prefer goimports to gofmt if installed
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))
    ;; gofmt on save
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))
    (set (make-local-variable 'company-backends) '(company-go))
    (go-eldoc-setup)
    (subword-mode +1))
  :config

  (setenv "PATH" (concat "/usr/local/go/bin:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/go/bin")

  (add-to-list 'completion-ignored-extensions ".test")
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (add-hook 'go-mode-hook (lambda ()
                            (subword-mode +1)
                            (go-mode-config))))

(use-package company-go
  :after go-mode)

(use-package go-eldoc
  :after go-mode)

(use-package go-projectile
  :after go-mode)

(use-package gotest
  :after go-mode)


(provide 'aza-go)
