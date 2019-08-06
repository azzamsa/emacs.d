(use-package geiser
  :ensure t
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-mode-start-repl-p t)
  ;; keep the home clean
  (setq geiser-repl-history-filename
        (expand-file-name "geiser-history" aza-savefile-dir)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (rainbow-delimiters-mode +1)
            (smartparens-strict-mode +1)))


(provide 'aza-scheme)
