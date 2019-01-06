(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.css\\'"
         "\\.php\\'")
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode +1)
                    (smartparens-mode -1)))
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))

(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("M-e" . emmet-expand-line))
  :config (add-hook 'web-mode-hook 'emmet-mode))

(provide 'aza-web)
