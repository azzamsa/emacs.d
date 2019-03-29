(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'")
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode +1)
                    (smartparens-mode -1)))
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))

(use-package css-mode
  :mode "\\.css\\'")

(use-package emmet-mode
  :delight
  :bind (:map emmet-mode-keymap
              ("M-e" . emmet-expand-line))
  :hook (css-mode sgml-mode web-mode))

(provide 'aza-web)
