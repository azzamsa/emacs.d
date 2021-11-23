(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.svelte\\'"
         "\\.js\\'")
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode +1)
                    (subword-mode +1)
                    (smartparens-mode -1)))
  :config
  (setq web-mode-markup-indent-offset 2))

(add-to-list 'auto-mode-alist
             '("\\.njk\\'" . (lambda ()
                               (web-mode)
                               (web-mode-set-engine "django")
                               (setq web-mode-enable-front-matter-block t))))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-level 2)
  (setq css-indent-offset 2))

(use-package emmet-mode
  :delight
  :bind ((:map emmet-mode-keymap
               ("M-e" . emmet-expand-line)))
  :hook (css-mode sgml-mode web-mode))

(provide 'aza-web)
