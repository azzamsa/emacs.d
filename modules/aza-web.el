(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.njk\\'"
         )
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode +1)
                    (subword-mode +1)
                    (smartparens-mode -1)))
  :config
  ;; default to django
  (web-mode-set-engine "django"))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-level 4)
  (setq css-indent-offset 4))

(use-package emmet-mode
  :delight
  :bind ((:map emmet-mode-keymap
              ("M-e" . emmet-expand-line)))
  :hook (css-mode sgml-mode web-mode))

(use-package prettier-js
  :delight " Pr")

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook ((vue-mode . prettier-js-mode))
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq prettier-js-args '("--parser vue"))

  (add-hook 'vue-mode-hook
            (lambda ()
              (emmet-mode +1)
              (subword-mode +1)
              (smartparens-mode -1))))

(provide 'aza-web)
