(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.php\\'")
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode +1)
                    (subword-mode +1)
                    (smartparens-mode -1)))
  :config
  (setq web-mode-enable-auto-pairing nil)

  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-script-padding 4)
  (setq web-mode-style-padding 4)
  (setq web-mode-comment-style 4)
  (setq web-mode-css-indent-offset 4)

  ;; (setq-default indent-tabs-mode nil)
  ;; (setq web-mode-enable-current-element-highlight t)
  ;; (setq web-mode-enable-auto-quoting nil)

  ;; default to django
  (setq web-mode-set-engine "django"))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-level 4)
  (setq css-indent-offset 4))

(use-package emmet-mode
  :delight
  :bind (:map emmet-mode-keymap
              ("M-e" . emmet-expand-line))
  :hook (css-mode sgml-mode web-mode))

(use-package prettier-js
  :delight " Pr")

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . prettier-js-mode)
  :config
  (add-hook 'vue-mode-hook #'lsp)
  (setq prettier-js-args '("--parser vue"))

  (add-hook 'vue-mode-hook
            (lambda ()
              (emmet-mode +1)
              (subword-mode +1)
              (smartparens-mode -1))))

(provide 'aza-web)
