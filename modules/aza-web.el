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

(use-package svelte-mode
  :mode "\\.svelte\\'"
  :hook ((svelte-mode . prettier-js-mode))
  :config
  (add-hook 'svelte-mode-hook #'lsp)
  (setq prettier-js-args '("--plugin-search-dir=."))

  (add-hook 'svelte-mode-hook
            (lambda ()
              (emmet-mode +1)
              (subword-mode +1)
              (smartparens-mode -1))))

(provide 'aza-web)
