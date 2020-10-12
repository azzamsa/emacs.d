(use-package markdown-mode
  :mode ((("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . markdown-mode)))
  :config
  (require 'smartparens-config)
  (sp-with-modes 'markdown-mode
    (sp-local-pair "~~" "~~")
    (sp-local-pair "*" "*")
    (sp-local-pair "**" "**")
    (sp-local-pair "_" "_"))
  (setq markdown-asymmetric-header t)

  (add-hook 'markdown-mode-hook
            (lambda ()
              (smartparens-mode +1)
              (turn-on-ws-writing))))

(use-package markdown-toc
  :after markdown-mode)

(provide 'aza-markdown)
