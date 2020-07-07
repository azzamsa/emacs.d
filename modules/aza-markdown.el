(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
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
              (visual-fll-column-mode))))

(use-package markdown-toc
  :after markdown-mode)

(provide 'aza-markdown)
