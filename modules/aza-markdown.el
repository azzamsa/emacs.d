(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-asymmetric-header t))

(use-package markdown-toc
  :ensure t
  :defer t)

(provide 'aza-markdown)
