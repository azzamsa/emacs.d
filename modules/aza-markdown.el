(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-asymmetric-header t))

(use-package markdown-toc
  :defer t)

(provide 'aza-markdown)
