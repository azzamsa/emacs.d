(use-package request
  :defer t
  :config
  (setq request-storage-directory
        (expand-file-name "request/" aza-savefile-dir)))

(use-package spinner :defer t)

(use-package ts
  :defer 3
  :load-path "~/emacs-packages/ts.el")

(provide 'aza-lib)
