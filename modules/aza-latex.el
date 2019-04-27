(use-package tex
  :ensure auctex
  :defer t  
  :config
  (require 'smartparens-latex)
  (setq LaTeX-verbatim-environments
        '("verbatim" "Verbatim" "lstlisting" "minted" "ignasicblock"))

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (define-key LaTeX-mode-map (kbd "<C-tab>") 'outline-toggle-children)
              (turn-on-auto-fill)
              (turn-on-auto-capitalize-mode)
              (yas-minor-mode t)
              (turn-on-reftex)
              (smartparens-mode +1)
              (outline-minor-mode)
              (flyspell-mode)))
  (aza-latex-path)
  :custom
  (TeX-PDF-mode t) ; output to pdf
  (TeX-auto-save t) ; enable parse on save.
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-source-correlate-mode t)
  (TeX-view-program-selection '((output-pdf "Evince")
                                (output-html "xdg-open"))))

(use-package bibtex
  :after auctex
  :preface
  (defun my/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120))
  :config
  (add-hook 'bibtex-mode-hook #'my/bibtex-fill-column))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package reftex
  :after auctex
  :bind (:map reftex-mode-map
              ("C-c r r" . reftex-query-replace-document)
              ("C-c r g" . reftex-grep-document)))

(use-package helm-bibtex
  :after (helm auctex)
  :bind ("C-c h b" . helm-bibtex-with-local-bibliography))


(provide 'aza-latex)
