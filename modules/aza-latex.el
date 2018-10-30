(use-package tex
  :ensure auctex
  :config
  (setq LaTeX-verbatim-environments
        '("verbatim" "Verbatim" "lstlisting" "minted"))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (yas-minor-mode t)
              (turn-on-reftex)
              (flyspell-mode)
              (turn-on-auto-fill)))
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
  :hook (bibtex-mode . my/bibtex-fill-column)
  :preface
  (defun my/bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package reftex
  :ensure t
  :after auctex
  :bind (:map reftex-mode-map
              ("C-c r r" . reftex-query-replace-document)
              ("C-c r g" . reftex-grep-document)))

(use-package helm-bibtex
  :ensure t
  :after (helm auctex)
  :bind ("C-c h b" . helm-bibtex-with-local-bibliography))


(provide 'aza-latex)
