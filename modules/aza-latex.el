(use-package tex
  :defer t
  :ensure auctex
  :config
  (use-package bibretrieve
    :ensure t)
  (use-package company-auctex
    :ensure t)
  (use-package helm-bibtex
    :ensure t
    :bind ("C-c h b" . helm-bibtex-with-local-bibliography))
  (progn
    (setq LaTeX-verbatim-environments
          '("verbatim" "Verbatim" "lstlisting" "minted"))
    ;;(setq TeX-parse-self t) ; Enable parse on load.
    (setq TeX-auto-save t) ; Enable parse on save.
    (setq-default TeX-PDF-mode t) ; output to pdf
    ;; Activate nice interface between RefTeX and AUCTeX
    (setq reftex-plug-into-AUCTeX t)
    ;; (add-to-list 'TeX-command-list
    ;;              '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
    ;;                TeX-run-command t t :help "Run xelatex") t)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (yas-minor-mode t)
                (turn-on-reftex)
                (turn-on-auto-fill)))))

(use-package reftex
  :ensure t
  :after tex
  :bind (:map reftex-mode-map
               ("C-c r r" . reftex-query-replace-document)
               ("C-c r g" . reftex-grep-document)))

(provide 'aza-latex)
