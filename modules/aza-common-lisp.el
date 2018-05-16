;; TODO check Common Lisp files
(use-package lisp-mode
  :defer t
  :mode ("\\.cl\\'"
         "\\.lisp\\'")
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (slime-mode t)
              (rainbow-delimiters-mode t)
              (show-paren-mode t)
              (smartparens-strict-mode +1)
              (prettify-symbols-mode t))))

;; TODO check Common Lisp files
(use-package slime
  :ensure t
  :defer t
  :config
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (rainbow-delimiters-mode 1)
              (show-paren-mode 1)))
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-company slime-fancy)
        slime-net-coding-system 'utf-8-unix))

(use-package slime-company
  :ensure t
  :defer t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(provide 'aza-common-lisp)
