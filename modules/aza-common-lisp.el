(use-package lisp-mode
  :defer t
  :mode ("\\.cl\\'"
         "\\.lisp\\'")
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (rainbow-delimiters-mode +1)
              (smartparens-strict-mode +1)
              (prettify-symbols-mode +1))))

(use-package slime
  :ensure t
  :defer t
  :bind (:map slime-mode-map
              ("C-c C-s" . slime-selector))
  :config
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t
        slime-auto-start 'always)
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-company slime-fancy slime-cl-indent))
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (smartparens-strict-mode +1)
              (whitespace-mode -1))))

(use-package slime-company
  :ensure t
  :defer t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(provide 'aza-common-lisp)
