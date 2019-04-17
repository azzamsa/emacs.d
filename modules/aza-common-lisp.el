(use-package slime
  :bind ((:map slime-mode-map
               ("C-c C-s" . slime-selector))
         (:map slime-repl-mode-map
               ("C-c C-l" . helm-slime-repl-history)))
  :config
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t
        slime-auto-start 'always)
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-company slime-fancy slime-cl-indent helm-slime)))

(use-package slime-company
  :after slime
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(use-package helm-slime
  :after slime)

(add-hook 'lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode +1)
            (smartparens-strict-mode +1)
            (prettify-symbols-mode +1)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (smartparens-strict-mode +1)
            (whitespace-mode -1)))

(provide 'aza-common-lisp)
