;; -*- lexical-binding: t; -*-

(use-package treesit-auto
  ;; Emacs 31 support treesiter out of the box
  :disabled
  :ensure t
  :after treesit
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist))

  (setq treesit-auto-langs '(rust toml json python go yaml nu))
  (global-treesit-auto-mode))

(use-package editorconfig
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode text-mode conf-mode))

(use-package colorful-mode
  :disabled
  :ensure t
  :hook (prog-mode text-mode))

;; Emacs rainbow delimiters mode
(use-package rainbow-delimiters
  :ensure t)

(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package smartparens
  :disabled
  :ensure t
  :config
  (sp-local-pair 'markdown-mode "```" "```")
  (smartparens-global-mode))

;; Run code formatter on buffer contents without moving point
(use-package apheleia
  :disabled
  :ensure t
  :config
  ;; Don't assume Emacs indentation is correct
  (setq apheleia-formatters-respect-indent-level nil)
  ;; Format remote files using local formatters
  (setq apheleia-remote-algorithm 'local)

  ;; Add more formatters
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'ruff))

  ;; Define the formatter
  (dolist
      (formatter-cmd '((just-fmt . ("just" "--fmt" "--unstable" "--justfile" filepath))))
    (add-to-list #'apheleia-formatters formatter-cmd))
  (setf (alist-get 'just-mode apheleia-mode-alist) '(just-fmt))

  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'yaml-mode apheleia-mode-alist) '(dprint))
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) '(dprint))

  (apheleia-global-mode +1))

;; Highlight TODO keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (cl-callf append hl-todo-keyword-faces
    '(("BUG"   . "#ee5555")
      ("FIX"   . "#0fa050")
      ("PROJ"  . "#447f44")
      ("IDEA"  . "#0fa050")
      ("INFO"  . "#0e9030")
      ("TWEAK" . "#fe9030")
      ("PERF"  . "#e09030"))))

(provide '+prog)
