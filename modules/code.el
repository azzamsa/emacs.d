;; -*- lexical-binding: t; -*-

(use-package project
  :config
  (setq project-mode-line t ; show project name in modeline
	project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project" ".jj")
	project-switch-commands
	'((?f "Find file" project-find-file)
	  (?/ "grep" consult-ripgrep)
	  (?d "Dired" project-dired)
	  (?g "Magit" magit-project-status)
	  (?\e "Escape" keyboard-escape-quit))))

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

(use-package editorconfig
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25))

(use-package flycheck-eglot
  :ensure t
  :hook (eglot-managed-mode . flycheck-eglot-mode))

;; A template system for Emacs
(use-package yasnippet
  :ensure t
  :after no-littering
  :hook ((prog-mode text-mode) . yas-minor-mode)  ; instead of global
  :config
  (setq private-yas-dir (no-littering-expand-etc-file-name "yasnippet/snippets"))
  (push private-yas-dir yas-snippet-dirs))

;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)
