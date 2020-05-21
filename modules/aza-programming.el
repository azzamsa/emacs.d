;;------------------------------------------------
;; Programming Utilities
;;------------------------------------------------

(use-package yasnippet
  :defer t
  :delight yas-minor-mode " Ys"
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package diff-hl
  :defer 3
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package symbol-overlay
  :defer t
  :delight " So"
  :bind ( "C-c '" . symbol-overlay-put))

(use-package editorconfig
  :defer 3
  :delight
  :config
  (add-hook 'prog-mode-hook #'editorconfig-mode))

(use-package prettify-symbols-mode
  :straight (:type built-in)
  :defer t
  :config
  (add-hook 'prog-mode-hook #'prettify-symbols-mode))

(use-package hl-todo
  :defer 3
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package realgud
  :defer t)

(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)
(use-package gitattributes-mode :defer t)

(use-package subword
  :ensure nil
  :defer t
  :delight "")

(use-package highlight-indentation
  :ensure nil
  :defer t
  :delight "")

(use-package yaml-mode
  :mode "\\.yml\\'"
  :interpreter ("yml" . yml-mode)
  :config
  (setq yaml-indent-offset 4))

(use-package rst-mode
  :straight (:type built-in)
  :mode "\\.rst\\'")

(use-package sphinx-mode
  :delight " Sphinx"
  :after rst)

(use-package json-mode
  :delight " J"
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer))))

(defun aza-prog-mode-defaults ()
  (flyspell-prog-mode)
  (smartparens-mode +1)
  (symbol-overlay-mode))

(setq aza-prog-mode-hook 'aza-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'aza-prog-mode-hook)))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'aza-programming)
