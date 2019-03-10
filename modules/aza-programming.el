;;------------------------------------------------
;; Programming Utilities
;;------------------------------------------------

(use-package yasnippet
  :defer t
  :delight " ⚄"
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package neotree
  :bind ([f8] . neotree-toggle)
  :config
  (use-package all-the-icons)
  (setq neo-theme
        (if (display-graphic-p)
            'icons
          'arrow))
  (setq neo-smart-open t)
  ;;work with projectile
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package diff-hl
  :defer 3
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package editorconfig
  :defer t
  :disabled
  :delight
  :config
  (add-hook 'prog-mode-hook #'editorconfig-mode))

(use-package guru-mode
  :disabled
  :defer 3
  :delight
  :config
  (setq guru-warn-only t)
  (guru-global-mode +1))

(use-package prettify-symbols-mode
  :defer t
  :ensure nil
  :config
  (add-hook 'prog-mode-hook #'prettify-symbols-mode))

(use-package which-function
  :ensure nil
  :disabled
  :defer 3
  :config
  (add-hook 'prog-mode-hook #'which-function-mode))

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
  :interpreter ("yml" . yml-mode))

(use-package json-mode
  :delight " J"
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer))))

(defun aza-prog-mode-defaults ()
  ;; (flyspell-prog-mode)
  (smartparens-mode +1))

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
