;;
;; built-in
;;
(use-package elisp-mode
  ;; defined in lisp/progmodes/elisp-mode.el
  ;; using `use-package emacs-lisp-mode' produces
  ;; so many oddities
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-r" . eval-region)
              ("C-c C-d" . eval-defun)
              ("C-c C-b" . eval-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook 'prog-mode-defaults))

(use-package sh-script
  ;; defined in lisp/progmodes/sh-script.el
  ;; using `use-package sh-mode' won't work
  :straight (:type built-in)
  :ensure nil
  :config
  (add-hook 'sh-mode-hook 'eglot-ensure))

(use-package js
  ;; defined in lisp/progmodes/js.el
  ;; using `use-package js-mode' won't work
  :straight (:type built-in)
  :ensure nil
  :config
  (add-hook 'js-mode-hook 'eglot-ensure))

(use-package yasnippet
  :defer t
  :delight yas-minor-mode ""
  :config
  ;; yas dirs
  (setq yas-snippet-dirs nil)
  ;; ~/.emacs.d/etc/yasnippet/snippets
  (setq private-yas-dir (no-littering-expand-etc-file-name "yasnippet/snippets"))
  ;; ~/.emacs.d/straight/repos/yasnippet-snippets/snippets
  (setq public-yas-dir (expand-file-name "/yasnippet-snippets/snippets" straight-repos-dir))

  (push private-yas-dir yas-snippet-dirs)
  (push public-yas-dir yas-snippet-dirs)

  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :defer 1
  :delight ""
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (global-company-mode +1))

(use-package editorconfig
  :defer 1
  :delight
  :config
  (editorconfig-mode 1))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))

  ;; cd ~/.nodebin
  ;; npm install typescript-language-server svelte-language-server
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls"))))

(use-package flycheck
  :defer t
  ;; both keys are free
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-r" . rust-run))
  :config
  (add-hook 'rust-mode-hook (lambda ()
                              (eglot-ensure))))

(use-package python
  :straight (:type built-in)
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (pyvenv-mode)
                                (highlight-indent-guides-mode)
                                (eglot-ensure))))

(use-package pyvenv
  ;; Using `(setq pyvenv-workon "foovenv")' as default in configuration
  ;; resulting in `foovenv' being used in non-visiting buffer such magit
  ;; before Emacs reading `pyvevn-workon' set in the `.dir-locals'.
  ;;
  ;; This creates a bug, where you can't change the venv anymore.
  ;; https://github.com/jorgenschaefer/pyvenv/pull/82/files
  ;; This behavior introduced by PR#28. It prevent re-activating new venv
  ;; if any venv already activated.
  :after python
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("(🐍" pyvenv-virtual-env-name ") ")))

  ;; Automatically use pyvenv-workon via dir-locals
  ;; Use `M-x add-dir-local-variable' -> python-mode -> pyenv-workon -> venv name
  (pyvenv-tracking-mode 1)

  ;; Update venv name after the venv changes.
  ;; It doesn't handled by default
  (defun my/pyvenv-post-activation ()
    "Update venv name after activation. "
    (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))
  (defun my/pyvenv-post-deactivation ()
    "Update venv name after deactivation."
    (setq python-shell-interpreter "python3"))

  (add-hook 'pyvenv-post-activate-hooks #'my/pyvenv-post-activation)
  (add-hook 'pyvenv-post-deactivate-hooks #'my/pyvenv-post-deactivation))

(use-package highlight-indent-guides
  :delight
  :config
  (setq highlight-indent-guides-method 'character))

(use-package markdown-mode
  :mode ((("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . markdown-mode)))
  :config
  (setq markdown-asymmetric-header t))

(use-package web-mode)
(use-package css-mode)

(use-package svelte-mode
  :mode "\\.svelte\\'"
  :config
  (add-hook 'svelte-mode-hook 'eglot-ensure))

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook 'eglot-ensure))

(use-package rainbow-mode)
(use-package lua-mode)
(use-package fish-mode)
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'prog-mode-defaults))

(defun prog-mode-defaults ()
  (subword-mode)
  (company-mode)
  (highlight-indent-guides-mode)
  (yas-minor-mode)
  (rainbow-mode)
  (flyspell-prog-mode)
  (smartparens-mode)
  (flycheck-mode))

(add-hook 'prog-mode-hook 'prog-mode-defaults)

(provide 'programming)
