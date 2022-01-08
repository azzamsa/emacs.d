;;
;; built-in
;;
(use-package elisp-mode
  ;; defined in lisp/progmodes/elisp-mode.el
  ;; using `use-package emacs-lisp-mode' produces
  ;; so many oddities
  :straight (:type built-in)
  :hook (prog-mode-defaults . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-r" . eval-region)
              ("C-c C-d" . eval-defun)
              ("C-c C-b" . eval-buffer)))

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
  :hook ((sh-mode js-mode rust-mode
                  svelte-mode typescript-mode python-mode) . eglot-ensure)
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
              ("C-c C-r" . rust-run)))

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
  :hook python-mode
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
  :hook python-mode
  :config
  (setq highlight-indent-guides-method 'character))

(use-package markdown-mode
  :mode ((("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . markdown-mode)))
  :config
  (setq markdown-asymmetric-header t))

(use-package web-mode)
(use-package css-mode)
(use-package svelte-mode)
(use-package typescript-mode)

(use-package rainbow-mode)
(use-package lua-mode)
(use-package fish-mode)

(use-package yaml-mode
  :hook (prog-mode-defaults . yaml-mode))

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
