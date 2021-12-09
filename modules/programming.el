;;
;; built-in
;;
(use-package emacs-lisp-mode
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-r" . eval-region)
              ("C-c C-d" . eval-defun)
              ("C-c C-b" . eval-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (run-hooks 'aza-prog-mode-hooks))))

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
  (setq public-yas-dir (expand-file-name "straight/repos/yasnippet-snippets/snippets" user-emacs-directory))

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
  :after python
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (setq pyvenv-workon "global3")
  (pyvenv-tracking-mode 1))

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

(defun prog-mode-defaults ()
  (subword-mode)
  (company-mode)
  (highlight-indent-guides-mode)
  (yas-minor-mode)
  (rainbow-mode)
  (flyspell-prog-mode)
  (smartparens-mode)
  (flycheck-mode))

(setq aza-prog-mode-hook 'prog-mode-defaults)
(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'aza-prog-mode-hook)))

(provide 'programming)
