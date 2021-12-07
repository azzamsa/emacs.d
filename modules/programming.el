(use-package yasnippet
  :delight yas-minor-mode ""
  :config
  (yas-reload-all)
  (yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
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
  :delight
  :config
  (editorconfig-mode 1))

;; elisp
;; oddly enough that emacs-lisp config never works inside `use-package`
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                  (run-hooks 'aza-prog-mode-hooks)))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))

  ;; cd ~/.nodebin
  ;; npm install typescript-language-server svelte-language-server
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls"))))

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
  (setq highlight-indent-guides-method 'character)
  (highlight-indent-guides-mode))

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
  (add-hook 'typescript -mode-hook 'eglot-ensure))

(use-package js-mode
  :straight (:type built-in)
  :config
  (add-hook 'js-mode-hook 'eglot-ensure))

(use-package sh-mode
  :straight (:type built-in)
  :config
  (add-hook 'sh-mode-hook 'eglot-ensure))

(use-package rainbow-mode)
(use-package lua-mode)

(defun prog-mode-defaults ()
  (subword-mode)
  (company-mode)
  (highlight-indent-guides-mode)
  (yas-minor-mode)
  (rainbow-mode))

(setq aza-prog-mode-hook 'prog-mode-defaults)
(add-hook 'prog-mode-hook (lambda ()
                (run-hooks 'aza-prog-mode-hook)))

(provide 'programming)
