;; -*- lexical-binding: t; -*-

;; Extra non-standard functionalities for Eglot
(use-package eglot-x
	:after eglot
  :ensure (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))

;; Boost `eglot' using `emacs-lsp-booster' (github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

;; Consult integration with Eglot
(use-package consult-eglot
  :ensure t
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))

(use-package lsp-mode
  :disabled
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :hook ((web-mode . lsp-deferred)
         (go-mode . lsp-deferred))
  :config
  ;; Disable invasive lsp-mode features
  (setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
        lsp-ui-doc-enable nil        ; slow and redundant with K
        lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some systems I don't care to have a
        ;; whole development environment for some ecosystems.
        +lsp-prompt-to-install-server nil
        ;; shut down if all buffer killed
        lsp-keep-workspace-alive nil

        lsp-session-file (expand-file-name "lsp-session" camp-cache-dir)
        lsp-completion-provider :none))

(use-package lsp-ui
  :disabled
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        ;; redundant with K
        lsp-ui-doc-enable nil))

(use-package lsp-bridge
  :disabled
  :ensure '(:host github :repo "manateelazycat/lsp-bridge"
                  :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                  :build (:not compile))
  :init
  (global-lsp-bridge-mode))

(use-package consult-lsp
  :disabled
  :ensure t)

(provide '+lsp)
