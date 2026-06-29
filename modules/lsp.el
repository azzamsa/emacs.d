;; -*- lexical-binding: t; -*-

(use-package eglot
  :hook ((rust-mode
          go-ts-mode
          python-ts-mode
          typescript-ts-mode
          js-ts-mode
          yaml-ts-mode
          lua-ts-mode) . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.1
	;; activate Eglot in referenced non-project files
        eglot-extend-to-xref t)
  ;; massive perf boost---don't log every event
  (fset #'jsonrpc--log-event #'ignore))

;; Extra non-standard functionalities for Eglot
(use-package eglot-x
  :after eglot
  :vc (:url "https://github.com/nemethf/eglot-x"))

;; Boost `eglot' using `emacs-lsp-booster'
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))

;; Consult integration with Eglot
(use-package consult-eglot
  :ensure t
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))

(use-package consult-lsp
  :disabled
  :ensure t)
