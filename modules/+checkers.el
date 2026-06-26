;; -*- lexical-binding: t; -*-

;;; checkers/syntax/config.el -*- lexical-binding: t; -*-

;;
;;; Flycheck

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


;;
;;; Flymake

(use-package flymake-popon
  :disabled
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method (if (modulep! +childframe)
                                 'posframe
                               'popon)))

;; Collection of checkers for flymake
(use-package flymake-collection
  :disabled
  :ensure t
  :hook (after-init . flymake-collection-hook-setup))

;; A beautiful inline overlay for Emacs (Flycheck | Flymake)
(use-package flyover
  ;; I don't want the errors to distract me.
  :disabled
  :hook (flycheck-mode . flyover-mode))

(provide '+checkers)
