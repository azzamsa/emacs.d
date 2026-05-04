;; -*- lexical-binding: t; -*-

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq no-littering-etc-directory
        (expand-file-name "etc/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "var/" user-emacs-directory)))

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

;; Without this package, several Emacs packages fail to locate the necessary binary files.
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide '+base)
