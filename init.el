;; -*- lexical-binding: t; -*-

(defvar roujack-modules
  '(pkg utils core base code completion langs lsp ui vc files helix keys))

(dolist (module (mapcar #'symbol-name roujack-modules))
  (load (expand-file-name (format "modules/%s.el" module) user-emacs-directory)))

;; Restore default GC value
(setq gc-cons-threshold 800000)
