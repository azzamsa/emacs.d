;; -*- lexical-binding: t; -*-

(defvar +modules
  '(pkg utils core base code completion langs lsp ui vc files keys evil))

(dolist (module (mapcar #'symbol-name +modules))
  (load (expand-file-name (format "modules/%s.el" module) user-emacs-directory)))

;; Restore default GC value
(setq gc-cons-threshold 800000)
