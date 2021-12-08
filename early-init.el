;;; early-init.el -*- lexical-binding: t; -*-

;; Credits:  hlissner/doom-emacs

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)


;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. My configs  handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
