;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (setq modus-themes-common-palette-overrides
        '(
          ;; magenta-intense is too harsh
          ;; (cursor magenta-warmer)

          (bg-main     "#000000")
          (bg-dim      "#0d0e1c") ; bg-main
          (bg-active   "#1d2235") ; bg-dim

          (bg-main   bg-main)
          ;; Modeline
          (bg-mode-line-active bg-active)
          (border-mode-line-active bg-mode-line-active)
          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-mode-line-inactive)
          ;; Line numbers
          (bg-line-number-active bg-dim)))
  (load-theme 'modus-vivendi t))

;; Nerd Font icons for Emacs
(use-package nerd-icons
  :ensure t)

;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-bar-width 5
        doom-modeline-height 37
        doom-modeline-buffer-encoding nil)
  (setq doom-modeline-check-simple-format t  ; lighter checker segment
        doom-modeline-env-version nil         ; skip runtime version (python/ruby/etc)
        doom-modeline-workspace-name nil)
  (doom-modeline-mode 1))

;; Display typographical ligatures in major modes
(use-package ligature
  :ensure t
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 'prog-mode
                          '(;; Common programming
                            "==" "!=" ">=" "<=" "::" ":::" ".." "..." "..="
                            "+=" "-=" "*=" "/=" "&&" "||" "--" "++" "__"
                            "|>" "<|" "<|>" "#!" "#[" "]#"
                            ;; Arrows
                            "->" "<-" "=>" "<=>" "<->" "-->" "<--" "->>" "<<-"
                            ;; Rust-specific
                            "/*" "*/" "//" "///" "?." "??" "|=" "^="
                            ;; Typography
                            "ff" "fi" "fl" "ffi")))

(use-package dashboard
  :ensure t
  :demand t
  :init
  (defun daily-quote (_list-size)
    (insert (propertize "🦾 Practice, It's Practice, Practice." 'face 'bold)))
  :config
  (setq dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-icon-type 'nerd-icons
	dashboard-banner-logo-title "Want to go camping?"
	dashboard-startup-banner (concat user-emacs-directory "docs/logo.png")
	dashboard-image-banner-max-width 200
	dashboard-projects-backend 'project-el
	dashboard-items '((daily-quote)
			  (recents . 2)
			  (projects . 2))
	dashboard-item-generators '((daily-quote . daily-quote)
				    (recents . dashboard-insert-recents)
				    (projects . dashboard-insert-projects)))

  (dashboard-setup-startup-hook))
