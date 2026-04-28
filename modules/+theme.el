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
  ;; (load-theme 'modus-vivendi t))
  (load-theme 'modus-vivendi-tinted t))

(use-package batppuccin
  :disabled
  :ensure (:host github :repo "bbatsov/batppuccin-emacs")
  :config
  (setq batppuccin-override-colors-alist
        '(("bat-base" . "#000000")
          ("bat-text" . "#ffffff")))
  (load-theme 'batppuccin-mocha t))

(use-package tokyo-night
  :disabled
  :vc (:url "https://github.com/bbatsov/tokyo-night-emacs" :rev :newest)
  :config
  (setq  tokyo-night-override-colors-alist
         '(
           ;;
           ;; Tokyo black
           ;;

           ;; Background shades
           ("tokyo-bg-darkest"    . "#0d0e1c")
           ("tokyo-bg-dark"       . "#1d2235")
           ("tokyo-bg"            . "#000000")

           ;; Foreground shades
           ("tokyo-fg"            . "#ffffff")
           ("tokyo-fg-dark"       . "#c8d3f5")
           ("tokyo-fg-muted"      . "#828bb8")
           ("tokyo-fg-gutter"     . "#3b4261")

           ;; UI elements
           ("tokyo-line-nr"       . "#828bb8")
           ("tokyo-line-nr-cur"   . "#fca7ea")
           ("tokyo-selection"     . "#292e42") ; 13.42:1 on white

           ;; brighter comments
           ("tokyo-comment" . "#9aa5ce")))
  (load-theme 'tokyo-night-moon t))

(use-package ef-themes
  :disabled
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  ;; All customisations here.
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; (setq ef-winter-palette-overrides '((bg-main "#000000")))
  ;; (setq ef-dark-palette-overrides '((bg-main "#000000")))

  (modus-themes-load-theme 'ef-winter))

(use-package doom-themes
  :disabled
  :ensure t
  :config
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Override background to pure black
  (custom-set-faces
   '(default ((t (:background "#000000"))))
   '(hl-line ((t (:background "#1a1a1a"))))
   '(line-number ((t (:background "#000000"))))
   '(line-number-current-line ((t (:background "#1a1a1a"))))
   '(fringe ((t (:background "#000000"))))))
