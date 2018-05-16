;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;  disable scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; disable menu-bar. I have <f12> to toggle it
(menu-bar-mode -1)
;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;;better bar cursor type
(setq-default cursor-type 'bar)

;;line number everywhere
;;(global-linum-mode t)

;; cursor color dissappear on emacs 25
(set-cursor-color "#f0fff0")

;;(set-frame-font "InconsolataGo-13")
(set-frame-font "Source Code Pro 12")
;;evaluate this everytime load emacs from daemon.
(setq default-frame-alist '((font . "Source Code Pro 12")))
;; highlight the current line
(global-hl-line-mode +1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(use-package beacon
  :ensure t
  :defer 1
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#b4eeb4"))

(use-package which-key
  :ensure t
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package smart-mode-line
  :ensure t
  :init
  (setq
   sml/no-confirm-load-theme t
   sml/theme nil
  (sml/setup))

(use-package dimmer
  :ensure t
  :defer 1
  :config
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (setq dimmer-fraction 0.50)
  (dimmer-mode t))

(provide 'aza-ui)
