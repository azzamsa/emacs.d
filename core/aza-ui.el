;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;  disable scroll bar
(when (fboundp 'set-scroll-bar-mode)
(set-scroll-bar-mode nil))

;; disable menu-bar. I have <f12> to toggle it
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;;better bar cursor type
(setq-default cursor-type 'bar)

;;line number everywhere
;;(global-linum-mode t)

;; cursor color dissappear on emacs 25
(set-cursor-color "#f0fff0")

(set-frame-font "Source Code Pro 13")
;; evaluate this everytime load emacs from daemon.
(setq default-frame-alist '((font . "Source Code Pro 13")))
;; fallback fonts for unicode icons
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

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

(use-package beacon
  :straight (beacon :type git :flavor melpa :host github :repo "Malabarba/beacon")
  :defer 0.2
  :delight beacon-mode
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#b4eeb4"))

(use-package which-key
  :straight (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key")
  :defer 0.9
  :delight which-key-mode
  :config
  (which-key-mode +1))

(use-package smart-mode-line  
  :demand t
  :init
  (sml/setup)
  :config
  (setq
   sml/no-confirm-load-theme t
   ;; use current active theme
   sml/theme nil))

(use-package dimmer
  :straight (dimmer :type git :flavor melpa :host github :repo "gonewest818/dimmer.el" :remote "upstream"
                    :fork (:host github :repo "azzamsa/dimmer.el" :remote "origin" :branch "myhack"))
  :defer 0.2
  :load-path "~/emacs-packages/dimmer.el/"
  :config
  (setq dimmer-exclusion-predicates '(helm--alive-p window-minibuffer-p))
  (setq dimmer-exclusion-regexp-list
        '("^\\*[h|H]elm.*\\*" "^\\*Minibuf-[0-9]+\\*"
          "^.\\*which-key\\*$" "^*Messages*" "*LV*"
          "transient"))
  (setq dimmer-fraction 0.50)
  (dimmer-mode t))

(use-package golden-ratio-scroll-screen
  :straight (golden-ratio-scroll-screen
             :type git :flavor melpa :host github :repo "jixiuf/golden-ratio-scroll-screen")
  :defer 0.2
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)
  :custom
  (golden-ratio-scroll-screen-ratio 1.718))

(use-package yascroll
  :straight (yascroll :type git :flavor melpa :host github :repo "emacsorphanage/yascroll")
  :defer 0.2
  :config
  (global-yascroll-bar-mode t)
  :custom-face
  (yascroll:thumb-fringe ((t (:background "#91ee98" :foreground "#91ee98")))))

(use-package minions
  :defer 0.2
  :straight (minions :type git :flavor melpa :host github :repo "tarsius/minions")
  :config
  (minions-mode))

(provide 'aza-ui)
