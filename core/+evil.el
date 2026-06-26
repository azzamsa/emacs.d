;; -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :preface
  ;; Needed by `evil-collection'
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  ;; BUG: setting this to t triggers errors on pressing . to repeat command
  (evil-respect-visual-line-mode nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  (+nvmap!
    "C-u" '(evil-scroll-up :wk "Scroll up"))

  (+map!
    ;; buffer
    "bN" '(evil-buffer-new :wk "New buffer")
    ;; window
    "ww" '(evil-window-next :wk "Next")
    "wW" '(evil-window-prev :wk "Previous")
    "ws" '(evil-window-split :wk "Split")
    "wv" '(evil-window-vsplit :wk "Vertical split")
    "wr" '(evil-window-rotate-downwards :wk "Rotate downwards")
    "wR" '(evil-window-rotate-upwards :wk "Rotate upwards")
    "w+" '(evil-window-increase-width :wk "Increase width")

    "w-" '(evil-window-decrease-width :wk "Decrease width"))
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-snipe
  :ensure t
  :after evil
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :config
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-auto-scroll nil
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-avy
  :ensure t
  :after evil
  :config
  (+nvmap!
    "gss" '(evil-avy-goto-char-2 :wk "Avy go to char")
    "gs/" '(evil-avy-goto-char-timer :wk "Avy go to char")))

(use-package evil-surround
  :ensure t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :init
  (+vmap!
    "S" '(evil-surround-region :wk "Surround region"))
  (+omap!
    "s" '(evil-surround-edit :wk "Surround edit")
    "S" '(evil-Surround-edit :wk "Surround edit"))
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (+nvmap!
    "gc" '(evilnc-comment-operator :wk "Comment")
    "gC" '(evilnc-copy-and-comment-operator :wk "Copy and comment")))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; highlight yanked line
(use-package evil-goggles
  :ensure t
  :init
  (evil-goggles-mode))

;; Multiple cursors for evil-mode, based on iedit
(use-package evil-multiedit
  :ensure t
  :after evil
  :init
  (+nvmap!
    "R"      '(evil-multiedit-match-all              :wk "Match all occurrences")
    "M-d"    '(evil-multiedit-match-symbol-and-next  :wk "Match symbol and go to next")
    "M-D"    '(evil-multiedit-match-symbol-and-prev  :wk "Match symbol and go to previous")
    "M-d"    '(evil-multiedit-match-and-next         :wk "Match and go to next")
    "M-D"    '(evil-multiedit-match-and-prev         :wk "Match and go to previous")
    "C-M-d"  '(evil-multiedit-restore                :wk "Restore previous match"))
  :config
  (evil-multiedit-default-keybinds))


(provide '+evil)
