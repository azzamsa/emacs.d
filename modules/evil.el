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
  :after evil)

(use-package evil-surround
  :ensure t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :after evil)

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
  :config
  (evil-multiedit-default-keybinds))
