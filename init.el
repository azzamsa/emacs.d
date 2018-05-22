;; init.el --- Azzamsa  Emacs configuration
;;
;; Copyright (c) 2016 Azzamsa
;;
;; Author: Azzamsa <me at azzamsa dot com>
;; https://github.com/azzamsa/emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.  Nothing more, nothing less.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Code:

;; Initialize the package system.
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
                         ("org" . "http://orgmode.org/elpa/")))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "azzamsa"
      user-mail-address "me@azzamsa.com")

;;; loading my  configuration
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/aza-packages/")

;; find my PATH. Solve auctex can't find xelatex
(setenv "PATH" (shell-command-to-string "bash -i -c 'echo -n $PATH'"))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst azzamsa-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p azzamsa-savefile-dir)
  (make-directory azzamsa-savefile-dir))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; no need double click to insert, Yey!
(delete-selection-mode +1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
ℝ𝕖𝕒𝕕𝕪 𝕥𝕠 𝕤𝕖𝕣𝕧𝕖 𝕪𝕠𝕦. 𝕄𝕒𝕤𝕥𝕖𝕣! ")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Make use-package available.
(require 'use-package)
(setq use-package-verbose t)

(use-package use-package-chords
  :ensure t
  :demand t
  :disabled
  :config
  (key-chord-mode 1))

(use-package use-package-ensure-system-package
  :ensure t
  :demand t)

(use-package diminish
  :demand t)

;; Theming
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; packages
(use-package projectile
  :defer t
  :ensure t
  :diminish " P"
  :bind ("s-p" . projectile-command-map))

(use-package expand-region
  :ensure t
  :defer 4
  :bind ("C-=" . er/expand-region))


(use-package smartparens
  :ensure t
  :diminish " Sp"
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1))

(use-package abbrev
  :defer 5
  :diminish " Abv"
  :config
  (cond ((file-exists-p "~/.abbrev_defs")
         (read-abbrev-file "~/.abbrev_defs")))
  (setq save-abbrevs t)
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package company
  :ensure t
  :defer 1
  :diminish " ⚡"
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (global-company-mode))

(use-package flycheck
  :ensure t
  :defer t)

(use-package undo-tree
  :defer 3
  :ensure t
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package flyspell
  :defer t
  :diminish " ⛿"
  :config
  (use-package flyspell-correct-helm
    :ensure t
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")))

(use-package uniquify
  :defer 2
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package super-save
  :ensure t
  :defer 2
  :diminish super-save-mode
  :config
  (super-save-mode +1))

(use-package smartparens
  :ensure t
  :defer 2
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer 5
  :diminish rainbow-delimiters-mode)

(use-package rainbow-mode
  :ensure t
  :defer 5
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package savehist
  :defer 2
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" azzamsa-savefile-dir))
  (savehist-mode +1))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :defer 2
  :init (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "saveplace" azzamsa-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package recentf
  :defer 1
  :config
  (setq recentf-save-file (expand-file-name "recentf" azzamsa-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (setq recentf-exclude '("/\\.emacs\\.d/documents/brain/"))
  (recentf-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c w" . crux-swap-windows)
         ("M-o" . crux-smart-open-line)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-^" . crux-top-join-line)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("s-k" . crux-kill-whole-line)
         ("s-j" . crux-top-join-line)
         ("C-<backspace>" . crux-kill-line-backwards)
          ("C-c n" . crux-cleanup-buffer-or-region)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)))

(use-package make-md-to-org
  :defer t
  :load-path "/aza-packages/"
  :disabled
  :bind ("C-c M-m" . make-md-to-org ))

(use-package aza-timestamp
  :defer 4
  :load-path "/aza-packages/"
  :bind (("s-t" . today)))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :defer 5
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1)
  :custom-face
  (vhl/default-face ((t (:background "#688060")))))

(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package move-text
  :ensure t
  :defer 3
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; clean up obsolete buffers automatically
(use-package midnight
  :ensure t
  :defer 6)

(use-package ace-window
  :ensure t
  :defer 1
  :bind ("s-W" . ace-window)
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package ediff
  :defer t
  :config
  ;; ediff - don't start another frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eyebrowse
  :ensure t
  :defer 1
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c M-e"))
  (global-unset-key (kbd "C-c C-w"))
  :config
  (eyebrowse-mode t)
  :custom
  (eyebrowse-switch-back-and-forth t)
  (eyebrowse-new-workspace t))

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" azzamsa-savefile-dir)
        bookmark-save-flag 1))

(use-package desktop
  :config
  (setq desktop-path (list azzamsa-savefile-dir))
  (setq desktop-dirname azzamsa-savefile-dir)
  (desktop-save-mode +1))

(use-package golden-ratio
  :ensure t
  :defer 2
  :diminish " φ"
  :config
  (golden-ratio-mode 1)
  :custom
  (golden-ratio-auto-scale t))

(use-package emacs-anywhere-buffer
  :demand t
  :load-path "/aza-packages/"
  :config
  (add-hook 'emacs-startup-hook
            (lambda ()
              (create-emacs-anywhere-buffer))))

(use-package avy
  :ensure t
  :defer 4
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char-timer))
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package spinner :defer t)
(use-package git-timemachine :defer t)

(require 'aza-emacs)

;;------------------------------------------------
;; Programming Utilities
;;------------------------------------------------

(use-package yasnippet
  :ensure t
  :defer t
  :diminish " yas"
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package neotree
  :ensure t
  :defer t
  :bind ([f8] . neotree-toggle)
  :config
  (use-package all-the-icons
    :ensure t)
  (setq neo-theme
        (if (display-graphic-p)
            'icons
          'arrow))
  (setq neo-smart-open t)
  ;;work with projectile
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package diff-hl
  :ensure t
  :defer 5
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package whitespace
  :diminish whitespace-mode
  :defer 3
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package multiple-cursors
  :ensure t
  :defer 4
  :init
  (progn
    ;; these need to be defined here - if they're lazily loaded with
    ;; :bind they don't work.
    (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package xterm-color
  :ensure t
  :demand t
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions
                            'xterm-color-filter nil t))))

(use-package bash-completion
  :ensure t
  :defer 3
  :init
  (bash-completion-setup))

(use-package editorconfig
  :ensure t
  :defer 5
  :diminish
  :config
  (add-hook 'prog-mode-hook #'editorconfig-mode))

(use-package which-function
  :defer 6
  :config
  (add-hook 'prog-mode-hook #'which-function-mode))

(use-package hl-todo
  :ensure t
  :defer 6
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;;------------------------------------------------
;; Modules
;;------------------------------------------------

(require 'aza-dired)
(require 'aza-dired-ext)
(require 'aza-helm)

;; writing
(require 'aza-org)
(require 'aza-latex)
(require 'aza-markdown)

;;; programming modules
(require 'aza-common-lisp)
(require 'aza-java)
(require 'aza-python)
(require 'aza-web)

;;; emacs is home
(require 'aza-home)
(require 'aza-calendar)
(require 'aza-irc)
(require 'aza-shell)

;; my packages
(require 'aza-timestamp)

;; local
(require 'init-local)

;;------------------------------------------------
;; Core
;;------------------------------------------------
(require 'aza-ui)
(require 'aza-global-keybinding)

;;------------------------------------------------
;; Misc
;;------------------------------------------------

;;; Hooks

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (visual-line-mode t)))

;;; Set

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" azzamsa-savefile-dir))

(setq history-delete-duplicates t)

;; -i gets alias definitions from .bash_profile
;;(setq shell-command-switch "-ic") ; break eshell with git prompt

;; I hate that custom fruit
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; diminish
(diminish 'visual-line-mode "Wr")
(diminish 'auto-fill-function "Fl")

;; keys
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

;;; init.el ends here
