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
(add-to-list 'load-path "~/.emacs.d/modes/")

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

(set-frame-font "InconsolataGo-13")
;;evaluate this everytime load emacs from daemon.
(setq default-frame-alist '((font . "InconsolataGo-13")))
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

(use-package paren
  :defer 1
  :config
  (show-paren-mode +1))

(use-package abbrev
  :defer 5
  :config
  (setq-default abbrev-mode t)
  (cond ((file-exists-p "~/.abbrev_defs")
         (read-abbrev-file "~/.abbrev_defs")))
  (setq save-abbrevs t)
  (setq save-abbrevs 'silently))

(use-package dired
  :ensure nil
  :bind ((:map dired-mode-map
               ("C-'" . ora-dired-up-directory)
               ("C-r" . ora-dired-rsync)
               ("C-o" . dired-view-current)
               ("n" . dired-view-next)
               ("p" . dired-view-previous)
               ("s" . xah-dired-sort))
         ("C-t" . shell-pop))
  :init
  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x)
  (require 'aza-dired)
  :config
  (use-package dired+
    :load-path "~/.emacs.d/elisp/diredp/"
    :config
    (diredp-toggle-find-file-reuse-dir 1))
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (setq delete-by-moving-to-trash t)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alGhvF --group-directories-first")

  ;; hide boring files
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files
        (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`\\.[^.]\\|\\`_minted"
                (regexp-opt
                 '("aux" "log" "pickle" "synctex.gz" "run.xml" "bcf" "am" "in" "blx.bib"
                   "vrb" "opt" "nav" "snm" "out"))
                (regexp-opt
                 '("compile_commands.json"
                   "__pycache__")))))

(use-package company
  :ensure t
  :defer 1
  :config
  (global-company-mode))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook #'global-flycheck-mode))

(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-c h m" . helm-mini)
         ("C-c h o" . helm-occur)
         ("C-c h /" . helm-find)
         ("C-c h l" . helm-locate)
         ("C-c p h" . helm-projectile))
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; update things reelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (setq helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-locate-fuzzy-match t
          helm-display-header-line nil))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;;use ack-grep instead of grep
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f")))

(use-package helm-org-rifle
  :ensure t
  :bind ("C-c h r" . helm-org-rifle))

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
  :config
  (super-save-mode +1))

(use-package smartparens
  :ensure t
  :defer 2
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package smart-mode-line
  :ensure t
  :init
  (setq
   sml/no-confirm-load-theme t
   sml/theme 'respectful
   sml/shorten-modes t
   rm-blacklist '(" Rbow"
                  " Undo-Tree"
                  " Ind"
                  " super-save"
                  " guru"
                  " WK"
                  " Helm"
                  " (*)"
                  " VHl"))
  (sml/setup))

(use-package org
  :ensure t
  :defer 1
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda))
  :init
  (progn
    (setq org-src-tab-acts-natively t)
    (setq org-log-done t)
    (setq org-startup-indented t)
    (setq org-src-fontify-natively t)
    (setq org-agenda-files '("~/.emacs.d/documents/gtd/inbox.org"
                             "~/.emacs.d/documents/gtd/project.org"
                             "~/.emacs.d/documents/gtd/tickler.org"))
    (setq org-todo-keywords '((sequence "TODO(t)"
                                        "STARTED(s!)"
                                        "WAITING(w@/!)"
                                        "|"
                                        "DONE(d!)"
                                        "CANCELLED(c@)")))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((java . t)
       (sh   . t)
       (python . t)
       (lisp . t)))
    (require 'aza-org))
  :config
  ;;org-refil
  (setq org-refile-targets '(("~/.emacs.d/documents/gtd/project.org" :maxlevel . 3)
                             ("~/.emacs.d/documents/gtd/someday.org" :level . 1)
                             ("~/.emacs.d/documents/gtd/tickler.org" :maxlevel . 2)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/.emacs.d/documents/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/.emacs.d/documents/gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")
                                ("S" "Sletz" entry
                                 (file+headline "~/.emacs.d/documents/sletz.org" "Tickler")
                                 "* %i%? \n %U")))
  (add-hook 'org-mode-hook (lambda ()
                             (my-org-mode-hook)
                             (turn-on-auto-fill))))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook
                  (lambda ()
                    (org-bullets-mode 1))))

(use-package org-cliplink
  :ensure t
  :bind ("C-c o c " . org-cliplink))

(use-package org-download
  :ensure t
  :defer t)

(use-package ox-gfm
  :ensure t
  :defer t)

(use-package calfw
  :defer t
  :init
  (use-package calfw-cal
    :ensure t)
  (use-package calfw-org
    :ensure t)
  :bind (("C-c A" . my-calendar)
         :map cfw:calendar-mode-map
         ("M-n" . cfw:navi-next-month-command)
         ("M-p" . cfw:navi-previous-month-command)
         ("j"   . cfw:navi-goto-date-command)
         ("g"   . cfw:refresh-calendar-buffer))

  :commands cfw:open-calendar-buffer
  :functions (cfw:open-calendar-buffer
              cfw:refresh-calendar-buffer
              cfw:org-create-source
              cfw:cal-create-source)

  :preface
  (defun my-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "#d6c9a7")  ; orgmode source
      (cfw:cal-create-source "White"))))
  :config
  (setq diary-file "~/.emacs.d/documents/diary")
  (setq cfw:display-calendar-holidays nil)
  (setq holiday-christian-holidays nil
        holiday-bahai-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-oriental-holidays nil))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-c g" . magit-status))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-asymmetric-header t))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (setq dimmer-fraction 0.50)
  (dimmer-mode t))

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
  (recentf-mode +1))

(use-package diff-hl
  :ensure t
  :defer 5
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :defer 1
  :config
  (which-key-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c w" . crux-swap-windows)
         ("C-c r o" . crux-open-with)
         ("C-c r w" . crux-kill-whole-line)
         ("C-c r r" . crux-rename-buffer-and-file)
         ("C-c r k" . crux-kill-other-buffers)
         ("C-c r d" . crux-duplicate-current-line-or-region)
         ("C-c r D" . crux-delete-file-and-buffer)
         ("C-c r s" . crux-create-scratch-buffer)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)))

(use-package make-md-to-org
  :defer t
  :load-path "/modes/"
  :bind ("C-c M-m" . make-md-to-org ))

(use-package aza-timestamp
  :defer 4
  :load-path "/modes/"
  :bind (("s-t" . today)))

(use-package nyan-mode
  :disabled
  :ensure t
  :init
  (add-hook 'after-init-hook 'nyan-mode)
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t))

(use-package sublimity
  :ensure t
  :defer 4
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(use-package guru-mode
  :ensure t
  :defer 4
  :init (guru-global-mode +1)
  :config
  (setq guru-warn-only t))

(use-package ledger-mode
  :ensure t
  :defer t
  :mode ("\\.journal\\'" "\\.hledger\\'"))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :defer 5
  :config
  (volatile-highlights-mode +1)
  (custom-set-faces
   '(vhl/default-face ((t (:background "#688060"))))))

(use-package beacon
  :ensure t
  :defer 1
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#b4eeb4"))

(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package diminish
  :demand t)

(use-package eterm-256color
  ;; cause breakage to ansi-term
  :ensure t
  :disabled)

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(use-package multi-term
  :ensure t
  :bind (("C-x M" . multi-term)
         ("C-x m" . switch-to-term-mode-buffer))
  :config
  (setq comint-prompt-read-only t)
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
  (add-hook 'term-exec-hook 'oleh-term-exec-hook)
  ;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;; cause breakage to ansi-term
  ;;(add-hook 'term-mode-hook #'eterm-256color-mode)
  )

(use-package pomodoro
  :defer 5
  :load-path "elisp/pomodoro/"
  :config
  (progn
    (pomodoro-add-to-mode-line)
    (setq pomodoro-show-number t)
    (setq pomodoro-long-break-time 20)
    (setq pomodoro-sound-player "/usr/bin/aplay")
    (setq pomodoro-break-start-sound
          "~/sounds/sparkle-work.wav")
    (setq pomodoro-work-start-sound
          "~/sounds/sparkle-work.wav")))

;; clean up obsolete buffers automatically
(use-package midnight
  :ensure t
  :defer t)


;; Programming modes

(require 'init-java)

(use-package lisp-mode
  :defer t
  :mode ("\\.cl\\'"
         "\\.lisp\\'")
  :config
  (add-hook 'lisp-mode-hook
            (lambda ()
              (slime-mode t)
              (rainbow-delimiters-mode t)
              (show-paren-mode t)
              (prettify-symbols-mode t))))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html?\\'"
         "\\.css\\'"
         "\\.php\\'")
  :init (add-hook 'web-mode-hook
                  (lambda ()
                    (emmet-mode 1)
                    (smartparens-mode nil)))
  :config
  (progn
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)))

(use-package slime
  :ensure t
  :defer t
  :config
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (rainbow-delimiters-mode 1)
              (show-paren-mode 1)))
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-company slime-fancy)
        slime-net-coding-system 'utf-8-unix))

(use-package slime-company
  :ensure t
  :defer t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish " yas"
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

(use-package php-beautifier
  :defer t
  :load-path "elisp/php-beautifier/")

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
              ("M-e" . emmet-expand-line))
  :config (add-hook 'web-mode-hook 'emmet-mode))

(use-package php-mode
  :ensure t
  :defer t
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'company-php)
               (company-mode t)
               (ac-php-core-eldoc-setup) ;; enable eldoc
               (make-local-variable 'company-backends)
               (add-to-list 'company-backends 'company-ac-php-backend))))

(use-package lispy
  :ensure t
  :disabled)

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

(use-package markdown-toc
  :ensure t)

(use-package flyspell
  :config
  (use-package flyspell-correct-helm
    :ensure t
    :config
    (define-key flyspell-mode-map (kbd "C-;")
      'flyspell-correct-previous-word-generic))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package whitespace
  :diminish
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :init
  (progn
    ;; these need to be defined here - if they're lazily loaded with
    ;; :bind they don't work.
    (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (use-package bibretrieve
    :ensure t)
  (use-package company-auctex
    :ensure t)
  (use-package helm-bibtex
    :ensure t
    :bind ("C-c h b" . helm-bibtex-with-local-bibliography))
  (progn
    (setq LaTeX-verbatim-environments
          '("verbatim" "Verbatim" "lstlisting" "minted"))
    ;;(setq TeX-parse-self t) ; Enable parse on load.
    (setq TeX-auto-save t) ; Enable parse on save.
    (setq-default TeX-PDF-mode t) ; output to pdf
    ;; Activate nice interface between RefTeX and AUCTeX
    (setq reftex-plug-into-AUCTeX t)
    (add-to-list 'TeX-command-list
                 '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
                   TeX-run-command t t :help "Run xelatex") t)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (yas-minor-mode t)
                (turn-on-auto-fill)
                (turn-on-reftex)))))

(use-package virtualenvwrapper
  :ensure t
  :defer t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package elpy
  :ensure t
  :bind ("C-l" . elpy-shell-clear-shell)
  :config
  (use-package company-jedi
    :ensure t)
  (elpy-enable)

  (defun elpy-shell-clear-shell ()
    "Clear the current shell buffer."
    (interactive)
    (with-current-buffer (process-buffer (elpy-shell-get-or-create-process))
      (comint-clear-buffer)))

  (add-hook 'python-mode-hook
            (lambda ()
              (company-mode t)
              (company-jedi t))))

(use-package eshell
  :init (require 'aza-eshell)
  :config
  (setq eshell-directory-name (expand-file-name "eshell" azzamsa-savefile-dir)))

(use-package helm-eshell
  :defer 3
  :init
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l")
                  'helm-eshell-history))))

(use-package shell-pop
  :ensure t
  :defer 3
  :config
  (custom-set-variables
   '(shell-pop-default-directory "~/")
   '(shell-pop-shell-type
     (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
   '(shell-pop-term-shell "/usr/bin/bash")
   '(shell-pop-universal-key "C-t")
   '(shell-pop-window-height 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))

(use-package eshell-autojump
  :ensure t
  :defer t)

(use-package editorconfig
  :ensure t
  :defer t
  :diminish
  :config
  (editorconfig-mode 1))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (visual-line-mode t)))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" azzamsa-savefile-dir))

;;; Misc
(use-package erc
  :defer t
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#emacs"
                                       "#emacs-beginners"))
        erc-server "irc.freenode.net"
        erc-nick "azzamsa")
  (setq erc-log-channels-directory "~/.erc/logs/")
  (setq erc-save-buffer-on-part t)
  (setq erc-log-insert-log-on-open nil))

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;; Emacs misc
(setq history-delete-duplicates t)

;; I hate that custom fruit
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; Global keybindings

(global-set-key [f7] (lambda () (interactive) (find-file user-init-file)))

;; organizer documents
(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file "~/.emacs.d/documents/gtd/inbox.org")))

(global-set-key (kbd "C-c s")
                (lambda () (interactive) (find-file "~/.emacs.d/documents/sletz.org")))

(global-set-key (kbd "C-c b")
                (lambda () (interactive) (find-file "~/.emacs.d/bookmarks.org")))

(global-set-key (kbd "C-c c") 'org-capture)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'helm-M-x)

;; replace zap-to-char functionality with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

;; resize windows in more more comfortable way
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;;; init.el ends here
