;;
;; straigth.el
;;

(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))

;; native compilation
(setq comp-deferred-compilation t)
(setq warning-minimum-level :emergency)

(setq straight-repository-branch "develop")
(setq straight-recipes-gnu-elpa-use-mirror t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;
;; better defaults
;;

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format " ")

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Text mode is initial mode
(setq initial-major-mode 'text-mode)

;; Text mode is default major mode
(setq default-major-mode 'text-mode)

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; No limit on font lock
(setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 80
(setq fill-column 80)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring))

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(menu-bar-mode -1)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Always load newest byte code
(setq load-prefer-newer t)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Make it hard to kill emacs
(setq confirm-kill-emacs #'y-or-n-p)

(setq history-delete-duplicates t)

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(setq scroll-preserve-screen-position 'always)
;;
;;
;;
(use-package exec-path-from-shell)
(add-to-list 'load-path "~/.emacs.d/modules/")

;;
;; no littering
;;

(use-package no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(when (file-exists-p custom-file)
  (load custom-file))

;;
;; looks
;;

;; set font and size
(setq default-frame-alist '((font . "Fira Code 15")))

(use-package doom-themes
  :straight (doom-themes
             :type git
             :flavor melpa
             :files (:defaults "themes/*.el" "doom-themes-pkg.el")
             :host github :repo "azzamsa/emacs-doom-themes")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-percent-position '(-3 ""))
  :custom-face
  (mode-line ((t (:family "Victor Mono" :height 1.0))))
  (mode-line-inactive ((t (:family "Victor Mono" :height 1.0)))))

(use-package yascroll
  :config
  (global-yascroll-bar-mode t)
  :custom-face
  (yascroll:thumb-fringe ((t (:background "#3b4252" :foreground "#3b4252")))))

(use-package ligature
  :straight (ligature :type git :flavor melpa :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://")))

;;
;; global keybindings
;;

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-only)
(global-set-key (kbd "C-c y") 'jump-to-current-directory)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c w") 'winner-undo)
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "C-c f") 'find-file)

;; remap from`C-x d` (dired). I uses `C-c y` to acces dired
(global-set-key (kbd "C-x d") 'delete-other-windows)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)
(global-set-key (kbd "C-x l") 'delete-window)

(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(defun jump-to-current-directory ()
  (interactive)
  (find-file "."))

;;
;; buit-in
;;

(use-package dired
  :straight (:type built-in)
  :bind ((:map dired-mode-map
               ("/" . ora-dired-up-directory)
               ("[" . dired-open-directory-in-file-manager)
               ("]" . term-here)
               ("'" . dired-omit-mode)))
  :config
  ;; sort by time
  (setq dired-listing-switches "-AltGhF --group-directories-first")

  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode 1)
                               (dired-hide-details-mode +1))))

;; avoid having hard to read `dired-omit-files regexs'
(defun ora-omit-regex (names postfixes prefixes &optional dotfiles)
  (mapconcat #'identity
             (delq nil
                   (list
                    (and postfixes (format "\\(?:\\.%s\\)" (regexp-opt postfixes)))
                    (and prefixes (format "\\(?:\\`%s\\)" (regexp-opt prefixes)))
                    (and names (regexp-opt names))
                    (and dotfiles "\\`\\.[^.]")))
             "\\|"))

(setq dired-omit-files
      (ora-omit-regex
       ;; names
       '("node_modules" "target" "htmlcov")
       ;; postfixes
       '("lock" "org_archive" "aux" "log" "egg-info")
       ;; prefixes
       '("_minted" "__")
       ;; dotfiles
       t))


(defun ora-dired-up-directory ()
  (interactive)
  (let ((buffer (current-buffer)))
    (dired-up-directory)
    (unless (equal buffer (current-buffer))
      (kill-buffer buffer))))

(defun dired-open-directory-in-file-manager ()
  (interactive)
  (start-process "" nil "thunar" "."))

(defun term-here ()
  (interactive)
  (message "Opening terminal in current directory...")
  (start-process "" nil "wezterm-here" default-directory))

(use-package wdired
  :after dired
  :config
  (setq wdired-use-dired-vertical-movement 'sometimes))

(use-package recentf
  :straight (:type built-in)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (setq recentf-exclude '("/\\.emacs\\.d/straight/build/"
                          "/tmp/" "COMMIT_EDITMSG"
                          ".jpg" ".png" ".pdf" ".org_archive"))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode +1))

(use-package winner
  :straight (:type built-in)
  :config
  (winner-mode 1))

(use-package whitespace
  :straight (:type built-in)
  :delight ""
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  ;; limit line length
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing space-before-tab)))

(use-package org
  :straight (:type built-in)
  :config
  ;; indent file at startup
  (setq org-startup-indented t))
;;
;; My Packages
;;

(use-package scripts.el
  :demand t
  :after secrets.el
  :straight (scripts.el :type git :host github :repo "azzamsa/scripts.el")
  :bind (("C-c K" . aza-kill-other-buffers)
         ("C-c t" . aza-today)))

;;
;; workarounds
;;

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun aza-delete-line ()
  "Delete from current position to end of line without pushing to `kill-ring'."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun aza-delete-whole-line ()
  "Delete whole line without pushing to kill-ring."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun crux-smart-delete-line ()
  "Kill to the end of the line and kill whole line on the next call."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (aza-delete-whole-line)
      (goto-char orig-point)
      (aza-delete-line))))

;;
;; search and narrowing
;;

(use-package selectrum
  :init
  (selectrum-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :after selectrum
  :bind (("s-m m" . consult-buffer) ;; dwim
         ("s-m g" . consult-go-to-line)
         ("s-m o" . consult-outline)
         ("s-m r" . consult-global-mark)
         ("s-m s" . consult-ripgrep)
         ("C-x 4 b" . consult-buffer-other-window))
  :config
  ;; live preview *loads* a file, thus loads all it's mode
  ;; and hog the machine
  (setq consult-preview-key nil)
  (setq consult-buffer-filter '("^ " "\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf"
                                "\\*Messages" "\\*Warning" "*magit-" "magit" "*vterm" "vterm" "^:" "*Occur"
                                "*straight-" "*elfeed-log" "*trace of SMTP session"
                                "*format-all-error" "*Async-" "COMMIT_EDITMSG"
                                "*lsp-" "*rust-" "*company-" "*pyls")))

;;
;; git
;;

(use-package magit
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  ;; Protect against accident pushes to upstream
  (defun query-magit-push-upstream (args)
    (when-let ((branch (magit-get-current-branch)))
      (when (or (string-equal branch "master")
                (string-equal branch "main"))
        (unless (yes-or-no-p (format "Push \"%s\" branch to \"%s\"? "
                                     branch
                                     ;;(magit-get "branch" branch "remote")
                                     (magit-get-push-remote branch)
                                     ))
          (user-error "Pushed aborted")))))

  (advice-add 'magit-push-current-to-upstream :before #'query-magit-push-upstream)
  (advice-add 'magit-push-current-to-pushremote :before #'query-magit-push-upstream))

(use-package diff-hl
  :defer 0.9
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;
;; dired
;;

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#81a1c1" "d.*")
  (dired-rainbow-define html "#81a1c1" ("css" "sass" "scss" "html"))
  (dired-rainbow-define xml "#8fbcbb" ("xml"  "json" "yaml" "yml" "toml"))
  (dired-rainbow-define markdown "#5e81ac" ("org" "markdown" "rst" "tex" "txt"))
  (dired-rainbow-define media "#b48ead" ("mp3" "mp4" "ogg" "wav"))
  (dired-rainbow-define image "#b48ead" ("gif" "ico" "jpeg" "jpg" "png" "svg"))
  (dired-rainbow-define vc "#d8dee9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#bf616a" "-.*x.*"))

;;
;; misc
;;

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c k" . crux-smart-delete-line)
         ("C-c j" . crux-top-join-line)
         ("C-c o" . crux-smart-open-line-above)
         ("C-c w" . crux-swap-windows)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ([(shift return)] . crux-smart-open-line))
  :config
  ;; add the ability to cut the current line, without marking it (C-w)
  (require 'rect)
  (crux-with-region-or-line kill-region))

(use-package multiple-cursors
  :bind (("C-c m" . 'mc/mark-next-like-this)
         ("C-c M" . 'mc/mark-all-like-this)))

(use-package super-save
  ;; automatically save buffers associated with files on buffer switch
  ;; and on windows switch
  :delight ""
  :config
  (add-to-list 'super-save-triggers 'selectrum)
  (add-to-list 'super-save-triggers 'find-file)
  (add-to-list 'super-save-triggers 'winner-undo)
  (super-save-mode +1))

(use-package expand-region
  :bind ("C-c e" . er/expand-region))

(use-package easy-kill)

(use-package avy
  :bind ("s-." . avy-goto-char-timer)
  :config
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (setq avy-background t)
  (setq avy-style 'at-full))

;;
;; modules
;;

(require 'programming)
