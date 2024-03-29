;;
;; Doc
;;
;; When to use `defer ?`
;; Use it for the package that loads itself directly in the config.
;; If a package config has `(foo-mode +1)`. It is the one that will
;; be loaded immediately at startup. Those are the packages that
;; you need to defer. Packages that have the bind and the one with
;; implicit/explicit `:mode` keys are lazily loaded
;; until the key is pressed. So they don't need to have `defer` explicit
;; declaration

;;
;; straigth.el
;;

(setq straight-repository-branch "develop")
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-default-vc 'git)
(setq straight-use-package-by-default t)

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
;;
;; better defaults
;;

;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No hostname in frame title
;; Without setting the `icon-title-format`. The window title will revert
;; back to its original value after loosing its focus.
(setq frame-title-format '("" invocation-name " - " "%b"))
(setq icon-title-format '("" invocation-name " - " "%b"))

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

;; Enable all font-lock decoration.
;; This is disabled by default due performance reason on old machines.
;; Thus resulting in some keywords not highlighted in programming modes.
;;
;; Available options are `1', `2' or `t'. `t' is maximum value.
(setq font-lock-maximum-decoration t)

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

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Buffer encoding
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)

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

;; Don't prompt for running process when quitting Emacs
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
(setq kill-buffer-query-functions nil)

;; Make it hard to kill emacs
(setq confirm-kill-emacs #'y-or-n-p)

(setq history-delete-duplicates t)

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(setq scroll-preserve-screen-position 'always)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; I never use overwrite-mode
(put 'overwrite-mode 'disabled t)

;; Show non-existent lines with a special glyph in the left fringe
(setq-default indicate-empty-lines t)

;;
;; Configuration structure
;;
;;

;; Without defining the `config-root-dir`
;; running the `init.el` from non-default directory will fails.
;; Such in CI machine.
(defvar config-root-dir (file-name-directory load-file-name))

(defvar aza-modules-dir (expand-file-name  "modules" config-root-dir)
  "This directory houses all of the additional modules.")
(defvar straight-repos-dir (expand-file-name  "straight/repos" config-root-dir)
  "This directory houses all package repositories.")

;; Add config directories to Emacs's `load-path'
(add-to-list 'load-path aza-modules-dir)

;;
;; Paths
;;
(use-package exec-path-from-shell)

;;
;; no littering
;;

;; Using third-party package `no-littering' has a similar
;; performance compared to load those environments manually.
(use-package no-littering
  ;; Can't use defer here
  ;; Packages will not be loaded
  :config
  ;; store all backup and autosave files in the tmp dir
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(when (file-exists-p custom-file)
  (load custom-file))

;;
;; looks
;;

;; set font and size
(cond
 ((find-font (font-spec :name "Victor Mono"))
  ;; regular font is too thin for me
  (set-frame-font "Victor Mono SemiBold-16"))
 ((find-font (font-spec :name "Fira Code"))
  (set-frame-font "Fira Code-16")))

;; cursor type
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

(use-package doom-themes
  :straight (doom-themes
             :flavor melpa
             :files (:defaults "themes/*.el" "doom-themes-pkg.el")
             :host github :repo "hlissner/emacs-doom-themes"
             :fork (:host github :repo "azzamsa/emacs-doom-themes" :branch "mine"))
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
  (doom-themes-org-config)
  :custom-face
  (doom-themes-visual-bell ((t (:background "#bf616a")))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-percent-position '(-3 ""))
  :custom-face
  (mode-line ((t (:family "Fira Code" :height 1.0))))
  (mode-line-inactive ((t (:family "Fira Code" :height 1.0)))))

(use-package yascroll
  :config
  (global-yascroll-bar-mode t)
  :custom-face
  (yascroll:thumb-fringe ((t (:background "#3b4252" :foreground "#3b4252")))))

(use-package ligature
  :defer 1
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
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "C-c f") 'find-file)
(global-set-key (kbd "s-n o") 'other-window)

;; remap from`C-x d` (dired). I uses `C-c y` to acces dired
(global-set-key (kbd "C-x l") 'delete-other-windows)
(global-set-key (kbd "C-x L") 'delete-window)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)

(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<C-backspace>") 'aza-delete-whole-line)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; Using M-v is okay, but C-v hurts.
;; The keys are so close in my keyboard.
(global-set-key (kbd "s-e") 'scroll-up-command)
(global-set-key (kbd "s-u") 'scroll-down-command)

(global-set-key (kbd "s-U") 'beginning-of-buffer)
(global-set-key (kbd "s-E") 'end-of-buffer)

(global-set-key (kbd "M-g g") 'avy-goto-line)

(defun jump-to-current-directory ()
  (interactive)
  (find-file "."))

;;
;; buit-in
;;

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("/" . ora-dired-up-directory)
              ("[" . file-manager-here)
              ("]" . terminal-here)
              ("'" . dired-omit-mode)
              ("s" . xah-dired-sort))
  :config
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; revert dired buffer if underlying content changes
  (setq dired-auto-revert-buffer t)

  ;; move deleted files to trash
  (setq delete-by-moving-to-trash t)

  ;; follow original dir in sysmlink
  (setq find-file-visit-truename t)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; sort by time
  (setq dired-listing-switches "-AltGhF --group-directories-first")

  ;; Need to use remap here.
  ;; Otherwise it always override `dired-ranger-paste'
  ;; Putting it inside `dired-ranger' config does not work.
  (define-key dired-mode-map [remap dired-do-relsymlink] 'dired-ranger-paste)

  ;; Default app for specific file
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince" "okular")
          ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\)\\'" "nomacs")
          ("\\.\\(?:xcf\\)\\'" "gimp")
          ("\\.\\(?:mp4\\|mp3\\|ogv\\|webm\\)\\(?:\\.part\\)?\\'" "mpv --force-window --loop")
          ("\\.html?\\'" "firefox")
          ("\\.\\(?:pptx?\\|odt\\|fodt\\|xlsx?\\|docx?\\)\\'" "libreoffice")))

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

(defun file-manager-here ()
  (interactive)
  (message "Opening file manager in current directory...")
  (start-process "" nil "thunar" "."))

(defun terminal-here ()
  (interactive)
  (message "Opening terminal in %s" default-directory)
  ;; Need to use `expand-file-name` to expand `~` into a full path
  ;; Otherwise, wezeterm-here fallback to `$HOME`
  (start-process "" nil "wezterm-here"  (expand-file-name default-directory)))

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Modified for my needs."
  (interactive)
  (let (sort-by arg)
    (setq sort-by (completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal sort-by "name") (setq arg "-Al"))
     ((equal sort-by "date") (setq arg "-Al -t"))
     ((equal sort-by "size") (setq arg "-Al -S"))
     ((equal sort-by "dir") (setq arg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other arg)))

(use-package wdired
  :after dired
  :config
  (setq wdired-use-dired-vertical-movement 'sometimes))

(use-package recentf
  :defer 1
  :straight (:type built-in)
  :hook (kill-emacs . recentf-cleanup)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (setq recentf-exclude '("/\\.emacs\\.d/straight/build/"
                          "/tmp/" "COMMIT_EDITMSG"
                          ".jpg" ".png" ".pdf" ".org_archive"))

  ;; Can't put this in no-littering config.
  ;; Because recentf still not loaded
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (recentf-mode +1))

(use-package saveplace
  ;; saveplace remembers your location in a file when saving files
  :defer 0.5
  :config
  (setq save-place-limit 100)
  ;; activate it for all buffers
  (setq-default save-place t)

  (save-place-mode 1))

(use-package winner
  :defer 1
  :straight (:type built-in)
  :bind (("s-n w" . winner-undo))
  :config
  (winner-mode 1))

(use-package whitespace
  :straight (:type built-in)
  :delight ""
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup))
  :config
  ;; limit line length
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing space-before-tab)))

(use-package org
  :straight (:type built-in)
  :config
  (defun goto-last-heading ()
    "Useful when adding new heading"
    (interactive)
    (org-end-of-subtree))

  ;; indent file at startup
  (setq org-startup-indented t)

  ;; add time information when a task moves to a DONE state
  (setq org-log-done 'time)

  ;; Fancy TODO states
  ;; https://thraxys.wordpress.com/2016/01/14/pimp-up-your-org-agenda/
  (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✓ DONE(d!)")
                            (sequence "⚑ WAITING(w@/!)" "|")
                            (sequence "◐ DOING(s!)" "|")
                            (sequence "|" "✘ CANCELED(c@)"))))

(use-package flyspell
  :defer t
  :hook ((markdown-mode
          org-mode
          text-mode) . flyspell-mode)
  :config
  (setq ispell-dictionary "en"
        ispell-local-dictionary "id"
        ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  :custom-face
  (flyspell-duplicate
   ((t (:inherit nil :underline (:color "#842879" :style wave)))))
  (flyspell-incorrect
   ((t (:inherit nil :underline (:color "#842879" :style wave))))))

;; Disable abbrev it the dictionary does not exists
(setq abbrev-file-name
      (expand-file-name "/aza-abbrevs/aza-abbrev.el" straight-repos-dir))

(when (file-exists-p abbrev-file-name)
  (use-package abbrev
    :straight (:type built-in)
    :defer 1
    :delight ""
    :config
    (setq save-abbrevs t)
    (setq-default abbrev-mode t)
    (quietly-read-abbrev-file)
    ;; bug: emacs28 doesn't save abbrevs count before quit
    (add-hook 'kill-emacs-hook
              (lambda ()
                (write-abbrev-file abbrev-file-name nil)))))


(use-package simple
  ;; `visual-line-mode' defined in lisp/simple.el
  :straight (:type built-in)
  :hook ((org-mode gfm-mode) . visual-line-mode))

;;
;; My Packages
;;

(use-package scripts.el
  :straight (scripts.el :type git :host github :repo "azzamsa/scripts.el" :branch "mine")
  :bind (("s-n K" . aza-kill-other-buffers)
         ("s-n t" . aza-today))
  :init
  (use-package ts
    :straight (ts :type git :flavor melpa :host github :repo "alphapapa/ts.el"))
  (use-package s
    :straight (s :type git :flavor melpa
                 :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el"))
  :config
  (require 'scripts.el))

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
  :bind (("s-n n" . consult-buffer) ;; dwim
         ("s-n N" . consult-buffer-other-window)
         ("s-n g" . consult-go-to-line)
         ("s-n r" . consult-global-mark)
         ("s-n s" . consult-ripgrep))
  :config
  ;; live preview *loads* a file, thus loads all it's mode
  ;; and hog the machine
  ;;(setq consult-preview-key nil)
  (consult-customize
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Search for all files under current project instead of
  ;; current directory if `consult-project-root-function' is not nil.
  ;; So we don't need to go to project root directory manually.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  (setq consult-buffer-filter '("^ " "\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf"
                                "\\*Messages" "\\*Warning" "*magit-" "magit" "*vterm" "vterm" "^:" "*Occur"
                                "*straight-" "*elfeed-log" "*trace of SMTP session"
                                "*format-all-error" "*Async-" "COMMIT_EDITMSG"
                                "*lsp-" "*EGLOT" "*rust-" "*company-" "*pyls")))

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
  :defer 1
  :hook ((dired-mode . diff-hl-dir-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode +1))

(use-package git-timemachine :defer t)

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
  (dired-rainbow-define-chmod directory "#5e81ac" "d.*")
  ;; my primary language
  (dired-rainbow-define lang1 "#a3be8c" ("rs" "py" "js" "el"))
  ;; secondary language
  (dired-rainbow-define lang2 "#b48ead" ("lua" "css" "sass" "scss" "html" "json" "njk"))
  (dired-rainbow-define text "#88c0d0" ("org" "md" "toml" "yml" "yaml" "txt"))
  (dired-rainbow-define document "#88c0d0" ("odt" "pdf" "epub" "odp"))
  (dired-rainbow-define media "#d08770" ("webm" "webp" "jpg" "jpeg" "png" "svg"))
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
         ("C-c t" . crux-top-join-line)
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
  :defer 1
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
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package smartparens
  :delight ""
  :bind ((:map smartparens-mode-map
               ("C-M-a" . sp-beginning-of-sexp)
               ("C-M-e" . sp-end-of-sexp)))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (show-smartparens-global-mode +1))

(use-package anzu
  :delight anzu-mode
  :bind ("C-c r" . anzu-query-replace-regexp)
  :config
  (global-anzu-mode))

(use-package move-text
  :bind (([(meta up)] . move-text-up)
         ([(meta down)] . move-text-down)))

(use-package zop-to-char
  :bind (("M-Z" . zop-up-to-char)
         ("M-z" . zop-to-char)))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ;; this is the default key of flyspell
              ("C-;" . flyspell-correct-previous)))

(use-package projectile
  :demand t
  :delight ""
  :bind ("s-p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package which-key
  :config
  (which-key-mode))

(use-package hydra
  :bind (("s-n h" . hydra-menu/body)))

(defhydra hydra-menu (:color pink)
  "
  _q_uit _m_ mc/next    _r_ winner-redo
         _w_inner-undo
  "
  ("q" nil)
  ("m" mc/mark-next-like-this)
  ("w" winner-undo)
  ("r" winner-redo))

(use-package dimmer
  ;; Active buffer hard to spot without dimming the whole
  ;; buffer. Doom-modeline dims the modeline but it hard to
  ;; notice.
  :defer 1
  :config
  (setq dimmer-exclusion-predicates '(window-minibuffer-p))
  (setq dimmer-exclusion-regexp-list
        '("^\\*Minibuf-[0-9]+\\*"
          "^.\\*which-key\\*$" "^*Messages*"
          "transient"))
  (setq dimmer-fraction 0.50)
  (dimmer-mode t))

(use-package symbol-overlay
  :defer 1
  :delight ""
  :bind ("s-n '" . symbol-overlay-put))

;;
;; modules
;;

(when (file-exists-p (expand-file-name  "personal.el" aza-modules-dir))
  (require 'personal))

(require 'programming)
(require 'writing)

