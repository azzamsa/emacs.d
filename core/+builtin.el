;; -*- lexical-binding: t; -*-

(use-package emacs
  :config
  (keymap-global-set "C-s" #'basic-save-buffer)
  (setopt
   ;; === Default directories for builtin packages ===
   custom-theme-directory (+ensure-directory camp-etc-dir "themes/")
   auto-insert-directory (+ensure-directory camp-var-dir "auto-insert/")
   auto-save-list-file-prefix (+ensure-directory camp-var-dir "auto-save/")
   backup-directory-alist (list (cons "." (+ensure-directory camp-var-dir "backup/")))
   bookmark-default-file (concat camp-var-dir "bookmark.el")
   project-list-file (concat camp-var-dir "project-list.el")
   save-place-file (concat camp-var-dir "save-place.el")
   savehist-file (concat camp-var-dir "savehist.el")

   ;; === Additional directories for non-builtin but common packages ===
   pcache-directory (concat camp-cache-dir "pcache/")

   ;; === Default behavior ===
   ;; Inhibit startup message
   inhibit-startup-message t
   ;; Do not ring
   ring-bell-function #'ignore
   ;; Set to non-nil to flash!
   visible-bell nil
   ;; Increase the large file threshold to 50 MiB
   large-file-warning-threshold (* 50 1024 1024)
   ;; No message in scratch buffer
   initial-scratch-message nil
   ;; Set initial buffer to fundamental-mode for faster load
   initial-major-mode 'fundamental-mode
   ;; Always prompt in minibuffer (no GUI)
   use-dialog-box nil
   ;; Use y or n instead of yes or no
   use-short-answers t
   ;; Confirm before quitting
   confirm-kill-emacs #'y-or-n-p
   ;; Filter duplicate entries in kill ring
   kill-do-not-save-duplicates t
   ;; Save existing clipboard text into the kill ring before replacing it.
   save-interprogram-paste-before-kill t
   ;; Save files only in sub-directories of current project
   save-some-buffers-default-predicate #'save-some-buffers-root
   ;; Use single space between sentences
   sentence-end-double-space nil
   ;; Move stuff to trash
   delete-by-moving-to-trash t
   ;; Select help window for faster quit!
   help-window-select t
   ;; More info on completions
   completions-detailed t
   ;; Do not ask obvious questions, follow symlinks
   vc-follow-symlinks t
   ;; Display the true file name for symlinks
   find-file-visit-truename t
   ;; Use completion in the minibuffer instead of definitions buffer
   xref-show-definitions-function #'xref-show-definitions-completing-read
   ;; Enable recursive calls to minibuffer
   enable-recursive-minibuffers t
   ;; Kill the shell buffer after exit
   shell-kill-buffer-on-exit t
   ;; Emacs doesn't play well with fish
   shell-file-name (executable-find "fish")
   ;; Revert non-file buffers like dired
   global-auto-revert-non-file-buffers t
   ;; Don't prompt for confirmation when we create a new file or buffer
   confirm-nonexistent-file-or-buffer nil
   ;; More intuitive buffer naming style
   uniquify-buffer-name-style 'forward
   ;; Disable case-sensitivity for file and buffer matching
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t
   completion-ignore-case t
   ;; Built-In Tree-Sitter
   treesit-auto-install-grammar 'always
   treesit-enabled-modes t

   ;; === Performances ===
   ;; Disable Bidirectional Text Scanning (arabic)
   bidi-display-reordering 'left-to-right
   bidi-paragraph-direction 'left-to-right
   bidi-inhibit-bpa t
   ;; Skip Fontification During Input
   redisplay-skip-fontification-on-input t

   ;; Don’t compact font caches during GC
   inhibit-compacting-font-caches t
   ;; Increase Process Output Buffer for LSP (default 4096)
   read-process-output-max (* 4 1024 1024) ; 4MB
   ;; Don’t Render Cursors in Non-Focused Windows
   cursor-in-non-selected-windows nil
   highlight-nonselected-windows nil

   ;; === Aesthetics and UI ===
   ;; Do force frame size to be a multiple of char size
   frame-resize-pixelwise t
   ;; Stretch cursor to the glyph width
   x-stretch-cursor t
   ;; Resize window combinations proportionally
   window-combination-resize t
   ;; No ugly button for widgets
   widget-image-enable nil
   ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
   prettify-symbols-unprettify-at-point t
   ;; Make tooltips last a bit longer (default 10s)
   tooltip-hide-delay 20
   ;; Use small frames to display tooltips instead of the default OS tooltips
   use-system-tooltips nil
   ;; Set line width for the divider in `window-divider-mode' to 2px
   window-divider-default-bottom-width 2
   window-divider-default-right-width 2

   ;; === Undo ===
   ;; 10MB (default is 160kB)
   undo-limit 10000000
   ;; 50MB (default is 240kB)
   undo-strong-limit 50000000
   ;; 50MB (default is 24MB)
   undo-outer-limit 50000000

   ;; === Editing ===
   ;; Hitting TAB behavior
   tab-always-indent 'complete
   ;; End files with newline
   require-final-newline t
   ;; Enable Drag-and-Drop of regions
   mouse-drag-and-drop-region t
   ;; Enable Drag-and-Drop of regions from Emacs to external programs
   mouse-drag-and-drop-region-cross-program t

   ;; === Backups ===
   ;; Disable lockfiles
   create-lockfiles nil
   ;; Enable making backup files
   make-backup-files t
   ;; Number each backup file
   version-control t
   ;; Copy instead of renaming current file
   backup-by-copying t
   ;; Clean up after itself
   delete-old-versions t
   ;; Keep up to 5 old versions of each file
   kept-old-versions 5
   ;; Keep up to 5 new versions of each file
   kept-new-versions 5

   ;; === Scrolling ===
   ;; Do not adjust window-vscroll to view tall lines. Fixes some lag issues see:
   ;; emacs.stackexchange.com/a/28746
   auto-window-vscroll nil
   ;; Fast scrolling
   fast-but-imprecise-scrolling t
   ;; Keep the point in the same position while scrolling
   scroll-preserve-screen-position t
   ;; Do not move cursor to the center when scrolling
   scroll-conservatively 101
   ;; Scroll at a margin of one line
   scroll-margin 1
   ;; The number of lines to scroll
   scroll-step 1
   ;; Columns from the window edge point allowed before horizontal scroll
   hscroll-margin 2
   ;; The number of columns to scroll
   hscroll-step 1
   ;; Make mouse scroll a little faster
   mouse-wheel-scroll-amount  '(2 ((shift) . hscroll) ((meta) . nil) ((control meta) . global-text-scale) ((control) . text-scale))
   ;; Make mouse scroll a little faster horizontally
   mouse-wheel-scroll-amount-horizontal 2

   ;; === Auto-Saving, sessions ===
   ;; Enable auto-save (use `recover-file' or `recover-session' to recover)
   auto-save-default t
   ;; Include big deletions
   auto-save-include-big-deletions t
   ;; Set file naming transform
   auto-save-file-name-transforms
   `(;; Prefix tramp autosaves with "tramp-"
     ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-list-file-prefix "tramp-\\2") t)
     ;; Local autosaves
     (".*" ,auto-save-list-file-prefix t)))

  (setq-default
   ;; === Buffer-local variables ===
   ;; Display long lines
   truncate-lines nil
   ;; Default fill column width
   fill-column 80
   ;; Never mix, use only spaces
   indent-tabs-mode nil
   ;; Width for line numbers
   display-line-numbers-width nil
   ;; Display absolute line numbers in narrowed regions
   display-line-numbers-widen t
   ;; Relative line numbering
   display-line-numbers-type 'relative
   ;; Small tab is enough!
   tab-width 2)

  ;; === Tweaks on file save ===
  ;; Make scripts (files starting with shebang "#!") executable when saved
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  ;; === Modes enabled locally, mainly for `prog-mode', `conf-mode' and `text-mode' ===
  ;; Show line numbers
  (add-hook 'prog-mode-hook  #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook  #'display-line-numbers-mode)
  (add-hook 'text-mode-hook  #'display-line-numbers-mode)

  ;; Wrap long lines
  (add-hook 'prog-mode-hook  #'visual-line-mode)
  (add-hook 'conf-mode-hook  #'visual-line-mode)
  (add-hook 'text-mode-hook  #'visual-line-mode)

  ;; === Modes enabled globally ===
  ;; Display divider between windows
  (window-divider-mode 1)

  ;; Scroll pixel by pixel, in Emacs29+ there is a more precise mode way to scroll
  (if (>= emacs-major-version 29)
      (pixel-scroll-precision-mode 1)
    (pixel-scroll-mode 1))

  ;; Replace selection after start typing
  (delete-selection-mode 1)
  ;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
  (minibuffer-depth-indicate-mode 1)
  ;; Save place in files
  (save-place-mode 1)
  ;; Enable saving minibuffer history
  (savehist-mode 1)
  ;; Auto load files changed on disk
  (global-auto-revert-mode 1)
  ;; Show line number in mode-line
  (line-number-mode 1)
  ;; Show column numbers (a.k.a. cursor position) in the mode-line
  (column-number-mode 1)
  ;; Better handling for files with so long lines
  (global-so-long-mode 1)
  ;; Global SubWord mode
  (global-subword-mode 1)
  :custom
  ;; Better defaults
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)

  ;; === Configure advance features ===
  ;; I never use overwrite-mode.
  (put 'overwrite-mode 'disabled t)

  (put 'downcase-region 'disabled nil))

;; Which key
(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package password-cache
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :custom
  (auth-sources '("~/.local/share/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package dired
  :hook (dired-mode . dired-omit-mode)
  :config

  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask))

(defun ora-dired-up-directory ()
  (interactive)
  (let ((buffer (current-buffer)))
    (dired-up-directory)
    (unless (equal buffer (current-buffer))
      (kill-buffer buffer))))

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

(use-package dired-x
  :after dired
  :config
  ;; Putting `dired-omit-files` inside `use-package dired`, `use-package dirvish`,
  ;; and `eval-after-load dired` doesn't work.
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(defun +dired-copy-dirpath-as-kill ()
  "Copy the current directory path into the kill ring."
  (interactive)
  (kill-new default-directory)
  (message "Copied: %s" default-directory))

(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  :config
  (setopt tramp-auto-save-directory (concat camp-var-dir "tramp/auto-save/")
          tramp-backup-directory-alist backup-directory-alist
          tramp-persistency-file-name (concat camp-var-dir "tramp/persistency.el"))
  :custom
  (tramp-default-remote-shell "/bin/bash"))

(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  :config
  (setq abbrev-file-name (concat camp-etc-dir "abbrev.el"))
  (setq save-abbrevs 'silently))

;; Use built-in `treesit' when available
(use-package treesit
  :config
  ;; Tree-Sitter grammars
  (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" camp-var-dir))
  :custom
  (treesit-font-lock-level 3))

(use-package project
  :demand t
  :config
  (setq
   project-list-file (concat camp-var-dir "project-list.el")
   project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project" ".jj")

   project-switch-commands
   '((?f "Find file" project-find-file)
     (?/ "grep" consult-ripgrep)
     (?d "Dired" project-dired)
     (?g "Magit" magit-project-status)
     (?\e "Escape" keyboard-escape-quit))))

(use-package tab-bar
  :disabled
  :custom
  (tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
  (tab-bar-auto-width-max '((150) 20))
  (tab-bar-tab-hints t)
  (tab-bar-show t))

(use-package elisp-mode
  :hook (prog-mode-defaults . emacs-lisp-mode))

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("\\.env.*\\'" . sh-mode)))

(use-package winner
  :config
  (winner-mode 1))

(use-package recentf
  :hook (kill-emacs . recentf-cleanup)
  :config
  (setq recentf-save-file (concat camp-var-dir "recentf-save.el")
        ;; Increase the maximum number of saved items (default: 20)
        recentf-max-saved-items 100
        ;; Ignore case when searching recentf files
        recentf-case-fold-search t
        ;; Exclude some files from being remembered by recentf
        recentf-exclude
        `(,(rx (* any)
               (or "emacs/var/" "eln-cache" "/cache/" ".cache/")
               (* any)
               (? (or "html" "pdf" "tex" "epub" "gz")))
          ,(rx "/"
               (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
               (* any))))

  (recentf-mode 1))

(use-package savehist
  :config
  (setq savehist-autosave-interval 60     ; save on kill only
        savehist-save-minibuffer-history t
        savehist-additional-variables
        '(mark-ring global-mark-ring       ; persist marks
                    search-ring regexp-search-ring vertico-repeat-history) ; persist searches
        savehist-file (expand-file-name "savehist" camp-var-dir))

  (savehist-mode +1))


;; Remember last cursor position in a file
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" camp-var-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package flymake
  :disabled
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package whitespace
  :config
  ;; Show trailing whitespace in `prog-mode' and `conf-mode'
  (defun +show-trailing-whitespace-h ()
    (setq-local show-trailing-whitespace t))
  (add-hook 'prog-mode-hook  #'+show-trailing-whitespace-h)
  (add-hook 'conf-mode-hook  #'+show-trailing-whitespace-h)
  (add-hook 'text-mode-hook  #'+show-trailing-whitespace-h)

  ;; Cleanup is handled by ws-butler
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)

  ;; limit line length
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing lines-tail space-before-tab)))

(use-package eglot
  :hook ((rust-mode
          go-ts-mode
          python-ts-mode
          typescript-ts-mode
          js-ts-mode
          yaml-ts-mode
          lua-tsmode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t ; shutdown after closing the last managed buffer
        eglot-sync-connect 0 ; async, do not block
        eglot-extend-to-xref t ; can be interesting!
        ;; disable annoying messages in echo area!
        eglot-report-progress nil))

(use-package eldoc
  :config
  (setq eldoc-idle-delay 3
        eldoc-help-at-pt t))

(provide '+builtin)
