;; -*- lexical-binding: t; -*-

;; Help keeping ~/.config/emacs clean.
(use-package no-littering
  :ensure t
  :demand t)

;; Make Emacs use the $PATH set up by the user's shell .
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package emacs
  :after no-littering
  :config
  (set-frame-font "JetBrainsMono Nerd Font 18" nil t)

  ;;
  ;; Better defaults
  ;;
  ;; https://git.sr.ht/~technomancy/better-defaults
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)

  (setq
    ;; Save existing clipboard text into the kill ring before replacing it.
    save-interprogram-paste-before-kill t
    apropos-do-all t
    mouse-yank-at-point t
    ;; End files with newline
    require-final-newline t
    ;; Flash!
    visible-bell t
    load-prefer-newer t
    ;; Copy instead of renaming current file
    backup-by-copying t
    search-default-mode t
    frame-inhibit-implied-resize t
    read-file-name-completion-ignore-case t
    read-buffer-completion-ignore-case t
    completion-ignore-case t
    ediff-window-setup-function 'ediff-setup-windows-plain
    custom-file (expand-file-name "custom.el" user-emacs-directory)
    backup-directory-alist `(("." . ,(concat no-littering-var-directory "backups/"))))

  ;;
  ;; Defaults
  ;;
  (setopt
    ;; Inhibit startup message
    inhibit-startup-message t
    ;; Do not ring
    ring-bell-function #'ignore
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

    ;; Automatically reread from disk if the underlying file changes
    auto-revert-avoid-polling t
    ;; Some systems don't do file notifications well; see
    ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
    auto-revert-interval 5
    auto-revert-check-vc-info t

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


;; displays available keybindings in popup
(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
    which-key-sort-uppercase-first nil
    which-key-add-column-padding 1
    which-key-max-display-columns nil
    which-key-min-display-lines 6
    which-key-side-window-slot -10)
  :config
  (which-key-add-key-based-replacements
    "C-c b" "buffer"
    "C-c c" "code"
    "C-c f" "file"
    "C-c h" "help"
    "C-c o" "open"
    "C-c p" "project"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c v" "vc"
    "C-c w" "window"
    "C-c q" "quit")
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package winner
  :bind (("C-c w u" . winner-undo)
    ("C-c w U" . winner-redo))
  :config
  (winner-mode 1))

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package recentf
  :after no-littering
  :config
  (setq recentf-save-file (concat no-littering-var-directory "recentf-save.el")
    recentf-max-saved-items 500
    recentf-max-menu-items 15
    ;; disable recentf-cleanup on Emacs start, because it can cause
    ;; problems with remote files
    recentf-auto-cleanup 'never
    recentf-case-fold-search t)

  (add-to-list 'recentf-exclude
    (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
    (recentf-expand-file-name no-littering-etc-directory))

  (recentf-mode +1))

(use-package savehist
  :config
  (setq savehist-autosave-interval 60     ; save on kill only
    savehist-save-minibuffer-history t
    savehist-additional-variables
    ;; persist marks
    '(mark-ring
      global-mark-ring
      ;; persist searches
      search-ring regexp-search-ring vertico-repeat-history))
  (savehist-mode +1))

;; Remember last cursor position in a file
(use-package saveplace
  :config
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package password-cache
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :custom
  (auth-sources '("~/.local/share/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  :config
  (setq tramp-default-remote-shell "/bin/bash"))

(use-package abbrev
  :after no-littering
  :init
  (setq-default abbrev-mode t)
  :config
  (setq abbrev-file-name (concat no-littering-etc-directory "abbrev.el"))
  (setq save-abbrevs 'silently))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package eldoc
  :config
  (setq eldoc-idle-delay 3))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package show-paren
  :disabled
  :config
  (show-paren-mode 1))

(use-package electric-pair
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))
