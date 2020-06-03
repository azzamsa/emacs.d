;; Bootstrap straight.el
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

;;; loading my  configuration
(add-to-list 'load-path "~/.emacs.d/modules/")
(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/aza-packages/")
(add-to-list 'load-path "~/emacs-packages/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst aza-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(defconst aza-core-dir (expand-file-name "core" user-emacs-directory))
(defconst aza-modules-dir (expand-file-name "modules" user-emacs-directory))
(defconst aza-pkgs-dir (expand-file-name "aza-packages" user-emacs-directory))
(defconst aza-epkgs-dir "~/emacs-packages")

;; create the savefile dir if it doesn't exist
(unless (file-exists-p aza-savefile-dir)
  (make-directory aza-savefile-dir))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; no need double click to insert, Yey!
(delete-selection-mode +1)

;; frame title
(setq-default  frame-title-format '("" invocation-name " - " "%b"))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Don't prompt for running process
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
(setq kill-buffer-query-functions nil)

;; Make it hard to kill emacs
(setq confirm-kill-emacs #'y-or-n-p)

(setq history-delete-duplicates t)

;; use extra key in org.
;; must be placed here. :init and :hook didn't work
(setq org-use-extra-keys t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq initial-major-mode 'fundamental-mode)

;; (setq initial-scratch-message "\
;; ℝ𝕖𝕒𝕕𝕪 𝕥𝕠 𝕤𝕖𝕣𝕧𝕖 𝕪𝕠𝕦. 𝕄𝕒𝕤𝕥𝕖𝕣! ")
(setq initial-scratch-message "\

  ██╗   ██╗███████╗███████╗       ███╗   ███╗ █████╗ ███████╗████████╗███████╗██████╗ ██╗
  ╚██╗ ██╔╝██╔════╝██╔════╝       ████╗ ████║██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗██║
   ╚████╔╝ █████╗  ███████╗       ██╔████╔██║███████║███████╗   ██║   █████╗  ██████╔╝██║
    ╚██╔╝  ██╔══╝  ╚════██║       ██║╚██╔╝██║██╔══██║╚════██║   ██║   ██╔══╝  ██╔══██╗╚═╝
     ██║   ███████╗███████║▄█╗    ██║ ╚═╝ ██║██║  ██║███████║   ██║   ███████╗██║  ██║██╗
     ╚═╝   ╚══════╝╚══════╝╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝

  ╦  ┌─┐┌┬┐  ┬─┐┌─┐┌─┐┌┬┐┬ ┬  ┌┬┐┌─┐  ┌─┐┌─┐┬─┐┬  ┬┌─┐  ┬ ┬┌─┐┬ ┬
  ║  ├─┤│││  ├┬┘├┤ ├─┤ ││└┬┘   │ │ │  └─┐├┤ ├┬┘└┐┌┘├┤   └┬┘│ ││ │
  ╩  ┴ ┴┴ ┴  ┴└─└─┘┴ ┴─┴┘ ┴    ┴ └─┘  └─┘└─┘┴└─ └┘ └─┘   ┴ └─┘└─┘o

")

(use-package delight
  :straight (delight :type git :host github :repo "emacs-straight/delight" :files ("*" (:exclude ".git"))))

;; core packages
(use-package ts
  :straight (ts :type git :flavor melpa :host github :repo "alphapapa/ts.el"))
(use-package s
  :straight (s :type git :flavor melpa
               :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el"))
(use-package f
  :straight (f :type git :flavor melpa
               :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el"))
(use-package request
  :straight (request :type git :flavor melpa
              :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request"))

;; my packages
(setq auth-sources '("~/.authinfo.gpg"))

(use-package aza-secrets
  :straight (aza-secrets :type git :local-repo "aza-secrets"))

(use-package aza-scripts
  :after aza-secrets
  :straight (aza-scripts :type git :local-repo "aza-scripts")
  :bind (("C-c k" . aza-kill-other-buffers)
         ("C-c t" . aza-today)
         ("C-c i" . insert-filename-as-heading))
  :init
  (require 'aza-scripts))


;; packages
(use-package hippie-expand
  :straight (:type built-in)
  :bind ("M-/" . hippie-expand)
  :config
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
  (setq dabbrev-ignored-buffer-regexps '(".*\.gpg$" "^ [*].*")))

(use-package projectile
  :delight " P"
  :bind ("s-p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'helm)
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" aza-savefile-dir))
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" aza-savefile-dir))
  (projectile-mode +1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :delight " ()"
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp))
  :preface
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  :config
  (define-key smartparens-mode-map (kbd "M-(") (prelude-wrap-with "("))
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (show-smartparens-global-mode +1))

(use-package abbrev
  :straight (:type built-in)
  :defer 0.9
  :delight ""
  :config
  (setq abbrev-file-name
        (expand-file-name "abbrevs/abbrev.el" aza-epkgs-dir))
  (setq save-abbrevs nil)
  (setq-default abbrev-mode t))

(use-package company
  :defer 0.2
  :delight " ⚡"
  :config
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)

  (defun my-company-dabbrev-ignore (buffer)
    (let (res)
      ;; don't search in org files, encrypted files, or hidden buffers
      (dolist (re '("\.gpg$" "^ [*]") res)
        (if (string-match-p re (buffer-name buffer))
            (setq res t)))))

  (setq company-dabbrev-ignore-buffers 'my-company-dabbrev-ignore)
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode)
  (add-hook 'company-mode #'company-quickhelp-mode))

(use-package flycheck
  :defer t)

(use-package undo-tree
  :delight undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode))

(use-package flyspell
  :defer t
  :hook ((markdown-mode org-mode text-mode) . flyspell-mode)
  :config
  (setq ispell-dictionary "en"
        ispell-local-dictionary "id"
        ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))

  (defadvice ispell-init-process (after ispell-init-process-after activate)
    (setq flyspell-mode-line-string
          (concat " ⛿:" (or ispell-local-dictionary ispell-dictionary "default"))))

  :custom-face
  (flyspell-duplicate
   ((t (:inherit nil :underline (:color "dark violet" :style wave)))))
  (flyspell-incorrect
   ((t (:inherit nil :underline (:color "magenta" :style wave))))))

(use-package flyspell-correct-helm
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-previous)))

(use-package uniquify
  :straight (:type built-in)
  :defer 0.5
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package super-save
  :defer 0.5
  :delight ""
  :load-path "~/emacs-packages/super-save/"
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'helm)
  (add-to-list 'super-save-triggers 'dired-jump)
  (add-to-list 'super-save-triggers 'winner-undo)

  (setq super-save-exclude '(".gpg"))

  (super-save-mode +1))

(use-package rainbow-delimiters
  :defer 0.9
  :delight rainbow-delimiters-mode)

(use-package rainbow-mode
  :defer 0.9
  :delight rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package magit
  ;; TODO use auth-source
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-diff-refine-hunk '(all)))

(use-package git-timemachine :defer t)

(use-package savehist
  :defer 0.5
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" aza-savefile-dir))
  (savehist-mode +1))

(use-package saveplace
  ;; saveplace remembers your location in a file when saving files
  :defer 0.5
  :init (save-place-mode 1)
  :config
  (setq save-place-limit 100)
  (setq save-place-file (expand-file-name "saveplace" aza-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package recentf
  :defer 0.2
  :config
  (setq recentf-save-file (expand-file-name "recentf" aza-savefile-dir)
        recentf-max-saved-items 100
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (setq recentf-exclude '("/\\.emacs\\.d/documents/brain/"
                          "/\\.emacs\\.d/documents/brain/"
                          "/thought/brain/"
                          "/\\.emacs\\.d/elpa/"
                          "/tmp/"
                          ".jpg" ".png" ".pdf" ".org_archive"
                          "/Email/memail/"))
  (recentf-mode +1))

(use-package crux
  :bind (("C-c w" . crux-swap-windows)
         ("C-a" . crux-move-beginning-of-line)
         ("M-o" . crux-smart-open-line)
         ("s-o" . crux-smart-open-line-above)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-^" . crux-top-join-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("s-k" . crux-smart-delete-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ([(shift return)] . crux-smart-open-line)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line))
  :config
  ;; add the ability to cut the current line, without marking it (C-w)
  (require 'rect)
  (crux-with-region-or-line kill-region)

  (crux-reopen-as-root-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :defer 0.9
  :delight volatile-highlights-mode
  :config
  (volatile-highlights-mode +1)
  :custom-face
  (vhl/default-face ((t (:background "#688060")))))

(use-package anzu
  :delight anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :demand t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package move-text
  :defer 0.9
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package midnight
  ;; clean up obsolete buffers automatically
  :defer 0.9)

(use-package ace-window
  :bind ("s-b" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key [remap other-window] 'ace-window)
  (setq aw-background nil)
  :custom-face
  (aw-leading-char-face
   ((t (:box (:line-width 3 :color "#78f503" :style released-button)
             :height 1.0)))))

(use-package zop-to-char
  :bind (("M-Z" . zop-up-to-char)
         ("M-z" . zop-to-char)))

(use-package ediff
  :defer t
  :config
  ;; ediff - don't start another frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; put windows side by side
  (setq ediff-split-window-function (quote split-window-horizontally)))

(use-package perspective
  :defer 0.9
  :init
  (setq persp-mode-prefix-key (kbd "s-v"))
  (global-unset-key (kbd "C-x x"))
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-initial-frame-name "*")
  (setq persp-modestring-dividers (quote ("{" "}" "|")))
  ;;(setq persp-save-dir (expand-file-name "persp-mode/" aza-savefile-dir))
  (setq persp-state-default-file
        (expand-file-name "perspective-el" aza-savefile-dir))
  :custom-face
  (persp-selected-face
   ((t (:inherit mode-line
                 :foreground "white smoke"
                 :weight extra-bold)))))

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" aza-savefile-dir)
        bookmark-save-flag 1))

(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char-timer))
  :config
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package with-editor
  :defer t
  :disabled)

(use-package auto-capitalize
  :defer 0.9
  :delight " Ac"
  :load-path "~/emacs-packages/auto-capitalize/")

(use-package alert
  :defer 0.9
  :custom (alert-default-style 'libnotify))

(use-package visual-line-mode
  :defer t
  :straight (:type built-in)  
  :config
  (add-hook 'text-mode-hook #'visual-line-mode))

(use-package winner
  :straight (:type built-in)
  :defer 0.5  
  :config
  (winner-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package origami
  :bind (:map origami-mode-map
              ("C-: :" . origami-recursively-toggle-node)
              ("C-: a" . origami-toggle-all-nodes)
              ("C-: t" . origami-toggle-node)
              ("C-: o" . origami-show-only-node)
              ("C-: C-r" . origami-reset)))

(use-package whitespace
  :delight ""
  :defer 0.9
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  ;; clean up handled by ws-butler
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  ;; limit line length
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing lines-tail space-before-tab)))

(use-package ws-butler
  ;; clean only edited lines
  :delight ""
  :defer 0.9
  :config
  (ws-butler-global-mode t))

(use-package multiple-cursors
  :defer 0.9
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-/" . 'mc/skip-to-next-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this))
  :config
  (setq mc/list-file (expand-file-name ".mc-lists.el" aza-savefile-dir)))


;;------------------------------------------------
;; Modules
;;------------------------------------------------
(require 'aza-path)
(require 'aza-themes)
(require 'aza-dired)
(require 'aza-helm)
(require 'aza-shell)
(require 'aza-hydra)

;; writing
(require 'aza-org)
(require 'aza-latex)
(require 'aza-markdown)

;;; programming modules
(require 'aza-python)
(require 'aza-web)
(require 'aza-js)
(require 'aza-emacs-lisp)
(require 'aza-common-lisp)
(require 'aza-scheme)
;(require 'aza-rust)
;(require 'aza-go)
;(require 'aza-java)
;(require 'aza-php)
;;(require 'aza-ocaml)
(require 'aza-xml)
(require 'aza-lsp)

;;; emacs is home
(require 'aza-home)
(require 'aza-erc)
(require 'aza-mu4e)
(require 'aza-fun)
(require 'aza-ledger)
(require 'aza-calfw)

;; emacs fix
(require 'aza-emacs-fix)

;; emacs enhancements
(require 'aza-emacs-enhc)

;; unpublished configuration
(when (file-exists-p (expand-file-name "aza-local.el" aza-modules-dir))
  (require 'aza-local))

;;------------------------------------------------
;; Core
;;------------------------------------------------
(require 'aza-ui)
(require 'aza-global-keybinding)
(require 'aza-programming)

;;------------------------------------------------
;; Misc
;;------------------------------------------------
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-dev")

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)

;; Litter
(setq url-configuration-directory
      (expand-file-name "url/configuration/" aza-savefile-dir))
(setq url-cache-directory
      (expand-file-name "url/cache/" aza-savefile-dir))
(setq custom-file (expand-file-name "custom.el" aza-savefile-dir))
(setq tramp-auto-save-directory
      (expand-file-name "tramp/tramp-persistency.el" aza-savefile-dir))
(setq tramp-persistency-file-name
      (expand-file-name "tramp/tramp-persistency.el" aza-savefile-dir))
(setq transient-history-file
      (expand-file-name "transient/history.el" aza-savefile-dir))
(setq transient-levels-file
      (expand-file-name "transient/levels.el" aza-savefile-dir))
(setq transient-values-file
      (expand-file-name "transient/values.el" aza-savefile-dir))
(setq nsm-settings-file
      (expand-file-name "network-security.data" aza-savefile-dir))


(when (file-exists-p custom-file)
  (load custom-file))

;; Run at full power please
(setq disabled-command-function 'ignore)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; delight
(delight 'auto-fill-function " Af" t)
(delight 'outline-minor-mode " ⛶" t)
(delight 'auto-revert-mode)

;;; init.el ends here
