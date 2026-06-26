;; -*- lexical-binding: t; -*-

;;; General.el
(use-package general
  :ensure t
  ;; PERF: Loading `general' early make Emacs very slow on startup.
  :after evil
  :config
  ;; Advise `define-key' to automatically unbind keys when necessary.
  (general-auto-unbind-keys)
  ;; Set up some basic equivalents (like `general-nmap') with short named
  ;; aliases (like `nmap') for VIM mapping functions.
  (general-evil-setup t)

  ;; Global leader
  (general-create-definer +camp--internal-map!
    ;; The order of states matters, the last is prioritized
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix camp-leader-key
    :global-prefix camp-global-leader-prefix)

  ;; Local leader
  (general-create-definer +camp--internal-map-local!
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix camp-localleader-key
    :global-prefix camp-global-mode-prefix)

  ;; Define the built-in global keybindings
  (+camp--internal-map!
    ;; Leader
    ";"   '(pp-eval-expression       :wk "Eval expression")
    ":"   '(execute-extended-command :wk "M-x")
    ","   '(consult-buffer           :wk "Switch buffer")
    "."   '(find-file                :wk "Find file")
    "/"   '(consult-ripgrep          :wk "Search project")
    "SPC" '(project-find-file        :wk "Find file in project")
    "RET" '(consult-bookmark         :wk "Jump to bookmark")
    "x"   '(+scratch-buffer          :wk "Pop up scratch buffer")
    "'"   '(vertico-repeat           :wk "Resume last search")
    "w"   '(evil-window-map          :wk "window")
    "u"   '(universal-argument       :wk "Universal argument")
    "h"   '(help-map                 :wk "help")

    ;;; <leader> TAB --- workspace
    "TAB" '(nil :wk "workspace")

    ;;; <leader> b --- buffer
    "b"    '(nil                    :wk "buffer")
    "bb"   '(consult-buffer         :wk "Switch workspace buffer")
    "bB"   '(switch-to-buffer       :wk "Switch buffer")
    "bd"   '(kill-current-buffer    :wk "Kill buffer")
    "bk"   '(kill-current-buffer    :wk "Kill buffer")
    "bK"   '(kill-all-buffers       :wk "Kill all buffers")
    "bO"   '(aza-kill-other-buffers :wk "Kill other buffers")
    "br"   '(revert-buffer          :wk "Revert buffer")
    "bR"   '(rename-buffer          :wk "Rename buffer")
    "bs"   '(basic-save-buffer      :wk "Save buffer")
    "bS"   '(evil-write-all         :wk "Save all buffers")

    ;;; <leader> c --- code
    "c"   '(nil                           :wk "code")
    ;; "ca"  '(lsp-execute-code-action       :wk "LSP Execute code action")
    ;; "cd"  '(lsp-find-definition           :wk "Jump to definition")
    "ca"  '(eglot-code-actions            :wk "LSP Execute code action")
    "cd"  '(eglot-find-declaration        :wk "Jump to definition")
    "cf"  '(apheleia-format-buffer        :wk "Format buffer")
    "cj"  '(consult-lsp-symbols           :wk "Jump to symbol in current workspace")
    "cJ"  `(,(+cmd!! (consult-lsp-symbols 'all-workspaces)) :wk "Jump to symbol in any workspace")
    ;; "cr"  '(lsp-rename                    :wk "LSP Rename")
    "cr"  '(eglot-rename                  :wk "LSP Rename")
    "cw"  '(delete-trailing-whitespace    :wk "Delete trailing whitespace")

    ;;; <leader> f --- file
    "f"    '(nil                          :wk "file")
    "fd"   '(dired                        :wk "Find directory")
    "fD"   '(crux-delete-file-and-buffer  :wk "Delete this file")
    "ff"   '(find-file                    :wk "Find file")
    "fe"   '(+find-file-in-emacsd         :wk "Find file in emacs.d")
    "fE"   '(+browse-in-emacsd            :wk "Browse emacs.d")
    "fr"   '(consult-recent-file          :wk "Recent files")
    "fs"   '(basic-save-buffer            :wk "Save buffer")
    "fu"   '(crux-sudo-edit               :wk "Find file as sudo")
    "fU"   '(crux-sudo-edit               :wk "Edit file as sudo")
    "fy"   '(+doom/yank-buffer-path       :wk "Yank filepath")
    "fY"   '(+doom/yank-buffer-path-relative-to-project  :wk "Yank filepath from project")

    ;;; <leader> g --- git/version control
    "g"   '(nil :wk "git/vc")
    "gg"  '(magit-status           :wk "git status")
    "gj"  '(jj-log                 :wk "jj log")
    "gt"  '(git-timemachine-toggle :wk "Git time machine")

   ;;; <leader> h --- help
    "h"   '(nil                :wk "help")
    "hf"  '(helpful-function   :wk "Describe function")
    "hk"  '(helpful-key        :wk "Describe key")
    "ho"  '(helpful-symbol     :wk "Describe anything")
    "hv"  '(helpful-variable   :wk "Describe variable")

   ;;; <leader> i --- insert
    "i"   '(nil :wk "insert")
    "id"  '(today   :wk "Insert date")

    ;;; <leader> n --- notes
    "n"   '(nil :wk "notes")

    ;;; <leader> o --- open
    "o"   '(nil :wk "open")
    "oo"  '(file-manager-here :wk "Open file manager here")
    "op"  '(neotree-project-dir :wk "Side panel")
    "ot"  '(ghostel :wk "Open terminal")
    "oT"  '(terminal-here :wk "Open terminal here")
    "ov"  '(vundo :wk "Visual Undo")

    ;;; <leader> p --- project
    "p"   '(nil                          :wk "project")
    "pb"  '(consult-project-buffer       :wk "Switch to project buffer")
    "pf"  '(project-find-file            :wk "Find file in project")
    "pk"  '(project-kill-buffers         :wk "Kill project buffers")
    "pm"  '(ghostel-project              :wk "Switch project")
    "pM"  '(ghostel-project-list-buffers :wk "Switch project")
    "pp"  '(project-switch-project       :wk "Switch project")

    ;;; <leader> q --- quit/session
    "q"   '(nil                        :wk "quit/session")
    "qq"  '(save-buffers-kill-terminal :wk "Quit Emacs")
    "qQ"  '(kill-emacs                 :wk "Kill Emacs")
    "qr"  '(restart-emacs              :wk "Restart Emacs")

    ;;; <leader> s --- search
    "s"    '(nil   :wk "search")
    "sb"   '(consult-line :wk "Search buffer")
    "sB"   `(,(+cmd!! (consult-line-multi 'all-buffers)) :wk "Search all open buffers")
    "sd"    '(+vertico/project-search-from-cwd :wk "Search current directory")
    "si"   '(imenu :wk "Jump to symbol")
    "sr"   '(dogears-go :wk "Jump to mark")
    "su"   '(vundo :wk "Undo history")

    ;;; <leader> t --- toggle
    "t"   '(nil                           :wk "toggle")
    "ti"  '(highlight-indent-guides-mode  :wk "Indent guides")
    "tr"  '(read-only-mode                :wk "Read-only mode")
    "tw"  '(visual-line-mode              :wk "Soft line wrapping")
    "tm"  '(minimap-mode                  :wk "Minimap mode")
    "tz"  '(+zen/toggle                   :wk "Zen mode")
    "tZ"  '(+zen/toggle-fullscreen        :wk "Zen mode (fullscreen)")

    ;;; <leader> w --- window
    "w"     '(nil                 :wk "window")
    "wd"    '(delete-window       :wk "Delete window")

    ;; hjkl => mnei
    "w <left>"   '(evil-window-left    :wk "Move to left window")
    "wm"         '(evil-window-left    :wk "Move to left window")
    "w <down>"   '(evil-window-down    :wk "Move to below window")
    "wn"         '(evil-window-down    :wk "Move to below window")
    "w <up>"     '(evil-window-up      :wk "Move to above window")
    "we"         '(evil-window-up      :wk "Move to above window")
    "w <right>"  '(evil-window-right   :wk "Move to right window")
    "wi"         '(evil-window-right   :wk "Move to right window")

    "w="    '(balance-windows         :wk "Balance windows")
    "wD"    '(delete-windows-on       :wk "Delete windows on")
    "wh"    '(maximize-window         :wk "Maximize window")
    "wu"    '(winner-undo             :wk "Undo window config")
    "wU"    '(winner-redo             :wk "Redo window config")

    ;; `w o` is definitely easier than `w C-o`. I keep this for Doom compatibility.
    "w C-o" '(delete-other-windows    :wk "Delete other windows")
    "wo"    '(delete-other-windows    :wk "Delete other windows")

    ;; ===  Mode specific a.k.a. "local leader" ===
    "m"   '(nil :wk "mode-specific"))

  ;; To handle repeated "SPC u" like repeated "C-u"
  (general-def
    :keymaps 'universal-argument-map
    :prefix camp-leader-key
    :global-prefix camp-global-mode-prefix
    "u" #'universal-argument-more)

  ;; This is a synchronization feature, providing `camp-general-ready' tells
  ;; the `+map!', `+map-local!', ... macros that `general' is ready and the
  ;; definers `+camp--internal-map!', `+camp--internal-map-local!', ...
  ;; are available (See the `+map!' macro definition in "elisp/+camp.el").
  (provide 'camp-general-ready))

;;;###autoload
(defmacro +map! (&rest args)
  "A wrapper around `+camp--internal-map!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (+camp--internal-map! ,@args)))

;;;###autoload
(defmacro +map-local! (&rest args)
  "A wrapper around `+camp--internal-map-local!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (+camp--internal-map-local! ,@args)))

;; Wrappers around `general's VIM like definers, needs `general-evil-setup' to
;; be executed (See `keybindings')
;;;###autoload
(defmacro +nmap! (&rest args)
  "A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured.
Define keybindings specific to normal state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-nmap ,@args)))

;;;###autoload
(defmacro +vmap! (&rest args)
  "A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured.
Define keybindings specific to visual state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-vmap ,@args)))

;;;###autoload
(defmacro +mmap! (&rest args)
  "A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured.
Defines keybindings specific to motion state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-mmap ,@args)))

;;;###autoload
(defmacro +imap! (&rest args)
  "A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured.
Defines keybindings specific to insert state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-imap ,@args)))

;;;###autoload
(defmacro +emap! (&rest args)
  "A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured.
Defines keybindings specific to Emacs state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-emap ,@args)))

;;;###autoload
(defmacro +omap! (&rest args)
  "A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured.
Define keybindings specific to operator state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-omap ,@args)))

;;;###autoload
(defmacro +rmap! (&rest args)
  "A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured.
Defines keybindings specific to replace state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-rmap ,@args)))

;;;###autoload
(defmacro +iemap! (&rest args)
  "A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured.
Defines keybindings specific to insert and emacs state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-iemap ,@args)))

;;;###autoload
(defmacro +nvmap! (&rest args)
  "A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured.
Defines keybindings specific to normal and visual state."
  (declare (indent defun))
  `(with-eval-after-load 'camp-general-ready
     (general-nvmap ,@args)))

(provide '+keys)
