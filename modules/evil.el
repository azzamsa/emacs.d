;; -*- lexical-binding: t; -*-

(defvar camp-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar camp-localleader-key ","
  "The localleader prefix key, for mode-specific commands.")

(defvar camp-global-leader-prefix "C-SPC"
  "Global (non-evil-state) fallback for the leader key,
so it works in the minibuffer, Insert state, etc.")

(defvar camp-global-mode-prefix "C-,"
  "Global (non-evil-state) fallback for the localleader key.")

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
