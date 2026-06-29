;; -*- lexical-binding: t; -*-

(use-package emacs
  :config
  (keymap-global-set "C-/" #'+eat-toggle)
  :bind (
         ;; Emacs
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-line)
         ([remap imenu]                         . consult-imenu)
         ([remap Info-search]                   . consult-info)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap ispell-word]                   . jinx-correct)

         ;; Top
         ("C-c s ." . vertico-repeat)

         ;; Buffer
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)

         ("C-c b b" . consult-buffer)
         ("C-c b B" . consult-buffer-other-window)
         ("C-c b d" . kill-current-buffer)
         ("C-c b k" . kill-current-buffer)
         ("C-c b K" . +kill-all-buffers)
         ("C-c b O" . +aza-kill-other-buffers)
         ("C-c b r" . revert-buffer)
         ("C-c b R" . rename-buffer)
         ("C-c b s" . basic-save-buffer)

         ;; Code
         ("C-c c c" . compile)
         ("C-c c C" . recompile)
         ("C-c c w" . delete-trailing-whitespace)

         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format)
         ("C-c c a" . eglot-code-actions)
         ("C-c c d" . eglot-find-definition)

         ;; File
         ([remap recentf-open-files]            . consult-recent-file)

         ("C-c f d" . dired)
         ("C-c f f" . find-file)
         ("C-c f r" . consult-recent-file)
         ("C-c f D" . crux-delete-file-and-buffer)
         ("C-c f u" . crux-sudo-edit)
         ("C-c f y" . +yank-buffer-path)
         ("C-c f Y" . +yank-buffer-path-relative-to-project)

         ;; Help
         ([remap describe-function]             . helpful-callable)
         ([remap describe-command]              . helpful-command)
         ([remap describe-variable]             . helpful-variable)
         ([remap describe-key]                  . helpful-key)
         ([remap describe-symbol]               . helpful-symbol)
         ("C-c h f" . helpful-function)
         ("C-c h k" . helpful-key)
         ("C-c h o" . helpful-symbol)
         ("C-c h v" . helpful-variable)

         ;; Open
         ("C-c o p" . +neotree-project-dir)
         ("<f9>" . +neotree-project-dir)
         ("C-c o v" . vundo)

         ;; Project
         ("C-c p b" . consult-project-buffer)
         ("C-c p p" . project-switch-project)

         ;; Search
         ("C-c s b" . consult-line)
         ("C-c s s" . consult-ripgrep)
         ("C-c s m" . consult-bookmark)
         ("C-c s I" . consult-imenu-multi)
         ("C-c s ." . vertico-repeat)
         ("C-c s d" . +vertico/project-search-from-cwd)

         ;; Toggle
         ("C-c t c" . global-display-fill-column-indicator-mode)
         ("C-c t F" . toggle-frame-fullscreen)
         ("C-c t i" . indent-bars-mode)
         ("C-c t r" . read-only-mode)
         ("C-c t w" . visual-line-mode)

         ;; VC
         ("C-c v g" . magit-status)
         ("C-c v t" . git-timemachine-toggle)
         ("C-c v B" . magit-blame-addition)
         ("C-c v F" . magit-fetch)
         ("C-c v L" . magit-log-buffer-file)
         ("C-c v S" . magit-stage-file)
         ("C-c v U" . magit-unstage-file)

         ;; Window
         ("C-c w d"   . delete-window)
         ("C-c w ="   . balance-windows)
         ("C-c w D"   . delete-windows-on)
         ("C-c w h"   . maximize-window)
         ("C-c w o"   . delete-other-windows)
         ("C-c w C-o" . delete-other-windows)
         ("C-c w u"   . winner-undo)
         ("C-c w U"   . winner-redo)

         ;; Quit
         ("C-c q r" . restart-emacs)
         ("C-c q q" . save-buffers-kill-terminal)
         ("C-c q Q" . kill-emacs)

         ;; Misc
         ("C-c a" . embark-act)))
