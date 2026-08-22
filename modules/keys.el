;; -*- lexical-binding: t; -*-

(keymap-global-set "C-/" #'ghostel-project)

(use-package emacs
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
         ("s-s s ." . vertico-repeat)

         ;; Buffer
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)

         ("s-s b b" . consult-buffer)
         ("s-s b B" . consult-buffer-other-window)
         ("s-s b d" . kill-current-buffer)
         ("s-s b k" . kill-current-buffer)
         ("s-s b K" . +kill-all-buffers)
         ("s-s b O" . +aza-kill-other-buffers)
         ("s-s b r" . revert-buffer)
         ("s-s b R" . rename-buffer)
         ("s-s b s" . basic-save-buffer)

         ;; Code
         ("s-s c c" . compile)
         ("s-s c C" . recompile)
         ("s-s c w" . delete-trailing-whitespace)

         ("s-s c r" . eglot-rename)
         ("s-s c f" . eglot-format)
         ("s-s c a" . eglot-code-actions)
         ("s-s c d" . eglot-find-definition)

         ;; File
         ([remap recentf-open-files]            . consult-recent-file)
         ("s-s f f" . find-file)
         ("s-s f d" . dirvish)
         ("s-s f -" . dirvish)
         ("s-s f r" . consult-recent-file)
         ("s-s f D" . crux-delete-file-and-buffer)
         ("s-s f u" . crux-sudo-edit)
         ("s-s f y" . +yank-buffer-path)
         ("s-s f Y" . +yank-buffer-path-relative-to-project)

         ;; Git
         ("s-s g v" . magit-status)
         ("s-s g g" . magit-status)
         ("s-s g t" . git-timemachine-toggle)
         ("s-s g B" . magit-blame-addition)
         ("s-s g F" . magit-fetch)
         ("s-s g L" . magit-log-buffer-file)
         ("s-s g S" . magit-stage-file)
         ("s-s g U" . magit-unstage-file)

         ;; Help
         ([remap describe-function]             . helpful-callable)
         ([remap describe-command]              . helpful-command)
         ([remap describe-variable]             . helpful-variable)
         ([remap describe-key]                  . helpful-key)
         ([remap describe-symbol]               . helpful-symbol)
         ("s-s h f" . helpful-function)
         ("s-s h k" . helpful-key)
         ("s-s h o" . helpful-symbol)
         ("s-s h v" . helpful-variable)

         ;; Open
         ("s-s o p" . +neotree-project-dir)
         ("<f9>" . +neotree-project-dir)
         ("s-s o v" . vundo)

         ;; Project
         ("s-s p p" . project-switch-project)
         ("s-s p b" . project-find-file)
         ("s-s p f" . project-find-file)
         ("s-s p s" . consult-ripgrep)

         ;; Search
         ("s-s s s" . consult-ripgrep)
         ("s-s s b" . consult-line)
         ("s-s s m" . consult-bookmark)
         ("s-s s I" . consult-imenu-multi)
         ("s-s s ." . vertico-repeat)
         ("s-s s d" . +vertico/project-search-from-cwd)

         ;; Toggle
         ("s-s t c" . global-display-fill-column-indicator-mode)
         ("s-s t F" . toggle-frame-fullscreen)
         ("s-s t i" . indent-bars-mode)
         ("s-s t r" . read-only-mode)
         ("s-s t w" . visual-line-mode)

         ;; Window
         ("s-s w d"   . delete-window)
         ("s-s w ="   . balance-windows)
         ("s-s w D"   . delete-windows-on)
         ("s-s w h"   . maximize-window)
         ("s-s w o"   . delete-other-windows)
         ("s-s w C-o" . delete-other-windows)
         ("s-s w u"   . winner-undo)
         ("s-s w U"   . winner-redo)

         ;; Quit
         ("s-s q r" . restart-emacs)
         ("s-s q q" . save-buffers-kill-terminal)
         ("s-s q Q" . kill-emacs)

         ;; Misc
         ([remap ispell-correct]             . jinx-correct)
         ("s-s z z" . jinx-correct)

         ;; Misc
         ("s-s a" . embark-act)))
