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
         ([remap ispell-correct]                . jinx-correct)

         ;; Buffer
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)

         ;; File
         ([remap recentf-open-files]            . consult-recent-file)


         ;; Help
         ([remap describe-function]             . helpful-callable)
         ([remap describe-command]              . helpful-command)
         ([remap describe-variable]             . helpful-variable)
         ([remap describe-key]                  . helpful-key)
         ([remap describe-symbol]               . helpful-symbol)

         ;; Open
         ("<f9>" . +neotree-project-dir)))
