;; -*- lexical-binding: t; -*-

;; It is the 21st century, should I save file manually?
(use-package super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t)
  ;; disable built-in auto-save.
  ;; There is no point of keeping the backups.
  (setq auto-save-default nil)
  (setq super-save-all-buffers t)
  ;; Save silently
  (setq super-save-silent t)

  ;; Cleanup is handled by ws-butler
  ;; Enable deleting trailing white spaces before saving
  ;; (setq super-save-delete-trailing-whitespace t)

  (super-save-mode +1))

;; Visualize and navigate the undo tree
(use-package vundo
  :ensure t
  :defer t
  :config
  (setq vundo-compact-display t
        vundo-window-max-height 6
        vundo-glyph-alist
        '((selected-node   . ?●)
          (node            . ?○)
          (vertical-stem   . ?│)
          (branch          . ?├)
          (last-branch     . ?╰)
          (horizontal-stem . ?─))))

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (setq undo-fu-session-compression 'zst
        undo-fu-session-directory (expand-file-name "undo-fu-session" camp-var-dir))
  (global-undo-fu-session-mode 1))

(use-package unicode-fonts
  :disabled
  :ensure t
  :config
  (defun +unicode-fonts-setup ()
    "Prefer the `:unicode-font-family' from `camp-fonts'."
    (when-let ((frame (selected-frame)))
      (when (display-multi-font-p frame)
        (with-selected-frame frame
          (when-let ((unicode-font-family (plist-get camp-fonts :unicode-font-family)))
            (dolist (unicode-block unicode-fonts-block-font-mapping)
              (push unicode-font-family (cadr unicode-block))))
          (unicode-fonts-setup))))))


;; Drag stuff around in Emacs
(use-package drag-stuff
  :ensure t
  :defer t
  :init
  (+nvmap!
    "<M-up>"   '(drag-stuff-up   :wk "Drag up")
    "<M-down>" '(drag-stuff-down :wk "Drag down")))

;; Fast, configurable indentation guide-bars for Emacs
(use-package indent-bars
  :ensure t
  :hook ((prog-mode text-mode conf-mode) . indent-bars-mode)
  :config
  (setq indent-bars-prefer-character
        (or
         ;; Bitmaps are far slower on MacOS, inexplicably, but this needs more
         ;; testing to see if it's specific to ns or emacs-mac builds, or is
         ;; just a general MacOS issue.
         (featurep :system 'macos)
         ;; A bitmap init bug in emacs-pgtk (before v30) could cause
         ;; crashes (see jdtsmith/indent-bars#3).
         (and (featurep 'pgtk)
              (< emacs-major-version 30)))

        ;; Show indent guides starting from the first column.
        indent-bars-starting-column 0
        ;; Make indent guides subtle; the default is too distractingly colorful.
        indent-bars-width-frac 0.1  ; make bitmaps thinner
        indent-bars-color-by-depth nil
        indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.250)
        ;; Don't highlight current level indentation; it's distracting and is
        ;; unnecessary overhead for little benefit.
        indent-bars-highlight-current-depth nil))

(use-package expand-region
  :ensure t)

(use-package helpful
  ;; a better *help* buffer
  :ensure t
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

;; Clean only edited lines
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode t))

(use-package dogears
  :disabled
  :ensure t)

(provide '+editor)
