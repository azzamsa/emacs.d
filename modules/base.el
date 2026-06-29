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
  ;; (setq super-save-delete-trailing-whitespace t)
  (super-save-mode +1))

;; Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :ensure t
  :bind (("C-c f D" . crux-delete-file-and-buffer)
	 ("C-c f u" . crux-sudo-edit)))

;; a better *help* buffer
(use-package helpful
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

;; Visualize and navigate the undo tree
(use-package vundo
  :ensure t
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
  (setq undo-fu-session-compression 'zst)
  (global-undo-fu-session-mode 1))

;; Drag stuff around in Emacs
(use-package drag-stuff
  :ensure t)

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

;; Clean only edited lines
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode t))

(use-package avy
  :ensure t
  :config
  (setq avy-background t
	avy-style 'at-full))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Dictionary
(setopt dictionary-use-single-buffer t)
(setopt dictionary-server "dict.org")

;; Just-in-time spell checker
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (setq jinx-languages "en_US id_ID"
        jinx-camel-modes '(prog-mode)
        jinx-delay 0.01))

;; Olivetti: Set the window margins so your text is centered
(use-package olivetti
  :ensure t)

;; Distraction-free words correction with `flyspell' via `completing-read'
(use-package flyspell-correct
  :ensure t)

;; Run code formatter on buffer contents without moving point
(use-package apheleia
  :ensure t
  :hook ((emacs-lisp-mode) . apheleia-mode)
  :config
  ;; Don't assume Emacs indentation is correct
  (setq apheleia-formatters-respect-indent-level nil)
  ;; Format remote files using local formatters
  (setq apheleia-remote-algorithm 'local))
