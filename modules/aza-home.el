;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomidor
  :disabled
  :defer t
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        pomidor-sound-overwork
        (expand-file-name (concat (getenv "HOME") "/sounds/ding-rest.wav"))
        pomidor-sound-break-over
        (expand-file-name (concat (getenv "HOME") "/sounds/coin-work.wav")))
  (setq alert-default-style 'libnotify)
  :custom-face
  (pomidor-time-face ((t (:height 3.0))))
  (pomidor-timer-face ((t (:height 3.0)))))

(use-package pomodoro
  :defer 3
  :load-path "~/emacs-packages/pomodoro.el/"
  :config
  (setq pomodoro-show-number t)
  (setq pomodoro-long-break-time 20)
  (setq pomodoro-work-time 25)
  (setq pomodoro-break-time 5)
  (setq pomodoro-sound-player "/usr/bin/mpv")
  (setq pomodoro-break-start-sound
        "~/sounds/ding-rest-long.mp3")
  (setq pomodoro-work-start-sound
        "~/sounds/coin-work-long.mp3")
  (pomodoro-add-to-mode-line))

(use-package pdf-tools
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package mingus
  :ensure-system-package mpd
  :defer t)

(use-package ledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :config
  (setq ledger-binary-path "hledger")
  (setq ledger-mode-should-check-version nil)
  ;; fix report when using hldeger
  (setq ledger-report-auto-width nil
        ledger-report-use-native-highlighting nil)

  ;; automatically show new transactions from hledger add or hledger-web
  (add-hook 'ledger-mode-hook 'auto-revert-tail-mode)

  ;; M-1 to collapse all transactions to one line, M-0 to reset. Useful for quickly scanning
  (global-set-key "\M-0" (lambda () (interactive) (set-selective-display (* tab-width 0))))
  (global-set-key "\M-1" (lambda () (interactive) (set-selective-display (* tab-width 1))))
  (add-hook 'ledger-mode-hook (lambda () (setq tab-width 4)))

  (defvar ledger-report-balance
    (list "bal" (concat ledger-binary-path " -f %(ledger-file) bal")))
  (defvar ledger-report-reg
    (list "reg" (concat ledger-binary-path " -f %(ledger-file) reg")))
  (defvar ledger-report-payee
    (list "payee" (concat ledger-binary-path " -f %(ledger-file) reg @%(payee)")))
  (defvar ledger-report-account
    (list "account" (concat ledger-binary-path " -f %(ledger-file) reg %(account)")))

  (setq ledger-reports
        (list ledger-report-balance
              ledger-report-reg
              ledger-report-payee
              ledger-report-account))
  :custom
  (ledger-init-file-name " ")
  ;; move default amount position right allowing longer account names
  (ledger-post-amount-alignment-column 64)
  ;; disable distracting highlight
  (ledger-highlight-xact-under-point nil))

(use-package flycheck-ledger
  :after ledger-mode)

(use-package org-brain
  :defer t
  :init
  (setq org-brain-path "~/.emacs.d/documents/brain")
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 30))

(use-package presentation
  :defer t)

(use-package keyfreq
  :defer 3
  :config
  (setq keyfreq-file (expand-file-name "emacs.keyfreq" azzamsa-savefile-dir))
  (setq keyfreq-file-lock (expand-file-name "emacs.keyfreq.lock" azzamsa-savefile-dir))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package calfw
  :defer t
  :init
  (use-package calfw-cal
    :ensure t)
  (use-package calfw-org
    :ensure t)
  :bind (("C-c A" . my-calendar)
         :map cfw:calendar-mode-map
         ("M-n" . cfw:navi-next-month-command)
         ("M-p" . cfw:navi-previous-month-command)
         ("j"   . cfw:navi-goto-date-command)
         ("g"   . cfw:refresh-calendar-buffer))

  :commands cfw:open-calendar-buffer
  :functions (cfw:open-calendar-buffer
              cfw:refresh-calendar-buffer
              cfw:org-create-source
              cfw:cal-create-source)

  :config
  (defun my-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "#d6c9a7")  ; orgmode source
      (cfw:cal-create-source "White"))))

  (setq diary-file "~/.emacs.d/documents/diary")
  (setq cfw:display-calendar-holidays nil)
  (setq holiday-christian-holidays nil
        holiday-bahai-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-oriental-holidays nil)
  :custom-face
  (cfw:face-annotation ((t :foreground "#ffffff" :inherit cfw:face-day-title)))
  (cfw:face-day-title ((t :background "grey10")))
  (cfw:face-default-content ((t :foreground "#ffffff")))
  (cfw:face-default-day ((t :foreground "#b4eeb4" :weight bold :inherit cfw:face-day-title)))
  (cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
  (cfw:face-grid ((t :foreground "#BADEAC")))
  (cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
  (cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
  (cfw:face-periods ((t :foreground "#ffe259")))
  (cfw:face-saturday ((t :foreground "8cd0d3" :weight bold)))
  (cfw:face-select ((t :background "#2f2f2f")))
  (cfw:face-sunday ((t :foreground "#cc9393" :weight bold)))
  (cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
  (cfw:face-today ((t :background: "grey10" :weight bold)))
  (cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
  (cfw:face-toolbar ((t :foreground "Steelblue4" :background "#3F3F3F")))
  (cfw:face-toolbar-button-off ((t :foreground "#f5f5f5" :weight bold)))
  (cfw:face-toolbar-button-on ((t :foreground "#ffffff" :weight bold))))

(use-package pelican-mode
  :pin manual
  :commands pelican-mode
  :load-path "~/emacs-packages/pelican-mode/"
  :config
  (pelican-global-mode))

(use-package atomic-chrome
  :defer t
  :config
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)))
  (atomic-chrome-start-server))

(use-package emojify
  :hook
  ((markdown-mode . emojify-mode)
   (git-commit-mode . emojify-mode)
   (magit-status-mode . emojify-mode)
   (magit-log-mode . emojify-mode)
   (org-mode . emojify-mode))
  :config
  (setq emojify-emoji-styles '(github unicode))
  (setq emojify-emojis-dir
        (expand-file-name "emojify/"  azzamsa-savefile-dir)))

(use-package chronos
  :pin manual
  :defer t
  :load-path "~/emacs-packages/chronos/"
  :config
  (setq chronos-shell-notify-program "mpv"
        chronos-shell-notify-parameters '("--really-quiet"
                                          "--af=scaletempo=speed=pitch"
                                          "--speed=0.65"
                                          "~/sounds/early-sunrise-yey.wav")
        chronos-text-to-speech-program "espeak-ng"
        chronos-text-to-speech-program-parameters '("-s 100"
                                                    "-v mb-id1")
        chronos-expiry-functions '(chronos-dunstify
                                   chronos-buffer-notify
                                   chronos-shell-notify
                                   chronos-text-to-speech-notify)))

(use-package define-word
  :defer t)

(use-package salah-time
  :load-path "~/emacs-packages/salah-time"
  :commands salah-time
  :config
  (setq salah-time-city "Malang"))

(use-package google-translate
  :defer t
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("en" . "id") ("id" . "en")))
  (setq google-translate-output-destination nil)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "id"))

(provide 'aza-home)
