;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomodoro
  :defer t
  :commands pomodoro-start
  :config
  (setq pomodoro-show-number t)
  (setq pomodoro-long-break-time 20)
  (setq pomodoro-work-time 25)
  (setq pomodoro-break-time 5)
  (setq pomodoro-sound-player "/usr/bin/ogg123")
  (setq pomodoro-break-start-sound
        "~/sounds/ding-rest-medium.ogg")
  (setq pomodoro-work-start-sound
        "~/sounds/coin-work-medium.ogg")
  (pomodoro-add-to-mode-line))

(use-package org-pomodoro
  :config
  (setq org-pomodoro-audio-player "/usr/bin/mpv")
  (setq org-pomodoro-start-sound "~/sounds/coin-work-medium.ogg")
  (setq org-pomodoro-start-sound-args "--volume 30")
  (setq org-pomodoro-short-break-sound "~/sounds/ding-rest-medium.ogg")
  (setq org-pomodoro-short-break-args "--volume 30")
  (setq org-pomodoro-overtime-sound "~/sounds/ding-rest-medium.ogg")
  (setq org-pomodoro-overtime-sound-args "--volume 30")
  (setq org-pomodoro-long-break-sound "~/sounds/ding-rest-medium.ogg")
  (setq org-pomodoro-long-break-sound-args "--volume 30")
  (setq org-pomodoro-finished-sound "~/sounds/ding-rest-medium.ogg")
  (setq org-pomodoro-finished-sound-args "--volume 30"))

(use-package pdf-tools
  :defer t
  :disabled
  :magic (("%PDF" . pdf-view-mode))
  :config
  (pdf-tools-install))

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
  :defer 0.9
  :config
  (setq keyfreq-file (expand-file-name "emacs.keyfreq" aza-savefile-dir))
  (setq keyfreq-file-lock (expand-file-name "emacs.keyfreq.lock" aza-savefile-dir))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package explain-pause-mode
  :defer t
  :straight (explain-pause-mode :type git
                                :files ("explain-pause-mode.el")
                                :host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode t))

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
   (cfw:calendar-mode . emojify-mode)
   (org-mode . emojify-mode))
  :config
  (setq emojify-emoji-styles '(github unicode))
  (setq emojify-emojis-dir
        (expand-file-name "emojify/"  aza-savefile-dir)))

(use-package define-word
  :defer t)

(use-package google-translate
  :defer t
  :commands google-translate-smooth-translate
  :init
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("en" . "id") ("id" . "en")))
  (setq google-translate-output-destination nil)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "id"))

(use-package password-store
  :straight (password-store :type git :flavor melpa
                            :files ("contrib/emacs/*.el" "password-store-pkg.el")
                            :host github :repo "zx2c4/password-store"))

(use-package elfeed)

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.config/elfeed.org")))

(use-package keycast)

(use-package appt
  :straight (:type built-in)
  :config
  ;; thanks scaramouche
  (appt-activate t)
  (setq appt-message-warning-time 5)
  (setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
  (setq appt-display-mode-line nil)

  (defun my-org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  (my-org-agenda-to-appt)
  (run-at-time "12:05am" (* 24 3600) 'my-org-agenda-to-appt)

  (add-hook 'after-save-hook
            '(lambda ()
               (if (string= (buffer-file-name) my-inbox-gtd)
                   (my-org-agenda-to-appt))))

  (setq appt-disp-window-function 'my-appt-display)
  (setq appt-delete-window-function (lambda () t))

  (setq my-appt-notification-app (concat (getenv "HOME") "/bin/appt-notify"))

  (defun my-appt-display (min-to-app new-time msg)
    (if (atom min-to-app)
        (start-process "my-appt-notification-app" nil my-appt-notification-app min-to-app msg)
      (dolist (i (number-sequence 0 (1- (length min-to-app))))
        (start-process "my-appt-notification-app" nil my-appt-notification-app (nth i min-to-app) (nth i msg))))))


(use-package time
  :straight (:type built-in)
  :config
  (setq display-time-world-time-format "%z\t%a %d %b %R")
  (setq display-time-world-list '(("Africa/Timbuktu" "Troll")
                                  ("America/New_York" "New York")
                                  ("Asia/Jakarta" "Jakarta"))))

(use-package world-time-mode)

(use-package activity-watch-mode
  :config
  (global-activity-watch-mode))


(provide 'aza-home)
