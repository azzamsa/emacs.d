;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomodoro
  :demand t
  :load-path "elisp/pomodoro/"
  :config
  (pomodoro-add-to-mode-line)
  (setq pomodoro-show-number t)
  (setq pomodoro-long-break-time 20)
  (setq pomodoro-sound-player "/usr/bin/aplay")
  (setq pomodoro-break-start-sound
        "~/sounds/sparkle-work.wav")
  (setq pomodoro-work-start-sound
        "~/sounds/sparkle-work.wav")
  (add-hook 'emacs-startup-hook
            (lambda ()
              (pomodoro-start 25))))

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install))

(use-package mingus
  :ensure t
  :ensure-system-package mpd
  :defer t)

(use-package ledger-mode
  :ensure t
  :defer t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :config
  (setq ledger-binary-path "hledger")
  (setq ledger-mode-should-check-version nil))

(provide 'aza-home)
