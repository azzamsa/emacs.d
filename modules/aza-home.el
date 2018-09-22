;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomidor
  :ensure t
  :config

  ;; (defadvice pomidor-stop (before pomidor-save-log activate)
  ;;     "Log pomidor data to the ~/pomidor-log.csv file.
  ;; Columns: date,work,overwork,break"
  ;;     (write-region (format "%s,%d,%d,%d\n"
  ;;                           (format-time-string "%d/%m/%Y")
  ;;                           (/ (time-to-seconds (pomidor-work-duration)) 60)
  ;;                           (/ (time-to-seconds (or (pomidor-overwork-duration) 0)) 60)
  ;;                           (/ (time-to-seconds (or (pomidor-break-duration) 0)) 60))
  ;;                   nil
  ;;                   "~/Documents/pomidor-log.csv"
  ;;                   'append))

  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        pomidor-sound-overwork
        (expand-file-name (concat (getenv "HOME") "/sounds/ding-rest.wav"))
        pomidor-sound-break-over
        (expand-file-name (concat (getenv "HOME") "/sounds/coin-work.wav")))
  (setq alert-default-style 'libnotify)
  :custom
  (pomidor-time-face ((t (:height 3.0))))
  (pomidor-timer-face ((t (:height 3.0)))))


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

(use-package org-brain
  :ensure t
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
  :ensure t
  :defer t)

(provide 'aza-home)
