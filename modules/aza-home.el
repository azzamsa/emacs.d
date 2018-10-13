;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomidor
  :ensure t
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

(defun crontab-e ()
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

;; TODO pyuic

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-file (expand-file-name "emacs.keyfreq" azzamsa-savefile-dir))
  (setq keyfreq-file-lock (expand-file-name "emacs.keyfreq.lock" azzamsa-savefile-dir))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'aza-home)
