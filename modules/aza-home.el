;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomidor
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
  (setq ledger-mode-should-check-version nil))

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

(defun crontab-e ()
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

;; TODO pyuic

(use-package keyfreq
  :defer 5
  :config
  (setq keyfreq-file (expand-file-name "emacs.keyfreq" azzamsa-savefile-dir))
  (setq keyfreq-file-lock (expand-file-name "emacs.keyfreq.lock" azzamsa-savefile-dir))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package erc
  :defer t
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"))
        erc-server "irc.freenode.net"
        erc-nick "azzamsa")
  (setq erc-log-channels-directory "~/.erc/logs/")
  (setq erc-save-buffer-on-part t)
  (setq erc-log-insert-log-on-open nil)
  ;; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)
  ;; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)
  ;; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)
  ;; open query buffers in the current window
  (setq erc-query-display 'buffer)
  (erc-truncate-mode +1)
  ;; exclude boring stuff from tracking
  (erc-track-mode t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")))

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


(provide 'aza-home)
