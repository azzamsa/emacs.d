(use-package erc
  :defer 1
  :preface
  ;; @rememberYou
  (defun my/erc-count-users ()
    "Displays the number of users connected on the current channel."
    (interactive)
    (if (get-buffer "irc.freenode.net:6667")
        (let ((channel (erc-default-target)))
          (if (and channel (erc-channel-p channel))
              (message "%d users are online on %s"
                       (hash-table-count erc-channel-users)
                       channel)
            (user-error "The current buffer is not a channel")))
      (user-error "You must first start ERC")))

  (defun my/erc-preprocess (string)
    "Avoids channel flooding."
    (setq str
          (string-trim
           (replace-regexp-in-string "\n+" " " str))))
  :hook (((erc-send-pre . my/erc-preprocess)))
  :config
  (require 'erc-desktop-notifications)

  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"))
        erc-server "irc.freenode.net"
        erc-nick "azzamsa")

  ;; Logging
  (setq erc-log-channels-directory "~/irclogs/")
  (setq erc-generate-log-file-name-function (quote erc-generate-log-file-name-with-date))
  (setq erc-save-buffer-on-part nil)
  (setq erc-save-queries-on-quit nil)
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  (setq erc-log-insert-log-on-open nil)
  (setq erc-kill-server-buffer-on-quit t)
  ;; open query buffers in the current window
  (setq erc-query-display 'buffer)

  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))

  (setq erc-prompt-for-password nil)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-autojoin-timing 'ident)
  (setq erc-server-reconnect-attempts 5)
  (setq erc-server-reconnect-timeout 3)

  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)

  (erc-track-mode +1)
  (erc-log-mode +1)
  (erc-truncate-mode +1)
  (erc-services-mode +1)
  (erc-update-modules))

(use-package erc-hl-nicks :after erc)

(use-package erc-view-log
  :after erc
  :config
  (add-to-list 'auto-mode-alist '("\\.erclogs/.*\\.log" . erc-view-log-mode)))

(provide 'aza-erc)
