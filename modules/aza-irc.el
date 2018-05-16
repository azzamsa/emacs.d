(use-package erc
  :defer t
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#emacs"
                                       "#emacs-beginners"))
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

(provide 'aza-irc)
