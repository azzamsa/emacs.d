(use-package mu4e
  :ensure nil
  :ensure-system-package mu
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit nil)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-get-mail-command "mbsync memail")
  (mu4e-maildir "~/Email/memail/")
  (mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)
     ("/All Mail" . ?a)
     ("/Drafts" . ?D)
     ("/Sent" . ?s)
     ("/Trash" . ?T)))
  (mu4e-refile-folder "/Archive")
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-attachment-dir "~/Email/memail/Attachments/")
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  :config
  (add-to-list 'mu4e-headers-actions '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions '("org-contact-add" . mu4e-action-add-org-contact) t))

(use-package org-mu4e
  :ensure nil
  :after mu4e
  :custom
  (org-mu4e-convert-to-html t))

(use-package mu4e-alert
  :after mu4e
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'libnotify))

(use-package message
  :ensure nil
  :after mu4e

  :custom (send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :custom
  (smtpmail-smtp-server "secret.com")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl))

(provide 'aza-mu4e)
