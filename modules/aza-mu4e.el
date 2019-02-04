(use-package mu4e
  :ensure nil
  :defer t
  :ensure-system-package mu
  :commands mu4e
  :preface
  (defun my-mu4e-compose-hook ()
    (set-fill-column 72)
    (flyspell-mode))
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit nil)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-get-mail-command "mbsync memail")
  (mu4e-maildir "~/Email/memail/")
  (mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)
     ("/Drafts" . ?d)
     ("/Sent" . ?s)
     ("/Trash" . ?t)))
  (mu4e-refile-folder "/Archive")
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-attachment-dir "~/Email/memail/Attachments/")
  (mu4e-html2text-command "w3m -T text/html")
  (mu4e-org-contacts-file "~/Email/contacts.org")
  (mu4e-update-interval 300)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-addresses t)
  (mu4e-view-show-images t)
  :config
  (add-to-list 'mu4e-view-actions '("Add to contact " . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
  (add-hook 'mu4e-compose-mode-hook #'my-mu4e-compose-hook))

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
  :config
  (require 'aza-secrets)
  (setq smtpmail-smtp-server my-smtpmail-smtp-server)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl))

(provide 'aza-mu4e)
