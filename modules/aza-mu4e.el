(use-package mu4e
  :ensure nil
  :defer t
  :ensure-system-package mu
  :commands mu4e
  :bind (:map mu4e-main-mode-map
              ("C-c x" . omail-compose))
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
  :config
  (defun omail-compose ()
    (interactive)
    (mu4e-compose-new)
    (org-mu4e-compose-org-mode))

  (defun omail-send ()
    "When in an org-mu4e-compose-org-mode message, htmlize and send it."
    (interactive)
    (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
      (org-mime-htmlize)
      (message-send-and-exit)))

  (add-hook 'org-ctrl-c-ctrl-c-hook 'omail-send t)
  :custom
  (org-mu4e-convert-to-html t))

(use-package org-mime
  :after mu4e
  :config
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "blockquote" "margin:1.2em 0px;border-left:4px solid rgb(221,221,221);padding:0px 1em;color:rgb(119,119,119);quotes:none"))))

(use-package mu4e-alert
  :hook
  ((after-init . mu4e-alert-enable-mode-line-display)
   (after-init . mu4e-alert-enable-notifications))
  :config
  ;; FIXME filter gitlab notification
  ;; (setq mu4e-alert-interesting-mail-query
  ;;       (concat
  ;;        "flag:unread"
  ;;        " AND NOT flag:trashed"
  ;;        " AND NOT maildir:"
  ;;        "\"/[Gmail].All Mail\""))
  (mu4e-alert-set-default-style 'libnotify))

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
