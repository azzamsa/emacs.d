(use-package mu4e
  :after aza-secrets
  :commands mu4e
  :bind ((:map mu4e-main-mode-map
              ("C-c x" . omail-compose)))
  :config
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-get-mail-command "mbsync -c ~/Email/.mbsyncrc me-azzamsa")
  (setq mu4e-maildir "~/Email/me-azzamsa/")
  (setq mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)
     ("/Drafts" . ?d)
     ("/Sent" . ?s)
     ("/Trash" . ?t)))
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-trash-folder "/Trash")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-attachment-dir "~/Email/me-azzamsa/Attachments/")
  (setq mu4e-html2text-command "w3m -T text/html")
  (setq mu4e-org-contacts-file "~/Email/contacts.org")
  (setq mu4e-update-interval 300)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-show-images t)

  ;; https://useplaintext.email/#mu4e
  (add-hook 'message-mode-hook 'auto-fill-mode)
  (setq mu4e-compose-format-flowed t)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  (add-to-list 'mu4e-view-actions '("Add to contact " . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t))

(use-package org-mu4e
  :straight (org-mu4e :type git :host github :repo "djcb/mu" :files ("mu/mu4e/*" (:exclude ".git")))
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
      (org-mu4e-compose-org-mode)
      (message-send)))

  (add-hook 'org-ctrl-c-ctrl-c-hook 'omail-send t)

  (setq org-mu4e-convert-to-html t))

(use-package org-mime
  :after mu4e
  :config
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "blockquote" "margin:1.2em 0px;border-left:4px solid rgb(221,221,221);padding:0px 1em;color:rgb(119,119,119);quotes:none"))))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; FIXME filter gitlab notification
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(use-package message
  :straight (:type built-in)
  :ensure nil
  :after mu4e
  :config
  (setq send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :config
  (setq smtpmail-smtp-server my-smtpmail-smtp-server)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl))

(provide 'aza-mu4e)
