(use-package mu4e
  :after aza-secrets
  :commands mu4e
  :bind ((:map mu4e-main-mode-map
               ("C-c x" . omail-compose)))
  :straight ( :host github
              :repo "djcb/mu"
              :branch "master"
              :files ("mu4e/*")
              :pre-build (("./autogen.sh") ("make")))
  :custom   (mu4e-mu-binary (expand-file-name "mu/mu" (straight--repos-dir "mu")))
  :config
  ;; thanks @irreal.org
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (setq mu4e-headers-fields
        '((:date          .  20)
          (:flags         .   6)
          (:from          .  22)
          (:subject       .  nil)))

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
  (add-hook 'message-mode-hook
            (lambda ()
              (auto-fill-mode +1)
              (display-line-numbers-mode +1)))
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

  ;; call it manually, most people prefer plain-text email
  ;; (add-hook 'org-ctrl-c-ctrl-c-hook 'omail-send t)

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

(use-package smtpmail
  :straight (:type built-in)
  :after mu4e
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials
        '(("box.azzamsa.com" 587 nil nil)))
  (setq smtpmail-smtp-server "box.azzamsa.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-debug-info t))

(provide 'aza-mu4e)
