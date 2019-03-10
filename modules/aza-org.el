(use-package org
  :defer 1
  :pin org
  :ensure org-plus-contrib ; use this to make sure Emacs didn't pick
                                        ; the default version
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-k" . my-delete-line))
  :init
  (setq org-src-tab-acts-natively t)
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/.emacs.d/documents/gtd/inbox.org"
                           "~/.emacs.d/documents/gtd/project.org"
                           "~/.emacs.d/documents/gtd/tickler.org"))
  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "STARTED(s!)"
                                      "WAITING(w@/!)"
                                      "|"
                                      "DONE(d!)"
                                      "CANCELLED(c@)")))
  (my-org-mode-hook)
  :preface
  (defun my-org-mode-hook ()
    (add-hook
     'completion-at-point-functions
     'pcomplete-completions-at-point nil t))
  :config
  ;; inline image
  (setq org-image-actual-width nil)
  (setq org-refile-targets '(("~/.emacs.d/documents/gtd/inbox.org" :maxlevel . 1)
                             ("~/.emacs.d/documents/gtd/project.org" :maxlevel . 3)
                             ("~/.emacs.d/documents/gtd/someday.org" :level . 1)
                             ("~/.emacs.d/documents/gtd/tickler.org" :maxlevel . 2)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/.emacs.d/documents/gtd/inbox.org" "Inbox")
                                 "* TODO %i%?")))

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (require 'smartparens-config)
  (sp-with-modes 'org-mode
    (sp-local-pair "~" "~")
    (sp-local-pair "*" "*") ;; wow it doesn't start when cursor in the first column
    (sp-local-pair "/" "/")
    (sp-local-pair "_" "_"))

  (add-hook 'org-mode-hook (lambda ()
                             (smartparens-mode +1)
                             (which-function-mode -1)
                             (turn-on-auto-capitalize-mode))))

(use-package ob-org :ensure nil :after org :defer 3)
(use-package ob-lisp :ensure nil :after org :defer 3)
(use-package ob-python :ensure nil :after org :defer 3)
(use-package ob-ruby :ensure nil :after org :defer 3)

(use-package ox-gfm
  :defer 3
  :after org)

(use-package org-download
  :pin manual
  :defer 3
  :after org
  :load-path "~/emacs-packages/org-download/"
  :bind (:map org-mode-map
              ("C-c y" . org-download-yank))
  :config
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq org-download-screenshot-method "maim -s")
  (setq org-download-image-org-width 400)
  (setq org-download-delete-image-after-download t))

(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook
                  (lambda ()
                    (org-bullets-mode 1))))

(use-package org-cliplink
  :defer 3
  :bind (:map org-mode-map
              ("C-l" . org-cliplink))
  :config
  (setq org-cliplink-max-length 60))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(provide 'aza-org)
