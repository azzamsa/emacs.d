(use-package org
  :ensure org
  :defer 1
  :pin org
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda))
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
  ;;org-refil
  (setq org-refile-targets '(("~/.emacs.d/documents/gtd/inbox.org" :maxlevel . 1)
                             ("~/.emacs.d/documents/gtd/project.org" :maxlevel . 3)
                             ("~/.emacs.d/documents/gtd/someday.org" :level . 1)
                             ("~/.emacs.d/documents/gtd/tickler.org" :maxlevel . 2)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/.emacs.d/documents/gtd/inbox.org" "Inbox")
                                 "* TODO %i%?")))
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (add-hook 'org-mode-hook (lambda ()
                             (my-org-mode-hook)
                             (which-function-mode -1))))

(use-package ob-org :ensure nil :after org)
(use-package ob-lisp :ensure nil :after org)
(use-package ob-python :ensure nil :after org)

(use-package ox-gfm
  :defer 3
  :after org)

(use-package org-download
  :defer 3
  :after org
  :config
  ;; FIXME take from screenshot
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq org-download-screenshot-method "scrot -s %s")
  (setq org-download-image-org-width 400))

(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook
                  (lambda ()
                    (org-bullets-mode 1))))

(use-package org-cliplink
  :defer 3
  :bind ("C-l" . org-cliplink)
  :config
  (setq org-cliplink-max-length 60))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(provide 'aza-org)
