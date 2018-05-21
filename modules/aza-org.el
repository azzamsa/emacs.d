(use-package org
  :ensure t
  :defer 1
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda))
  :init
  (progn
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
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((java . t)
       (sh   . t)
       (python . t)
       (lisp . t)))
    (my-org-mode-hook))

  :preface
  (defun my-org-mode-hook ()
    (add-hook
     'completion-at-point-functions
     'pcomplete-completions-at-point nil t))

  :config
  (use-package ox-gfm
    :ensure t)
  (use-package org-download
    :ensure t)
  ;;org-refil
  (setq org-refile-targets '(("~/.emacs.d/documents/gtd/project.org" :maxlevel . 3)
                             ("~/.emacs.d/documents/gtd/someday.org" :level . 1)
                             ("~/.emacs.d/documents/gtd/tickler.org" :maxlevel . 2)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/.emacs.d/documents/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/.emacs.d/documents/gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")
                                ("S" "Sletz" entry
                                 (file+headline "~/.emacs.d/documents/sletz.org" "Tickler")
                                 "* %i%? \n %U")))
  (add-hook 'org-mode-hook (lambda ()
                             (my-org-mode-hook)
                             (turn-on-auto-fill)
                             (which-function-mode -1))))
                             ;;(flycheck-mode -1)
                             ;;(flyspell-mode-off))))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook
                  (lambda ()
                    (org-bullets-mode 1))))

(use-package org-cliplink
  :ensure t
  :demand t
  :bind ("C-l" . org-cliplink)
  :config
  (setq org-cliplink-max-length 60))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(provide 'aza-org)
