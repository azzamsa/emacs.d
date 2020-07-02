(straight-use-package 'org-plus-contrib)

(use-package org
  :after aza-secrets
  :bind ((:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-k" . my-delete-line)))
  :config
  (setq org-src-tab-acts-natively t)
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files my-agenda-files)
  ;; (setq org-agenda-files '("/media/azzamsya/rhd/azzamsa/gtd/"))
  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "STARTED(s!)"
                                      "WAITING(w@/!)"
                                      "|"
                                      "DONE(d!)"
                                      "CANCELLED(c@)")))
  (add-hook
   'completion-at-point-functions
   'pcomplete-completions-at-point nil t)
  ;; inline image
  (setq org-image-actual-width nil)
  (setq org-refile-targets '((my-inbox-gtd :maxlevel . 1)
                             (my-projects-gtd :maxlevel . 3)
                             (my-someday-gtd :level . 1)
                             (my-tickler-gtd :maxlevel . 2)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline my-inbox-gtd "Inbox")
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

(use-package ob-org :straight (:type built-in) :after org :defer 0.9)
(use-package ob-lisp :straight (:type built-in) :after org :defer 0.9)
(use-package ob-python :straight (:type built-in) :after org :defer 0.9)
(use-package ob-ruby :straight (:type built-in) :after org :defer 0.9)

(use-package ox-gfm
  :defer 0.9
  :after org)

(use-package org-download
  :defer 0.9
  :after org
  :load-path "~/emacs-packages/org-download/"
  :bind (:map org-mode-map
              ("C-c y" . org-download-yank))
  :config
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s")
  (setq org-download-image-org-width 400))

(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook
                  (lambda ()
                    (org-bullets-mode 1))))

(use-package org-cliplink
  :defer 0.9
  :bind (:map org-mode-map
              ("C-l" . org-cliplink))
  :config
  (setq org-cliplink-max-length 60))

(eval-after-load 'org-indent '(delight 'org-indent-mode))

(provide 'aza-org)
