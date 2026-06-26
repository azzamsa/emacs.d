;; -*- lexical-binding: t; -*-

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  ;; find current file and jump to node.
  (setq neo-smart-open t)
  (setq neo-hidden-regexp-list
        '(
          ;; hidden files
          "^\\."
          ;; temp files
          "~$" "^#.*#$"
          ;; generated files
          "^__" "\\.egg-info$" "\\.lock$"
          ;; docker
          "_volumes"
          )))

(defun neotree-project-dir ()
  "Always open NeoTree in project root."
  (interactive)
  (let ((project-dir (ignore-errors (project-root (project-current))))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (if file-name
                  (neotree-find file-name))))
      (message "Could not find project root."))))

;; One tab per project, with unique names - simple implementation of workspaces
(use-package otpp
  :disabled
  :ensure t
  :after project
  :config
  (setq otpp-project-aware-commands-regexp (rx (seq bol (or "project-" "+project-" "projection-")))))

;; Projectile like project management library built on Emacs' `project'
(use-package projection
  :disabled
  :ensure t
  :hook (after-init . global-projection-hook-mode)
  :after project)

(provide '+project)
