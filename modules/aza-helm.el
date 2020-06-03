(use-package helm
  :delight helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("s-m" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-c h s" . helm-swoop)
         ("C-c h /" . helm-find)
         ("C-c h l" . helm-locate)
         ("C-c p h" . helm-projectile)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c m" . helm-imenu)
         (:map isearch-mode-map
               ("C-o" . helm-occur-from-isearch)))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; fix display not ready
  (setq helm-exit-idle-delay 0)
  (setq
   helm-autoresize-max-height 30
   helm-autoresize-min-height 20)

  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))

  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-buffers-truncate-lines nil)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t)

  (setq helm-boring-buffer-regexp-list
        '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf"
          "\\*Messages" "*magit-" "magit" "*vterm" "vterm" "^:"))

  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
          "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack-grep -H --no-group --no-color %e %p %f")))

(use-package helm-org-rifle
  :after helm
  :bind (("C-c h r c" . helm-org-rifle-current-buffer)
         ("C-c h r a" . helm-org-rifle)))

(use-package helm-ag
  :after helm
  :delight helm-ag-mode
  :bind (("C-c a a" . helm-ag)
         ("C-c a t" . helm-ag-this-file)
         ("C-c a p" . helm-ag-project-root)))

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :defer t
  :after helm)

(provide 'aza-helm)
