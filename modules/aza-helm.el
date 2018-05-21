(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-c f" . helm-recentf)
         ("C-x C-b" . helm-buffers-list)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-c h /" . helm-find)
         ("C-c h l" . helm-locate)
         ("C-c p h" . helm-projectile)
         ("C-h SPC" . helm-all-mark-rings)
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

  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t)

  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
          "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack-grep -H --no-group --no-color %e %p %f")))

(use-package helm-org-rifle
  :ensure t
  :bind ("C-c h r" . helm-org-rifle))

(use-package helm-ag
  :ensure t
  :ensure-system-package ag
  :defer 4
  :diminish helm-ag-mode
  :bind ("C-c a" . helm-ag))

(provide 'aza-helm)
