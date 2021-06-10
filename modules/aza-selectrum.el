(use-package selectrum
  :init
  (selectrum-mode +1)
  :config
  ;; fix selectrum minibuffer is not resized
  (setq selectrum-fix-vertical-window-height t))

(use-package selectrum-prescient
  :after selectrum
  :config
  (setq prescient-save-file
        (expand-file-name "prescient-save.el" aza-savefile-dir))

  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :after selectrum
  :bind (("s-m m" . consult-buffer) ;; dwim
         ("s-m l" . consult-line)
         ("s-m o" . consult-outline)
         ("s-m r" . consult-mark)
         ("s-m R" . consult-global-mark)
         ("s-m s" . consult-ripgrep)
         ("s-m g" . consult-grep)
         ("C-x 4 b" . consult-buffer-other-window))
  :config
  ;; live preview *loads* a file, thus loads all it's mode
  ;; and hog the machine
  (setq consult-preview-key nil)
  (setq consult-buffer-filter '("^ " "\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf"
                                "\\*Messages" "\\*Warning" "*magit-" "magit" "*vterm" "vterm" "^:" "*Occur"
                                "*straight-" "*elfeed-log" "*trace of SMTP session"
                                "*format-all-error" "*Async-"
                                "*lsp-" "*rust-" "*company-" "*pyls"))
  :custom-face
  (font-lock-function-name-face ((t (:foreground "#ed9366")))))

(use-package embark
  :after selectrum
  :bind (:map minibuffer-local-map
              ("s-/" . embark-act)
              ("s-?" . embark-act-noexit)
              :map embark-file-map
              ("j" . dired-jump)
              ("s" . sudo-edit)))

(provide 'aza-selectrum)
