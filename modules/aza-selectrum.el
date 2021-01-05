(use-package selectrum
  :init
  (selectrum-mode +1))

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
         ;;("s-m s" . consult-multi-occur)
         ("s-m f" . consult-ripgrep)
         )
  :config
  (consult-preview-mode)
  :custom-face
  (font-lock-function-name-face ((t (:foreground "salmon1")))))

(use-package consult-selectrum
  :after consult
  :demand t)

(use-package marginalia
  :after consult
  :init
  (marginalia-mode)

  (setq fringes-outside-margins nil)
  (setq left-margin-width 1)
  (setq right-margin-width 1))


(use-package embark
  :after selectrum
  :bind (:map minibuffer-local-map
              ("s-/" . embark-act)
              ("s-?" . embark-act-noexit)
              :map embark-file-map
              ("j" . dired-jump)
              ("s" . sudo-edit)))

(provide 'aza-selectrum)
