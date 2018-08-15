(require 'aza-shell-prompt)

(use-package eshell
  :demand t
  :config
  (require 'aza-shell-prompt)
  (require 'helm-eshell)

  (setq eshell-directory-name
        (expand-file-name "eshell" azzamsa-savefile-dir))
  ;; eshell-history-file-name need to be set explicitly
  (setq eshell-history-file-name
        (expand-file-name "history" azzamsa-eshell-dir))

  ;; very strange!. can't use `:bind' for eshell-mode-map
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l")
                  'helm-eshell-history))))

(use-package shell
  :demand t
  :bind ((:map shell-mode-map
               ("C-c C-l" . helm-comint-input-ring))
         ("s-g" . dirs))
  :config
  (setq comint-prompt-read-only t) ; make shell-prompt read-only
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook
            'ansi-color-for-comint-mode-on ; add color to shell
            'dirtrack-mode t))

(use-package shell-pop
  :ensure t
  :demand t
  :bind ((:map shell-mode-map
               ("C-c C-l" . helm-comint-input-ring))
         (:map eshell-mode-map
               ("C-c C-l" . helm-eshell-history)))
  :custom
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*"
                                (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop-term-shell "/bin/bash")
  (shell-pop-universal-key "C-t")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))

(provide 'aza-shell)
