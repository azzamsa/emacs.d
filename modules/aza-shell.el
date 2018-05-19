(require 'aza-shell-prompt)

(use-package eshell
  :demand t
  :config
  (require 'aza-shell-prompt)
  ;; very strange!. can't use `:bind' for eshell-mode-map
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-c C-l")
                  'helm-eshell-history)))
  (setq eshell-directory-name
        (expand-file-name "eshell" azzamsa-savefile-dir)))

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
  :bind (("C-t" . shell-pop-eshell)
         ;;("C-z" . shell-pop-shell)
         (:map shell-mode-map
               ("C-c C-l" . helm-comint-input-ring))
         (:map eshell-mode-map
               ("C-c C-l" . helm-eshell-history)))
  :preface
  (defun shell-pop-eshell ()
    (interactive)
    (let ((shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
          (shell-pop-term-shell "eshell"))
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
      (call-interactively 'shell-pop)))

  (defun shell-pop-shell ()
    (interactive)
    (let ((shell-file-name "/bin/bash")
          (shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
          (shell-pop-term-shell "shell"))
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
      (call-interactively 'shell-pop))))

(provide 'aza-shell)
