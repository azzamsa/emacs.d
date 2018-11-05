(require 'aza-shell-prompt)

;; ---
;; shell-mode
;; ---
(use-package shell
  :bind ((:map shell-mode-map
               ("M-p" . helm-comint-input-ring))
         ("s-g" . dirs)))

(use-package shell-here
  :after shell)

(use-package xterm-color
  :defer t
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions
                                 'xterm-color-filter nil t))))

;; ---
;; eshell-mode
;; ---
(use-package eshell
  :defer t
  :disabled
  :config
  (require 'aza-shell-prompt)
  (require 'helm-eshell)

  (setq eshell-directory-name
        (expand-file-name "eshell" azzamsa-savefile-dir))
  ;; eshell-history-file-name need to be set explicitly
  (setq eshell-history-file-name
        (expand-file-name "history" azzamsa-eshell-dir))

  (defun eshell-this-dir ()
    "Open or move eshell in `default-directory'."
    (interactive)
    (unless (get-buffer eshell-buffer-name)
      (eshell))
    (switch-to-buffer eshell-buffer-name)
    (goto-char (point-max))
    (eshell-kill-input)
    (insert (format "cd %s" default-directory))
    (eshell-send-input)
    (goto-char (point-max)))

  (setq comint-input-ignoredups t)
  (setq comint-prompt-read-only t)

  ;; don't create new frame for helm-eshell-history
  (setq helm-show-completion-display-function
        #'helm-show-completion-default-display-function)

  ;; very strange!. can't use `:bind' for eshell-mode-map
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history))))

;; ---
;; ansi-term
;; ---
(use-package ansi-term
  :ensure nil
  :disabled
  :no-require t
  :defer t
  :bind ((:map shell-mode-map
               ("M-p" . helm-comint-input-ring))
         ("s-g" . dirs))
  :init
  (defun oleh-term-exec-hook ()
    (let* ((buff (current-buffer))
           (proc (get-buffer-process buff)))
      (set-process-sentinel
       proc
       `(lambda (process event)
          (if (string= event "finished\n")
              (kill-buffer ,buff))))))
  :config
  (defun named-term (name)
    (interactive "sName: ")
    (ansi-term "/bin/bash" name))

  (setq comint-prompt-read-only t) ; make shell-prompt read-only
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook
            'ansi-color-for-comint-mode-on ; add color to shell
            'dirtrack-mode t)

  (eval-after-load "term"
    '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))
  (add-hook 'term-exec-hook 'oleh-term-exec-hook)

  ;; Non-zero values for `line-spacing' can mess up ansi-term and co,
  ;; so we zero it explicitly in those cases.
  (add-hook 'term-mode-hook
            (lambda ()
              (setq line-spacing 0))))

;; ---
;; other
;; ---
(use-package eterm-256color
  :defer t
  :disabled
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))

;;TODO Do I need this on GNU/Linux
(use-package exec-path-from-shell
  :defer 3
  :disabled
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package bash-completion
  :ensure t
  :defer 3
  :init
  (bash-completion-setup))

(provide 'aza-shell)
