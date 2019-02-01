(require 'aza-shell-prompt)

(use-package shell
  :defer t
  :bind ((:map shell-mode-map
               ("M-p" . helm-comint-input-ring))
         ("s-g" . dirs))
  :config
  (setq comint-prompt-read-only t) ; make shell-prompt read-only
  (setq comint-input-ignoredups t))

(use-package shell-here
  :after shell)

(use-package xterm-color
  :after shell
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions
                            'xterm-color-filter nil t))))

;; Need this even in GNU/Linux e.g for GOPATH
(use-package exec-path-from-shell
  :defer 2
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package bash-completion
  :after shell
  :init
  (bash-completion-setup))

(provide 'aza-shell)
