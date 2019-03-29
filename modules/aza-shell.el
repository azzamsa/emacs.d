;;(require 'aza-shell-prompt)

(use-package shell
  :defer t
  :bind ((:map shell-mode-map
               ("C-c C-l" . helm-comint-input-ring))
         ("s-g" . dirs))
  :config
  (setq comint-prompt-read-only t) ; make shell-prompt read-only
  (setq comint-input-ignoredups t))

(use-package shell-here
  :defer t
  :after shell
  :commands shell-here)

(use-package xterm-color
  :after shell-here
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions
                            'xterm-color-filter nil t))))

(use-package bash-completion
  :after shell-here
  :init
  (bash-completion-setup))

(provide 'aza-shell)
