(use-package meghanada
  :defer t
  :pin manual
  :commands (meghanada-mode)
  :bind (:map meghanada-mode-map
              ("C-S-t" . meghanada-switch-testcase)
              ("M-RET" . meghanada-local-variable)
              ("C-M-." . helm-imenu)
              ("M-r" . meghanada-reference)
              ("M-t" . meghanada-typeinfo)
              ("C-c C-C m". meghanada-exec-main))
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")

  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              (highlight-symbol-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

(use-package autodisass-java-bytecode
  :defer t
  :disabled)

(provide 'aza-java)
