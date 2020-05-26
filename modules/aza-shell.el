(use-package vterm
  :straight (vterm :type git :flavor melpa
                   :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
                   :host github :repo "akermu/emacs-libvterm")
  :bind (:map vterm-mode-map
              ("<f2>" . vterm-toggle)
              ([(control return)]  . vterm-toggle-insert-cd)
              ("C-c C-l" . helm-comint-input-ring)
              ("C-n" . vterm-toggle-forward)
              ("C-b" . vterm-toggle-backward))
  :config
  (setq cursor-type 'bar))

(use-package vterm-toggle
  :after vterm
  :straight (vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle" :no-build t))

(provide 'aza-shell)
