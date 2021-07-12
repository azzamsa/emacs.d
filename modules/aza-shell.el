(use-package vterm
  :bind (:map vterm-mode-map
              ("<f2>" . vterm-toggle)
              ("C-<f2>" . vterm-toggle-cd)
              ([(control return)]  . vterm-toggle-insert-cd)))

(use-package vterm-toggle
  :after vterm
  :config
  ;; show vterm buffer in bottom side
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide 'aza-shell)
