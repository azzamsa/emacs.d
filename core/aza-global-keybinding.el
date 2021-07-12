;;------------------------------------------------
;; Global keybindings
;;------------------------------------------------

(define-key minibuffer-local-map (kbd "C-c C-l") 'consult-history)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "s-j") 'dired-jump)
(global-set-key [remap just-one-space] 'cycle-spacing)

;; emacs fix
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; emacs enhancement
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-M-Q") 'unfill-region)

(global-unset-key (kbd "C-x m"))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(provide 'aza-global-keybinding)
