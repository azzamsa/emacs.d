;;------------------------------------------------
;; Global keybindings
;;------------------------------------------------

(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "s-j") 'dired-jump)
(global-set-key [remap just-one-space] 'cycle-spacing)

;; emacs fix
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

(keyboard-translate ?\C-h ?\C-p) ; swap
(keyboard-translate ?\C-p ?\C-h) ; swap

;; emacs enhancement
(global-set-key (kbd "M-Q") 'unfill-paragraph)

(provide 'aza-global-keybinding)
