;;------------------------------------------------
;; Global keybindings
;;------------------------------------------------

;; Unbind Pesky Sleep Button
(global-unset-key [(control x)(control z)])

(global-set-key [f7] (lambda () (interactive) (find-file user-init-file)))

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

;; resize windows in more more comfortable way
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; emacs fix
(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; emacs additions
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; minibuffer
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(provide 'aza-global-keybinding)
