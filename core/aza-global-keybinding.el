;;------------------------------------------------
;; Global keybindings
;;------------------------------------------------

(global-set-key [f7] (lambda () (interactive) (find-file user-init-file)))
(global-set-key [f6] (lambda ()
                       (interactive)
                       (find-file (expand-file-name
                                   "documents/gtd/inbox.org" user-emacs-directory))))

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
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "<C-backspace>") 'my-delete-line-backward)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; emacs enhancement
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-c U") 'unfill-paragraph)

;; minibuffer
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(provide 'aza-global-keybinding)
