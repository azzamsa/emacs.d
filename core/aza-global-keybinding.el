;;------------------------------------------------
;; Global keybindings
;;------------------------------------------------

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

;; emacs fix
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "<C-backspace>") 'my-delete-line-backward)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

(keyboard-translate ?\C-h ?\C-p) ; swap
(keyboard-translate ?\C-p ?\C-h) ; swap

;; emacs enhancement
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "C-c U") 'unfill-paragraph)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(global-set-key (kbd "<f12>") 'menu-bar-mode)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key [C-next] (lambda ()
                           (interactive)
                           (message "Ups, wrong key. Calm dude :)")))

(provide 'aza-global-keybinding)
