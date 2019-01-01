;;------------------------------------------------
;; Change Emacs behavior to my convenience
;;------------------------------------------------

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

;; FIXME It will treat all images and pdfs larger than specified value
;; to be opened in fundamental mode.
;; (defun my-treat-largefile-special-way ()
;;   "If a file is over a given size, treat it special way.
;; by Trey Jackson"
;;   (when (> (buffer-size) (* 1024 1024))
;;     (buffer-disable-undo)
;;     (fundamental-mode)
;;     (diff-hl-mode -1)))

;; (add-hook 'find-file-hook 'my-treat-largefile-special-way)


(provide 'aza-emacs-fix)
