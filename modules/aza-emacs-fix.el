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

(defun aza-delete-line ()
  "Delete from current position to end of line without pushing to `kill-ring'."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun aza-delete-whole-line ()
  "Delete whole line without pushing to kill-ring."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun crux-smart-delete-line ()
  "Kill to the end of the line and kill whole line on the next call."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (aza-delete-whole-line)
      (goto-char orig-point)
      (aza-delete-line))))

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
