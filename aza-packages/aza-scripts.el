(require 'cl-lib)
(require 's)

(defun save-all-buffers-silently ()
  (save-some-buffers t))

;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (save-all-buffers-silently)
  (when (y-or-n-p "Really kill all other buffers ? ")
    (let ((killed-bufs 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (let ((name (buffer-name buffer)))
          (when (and name (not (string-equal name ""))
                     (/= (aref name 0) ?\s)
                     (string-match "^[^\*]" name))
            (cl-incf killed-bufs)
            (funcall 'kill-buffer buffer))))
      (message "Killed %d buffer(s)" killed-bufs))))

(defun aza-today ()
  "Insert date according to given argument.
If no argument is given, insert today's date"
  (interactive)
  (insert
   (format-time-string "%A, %B %e, %Y"
                       (if current-prefix-arg
                           (time-add (current-time) (* (* 24 3600)
                                                       current-prefix-arg))
                         (current-time)))))

(defun now ()
  (interactive)
  (insert (format-time-string "%F %H:%M")))

(defun hour ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(provide 'aza-scripts)
