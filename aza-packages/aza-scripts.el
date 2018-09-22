(require 'cl-lib)

;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (let ((killed-bufs 0))
    (dolist (buffer (delq (current-buffer) (buffer-list)))
      (let ((name (buffer-name buffer)))
        (when (and name (not (string-equal name ""))
                   (/= (aref name 0) ?\s)
                   (string-match "^[^\*]" name))
          (cl-incf killed-bufs)
          (funcall 'kill-buffer buffer))))
    (message "Killed %d buffer(s)" killed-bufs)))

(provide 'aza-scripts)
