;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers"
  (interactive)
  (dolist (buffer (delq (current-buffer) (buffer-list)))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (/= (aref name 0) ?\s)
                 (string-match "^[^\*]" name))
        (funcall 'kill-buffer buffer)))))

(provide 'aza-scripts)
