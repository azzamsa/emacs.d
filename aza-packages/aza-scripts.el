;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers"
  (interactive)
  (dolist (buffer (delq (current-buffer) (buffer-list)))
    (let ((name (buffer-name buffer)))
      (when (string-match "^[^\*]" name)
        (funcall 'kill-buffer buffer)))))

(provide 'aza-scripts)
