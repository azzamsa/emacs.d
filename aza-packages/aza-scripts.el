(require 'cl)

;;;###autoload
(defun noprompt-kill-buffers ()
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive)
  (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (kill-matching-buffers "^[^\*]")))

(provide 'aza-scripts)
