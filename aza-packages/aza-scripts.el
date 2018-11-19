(require 'cl-lib)
(require 's)

;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
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

(defun now ()
  (interactive)
  (insert (format-time-string "%F %H:%M")))

(defun today ()
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

(defun hour ()
  (interactive)
  (insert (format-time-string "%H:%M")))


;;;###autoload
(defun make-md-to-org ()
  "transform md link format to org"
  (interactive)
  (let ((mdlink
         (buffer-substring-no-properties (region-beginning) (region-end))))
    (let ((orglink
           (s-concat "[["
                     (s-chop-suffix ")" (s-chop-prefix "](" (second (s-slice-at "](" mdlink))))
                     "]["
                     (s-chop-prefix "[" (first (s-slice-at "](" mdlink)))
                     "]]")))
      (delete-region (region-beginning) (region-end))
      (insert orglink))))

;;;###autoload
(defun create-emacs-anywhere-buffer ()
  "create temporary markdown buffer for easy access"
  (interactive)
  (create-file-buffer "emacs-anywhere.md")
  (with-current-buffer "emacs-anywhere.md"
    (markdown-mode)))

(defun switch-to-emacs-anywhere-buffer ()
  (interactive)
  (switch-to-buffer "emacs-anywhere.md"))


(provide 'aza-scripts)
