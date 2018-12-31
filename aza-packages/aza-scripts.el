(require 'cl-lib)
(require 's)
(when (file-exists-p (expand-file-name "aza-secrets.el" aza-packages-dir))
  (require 'aza-secrets))

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

(defun insert-filename-as-heading ()
  (interactive)
  (insert
   (capitalize
    (replace-regexp-in-string "-" " " (file-name-sans-extension (buffer-name))))))

(defun now ()
  (interactive)
  (insert (format-time-string "%F %H:%M")))

(defun hour ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun rm-mysecrets ()
  "Remove all confidential information.
Thanks Trey Jackson for dolist and save-excursion"
  (interactive)
  (dolist (pair (list-my-secrets))
    (save-excursion
      (replace-string (car pair) (cdr pair)))))

(defun set-light ()
  "Set light value directly inside Emacs"
  (interactive)
  (let ((light-value (read-string "Set Value: ")))
    (start-process "" nil "light" "-S" light-value)))

(defun compile-ui-to-py ()
  "Compile Qt5 user interfaces to Python code directly from
Emacs"
  (interactive)
  (let ((inputfile (dired-get-filename))
        (outputfile
         (file-name-sans-extension
          (file-name-nondirectory (dired-get-filename)))))
    (start-process "" nil "pyuic5" inputfile
                   (concat "--output=" default-directory outputfile ".py"))))

(provide 'aza-scripts)
