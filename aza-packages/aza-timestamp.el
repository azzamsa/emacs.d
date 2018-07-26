;;;; aza-timestamp
;;; http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
;;; by Michael Paulukonis

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%F %H:%M")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,e.g. Sunday, September 17, 2000."
  (interactive) ;; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

(defun hour ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(provide 'aza-timestamp)
