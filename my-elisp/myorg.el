;;;; My org

;;; comentary
;;; http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
;;; by Michael Paulukonis

;;; code:

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  ;;(insert (format-time-string "%D %-I:%M %p")))
  ;; i cange the format to 04.11.2016 8:40 Am, before the month was in front.
  (insert (format-time-string "%d.%m.%Y %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

(defun hour ()
  (interactive)                 
  (insert (format-time-string "%-I:%M %p")))

;;; my.org ends here 
