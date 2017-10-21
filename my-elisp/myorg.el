;;; My org
;;; ingin nambah untuk kjots, coba ya 11/01/16 7:20 PM
;;;ilmu

(defun ilmu-start ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "{--^^--")))

(defun ilmu-end ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "--^^--}")))

;;; batas
(defun batas ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "------****------")))


;;; 3.  Ingatan tiba-tiba / Sletz
(defun sletz ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string ">~~Sletz~~ = ")))

;;;4. Planz_langsung

(defun planz ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string ">>>Planz = ")))


;;; 5. Planz_Pending

  (defun planzpn ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string ">><Planz_Pending =  ")))


;;;6. Pemikiran

  (defun pemikiran ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string ">**Pemikiran**= ")))


;;;7. Qoutes

  (defun quotes ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string ">“Quotes”= ")))

  
  ;;; cara lain http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
;;; by Michael Paulukonis
;;; 11/01/16 7:19 PM
(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  ;(insert (format-time-string "%D %-I:%M %p")))
  ; i cange the format to 04.11.2016 8:40 Am, before the month was in front.
  (insert (format-time-string "%d.%m.%Y %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))
  
(defun hour ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  ;(insert (format-time-string "%D %-I:%M %p")))
  ; i cange the format to 04.11.2016 8:40 Am, before the month was in front.
  (insert (format-time-string " %-I:%M %p")))

 
  
  
  

;;; --- kjots conf; end here ;
