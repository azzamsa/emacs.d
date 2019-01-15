(require 'request)
(require 'alert)

(defun siswadi-request ()
  "Get waktu shalat from siswadi. Return 400 if failed"
  (let* ((siswadi-response (request "https://time.siswadi.com/pray/Malang"
                                    :parser 'json-read
                                    :sync t))
         (data (request-response-data siswadi-response))
         (status (request-response-status-code siswadi-response)))
    (if (eq status 200)
        data
      404)))

(defvar shalat-alias-data '(("s" . "Fajr")
                            ("d" . "Dhuhr")
                            ("a" . "Asr")
                            ("m" . "Maghrib")
                            ("i" . "Isha"))
  "Alias list for tipe shalat. User can input single letter
  instead of full tipe shalat name")

(defun waktu-tunggu (goal-time)
  "Return Hour and Minutes of remaining time against shalat time"
  (let* ((curr-time (split-string (format-time-string "%H %M" (current-time))))
         (curr-hour-in-sec (* (string-to-number (nth 0 curr-time)) 3600))
         (curr-minute-in-sec (* (string-to-number (nth 1 curr-time)) 60))
         (curr-time-in-sec (+ curr-hour-in-sec curr-minute-in-sec))
         (goal-hour-in-sec (* (nth 2 (parse-time-string goal-time)) 3600))
         (goal-minute-in-sec (* (nth 1 (parse-time-string goal-time)) 60))
         (goal-in-sec (+ goal-hour-in-sec goal-minute-in-sec))
         (remaining-sec (- goal-in-sec curr-time-in-sec)))
    (format-seconds "%H %M" remaining-sec)))

;;;###autoload
(defun waktu-shalat ()
  "Show waktu shalat via alert. Accept user input defined in
'shalat-alias-data'"
  (interactive)
  (let* ((shalat-alias (read-string "Waktu shalat: "))
         (tipe-shalat (cdr (assoc shalat-alias shalat-alias-data)))
         (siswadi-response (siswadi-request)))
    (if (eq siswadi-response 404)
        (message "Waktu shalat request failed")
      (let ((waktu (assoc (intern tipe-shalat) (nth 0 siswadi-response))))
        (alert (format "\nWaktu shalat %s jam %s\nWaktu tunggu %s" (car waktu)
                       (cdr waktu) (waktu-tunggu (cdr waktu)))
               :title "Waktu Shalat")))))

(provide 'aza-waktu-shalat)
