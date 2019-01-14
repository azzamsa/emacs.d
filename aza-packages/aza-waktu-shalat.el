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
        (alert (format "Waktu shalat %s jam %s" (car waktu) (cdr waktu))
               :title "Waktu Shalat")))))

(provide 'aza-waktu-shalat)
