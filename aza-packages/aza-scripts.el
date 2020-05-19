(require 'cl-lib)
(require 's)
(require 'f)
(require 'ts)
(require 'request)
(require 'keys (expand-file-name "keys/keys.el.gpg" aza-epkgs-dir))

(when (file-exists-p (expand-file-name "aza-secrets.el" aza-pkgs-dir))
  (require 'aza-secrets))

(defun save-all-buffers-silently ()
  (save-some-buffers t))

;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (when (y-or-n-p "Save and kill all other buffers ? ")
    (save-all-buffers-silently)
    (let ((killed-bufs 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (let ((name (buffer-name buffer)))
          (when (and name (not (string-equal name ""))
                     (/= (aref name 0) ?\s)
                     (string-match "^[^\*]" name))
            (cl-incf killed-bufs)
            (funcall 'kill-buffer buffer))))
      (message "Saved & killed %d buffer(s)" killed-bufs))))

(defun aza-today (&optional arg)
  "Insert today's date.

A prefix ARG specifies how many days to move;
negative means previous day.

If region selected, parse region as today's date pivot."
  (interactive "P")
  (let ((date (if (use-region-p)
                  (ts-parse (buffer-substring-no-properties (region-beginning) (region-end)))
                (ts-now)))
        (arg (or arg 0)))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end)))
    (insert (ts-format "%A, %B %e, %Y" (ts-adjust 'day arg date)))))

(defun insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (s-join " " (s-split-words (file-name-sans-extension (buffer-name)))))))

(defun now ()
  (interactive)
  (insert (format-time-string "%F %H:%M")))

(defun hour ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun rm-mysecrets ()
  "Remove all confidential information."
  (interactive)
  (dolist (pair (list-my-secrets))
    (save-excursion
      (replace-string (car pair) (cdr pair)))))

(defun light-set-value ()
  "Set light value directly inside Emacs"
  (interactive)
  (let* ((current-value (s-trim (shell-command-to-string "light -G")))
         (light-value (read-string "Set Value: " current-value)))
    (start-process "" nil "light" "-S" light-value)))

(defun pyqt-compile-ui ()
  "Compile Qt5 user interfaces to Python code directly from
Emacs"
  (interactive)
  (let ((inputfile (dired-get-filename))
        (outputfile
         (file-name-sans-extension
          (file-name-nondirectory (dired-get-filename)))))
    (start-process "" nil "pyuic5" inputfile
                   (concat "--output=" default-directory outputfile "_ui.py"))))

(defun mpv-here ()
  "Play all multimedia files in current directory"
  (interactive)
  (start-process "" nil "mpv" "--force-window" "--loop" default-directory))

(defun trash-empty ()
  "Empty trash using trash-cli. It's safer and painless"
  (interactive)
  (start-process "" nil "rm" "-rf" trash-dir1 trash-dir2 trash-dir3))

(defun anacron-show-last-backup ()
  "Show last backup of my anacron status."
  (interactive)
  (let ((contents (f-read "~/cron-report/anacron-status.txt" 'utf-8)))
    (message (car (last (delete "" (split-string contents "\n")))))))

(defun what-day ()
  "Show day name from spesific time"
  (interactive)
  (let ((date (read-string "Date: "))
        (month (read-string "Month: "))
        (year (read-string "Year: " (number-to-string (ts-year (ts-now))))))
    (message (ts-day-name (ts-parse (s-join " " (list date month year)))))))

(defun ask-github ()
  "GET Github notification API."
  (let* ((archive-response (request "https://api.github.com/notifications?all"
                             :parser 'json-read
                             :headers `(("Authorization" . ,(concat "token" " " github-pass))
                                        ("Content-Type" . "application/json"))
                             :sync t))
         (data (request-response-data archive-response))
         (status (request-response-status-code archive-response)))
    (if (eq status 200)
        data
      404)))

(defun github-show-notification ()
  "Check if Github notification exist without opening browser
Reduce Distraction."
  (interactive)
  (let ((result (ask-github)))
    (if (not (equal result 404))
        (if (equal result '[])
            (message "No notification.")
          (message "Yey, You have notification!"))
      (message "Request failed"))))

(defun ask-archive (url)
  "Get request to Wayback API"
  (let* ((archive-response (request (concat "http://archive.org/wayback/available?url=" url)
                             :parser 'json-read
                             :sync t))
         (data (request-response-data archive-response))
         (status (request-response-status-code archive-response)))
    (if (eq status 200)
        data
      404)))

(defun wayback-save-page ()
  "Archive page to Wayback"
  (interactive)
  (let* ((url (read-string "Url: "))
         (response (nth 1 (nth 1 (ask-archive url)))))
    (if response
        (message "Webpage had an archive")
      (message "Webpage archived"))))

(defun play-reminder-sound ()
  (start-process "" nil "mpv" coin-work-medium-sound))

(defun remind-me ()
  "Notify with a sound after certain time"
  (interactive)
  (let ((time (read-string "Time (min|sec): " "10 min")))
    (message "I will remind you after %s" time)
    (run-at-time time nil #'play-reminder-sound)))

(defun to-snake-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))


(provide 'aza-scripts)
