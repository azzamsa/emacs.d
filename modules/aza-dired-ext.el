(defun ora-dired-rsync (dest)
  (interactive
   (list (expand-file-name
          (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command "rsync -arvz --progress "))
    ;; add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (if (string-match "^/ssh:\\(.*:\\)\\(.*\\)$" file)
                        (format " -e ssh \"%s%s\""
                                (match-string 1 file)
                                (shell-quote-argument (match-string 2 file)))
                      (shell-quote-argument file)) " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (if (string-match "^/ssh:\\(.*\\)$" dest)
                      (format " -e ssh %s" (match-string 1 dest))
                    (shell-quote-argument dest))))
    ;; run the async shell command
    (let ((default-directory (expand-file-name "~")))
      (async-shell-command tmtxt/rsync-command))
    (message tmtxt/rsync-command)
    ;; finally, switch to that window
    (other-window 1)))

(defun ora-dired-up-directory ()
  (interactive)
  (let ((buffer (current-buffer)))
    (dired-up-directory)
    (unless (equal buffer (current-buffer))
      (kill-buffer buffer))))

(defun dired-view-next ()
  "Move down one line and view the current file in another window."
  (interactive)
  (dired-next-line 1)
  (dired-view-current))

(defun dired-view-previous ()
  "Move up one line and view the current file in another window."
  (interactive)
  (dired-previous-line 1)
  (dired-view-current))

(defun dired-view-current ()
  "View the current file in another window (possibly newly created)."
  (interactive)
  (if (not (window-parent))
      (split-window))
  (let ((file (dired-get-file-for-visit))
        (dbuffer (current-buffer)))
    (other-window 1)
    (unless (equal dbuffer (current-buffer))
      (if (or view-mode (equal major-mode 'dired-mode))
          (kill-buffer)))
    (let ((filebuffer (get-file-buffer file)))
      (if filebuffer
          (switch-to-buffer filebuffer)
        (view-file file))
      (other-window -1))))

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal $sort-by "name") (setq $arg "-Al --si --time-style long-iso "))
     ((equal $sort-by "date") (setq $arg "-Al --si --time-style long-iso -t"))
     ((equal $sort-by "size") (setq $arg "-Al --si --time-style long-iso -S"))
     ((equal $sort-by "dir") (setq $arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))

(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "evince" "okular")
        ("\\.\\(?:djvu\\|eps\\)\\'" "evince")
        ("\\.\\(?:jpg\\|jpeg\\|png\\|svg\\|tiff\\|xpm\\|bmp\\)\\'" "sxiv")
        ("\\.\\(?:xcf\\)\\'" "gimp")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\|ifo\\|m4v\\|wmv\\|m4a\\|webm\\)\\(?:\\.part\\)?\\'"
         "mpv --force-window --loop")
        ("\\.\\(?:mp3\\|flac\\|wv\\)\\'" "mpv --force-window --loop")
        ("\\.html?\\'" "firefox-dev")
        ("\\.mm?\\'" "freeplane")
        ("\\.\\(?:pptx?\\|odt\\|xlsx?\\|docx?\\)\\'" "libreoffice")
        ("\\.ui?\\'" "qtchooser -run-tool=designer -qt=5")
        ("\\.gif?\\'" "sxiv -a")
        ("\\.\\(?:zip\\|tgz\\)\\'" "file-roller")))

(defun dired-open-directory-in-thunar ()
  (interactive)
  (start-process "" nil "thunar" "."))

(defun term-here ()
  (interactive)
  (start-process "" nil "term-here"))

(defun ora-shell-command-sentinel (process signal)
  (when (memq (process-status process) '(exit signal))
    (advice-remove 'shell-command-sentinel 'ora-shell-command-sentinel)
    (message (with-current-buffer (process-buffer process)
               (string-trim (buffer-string))))))

(defun ora-dired-do-async-shell-command ()
  "Wrap `dired-do-async-shell-command' without popup windows."
  (interactive)
  (advice-add 'shell-command-sentinel :override #'ora-shell-command-sentinel)
  (save-window-excursion
    (call-interactively 'dired-do-async-shell-command)))

(defun ora-dired-other-window ()
  (interactive)
  (if (string= (buffer-name) "*Find*")
      (find-file-other-window
       (file-name-directory (dired-get-file-for-visit)))
    (save-selected-window
      (dired-find-file-other-window))))

(defun ora-dired-get-size ()
  (interactive)
  (let* ((cmd (concat "du -sch "
                      (mapconcat (lambda (x) (shell-quote-argument (file-name-nondirectory x)))
                                 (dired-get-marked-files) " ")))
         (res (shell-command-to-string cmd)))
    (if (string-match "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$" res)
        (message (match-string 1 res))
      (error "unexpected output %s" res))))

(provide 'aza-dired-ext)
