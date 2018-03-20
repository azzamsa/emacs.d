;;------------
;; dired conf from oramac
;;------------

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

(provide 'my-dired)
