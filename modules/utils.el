;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +today ()
  "Inserts the current date."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

;;;###autoload
(defun +now ()
  "Inserts the current date and time."
  (interactive)
  (insert (format-time-string "%F %H:%M")))

;;;###autoload
(defun +aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (when (y-or-n-p "Save and kill all other buffers ? ")
    (+save-all-buffers-silently)
    (let ((killed-bufs 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (let ((name (buffer-name buffer)))
          (when (and name (not (string-equal name ""))
                     (/= (aref name 0) ?\s)
                     (string-match "^[^\*]" name))
            (cl-incf killed-bufs)
            (funcall 'kill-buffer buffer))))
      (message "Saved & killed %d buffer(s)" killed-bufs))))

;;;###autoload
(defun +kill-all-buffers ()
  "Kill all buffers, including the current one."
  (interactive)
  (when (y-or-n-p "Kill all buffers? ")
    (+save-all-buffers-silently)
    (dolist (buf (buffer-list))
      (unless (string= (buffer-name buf) "*dashboard*")
        (kill-buffer buf)))))

(defun +save-all-buffers-silently ()
  (save-some-buffers t))

(defun +save-buffers-and-clean ()
  "Save buffers and delete trailing whitespaces"
  (interactive)
  (basic-save-buffer)
  (delete-trailing-whitespace))

;;;###autoload
(defun +elpaca-write-lockfile ()
  (interactive)
  (elpaca-write-lock-file (concat user-emacs-directory "etc/package-lock")))

;;;###autoload
(defun +emacs-version ()
  (interactive)
  (with-temp-file (expand-file-name "emacs-version" user-emacs-directory)
    ;; remove `\n `
    (insert (s-replace "\n " " " (emacs-version)))))

;; Credits: Doom
;;;###autoload
(defun +yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or (buffer-file-name (buffer-base-buffer))
                          (bound-and-true-p list-buffers-directory))))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

;; Credits: Doom
;;;###autoload
(defun +yank-buffer-path-relative-to-project (&optional include-root)
  "Copy the current buffer's path to the kill ring.
With non-nil prefix INCLUDE-ROOT, also include the project's root."
  (interactive "P")
  (+yank-buffer-path
   (if include-root
       (file-name-directory (directory-file-name (project-root (project-current))))
     (project-root (project-current)))))

;;;###autoload
(defun +neotree-project-dir ()
  "Always open NeoTree in project root."
  (interactive)
  (let ((project-dir (ignore-errors (project-root (project-current))))
	      (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	      (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (if file-name
		              (neotree-find file-name))))
      (message "Could not find project root."))))

;; Credits: Abo-abo
;;;###autoload
(defun +dired-up-directory ()
  (interactive)
  (let ((buffer (current-buffer)))
    (dired-up-directory)
    (unless (equal buffer (current-buffer))
      (kill-buffer buffer))))

;; Credits: Xah Lee http://ergoemacs.org/emacs/dired_sort.html
;;;###autoload
(defun +dired-sort ()
  "Sort dired listing in different ways."
  (interactive)
  (let (sort-by arg)
    (setq sort-by (completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal sort-by "name") (setq arg "-Al"))
     ((equal sort-by "date") (setq arg "-Al -t"))
     ((equal sort-by "size") (setq arg "-Al -S"))
     ((equal sort-by "dir") (setq arg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other arg)))

;;;###autoload
(defun +dired-copy-dirpath-as-kill ()
  "Copy the current directory path into the kill ring."
  (interactive)
  (kill-new default-directory)
  (message "Copied: %s" default-directory))

;;;###autoload
(defun +eat-toggle ()
  (interactive)
  (if (string= (buffer-name) "*eat*")
      (delete-window)
    (eat-other-window)))
