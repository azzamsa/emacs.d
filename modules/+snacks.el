;;  -*- lexical-binding: t; -*-
;; 🍿 A collection of small QoL scripts

;;;###autoload
(defun today ()
  "Inserts the current date."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

;;;###autoload
(defun now ()
  "Inserts the current date and time."
  (interactive)
  (insert (format-time-string "%F %H:%M")))

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

;;;###autoload
(defun kill-all-buffers ()
  "Kill all buffers, including the current one."
  (interactive)
  (when (y-or-n-p "Kill all buffers? ")
    (save-all-buffers-silently)
    (dolist (buf (buffer-list))
      (unless (string= (buffer-name buf) "*dashboard*")
        (kill-buffer buf)))))

;;;###autoload
(defun file-manager-here ()
  "Open current directory with default file manager."
  (interactive)
  (message "Opening file manager in current directory...")
  ;; `xdg-open' will pick the default file manager
  (start-process "" nil "xdg-open" "."))

;;;###autoload
(defun terminal-here ()
  "Open a new terminal with the current directory as PWD."
  (interactive)
  (message "Opening terminal in %s" default-directory)
  ;; Need to use `expand-file-name` to expand `~` into a full path
  ;; Otherwise, termhere fallback to `$HOME`
  ;; The Rust version of `termhere' only works with `call-process-shell-command',
  ;; `async-shell-command', and `shell-command'. But the (b)ash version works
  ;; out of the box. Including with `start-process'.
  ;; See https://github.com/azzamsa/dotfiles/blob/master/xtool/src/termhere.rs
  (call-process-shell-command (concat "termhere " (expand-file-name default-directory))))

(defun save-all-buffers-silently ()
  (save-some-buffers t))

(defun save-buffers-and-clean ()
  "Save buffers and delete trailing whitespaces"
  (interactive)
  (basic-save-buffer)
  (delete-trailing-whitespace))

;;;###autoload
(defun +scratch-buffer ()
  "Toggle persistent scratch buffer"
  (interactive)
  (let ((filename camp-scratch-file))
    (if-let ((buffer (find-buffer-visiting filename)))
        (if (eq (selected-window) (get-buffer-window buffer))
            (delete-window)
          (if (get-buffer-window buffer)
              (select-window (get-buffer-window buffer))
            (pop-to-buffer buffer)))
      (progn
        (split-window-vertically)
        (other-window 1)
        (find-file filename)))))

(defun +find-file-other-window-vertically (f)
  "Edit a file in another window, split vertically."
  (interactive)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    (find-file-other-window f)))

;;
;; Project

;;;###autoload
(defun camp/browse-in-emacsd ()
  "Browse files from `user-emacs-directory'."
  (interactive) (camp-project-browse user-emacs-directory))

;;;###autoload
(defun camp/find-file-in-emacsd ()
  "Find a file under `user-emacs-directory', recursively."
  (interactive) (camp-project-find-file user-emacs-directory))

(defun camp-project-find-file (dir)
  "Jump to a file in DIR (searched recursively). "
  (let* ((default-directory (file-truename dir))
         (pr (+project-from-dir dir))
         (root (project-root pr))
         (dirs (list root)))
    (if pr
        (project-find-file-in nil dirs pr nil)
      (call-interactively #'find-file))))

(defun +project-from-dir (&optional dir)
  "Helper method to return project instance if DIR is a valid project."
  (project--find-in-directory dir))

(defun camp-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'find-file)))

(defun +vertico/project-search-from-cwd ()
  "Search files from the current working directory using Vertico and Consult."
  (interactive)
  (let ((dir (file-truename default-directory)))
    (consult-ripgrep dir)))

;;;###autoload
(defun +doom/yank-buffer-path (&optional root)
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

;;;###autoload
(defun +doom/yank-buffer-path-relative-to-project (&optional include-root)
  "Copy the current buffer's path to the kill ring.
With non-nil prefix INCLUDE-ROOT, also include the project's root."
  (interactive "P")
  (+doom/yank-buffer-path
   (if include-root
       (file-name-directory (directory-file-name (doom-project-root)))
     (project-root (project-current)))))
