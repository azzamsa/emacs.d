;;; simple-crux
;;

;;; Commentary:
;; modified from https://github.com/bbatsov/crux
;; thanks Bozhidar Batsov!


;;; Code:

(require 'thingatpt)
(require 'seq)
(require 'tramp)


(defvar crux-line-start-regex-term-mode "^[^#$%>\n]*[#$%>] "
  "Match terminal prompts.
Used by crux functions like crux-move-beginning-of-line to skip over the prompt")

(defvar crux-line-start-regex-eshell-mode "^[^$\n]*$ " "Match eshell prompt.
Used by crux functions like crux-move-beginning-of-line to skip over the prompt")

(defvar crux-line-start-regex "^[[:space:]]*" "Match whitespace in from of line.
Used by crux functions like crux-move-beginning-of-line to skip over whitespace")


;;;###autoload
(defun crux-transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

(defalias 'crux-swap-windows 'crux-transpose-windows)

;;;###autoload
(defun crux-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (seq-each
     #'kill-buffer
     (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

;;;###autoload
(defun crux-reopen-as-root ()
  "Find file as root if necessary.
Meant to be used as `find-file-hook'.
See also `crux-reopen-as-root-mode'."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (equal major-mode 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (crux-file-owned-by-user-p buffer-file-name))
    (crux-find-alternate-file-as-root buffer-file-name)))

(defun crux-file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (crux-file-owner-uid filename)
         (user-uid)))

(defun crux-file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.
See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))


(defun crux-find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening FILENAME as root."
  (find-alternate-file (concat "/sudo:root@localhost:" filename)))

;;;###autoload
(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defalias 'crux-delete-buffer-and-file #'crux-delete-file-and-buffer)

;;;###autoload
(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'crux-rename-buffer-and-file #'crux-rename-file-and-buffer)

;;;###autoload
(defun crux-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-to-mode-line-start)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun move-to-mode-line-start ()
  "Move to the beginning, skipping mode specific line start regex."
  (interactive)
  (move-beginning-of-line nil)
  (let ((line-start-regex (cond ((eq major-mode 'term-mode) crux-line-start-regex-term-mode)
                                ((eq major-mode 'eshell-mode) crux-line-start-regex-eshell-mode)
                                (t crux-line-start-regex))))
    (search-forward-regexp line-start-regex (line-end-position) t)))

(provide 'simple-crux)
;;; simple-crux.el ends here
