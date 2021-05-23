(defun crontab-edit ()
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun prelude-ddg ()
  "Googles a query or region if any. Thanks Prelude."
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?t=lm&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Search DuckDuckGo: ")))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
of text. By Stefan Monnier"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; default minor mode format
(defvar aza-modeline-toggle-state nil)
(setq doom-modeline-minor-modes nil)
(setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("")))

(defun aza-modeline-toggle ()
  (interactive)
  (if (not aza-modeline-toggle-state)
      (progn
        (setq doom-modeline-minor-modes t)
        (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                           ("[" pyvenv-virtual-env-name "] ")))
        (setq aza-modeline-toggle-state t))
    (progn
      (setq doom-modeline-minor-modes nil)
      (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                         ("")))
      (setq aza-modeline-toggle-state nil))))

;; Enable the code before doing screencast
;; it will hide `save-buffer' "Wrote ..." message that contains buffer's path
;; and show buffer's filename instead

;; @https://emacs.stackexchange.com/a/62516/11777
(defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN, forward ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

(advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'save-buffer :around #'sb/inhibit-message-call-orig-fun)

(defun hide-save-buffer-message (&optional arg)
  "Adds feedback after `save-buffer' without showing the buffers' path."
  (message "Wrote %s" (buffer-name)))

(advice-add 'save-buffer :after #'hide-save-buffer-message)

(provide 'aza-emacs-enhc)
