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

(provide 'aza-emacs-enhc)
