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

(defvar ws-writing-state nil)

(defun turn-on-ws-writing ()
  (interactive)

  (whitespace-mode +1)
  (visual-line-mode +1)
  (visual-fill-column-mode +1)

   (auto-fill-mode -1)
  (display-line-numbers-mode +1)

  (setq ws-writing-state t)
  (message "Turn on ws-writing"))


(defun turn-off-ws-writing ()
  "Provide writing environment that prefer soft-breaks"
  (interactive)
  (kill-local-variable 'whitespace-style)
  (kill-local-variable 'whitespace-display-mappings)

  (whitespace-mode -1)
  (visual-line-mode -1)
  (visual-fill-column-mode -1)
  (auto-fill-mode +1)
  (display-line-numbers-mode -1)

  (setq ws-writing-state nil)
  (message "Turn off ws-writing"))

(defun ws-writing-toggle ()
  "Provide writing environment that prefer soft-breaks"
  (interactive)
  (make-local-variable 'whitespace-style)
  (make-local-variable 'whitespace-display-mappings)

  (if (not ws-writing-state)
      (turn-on-ws-writing)
    (turn-off-ws-writing)))


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

(provide 'aza-emacs-enhc)
