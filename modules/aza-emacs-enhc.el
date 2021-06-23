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

(provide 'aza-emacs-enhc)
