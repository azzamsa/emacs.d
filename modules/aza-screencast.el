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

(provide 'aza-fun)
