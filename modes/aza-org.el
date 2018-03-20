;;;; aza org

;;; code:

(defun my-org-mode-hook ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(provide 'aza-org)
