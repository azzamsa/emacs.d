;;;###autoload
(defun create-emacs-anywhere-buffer ()
  "create temporary markdown buffer for easy access"
  (interactive)
  (create-file-buffer "emacs-anywhere.md")
  (with-current-buffer "emacs-anywhere.md"
    (markdown-mode)))

(defun switch-to-emacs-anywhere-buffer ()
  (interactive)
  (switch-to-buffer "emacs-anywhere.md"))

(provide 'emacs-anywhere-buffer)
