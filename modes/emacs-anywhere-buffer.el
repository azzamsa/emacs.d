;;;###autoload
(defun create-emacs-anywhere-buffer ()
  "create temporary markdown buffer for easy access"
  (interactive)
  (create-file-buffer "emacs-anywhere.md")
  (with-current-buffer "emacs-anywhere.md"
     (markdown-mode)))

(provide 'emacs-anywhere-buffer)
