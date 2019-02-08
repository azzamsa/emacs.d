;; Now I use org-cliplink

;;;###autoload
(defun make-md-to-org ()
  "transform md link format to org"
  (interactive)
  (let ((mdlink
         (buffer-substring-no-properties (region-beginning) (region-end))))
    (let ((orglink
           (s-concat "[["
                     (s-chop-suffix ")" (s-chop-prefix "](" (second (s-slice-at "](" mdlink))))
                     "]["
                     (s-chop-prefix "[" (first (s-slice-at "](" mdlink)))
                     "]]")))
      (delete-region (region-beginning) (region-end))
      (insert orglink))))
