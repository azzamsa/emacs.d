(use-package org-superstar
  :after org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (org-superstar-mode +1))))

(use-package org-cliplink
  :after org
  :bind (:map org-mode-map
              ("C-l" . org-cliplink))
  :config
  (setq org-cliplink-max-length 60)
  (setq org-cliplink-transport-implementation 'curl))

(use-package svg-tag-mode
  :after org
  :config

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  ;; taken from https://github.com/rougier/svg-tag-mode/blob/fee61c6a0b0570bd24fd335efef17c7385297aa0/examples/example-2.el
  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; Org TODO keywords
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-level-2 :inverse t :margin 0))))
          ("DOING" . ((lambda (tag) (svg-tag-make "DOING" :face 'org-level-7 :inverse t :margin 0))))
          ("WAITING" . ((lambda (tag) (svg-tag-make "WAITING" :face 'org-level-7 :inverse t :margin 0))))
          ("CANCELLED" . ((lambda (tag) (svg-tag-make "CANCELLED" :face 'org-level-2 :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))

          ;; Active date (without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s *\\)%s>" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s *\\(%s>\\)" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

  (add-hook 'org-mode-hook
            (lambda ()
              (svg-tag-mode +1))))

(provide 'writing)
