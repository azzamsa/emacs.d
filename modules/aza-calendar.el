(use-package calfw
  :defer t
  :init
  (use-package calfw-cal
    :ensure t)
  (use-package calfw-org
    :ensure t)
  :bind (("C-c A" . my-calendar)
         :map cfw:calendar-mode-map
         ("M-n" . cfw:navi-next-month-command)
         ("M-p" . cfw:navi-previous-month-command)
         ("j"   . cfw:navi-goto-date-command)
         ("g"   . cfw:refresh-calendar-buffer))

  :commands cfw:open-calendar-buffer
  :functions (cfw:open-calendar-buffer
              cfw:refresh-calendar-buffer
              cfw:org-create-source
              cfw:cal-create-source)

  :config
  (defun my-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "#d6c9a7")  ; orgmode source
      (cfw:cal-create-source "White"))))

  (setq diary-file "~/.emacs.d/documents/diary")
  (setq cfw:display-calendar-holidays nil)
  (setq holiday-christian-holidays nil
        holiday-bahai-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-oriental-holidays nil))

(provide 'aza-calendar)
