(use-package calfw
  :after aza-secrets
  :straight (calfw :type git :flavor melpa :files ("calfw.el" "calfw-pkg.el")
                   :host github :repo "kiwanami/emacs-calfw"
                   :fork (:host github :repo "azzamsa/emacs-calfw"  :branch "myhack"))
  :defer t
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
      (cfw:org-create-source "#e0a899")  ; orgmode source
      (cfw:cal-create-source "#ffffba")
      (cfw:org-create-file-source "Success" my-success-cal "#baffc9")
      (cfw:org-create-file-source "Success Atom" my-success-atom-cal "#E3FFE9")
      (cfw:org-create-file-source "Moment" my-moment-cal "#bae1ff")
      (cfw:org-create-file-source "Target" my-target-cal "#40e0d0")
      (cfw:org-create-file-source "Hevent" my-hevent-cal "#ffb3ba"))))

  (setq diary-file my-diary-cal)
  (setq cfw:display-calendar-holidays nil)
  (setq holiday-christian-holidays nil
        holiday-bahai-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-oriental-holidays nil)
  :custom-face
  (cfw:face-annotation ((t :foreground "#ffffff" :inherit cfw:face-day-title)))
  (cfw:face-day-title ((t :background "grey10")))
  (cfw:face-default-content ((t :foreground "#ffffff")))
  (cfw:face-default-day ((t :foreground "#b4eeb4" :weight bold :inherit cfw:face-day-title)))
  (cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
  (cfw:face-grid ((t :foreground "#BADEAC")))
  (cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
  (cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
  (cfw:face-periods ((t :foreground "#ffe259")))
  (cfw:face-saturday ((t :foreground "8cd0d3" :weight bold)))
  (cfw:face-select ((t :background "#2f2f2f")))
  (cfw:face-sunday ((t :foreground "#cc9393" :weight bold)))
  (cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
  (cfw:face-today ((t :background: "grey10" :weight bold)))
  (cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
  (cfw:face-toolbar ((t :foreground "Steelblue4" :background "#3F3F3F")))
  (cfw:face-toolbar-button-off ((t :foreground "#f5f5f5" :weight bold)))
  (cfw:face-toolbar-button-on ((t :foreground "#ffffff" :weight bold))))

(use-package calfw-cal
  :after calfw)

(use-package calfw-org
  :after calfw)

(use-package calfw-ical
  :after calfw)

(provide 'aza-calfw)
