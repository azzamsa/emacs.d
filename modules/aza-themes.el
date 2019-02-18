(use-package zenburn-theme)

(use-package github-theme :defer t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(zenburn))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun theme-toggle-light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(github))
  (reapply-themes))

(defun theme-toggle-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(zenburn))
  (reapply-themes))


(provide 'aza-themes)
