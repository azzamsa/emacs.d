;;------------------------------------------------
;; Emacs is a home
;;------------------------------------------------

(use-package pomodoro
  :defer t
  :commands pomodoro-start
  :config
  (setq pomodoro-show-number t)
  (setq pomodoro-long-break-time 20)
  (setq pomodoro-work-time 25)
  (setq pomodoro-break-time 5)
  (setq pomodoro-sound-player "/usr/bin/ogg123")
  (setq pomodoro-break-start-sound
        "~/sounds/ding-rest-medium.ogg")
  (setq pomodoro-work-start-sound
        "~/sounds/coin-work-medium.ogg")
  (pomodoro-add-to-mode-line))

(use-package pdf-tools
  :defer t
  :disabled
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package org-brain
  :defer t
  :init
  (setq org-brain-path "~/.emacs.d/documents/brain")
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 30))

(use-package presentation
  :defer t)

(use-package keyfreq
  :defer 0.9
  :config
  (setq keyfreq-file (expand-file-name "emacs.keyfreq" aza-savefile-dir))
  (setq keyfreq-file-lock (expand-file-name "emacs.keyfreq.lock" aza-savefile-dir))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package explain-pause-mode
  :defer t
  :straight (explain-pause-mode :type git
                           :files ("explain-pause-mode.el")
                           :host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode t))

(use-package pelican-mode
  :straight (pelican-mode :type git :flavor melpa
                          :repo "https://git.korewanetadesu.com/pelican-mode.git"
                          :fork (:host github :repo "azzamsa/pelican-mode" :branch "myhack"))
  :commands pelican-mode
  :load-path "~/emacs-packages/pelican-mode/"
  :config
  (pelican-global-mode))

(use-package atomic-chrome
  :defer t
  :config
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)))
  (atomic-chrome-start-server))

(use-package emojify
  :hook
  ((markdown-mode . emojify-mode)
   (git-commit-mode . emojify-mode)
   (magit-status-mode . emojify-mode)
   (magit-log-mode . emojify-mode)
   (cfw:calendar-mode . emojify-mode)
   (org-mode . emojify-mode))
  :config
  (setq emojify-emoji-styles '(github unicode))
  (setq emojify-emojis-dir
        (expand-file-name "emojify/"  aza-savefile-dir)))

(use-package define-word
  :defer t)

(use-package google-translate
  :defer t
  :commands google-translate-smooth-translate
  :init
  (require 'google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist
        '(("en" . "id") ("id" . "en")))
  (setq google-translate-output-destination nil)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "id"))

(use-package password-store
  :straight (password-store :type git :flavor melpa
                            :files ("contrib/emacs/*.el" "password-store-pkg.el")
                            :host github :repo "zx2c4/password-store"))

(provide 'aza-home)
