(use-package request
  :defer t
  :config
  (setq request-storage-directory
        (expand-file-name "request/" azzamsa-savefile-dir)))

(use-package spinner :defer t)

(use-package async
  :defer 3
  :config
  (async-bytecomp-package-mode t)

  (defun my/dired-async-message-function (text _face &rest args)
    "Log messages from dired-async to messages buffer."
    ;; For whatever reason, the default for this *doesn't* log it to
    ;; *Messages*.  Instead, it just displays the notification in the
    ;; mode line for 3 seconds, but if you type something it
    ;; immediately goes away.  So just log it to *Messages* like a sane
    ;; person instead:
    (message (format "Finished %s" (apply #'format text args))))
  ;; do dired actions asynchronously
  (dired-async-mode)
  :custom
  (dired-async-message-function #'my/dired-async-message-function))

(use-package ts
  :defer 3
  :load-path "~/emacs-packages/ts.el")

(provide 'aza-lib)
