(use-package vterm
  :straight (vterm :type git :flavor melpa
                   :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
                   :host github :repo "akermu/emacs-libvterm")
  :bind (:map vterm-mode-map
              ("<f2>" . hydra-vterm/body)
              ([(control return)]  . vterm-toggle-insert-cd))
  :config
  (setq cursor-type 'bar)

  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *")
  (setq vterm-history '())
  (setq vterm-history-file
        (expand-file-name "vterm-history" aza-savefile-dir))

  (add-hook 'kill-emacs-hook
            (lambda ()
              (write-region (string-join vterm-history "\n") nil vterm-history-file)))

  (if (not vterm-history)
      (setq vterm-history (s-split "\n" (f-read vterm-history-file))))

  (defun vterm-send-return ()
    "Send `C-m' to the libvterm."
    (interactive)
    (when vterm--term
      (let* ((beg (vterm--get-prompt-point))
             (end (vterm--get-end-of-line))
             (history (buffer-substring-no-properties beg end)))
        (if (not (string= history ""))
            (add-to-list 'vterm-history history)))

      ;; default part from vterm.el
      (if (vterm--get-icrnl vterm--term)
          (process-send-string vterm--process "\C-j")
        (process-send-string vterm--process "\C-m"))))

  (defun helm-vterm-history ()
    (interactive)
    (let ((histories vterm-history))
      (helm :sources `((name . "vterm history")
                       (candidates . histories)
                       (action . (lambda (candidate)
                                   (vterm-send-string candidate))))
            :candidate-number-limit 10000))))

(use-package vterm-toggle
  :after vterm
  :straight (vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle" :no-build t))

(provide 'aza-shell)
