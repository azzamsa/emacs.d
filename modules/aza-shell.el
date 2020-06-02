(use-package vterm
  :straight (vterm :type git :flavor melpa
                   :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
                   :host github :repo "akermu/emacs-libvterm")
  :bind (:map vterm-mode-map
              ("<f2>" . vterm-toggle)
              ([(control return)]  . vterm-toggle-insert-cd)
              ("C-c C-l" . helm-vterm-history)
              ("C-n" . vterm-toggle-forward)
              ("C-b" . vterm-toggle-backward))
  :config
  (setq cursor-type 'bar)
  (vterm "vterm-main")

  :config
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *")
  (setq vterm-history
        (expand-file-name "vterm-history" aza-savefile-dir))

  (defun vterm-send-return ()
    "Send `C-m' to the libvterm."
    (interactive)
    (when vterm--term
      (let* ((beg (vterm--get-prompt-point))
             (end (vterm--get-end-of-line))
             (string (buffer-substring-no-properties beg end))
             (file vterm-history))
        (if (not (string= string ""))
          (write-region (concat string "\n") nil file t 0)))
      (if (vterm--get-icrnl vterm--term)
          (process-send-string vterm--process "\C-j")
        (process-send-string vterm--process "\C-m"))))

  (defun helm-vterm-history ()
    (interactive)
    (let ((contents (with-temp-buffer
                      (insert-file-contents vterm-history)
                      (reverse (split-string (buffer-string) "\n")))))
      (helm :sources `((name . "vterm history")
                       (candidates . contents)
                       (action . (lambda (candidate)
                                   (vterm-send-string candidate))))
            :candidate-number-limit 10000))))

(use-package vterm-toggle
  :after vterm
  :straight (vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle" :no-build t))

(provide 'aza-shell)
