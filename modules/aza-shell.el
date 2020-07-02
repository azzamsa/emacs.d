(use-package vterm
  :straight (vterm :type git :flavor melpa
                   :files ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
                   :host github :repo "akermu/emacs-libvterm")
  :bind ((:map vterm-mode-map
              ("<f2>" . hydra-vterm/body)
              ("C-c C-l" . helm-vterm-history)
              ([(control return)]  . vterm-toggle-insert-cd)))
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (setq vterm-always-compile-module t)
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

    (defun get-current-cmd ()
    (let* ((beg (vterm--get-prompt-point))
           (end (vterm--get-end-of-line)))
      (buffer-substring-no-properties beg end)))

  (defun vterm-send-return ()
    "Send `C-m' to the libvterm."
    (interactive)
    (when vterm--term
      (let* ((current-cmd (get-current-cmd)))
        (if (not (string= current-cmd ""))
            (add-to-list 'vterm-history current-cmd)))

      ;; default part from vterm.el
      ;; (setq cursor-type 'bar)
      (if (vterm--get-icrnl vterm--term)
          (process-send-string vterm--process "\C-j")
        (process-send-string vterm--process "\C-m"))))

  (defun helm-vterm-history ()
    (interactive)
    (let ((histories vterm-history)
          (current-cmd (get-current-cmd)))
      (helm :sources `((name . "vterm history")
                       (candidates . histories)
                       (action . (lambda (candidate)
                                   (vterm-send-string candidate))))
            :input current-cmd
            :candidate-number-limit 10000)))

  (defun remove-vterm-history ()
    (interactive)
    (setq vterm-history '())))

(use-package vterm-toggle
  :after vterm
  :straight (vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle")
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide 'aza-shell)
