;;; fancy eshell prompt
;;
;; original author :https://github.com/jimm/elisp/blob/master/eshell-customize.el
;; Modified by :http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/
;; then by Azzam S.A :)

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License:

;;; Code:

(setq eshell-history-size 1024)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
                                        ;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
                                        ;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))


;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (concat (concat " on ")
              (propertize (concat " ["
                                  (if (> (length git-output) 0)
                                      (substring git-output 0 -1)
                                    "(no branch)")
                                  "]") 'face `(:foreground "#8cd0d3"))))))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (user-login-name) 'face `(:foreground "#bc6c4c"))
         (concat " at ")
         (propertize (system-name) 'face `(:foreground "#f0dfaf"))
         (concat " in " )
         (propertize ((lambda (p-lst)
                        (if (> (length p-lst) 3)
                            (concat
                             (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                   (substring elm 0 1)))
                                        (butlast p-lst 3)
                                        "/")
                             "/"
                             (mapconcat (lambda (elm) elm)
                                        (last p-lst 3)
                                        "/"))
                          (mapconcat (lambda (elm) elm)
                                     p-lst
                                     "/")))
                      (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "#7cac7c"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize " $ " 'face 'default))))

(setq eshell-highlight-prompt nil)

(provide 'aza-eshell)
;;; aza-eshell ends here
