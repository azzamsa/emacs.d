(require 'aza-dired-ext)

(use-package dired
  :straight (:type built-in)
  :defer 0.5
  :bind ((:map dired-mode-map
               ("/" . ora-dired-up-directory)
               ("C-r" . ora-dired-rsync)
               ("C-o" . dired-view-current)
               ("n" . dired-view-next)
               ("p" . dired-view-previous)
               ("s" . xah-dired-sort)
               ("&" . ora-dired-do-async-shell-command)
               ("O" . ora-dired-other-window)
               ("z" . ora-dired-get-size)
               ("'" . eshell-this-dir)
               ("`" . shell-here)
               ("s-o" . dired-open-directory-in-thunar)
               ("]" . term-here)
               ("j" . dired-next-line)
               ("k" . dired-previous-line)
               ("[" . hydra-dired/body)))
  :init
  (require 'aza-dired-ext)
  (use-package dired-x
    :straight (:type built-in))
  :config

  ;; error when outside dired config
  (use-package dired+
    :straight (dired+ :type git :host github :repo "emacsmirror/dired-plus")
    :config
    (diredp-toggle-find-file-reuse-dir 1))

  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (setq dired-auto-revert-buffer t)
  (setq delete-by-moving-to-trash t)
  (setq find-file-visit-truename t) ; follow original dir in sysmlink

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-AltGhF --group-directories-first")
  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode 1)))
  (setq dired-omit-files
        (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`_minted"
                (regexp-opt
                 '("aux" "pickle" "synctex.gz" "run.xml" "bcf" "am" "blx.bib"
                   "vrb" "opt" "nav" "snm" "out" "org_archive" "auto"))
                (regexp-opt
                 '("compile_commands.json" "__pycache__" ".pytest_cache" ".retry"
                   "._sync_")))))

(use-package wdired
  :after dired
  :config
  (setq wdired-use-dired-vertical-movement 'sometimes))

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-subtree
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-narrow
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("\\" . dired-narrow)))

(use-package dired-avfs
  :after dired)

(use-package dired-collapse
  :disabled
  ;; disabled until this issue solved
  ;; https://github.com/Fuco1/dired-hacks/issues/144
  :after dired)

(use-package dired-filter
  :defer t
  :init
  (setq dired-filter-prefix "f")
  :commands dired-filter-by-mode)

(use-package dired-async
  :straight (dired-async :type git :host github :repo "jwiegley/emacs-async")
  :after dired
  :delight ""
  :preface
  (defun my/dired-async-message-function (text _face &rest args)
    "Log messages from dired-async to messages buffer."
    (message (format "Finished %s" (apply #'format text args))))
  :config
  (dired-async-mode 1)
  :custom
  (dired-async-message-function #'my/dired-async-message-function))

(use-package dired-sidebar
  :bind ([f8] . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-custom-font t))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define-chmod executable-unix "#77e323" "-.*x.*")
  (dired-rainbow-define sourcefile "#b4fa70" ("py" "cl" "el" "ruby" "java"
                                              "html" "xml"))
  (dired-rainbow-define document "#fce94f" ("odt" "fodt" "ods" "pdf" "epub"))
  (dired-rainbow-define media "#dfe3ee" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg"
                                         "flv" "ogg" "mkv" "m4a"))
  (dired-rainbow-define image "#8b9dc3" ("jpg" "png" "jpeg" "gif"))

  (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz"  "gz"
                                              "rar" "7z" "tar"))
  (dired-rainbow-define encrypted "#ffcc5c" ("gpg" "pgp")))

(provide 'aza-dired)
