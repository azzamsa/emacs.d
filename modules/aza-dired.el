(require 'aza-dired-ext)

(use-package dired
  :ensure nil
  :ensure-system-package (thunar urxvt tmux sxiv)
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
               ("]" . term-here)))
  :init
  (require 'aza-dired-ext)
  (require 'dired-x) ; dired-jump is cool
  :config

  ;; error when outside dired config
  (use-package dired+
    :load-path "~/.emacs.d/elisp/diredp/"
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
  (add-hook 'dired-mode-hook #'dired-filter-group-mode)
  (add-hook 'dired-mode-hook #'dired-filter-mode)
  (setq dired-omit-files
        (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`_minted"
                (regexp-opt
                 '("aux" "log" "pickle" "synctex.gz" "run.xml" "bcf" "am" "in" "blx.bib"
                   "vrb" "opt" "nav" "snm" "out"))
                (regexp-opt
                 '("compile_commands.json"
                   "__pycache__")))))

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
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("\\" . dired-narrow)))

(use-package dired-avfs
  :after dired)

(use-package dired-filter
  :after dired
  :init
  (setq dired-filter-prefix "f"))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))

  (dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps"
                                            "rtf" "djvu" "epub"))
  (dired-rainbow-define excel "#3465a4" ("xlsx"))
  (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv"
                                         "ogg" "wmv" "mkv" "mov" "wma"))
  (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))

  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "h" "java" "pl" "rb"
                                              "R" "php"))

  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz"
                                              "z" "Z" "jar" "war" "ear" "rar"
                                              "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
  (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*"))

(provide 'aza-dired)
