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
               ("[" . hydra-dired/body)
               ("RET" . dired-find-alternate-file)))
  :init
  (require 'aza-dired-ext)
  (use-package dired-x
    :straight (:type built-in))
  :config
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
                               (dired-omit-mode 1)
                               (dired-hide-details-mode +1)))
  (setq dired-omit-files
        (format "\\(?:\\.%s\\'\\)\\|%s\\|\\`_minted"
                (regexp-opt
                 '("aux" "pickle" "synctex.gz" "run.xml" "bcf" "am" "blx.bib"
                   "vrb" "opt" "nav" "snm" "out" "org_archive" "auto"))
                (regexp-opt
                 '("compile_commands.json" "__pycache__" ".pytest_cache" ".retry"
                   "._sync_"
                   "Cargo.lock" "yarn.lock" "node_modules")))))

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
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#698946" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ed666a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ed9366" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(provide 'aza-dired)
