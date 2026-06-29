;; -*- lexical-binding: t; -*-

(use-package neotree
  :ensure t
  :bind (
    :map neotree-mode-map
    ("q"   . neotree-hide)
    ("RET" . neotree-enter)
    ("SPC" . neotree-quick-look)
    ("c"   . neotree-create-node)
    ("D"   . neotree-delete-node)
    ("g"   . neotree-refresh)
    ("r"   . neotree-rename-node)
    ("R"   . neotree-refresh)
    ("n"   . neotree-next-line)
    ("p"   . neotree-previous-line)
    ("N"   . neotree-select-next-sibling-node)
    ("P"   . neotree-select-previous-sibling-node))
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  ;; find current file and jump to node.
  (setq neo-smart-open t)
  (setq neo-hidden-regexp-list
    '(
      ;; hidden files
      "^\\."
      ;; temp files
      "~$" "^#.*#$"
      ;; generated files
      "^__" "\\.egg-info$" "\\.lock$"
      ;; docker
      "_volumes")))

(use-package dired
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
    ;; don't prompt to revert, just do it
    dired-auto-revert-buffer #'dired-buffer-stale-p
    ;; Always copy/delete recursively
    dired-recursive-copies  'always
    dired-recursive-deletes 'top
    ;; Ask whether destination dirs should get created when copying/removing files.
    dired-create-destination-dirs 'ask))

(use-package dired-x
  :after dired
  :config
  ;; Putting `dired-omit-files` inside `use-package dired`, `use-package dirvish`,
  ;; and `eval-after-load dired` doesn't work.
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

;; Collection of useful dired additions
(use-package dired-hacks
  :vc (:url "https://github.com/Fuco1/dired-hacks"))

(use-package dirvish
  :ensure t
  :after dired
  :bind (:map dirvish-mode-map
    ("?"     . dirvish-dispatch)
    ("q"     . dirvish-quit)
    ("b"     . dirvish-quick-access)
    ("f"     . dirvish-file-info-menu)
    ("p"     . dirvish-yank)
    ("z"     . dirvish-history-jump)
    ("h"     . dired-up-directory)
    ("l"     . dired-find-file)
    ([left]  . dired-up-directory)
    ([right] . dired-find-file)
    ("s"     . dirvish-subtree-toggle)
    ("TAB"   . dirvish-subtree-toggle)
    ("X"     . dired-do-flagged-delete)
    ("x"     . dired-do-delete)
    ("yl"    . dirvish-copy-file-true-path)
    ("yn"    . dirvish-copy-file-name)
    ("yp"    . dirvish-copy-file-path)
    ("yy"    . dired-do-copy))
  :config
  (setq dirvish-hide-details t)
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(nerd-icons subtree-state file-size))
  (setq dirvish-quick-access-entries
    '(("h" "~/"      "Home")
      ("l" "~/labs/" "Labs")
      ("p" "~/code/" "Code")
      ("o" "~/hq/"   "Hq")
      ("t" "/tmp"    "/tmp"))))
