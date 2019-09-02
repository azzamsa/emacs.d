(use-package hydra
  :defer 2
  :bind (("C-x }" . hydra-windows/body)
         ("<f7>" . hydra-go-to/body)
         ("s-l" . hydra-page/body))
  :config
  (when (file-exists-p (expand-file-name "aza-secrets.el" aza-pkgs-dir))
    (require 'aza-secrets)))

(defhydra hydra-page (:color pink)
  "
  ^
  ^Windows^           ^Page^
  ^───────^───────────^──────^──────
  _q_ quit            _k_ up
  ^^                  _j_ down
  "
  ("q" nil)
  ("k" (golden-ratio-scroll-screen-down))
  ("j" (golden-ratio-scroll-screen-up)))

(defhydra hydra-windows (:color pink)
  "
  ^
  ^Windows^           ^Window^            ^Zoom^
  ^───────^───────────^──────^────────────^────^──────
  _q_ quit            _b_ balance         _-_ out
  ^^                  _i_ heighten        _+_ in
  ^^                  _j_ narrow          _=_ reset
  ^^                  _k_ lower           ^^
  ^^                  _l_ widen           ^^
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("b" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0)))

(defhydra hydra-go-to (:color blue)
  "
  ^
  ^Go To^      ^File^        ^Directory^    ^Action^
  ^─────^──────^────^────────^─────────^────^─────────^
  _q_ quit     _fe_ emacs      _dp_ proj   _ap_ pmdr-start
  ^^           _fi_ inbox      _dc_ code   _at_ translate
  ^^           _fp_ project    _dh_ home   _as_ salah
  ^^           _fn_ notes      _fa_ artcl  _af_ autofill
  ^^           _ft_ thoughts   _ds_ song   _am_ menubar
  ^^           _fm_ moments    ^^          ^^
  ^^           _fs_ success    ^^          ^^
  ^^           ^^              ^^          ^^
  "
  ("q" nil)
  ("fe" (find-file user-init-file))
  ("fi" (find-file (expand-file-name "documents/gtd/inbox.org" user-emacs-directory)))
  ("fp" (find-file (expand-file-name "documents/gtd/project.org" user-emacs-directory)))
  ("fn" (find-file my-notes))
  ("ft" (find-file my-thoughts))
  ("fm" (find-file my-moments))
  ("fs" (find-file my-success))
  ("fa" (find-file my-article-tracking))
  ("dp" (find-file my-projects-dir))
  ("dc" (find-file my-code-coba-dir))
  ("dh" (find-file my-home-dir))
  ("ds" (find-file my-songs-dir))
  ("ap" (pomodoro-start 25))
  ("at" (google-translate-smooth-translate))
  ("as" (salah-time))
  ("af" auto-fill-mode)
  ("am" menu-bar-mode))

(defhydra hydra-dired (:color blue)
  "
  ^
  ^Go To^      ^Action^
  ^─────^──────^──────^────────
  _q_ quit    _ao_ omit
  ^^           ^^
  ^^           ^^
  ^^           ^^
  ^^           ^^
  "
  ("q" nil)
  ("ao" dired-omit-mode))

(provide 'aza-hydra)
