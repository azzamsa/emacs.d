(use-package hydra
  :after aza-secrets
  :defer 2
  :bind (("C-x }" . hydra-windows/body)
         ("<f7>" . hydra-go-to/body)))

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
  _q_ quit     _fe_ emacs     _dp_ 🎨     _ap_ ⌛
  ^^           _fi_ inbox     _ds_ 🎵      _as_ 🔑
  ^^           _fn_ notes     _dh_ home   ^^
  ^^           ^^             _fa_ artcl  ^^
  ^^           ^^             ^^          ^^
  "
  ("q" nil)
  ("fe" (find-file user-init-file))
  ("fi" (find-file (expand-file-name "documents/gtd/inbox.org" user-emacs-directory)))  
  ("fn" (find-file my-notes))
  ("ft" (find-file my-thoughts))
  ("fm" (find-file my-moments))
  ("fs" (find-file my-success))
  ("fa" (find-file my-article-tracking))
  ("dp" (find-file my-projects-dir))
  ("dc" (find-file my-code-coba-dir))
  ("dh" (find-file my-home-dir))
  ("ds" (find-file my-songs-dir))
  ("ap" (hydra-pomodoro/body))
  ("at" (google-translate-smooth-translate))
  ("as" (hydra-pass/body)))

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

(defhydra hydra-pomodoro (:color blue)
  "
  ^
  ^Go To^      ^Action^
  ^─────^──────^──────^────────
  _q_ quit    _s_ start
  ^^          _p_ pause
  ^^          _P_ resume
  ^^          _S_ stop
  ^^           ^^
  "
  ("q" nil)
  ("s" (pomodoro-start 25))
  ("p" (pomodoro-pause))
  ("P" (pomodoro-resume))
  ("S" (pomodoro-stop)))

(defhydra hydra-pass (:color blue)
  "
  ^
  ^Go To^      ^Action^
  ^─────^──────^──────^────────
  _q_ quit    _c_ copy
  ^^          _e_ edit
  ^^           ^^
  ^^           ^^
  ^^           ^^
  "
  ("q" nil)
  ("c" password-store-copy-field)
  ("e" password-store-edit))

(provide 'aza-hydra)
