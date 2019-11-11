(use-package hydra
  :defer 2
  :bind (("C-x }" . hydra-windows/body)
         ("<f7>" . hydra-go-to/body))
  :config
  (when (file-exists-p (expand-file-name "aza-secrets.el" aza-pkgs-dir))
    (require 'aza-secrets)))

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
  _q_ quit     _fe_ emacs      _dp_ proj   _ap_ pomodoro
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
  ("ap" (hydra-pomodoro/body))
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

(provide 'aza-hydra)
