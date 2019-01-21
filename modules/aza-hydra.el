(use-package hydra
  :defer 2
  :bind (("C-x }" . hydra-windows/body)
         ("<f7>" . hydra-go-to/body)
         ("s-l" . hydra-page/body))
  :config
  (require 'aza-secrets))

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
  ^Go To^           ^File^        ^Directory^
  ^─────^───────────^────^────────^─────────^
  _q_ quit          _fe_ emacs      _dp_ projects
  ^^                _fi_ inbox      _dc_ code-coba
  ^^                _fp_ project    _dh_ home
  ^^                _dn_ notes      ^^
  ^^                _dt_ thoughts   ^^
  ^^                _dm_ moments    ^^
  ^^                _ds_ success    ^^
  "
  ("q" nil)
  ("fe" (find-file user-init-file))
  ("fi" (find-file (expand-file-name "documents/gtd/inbox.org" user-emacs-directory)))
  ("fp" (find-file (expand-file-name "documents/gtd/project.org" user-emacs-directory)))
  ("dp" (find-file my-projects-dir))
  ("dc" (find-file my-code-coba-dir))
  ("dh" (find-file my-home-dir))
  ("dn" (find-file my-notes))
  ("dt" (find-file my-thoughts))
  ("dm" (find-file my-moments))
  ("ds" (find-file my-success)))

(provide 'aza-hydra)
