(use-package hydra
  :bind ("<f7>" . hydra-menu/body))

(defhydra hydra-menu (:color pink)
  "
  ^
  ^Menu^           ^Menu^
  ^───────^───────────^──────^──────
  _q_ quit            _z_ undo
  ^^                  _r_ replace
  ^^                  _e_ er
  ^^                  _n_ mc/next
  ^^                  _+_ in
  ^^                  _-_ out
  ^^                  _=_ reset
  ^^                  ^^
  "
  ("q" nil)
  ("z" undo)
  ("r" anzu-query-replace-regexp)
  ("e" er/expand-region)
  ("n" mc/mark-next-like-this)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("=" (text-scale-increase 0)))

(provide 'aza-hydra)
