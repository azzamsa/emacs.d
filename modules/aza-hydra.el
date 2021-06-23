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
  ^^                  ^^
  "
  ("q" nil)
  ("z" undo)
  ("r" anzu-query-replace-regexp)
  ("e" er/expand-region)
  ("n" mc/mark-next-like-this))

(provide 'aza-hydra)
