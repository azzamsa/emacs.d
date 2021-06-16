(use-package hydra
  :after aza-secrets
  :bind ("<f7>" . hydra-menu/body))

(defhydra hydra-menu (:color pink)
  "
  ^
  ^Menu^           ^Menu^
  ^───────^───────────^──────^──────
  _q_ quit            _z_ undo
  ^^                  _m_ mc/all
  ^^                  ^^
  "
  ("q" nil)
  ("z" undo)
  ("m" mc/mark-all-like-this))

(provide 'aza-hydra)
