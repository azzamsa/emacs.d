(use-package hydra
  :bind (("s-l m" . hydra-menu/body)
         ("s-l c" . hydra-capital/body)))

(defhydra hydra-menu (:color pink)
  "
  _q_uit  _x_ redo     _r_eplace
  _e_r    _n_ mc/next  _+_ in     _-_ out  _=_ reset
  "
  ("q" nil)
  ("z" (undo-only))
  ("x" (undo-redo))
  ("r" anzu-query-replace-regexp)
  ("e" er/expand-region)
  ("n" mc/mark-next-like-this)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("=" (text-scale-increase 0)))

(defhydra hydra-capital (:color blue)
  "
  _q_uit  _d_owncase  _u_pcase  _c_apital
  "
  ("q" nil)
  ("d" downcase-region)
  ("u" upcase-region)
  ("c" capitalize-region))


(provide 'aza-hydra)
