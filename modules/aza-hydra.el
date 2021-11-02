(use-package hydra
  :bind (("s-l m" . hydra-menu/body)
         ("s-l c" . hydra-capital/body)
         ("s-l t" . hydra-text/body)))

(defhydra hydra-menu (:color pink)
  "
  _q_uit  _r_eplace    _r_ wu
  _e_r    _n_ mc/next  _t_ wr
  "
  ("q" nil)
  ("r" anzu-query-replace-regexp)
  ("e" er/expand-region)
  ("n" mc/mark-next-like-this)
  ("r" winner-undo)
  ("t" winner-redo))

(defhydra hydra-capital (:color blue)
  "
  _q_uit  _d_owncase  _u_pcase  _c_apital
  "
  ("q" nil)
  ("d" downcase-region)
  ("u" upcase-region)
  ("c" capitalize-region))

(defhydra hydra-text (:color pink)
  "
  _q_uit  _f_ +  _s_ -  _c_ =
  "
  ("q" nil)
  ("f" text-scale-increase)
  ("s" text-scale-decrease)
  ("c" (text-scale-increase 0)))


(provide 'aza-hydra)
