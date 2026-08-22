;; -*- lexical-binding: t; -*-

(use-package pcre2el :ensure t)
(use-package dash :ensure t)
(use-package ultra-scroll :ensure t)

(use-package hel
  :ensure t
  :vc (:url "https://github.com/anuvyklack/hel.git" :rev "main")
  :config
  (hel-mode))

(use-package hel-ghostel
  :ensure t
  :vc (:url "https://github.com/anuvyklack/hel-ghostel.git" :rev "main")
  :after (ghostel hel))
