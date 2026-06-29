;; -*- lexical-binding: t; -*-

(use-package dash :ensure t)
(use-package pcre2el :ensure t)
(use-package s :ensure t)

(use-package hel
  :vc (:url "https://github.com/anuvyklack/hel.git" :rev "main")
  :config
  (hel-mode))
