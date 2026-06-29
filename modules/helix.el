;; -*- lexical-binding: t; -*-

(use-package pcre2el :ensure t)
(use-package dash :ensure t)

(use-package hel
  :ensure t
  :vc (:url "https://github.com/anuvyklack/hel.git" :rev "main")
  :config
  (hel-mode))
