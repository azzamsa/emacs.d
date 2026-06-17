;; -*- lexical-binding: t; -*-

(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package toml-ts-mode
  :mode "\\.toml\\'")

(use-package json-ts-mode
  :mode ("\\.json\\|\\.jsonc\\'"))

(use-package jsonnet-mode
  :ensure t)

(use-package just-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package vimrc-mode
  :ensure t
  :defer t)

(use-package nushell-mode
  :ensure t
  :defer t)

(use-package ledger-mode
  :ensure t
  :config
  (setq ledger-highlight-xact-under-point nil)
  (setq ledger-default-date-format ledger-iso-date-format))

(use-package flycheck-hledger
  :ensure t
  :after (flycheck ledger-mode)
  :demand t)

(use-package beancount
  :ensure t
  :config
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable)
  (add-hook 'beancount-mode-hook #'outline-minor-mode))

(use-package typst-ts-mode
  :disabled
  :ensure (:host codeberg :repo "meow_king/typst-ts-mode"))

(use-package scad-mode
  :ensure t)

(use-package ron-mode
  :ensure t
  :defer t
  :mode "\\.ron\\'")

(use-package pest-mode
  :ensure t
  :defer t)

(use-package hurl-mode
  :mode "\\.hurl\\'"
  :ensure (:host github :repo "jaszhe/hurl-mode"))

(use-package kdl-mode
  :ensure t)

(provide '+data)
