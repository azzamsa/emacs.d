;; -*- lexical-binding: t; -*-

(use-package rg
  :ensure t
  :defer t)

;; Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :ensure t
  :defer t)

(use-package dockerfile-ts-mode
  :mode "\\Dockerfile\\'")

(use-package ghostel
  :ensure t
  :bind (:map ghostel-semi-char-mode-map
              ("C-s"  . consult-line))
  :config
  (keymap-global-set "C-;" #'ghostel))

(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; Emulate A Terminal, in a region, in a buffer and in Eshell
(use-package eat
  :disabled
  :ensure t)

(defun eat-toggle ()
  (interactive)
  (if (string= (buffer-name) "*eat*")
      (delete-window)
    (eat-other-window "fish" nil)))

(provide '+tools)
