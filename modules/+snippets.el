;; -*- lexical-binding: t; -*-

;; A template system for Emacs
(use-package yasnippet
  :ensure t
  :defer t  ; don't load until needed
  :hook ((prog-mode text-mode) . yas-minor-mode)  ; instead of global
  :config
  (setq private-yas-dir (no-littering-expand-etc-file-name "yasnippet/snippets"))
  (push private-yas-dir yas-snippet-dirs))

;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; The Doom Emacs snippets library
(use-package doom-snippets
  :disabled
  :after yasnippet
  :ensure (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(provide '+snippets)
