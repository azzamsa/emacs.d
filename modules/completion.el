;; -*- lexical-binding: t; -*-

;; Search and navigate via completing-read
(use-package consult
  :ensure t
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package consult-flycheck
  :ensure t
  :after (consult flycheck))

(use-package consult-yasnippet
  :ensure t
  :after consult)

(use-package embark-consult
  :ensure t)

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult))

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :after consult
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode)
  :config
  (defun +vertico/project-search-from-cwd ()
    "Search files from the current working directory using Vertico and Consult."
    (interactive)
    (let ((dir (file-truename default-directory)))
      (consult-ripgrep dir))))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
    ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
    ("SPC" . corfu-insert-separator)
    ("C-n" . corfu-next)
    ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))
