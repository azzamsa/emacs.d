(use-package ledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :bind (:map ledger-mode-map
              ("M-1" . ledger-collapse-entry)
              ("M-0" . ledger-decollapse-entry))
  :config
  (defun ledger-collapse-entry ()
    (interactive)
    (set-selective-display (* tab-width 1)))

  (defun ledger-decollapse-entry ()
    (interactive)
    (set-selective-display (* tab-width 0)))

  (setq ledger-binary-path "hledger")
  (setq ledger-mode-should-check-version nil)
  ;; fix report when using hledger
  (setq ledger-report-auto-width nil
        ledger-report-use-native-highlighting nil)

  (defvar ledger-report-balance
    (list "bal" (concat ledger-binary-path " -f %(ledger-file) bal")))
  (defvar ledger-report-reg
    (list "reg" (concat ledger-binary-path " -f %(ledger-file) reg")))
  (defvar ledger-report-payee
    (list "payee" (concat ledger-binary-path " -f %(ledger-file) reg @%(payee)")))
  (defvar ledger-report-account
    (list "account" (concat ledger-binary-path " -f %(ledger-file) reg %(account)")))

  (setq ledger-reports
        (list ledger-report-balance
              ledger-report-reg
              ledger-report-payee
              ledger-report-account))

  ;; automatically show new transactions from hledger add or hledger-web
  (add-hook 'ledger-mode-hook 'auto-revert-tail-mode)
  (add-hook 'ledger-mode-hook 'auto-fill-mode -1)
  (add-hook 'ledger-mode-hook (lambda () (setq tab-width 4)))

  :custom
  (ledger-init-file-name " ")
  ;; move default amount position right allowing longer account names
  (ledger-post-amount-alignment-column 64)
  ;; disable distracting highlight
  (ledger-highlight-xact-under-point nil))

(use-package flycheck-ledger
  :after ledger-mode)

(provide 'aza-ledger)
