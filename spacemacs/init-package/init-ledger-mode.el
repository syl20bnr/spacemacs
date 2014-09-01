(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :init
  (progn 
    (setq ledger-post-amount-alignment-column 62)))
