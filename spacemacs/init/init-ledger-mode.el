(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode)
  :init
  (progn 
    (setq ledger-post-amount-alignment-column 62)
    (evil-leader/set-key-for-mode 'ledger-mode
      "md" 'ledger-delete-current-transaction
      "ma" 'ledger-add-transaction)))
