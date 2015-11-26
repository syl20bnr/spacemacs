(setq csv-packages '(csv-mode))

(defun csv/init-csv-mode ()
  "Initial csv mode"
  (use-package csv-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'csv-mode
      "t"  'csv-transpose
      "a"  'csv-align-fields
      "u"  'csv-unalign-fields
      "sf" 'csv-sort-fields
      "sn" 'csv-sort-numeric-fields
      "so" 'csv-toggle-descending
      "n"  'csv-forward-field
      "p"  'csv-backward-field
      "r"  'csv-reverse-region
      "d"  'csv-kill-fields
      "i"  'csv-toggle-invisibility
      "vf" 'csv-yank-fields
      "vt" 'csv-yank-as-new-table)))
