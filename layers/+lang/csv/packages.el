;;; packages.el --- csharp Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: William Casarin <bill@casarin.me>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq csv-packages '(csv-mode))

(defun csv/init-csv-mode ()
  (use-package csv-mode
    :defer t
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'csv-mode "ms" "sort")
      (space-macs/declare-prefix-for-mode 'csv-mode "mv" "yank")
      (space-macs/set-leader-keys-for-major-mode 'csv-mode
        "a"  'csv-align-fields
        "d"  'csv-kill-fields
        "h"  'csv-header-line
        "i"  'csv-toggle-invisibility
        "n"  'csv-forward-field
        "p"  'csv-backward-field
        "r"  'csv-reverse-region
        "sf" 'csv-sort-fields
        "sn" 'csv-sort-numeric-fields
        "so" 'csv-toggle-descending
        "t"  'csv-transpose
        "u"  'csv-unalign-fields
        "vf" 'csv-yank-fields
        "vt" 'csv-yank-as-new-table))))


