;;; packages.el --- csharp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: William Casarin <bill@casarin.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq csv-packages '(csv-mode))

(defun csv/init-csv-mode ()
  (use-package csv-mode
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'csv-mode "ms" "sort")
      (spacemacs/declare-prefix-for-mode 'csv-mode "mv" "yank")
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
        "vt" 'csv-yank-as-new-table))))
