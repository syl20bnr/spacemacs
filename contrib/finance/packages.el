;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar finance-packages
  '(
    flycheck-ledger
    ledger-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")


(defun finance/init-flycheck-ledger ()
  (eval-after-load 'flycheck
    '(require 'flycheck-ledger)))

(defun finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
      (evil-leader/set-key-for-mode 'ledger-mode
        "mhd" 'ledger-delete-current-transaction
        "ma"  'ledger-add-transaction))))
