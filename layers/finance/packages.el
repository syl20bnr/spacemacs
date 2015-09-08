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

(setq finance-packages
  '(
    company
    flycheck-ledger
    ledger-mode
    ))


(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun finance/init-flycheck-ledger ()
    (eval-after-load 'flycheck
      '(require 'flycheck-ledger))))

(defun finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
      (push 'company-capf company-backends-ledger-mode)
      (evil-leader/set-key-for-mode 'ledger-mode
        "mhd"   'ledger-delete-current-transaction
        "ma"    'ledger-add-transaction
        "mb"    'ledger-post-edit-amount
        "mc"    'ledger-toggle-current
        "mC"    'ledger-mode-clean-buffer
        "ml"    'ledger-display-ledger-stats
        "mp"    'ledger-display-balance-at-point
        "mq"    'ledger-post-align-xact
        "mr"    'ledger-reconcile
        "mR"    'ledger-report
        "mt"    'ledger-insert-effective-date
        "my"    'ledger-set-year
        "m RET" 'ledger-set-month)
      (evilify ledger-report-mode ledger-report-mode-map))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun finance/post-init-company ()
    (spacemacs|add-company-hook ledger-mode)))
