;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    (flycheck-ledger :toggle (configuration-layer/package-usedp 'flycheck))
    ledger-mode
    ))


(defun finance/init-flycheck-ledger ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger)))

(defun finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
      (push 'company-capf company-backends-ledger-mode)
      (spacemacs/set-leader-keys-for-major-mode 'ledger-mode
         "hd" 'ledger-delete-current-transaction
         "a" 'ledger-add-transaction
         "b" 'ledger-post-edit-amount
         "c" 'ledger-toggle-current
         "C" 'ledger-mode-clean-buffer
         "l" 'ledger-display-ledger-stats
         "p" 'ledger-display-balance-at-point
         "q" 'ledger-post-align-xact
         "r" 'ledger-reconcile
         "R" 'ledger-report
         "t" 'ledger-insert-effective-date
         "y" 'ledger-set-year
         "RET" 'ledger-set-month)
      ;; temporary hack to work-around an issue with evil-define-key
      ;; more info: https://bitbucket.org/lyro/evil/issues/301/evil-define-key-for-minor-mode-does-not
      ;; TODO remove this hack if the limitation is removed upstream
      (add-hook 'ledger-mode-hook 'evil-normalize-keymaps)
      (evilified-state-evilify ledger-report-mode ledger-report-mode-map))))

(defun finance/post-init-company ()
  (spacemacs|add-company-hook ledger-mode))
