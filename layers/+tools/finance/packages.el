;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
    (flycheck-ledger :requires flycheck)
    ledger-mode
    (evil-ledger :toggle (memq dotspacemacs-editing-style '(vim hybrid)))
    ))

(defun finance/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes ledger-mode))

(defun finance/init-flycheck-ledger ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger)))

(defun finance/init-evil-ledger ()
  (use-package evil-ledger
    :defer t
    :init (add-hook 'ledger-mode 'evil-ledger-mode)))

(defun finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
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
        "t" 'ledger-insert-effective-date)
      (spacemacs/set-leader-keys-for-major-mode 'ledger-reconcile-mode
        (or dotspacemacs-major-mode-leader-key ",") 'ledger-reconcile-toggle
        "a" 'ledger-reconcile-add
        "q" 'ledger-reconcile-quit
        "t" 'ledger-reconcile-change-target
        "RET" 'ledger-reconcile-finish)
      ;; temporary hack to work-around an issue with evil-define-key
      ;; more info: https://github.com/emacs-evil/evil/issues/301
      ;; TODO remove this hack if the limitation is removed upstream
      (add-hook 'ledger-mode-hook 'evil-normalize-keymaps)
      (add-hook 'ledger-mode-hook
                (lambda () (setq-local pcomplete-termination-string "")))
      (evilified-state-evilify ledger-report-mode ledger-report-mode-map))))
