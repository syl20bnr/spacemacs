;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst finance-packages
  '(company
    flycheck
    (flycheck-ledger :requires flycheck)
    ledger-mode
    hledger-mode
    (evil-ledger :toggle (memq dotspacemacs-editing-style '(vim hybrid)))))

(defun finance/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes ledger-mode)
  (spacemacs|add-company-backends
    :backends hledger-company
    :modes hledger-mode))

(defun finance/post-init-flycheck ()
  (spacemacs/enable-flycheck 'ledger-mode))

(defun finance/init-flycheck-ledger ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger)))

(defun finance/init-evil-ledger ()
  (use-package evil-ledger
    :defer t
    :init (add-hook 'ledger-mode-hook 'evil-ledger-mode)
    :config
    ;; Unset evil-ledger-mode's lighter
    (setf (cadr (assoc 'evil-ledger-mode minor-mode-alist)) "")))

(defun finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
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
      "RET" 'ledger-reconcile-finish)
    ;; temporary hack to work-around an issue with evil-define-key
    ;; more info: https://github.com/emacs-evil/evil/issues/301
    ;; TODO remove this hack if the limitation is removed upstream
    (add-hook 'ledger-mode-hook 'evil-normalize-keymaps)
    (add-hook 'ledger-mode-hook
              (lambda () (setq-local pcomplete-termination-string "")))
    (evilified-state-evilify-map ledger-reconcile-mode-map
      :eval-after-load ledger-reconcile
      :mode ledger-reconcile-mode)
    (evilified-state-evilify-map ledger-report-mode-map
      :eval-after-load ledger-report
      :mode ledger-report-mode)
    (evil-add-command-properties 'ledger-add-transaction :jump t)
    (evil-add-command-properties 'ledger-copy-transaction-at-point :jump t)))

(defun finance/init-hledger-mode ()
  (use-package hledger-mode
    :mode ("\\.journal\\'" . hledger-mode)
    :defer t
    :init
    (setq hledger-jfile (expand-file-name finance-hledger-journal-file))

    (define-key hledger-mode-map (kbd "<kp-add>") 'hledger-increment-entry-date)
    (define-key hledger-mode-map (kbd "<kp-subtract>") 'hledger-decrement-entry-date)))
