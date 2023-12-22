;;; evil-collection-monky.el --- Bindings for `monky' -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bindings for `monky'.

;;; Code:
(require 'evil-collection)
(require 'monky nil t)

(defconst evil-collection-monky-maps '(monky-mode-map
                                       monky-status-mode-map
                                       monky-log-mode-map
                                       monky-blame-mode-map
                                       monky-branches-mode-map
                                       monky-commit-mode-map
                                       monky-queue-mode-map
                                       monky-log-edit-mode-map))

;;;###autoload
(defun evil-collection-monky-setup ()
  "Set up `evil' bindings for `monky'."
  (evil-set-initial-state 'monky-mode 'normal)
  (add-hook 'monky-mode-hook 'evil-normalize-keymaps)
  (add-hook 'monky-status-mode-hook 'evil-normalize-keymaps)

  (evil-collection-define-key 'normal 'monky-mode-map
    "]]" 'monky-goto-next-section
    "gj" 'monky-goto-next-section
    (kbd "C-j") 'monky-goto-next-section
    "[[" 'monky-goto-previous-section
    "gk" 'monky-goto-previous-section
    (kbd "C-k") 'monky-goto-previous-section
    (kbd "RET") 'monky-visit-item
    (kbd "TAB") 'monky-toggle-section
    (kbd "SPC") 'monky-show-item-or-scroll-up
    (kbd "DEL") 'monky-show-item-or-scroll-down
    "gr" 'monky-refresh
    "`" 'monky-display-process
    "!" 'monky-hg-command
    "Ll" 'monky-log-current-branch
    "La" 'monky-log-all
    "LL" 'monky-log-all
    "Lr" 'monky-log-revset
    "b" 'monky-branches
    "Q" 'monky-queue
    "q" 'monky-quit-window)

  (evil-collection-define-key 'normal 'monky-status-mode-map
    "s" 'monky-stage-item
    "S" 'monky-stage-all
    "u" 'monky-unstage-item
    "U" 'monky-unstage-all
    "a" 'monky-commit-amend
    "c" 'monky-log-edit
    "e" 'monky-ediff-item
    "Y" 'monky-bookmark-create
    "C" 'monky-checkout
    "M" 'monky-merge
    "B" 'monky-backout
    "P" 'monky-push
    "F" 'monky-pull
    "x" 'monky-discard-item
    "m" 'monky-resolve-item
    "K" 'monky-unresolve-item ;; Not the best...
    "o" 'monky-reset-tip
    "A" 'monky-addremove-all
    "R" 'monky-rollback)

  (evil-collection-define-key 'normal 'monky-log-mode-map
    "+" 'monky-log-show-more-entries
    "b" 'monky-checkout-item ;; Like `magit-branch'.
    "m" 'monky-merge-item    ;; Like `magit-merge'.
    "B" 'monky-backout-item
    "i" 'monky-qimport-item)

  (evil-collection-define-key 'normal 'monky-branches-mode-map
    "b" 'monky-checkout-item ;; Like `magit-branch'.
    "m" 'monky-merge-item    ;; Like `magit-merge'.
    )

  ;; FIXME: Haven't used this mode yet so not sure if these are appropriate...
  (evil-collection-define-key 'normal 'monky-queue-mode-map
    "u" 'monky-qpop-item
    "U" 'monky-qpop-all
    "s" 'monky-qpush-item
    "S" 'monky-qpush-all
    "r" 'monky-qrefresh
    "R" 'monky-qrename-item
    "k" 'monky-qremove-item
    "N" 'monky-qnew
    "f" 'monky-qfinish-item
    "F" 'monky-qfinish-applied
    "d" 'monky-qfold-item
    "G" 'monky-qguard-item
    "o" 'monky-qreorder
    "A" 'monky-addremove-all)

  (evil-collection-define-key 'normal 'monky-log-edit-mode-map
    (kbd "C-c C-c") 'monky-log-edit-commit
    [remap evil-save-and-close] 'monky-log-edit-commit
    [remap evil-save-modified-and-close] 'monky-log-edit-commit
    (kbd "C-c C-k") 'monky-log-edit-cancel-log-message
    [remap evil-quit] 'monky-log-edit-cancel-log-message))

(provide 'evil-collection-monky)
;;; evil-collection-monky.el ends here
