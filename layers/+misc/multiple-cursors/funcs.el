;; -*- lexical-binding: t -*-
;;
;;; funcs.el --- Spacemacs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <codrut.gusoi@gmail.com>
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


(defun spacemacs//evil-mc-paste-transient-state-p ()
  "Return non-nil if the paste transient state is enabled."
  (and dotspacemacs-enable-paste-transient-state
    (or (not (fboundp 'evil-mc-get-cursor-count))
      (eq (evil-mc-get-cursor-count) 1))))

(defun spacemacs/evil-mc-paste-after (&optional count register)
  "Disable paste transient state if there is more than 1 cursor."
  (interactive "*P")
  (setq this-command 'evil-paste-after)
  (cond ((spacemacs//evil-mc-paste-transient-state-p)
         (spacemacs/paste-transient-state/evil-paste-after))
        ((and (bound-and-true-p org-src-mode)
              (get-text-property (point) 'table-cell))
         (*table--cell-yank))
        (t (evil-paste-after count (or register evil-this-register)))))

(defun spacemacs/evil-mc-paste-before (&optional count register)
  "Disable paste transient state if there is more than 1 cursor."
  (interactive "*P")
  (setq this-command 'evil-paste-before)
  (if (spacemacs//evil-mc-paste-transient-state-p)
    (spacemacs/paste-transient-state/evil-paste-before)
    (evil-paste-before count (or register evil-this-register))))
