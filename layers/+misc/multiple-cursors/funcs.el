;; -*- lexical-binding: t -*-
;;
;;; funcs.el --- Spacemacs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <codrut.gusoi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
