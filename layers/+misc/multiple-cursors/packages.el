;; -*- lexical-binding: t -*-
;;
;;; packages.el --- Spacemacs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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

(defconst multiple-cursors-packages
  '(
    (evil-mc :toggle (eq multiple-cursors-backend 'evil-mc))
    (multiple-cursors :toggle (eq multiple-cursors-backend 'mc))))

(defun multiple-cursors/init-evil-mc ()
  (use-package evil-mc
    :init
    (which-key-add-keymap-based-replacements evil-motion-state-map
      "gr"  "evil-mc")
    (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
    (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)
    :config
    (add-hook 'magit-mode-hook 'turn-off-evil-mc-mode)
    (setq-default evil-mc-one-cursor-show-mode-line-text nil)
    (when (or (spacemacs/system-is-mac) (spacemacs/system-is-mswindows))
      (setq evil-mc-enable-bar-cursor nil))

    ;; evil-mc is not compatible with the paste transient state
    (evil-define-key 'normal evil-mc-key-map
      "p" #'spacemacs/evil-mc-paste-after
      "P" #'spacemacs/evil-mc-paste-before)

    (evil-define-key '(normal insert) evil-mc-key-map
      (kbd "C-M-j") #'evil-mc-make-cursor-move-next-line
      (kbd "C-M-k") #'evil-mc-make-cursor-move-prev-line)))

(defun multiple-cursors/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :init
    (spacemacs/declare-prefix
      "sm"  "multiple-cursors"
      "sms" "specials")
    (spacemacs/set-leader-keys
      "sma" 'mc/mark-all-dwim
      "smb" 'mc/mark-all-like-this
      "smm" 'mc/mark-more-like-this-extended
      "smr" 'mc/edit-lines
      "smsl" 'mc/insert-letters
      "smsm" 'mc/mark-sgml-tag-pair
      "smsn" 'mc/insert-numbers
      "smsr" 'set-rectangular-region-anchor
      "smss" 'mc/sort-regions
      "smst" 'mc/reverse-regions)
    (setq mc/always-run-for-all t)
    (with-eval-after-load 'multiple-cursors-core
      (add-to-list 'mc/cmds-to-run-once 'spacemacs/helm-M-x-fuzzy-matching)
      (add-to-list 'mc/cmds-to-run-once 'counsel-M-x)
      (add-to-list 'mc/cmds-to-run-once 'spacemacs/default-pop-shell))))
