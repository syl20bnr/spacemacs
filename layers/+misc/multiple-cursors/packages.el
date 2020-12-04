;; -*- lexical-binding: t -*-
;;
;;; packages.el --- Space-macs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: CodruÈ› Constantin GuÈ™oi <codrut.gusoi@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq multiple-cursors-packages
      '(
        (evil-mc :toggle (eq multiple-cursors-backend 'evil-mc))
        (multiple-cursors :toggle (eq multiple-cursors-backend 'mc))))

(defun multiple-cursors/init-evil-mc ()
  (use-package evil-mc
    :init
    (progn
      (which-key-add-keymap-based-replacements evil-motion-state-map
        "gr"  "evil-mc")
      (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
      (add-hook 'text-mode-hook 'turn-on-evil-mc-mode))
    :config
    (progn
      (add-hook 'magit-mode-hook 'turn-off-evil-mc-mode)
      (setq-default evil-mc-one-cursor-show-mode-line-text nil)
      (when (or (space-macs/system-is-mac) (space-macs/system-is-mswindows))
        (setq evil-mc-enable-bar-cursor nil))

      ;; evil-mc is not compatible with the paste transient state
      (evil-define-key 'normal evil-mc-key-map
        "p" #'space-macs/evil-mc-paste-after
        "P" #'space-macs/evil-mc-paste-before)

      (evil-define-key '(normal insert) evil-mc-key-map
        (kbd "C-M-j") #'evil-mc-make-cursor-move-next-line
        (kbd "C-M-k") #'evil-mc-make-cursor-move-prev-line))))

(defun multiple-cursors/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :init
    (progn
      (space-macs/declare-prefix "sm" "multiple-cursors")
      (space-macs/declare-prefix "sms" "specials")
      (space-macs/set-leader-keys
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
        (add-to-list 'mc/cmds-to-run-once 'space-macs/helm-M-x-fuzzy-matching)
        (add-to-list 'mc/cmds-to-run-once 'counsel-M-x)
        (add-to-list 'mc/cmds-to-run-once 'space-macs/default-pop-shell)))))


