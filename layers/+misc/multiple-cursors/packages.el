;; -*- lexical-binding: t -*-
;;
;;; packages.el --- Spacemacs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <codrut.gusoi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq multiple-cursors-packages
      '(
        evil-mc
        ))

(defun multiple-cursors/init-evil-mc ()
  (use-package evil-mc
    :config
    (progn
      (setq-default evil-mc-one-cursor-show-mode-line-text nil)
      (when (or (spacemacs/system-is-mac) (spacemacs/system-is-mswindows))
        (setq evil-mc-enable-bar-cursor nil))

      ;; evil-mc is not compatible with the paste transient state
      (define-key evil-normal-state-map "p" 'spacemacs/evil-mc-paste-after)
      (define-key evil-normal-state-map "P" 'spacemacs/evil-mc-paste-before)

      (dolist (state-map `(,evil-normal-state-map ,evil-insert-state-map))
        (dolist (keybinding `((,(kbd "C-M-j") . evil-mc-make-cursor-move-next-line)
                              (,(kbd "C-M-k") . evil-mc-make-cursor-move-prev-line)))
          (define-key state-map (car keybinding) (cdr keybinding))))
      (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
      (add-hook 'text-mode-hook 'turn-on-evil-mc-mode))))
