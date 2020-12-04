;;; packages.el --- Tern Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst tern-packages
  '(tern))

(defun tern/init-tern ()
  (use-package tern
    :defer t
    :config
    (progn
      (space-macs|hide-lighter tern-mode)
      (dolist (mode tern--key-bindings-modes)
        (add-to-list (intern (format "space-macs-jump-handlers-%S" mode))
                     '(tern-find-definition :async t))
        (space-macs/set-leader-keys-for-major-mode mode
          "rrV" 'tern-rename-variable
          "hd" 'tern-get-docs
          "gG" 'tern-find-definition-by-name
          (kbd "C-g") 'tern-pop-find-definition
          "ht" 'tern-get-type)))))


