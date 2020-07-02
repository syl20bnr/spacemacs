;;; packages.el --- Tern Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst tern-packages
  '(tern))

(defun tern/init-tern ()
  (use-package tern
    :defer t
    :config
    (progn
      (spacemacs|hide-lighter tern-mode)
      (dolist (mode tern--key-bindings-modes)
        (add-to-list (intern (format "spacemacs-jump-handlers-%S" mode))
                     '(tern-find-definition :async t))
        (spacemacs/set-leader-keys-for-major-mode mode
          "rrV" 'tern-rename-variable
          "hd" 'tern-get-docs
          "gG" 'tern-find-definition-by-name
          (kbd "C-g") 'tern-pop-find-definition
          "ht" 'tern-get-type)))))
