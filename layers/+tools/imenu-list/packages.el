;;; packages.el --- imenu-list Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst imenu-list-packages
  '(
    golden-ratio
    imenu-list
    ))

(defun imenu-list/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")))

(defun imenu-list/init-imenu-list ()
  (use-package imenu-list
    :defer t
    :init
    (progn
      (setq imenu-list-focus-after-activation t
            imenu-list-auto-resize t)
      (spacemacs/set-leader-keys "bi" #'imenu-list-minor-mode))
    :config
    (evilified-state-evilify-map imenu-list-major-mode-map
      :mode imenu-list-major-mode
      :bindings
      "d" #'imenu-list-display-entry
      "q" #'imenu-list-minor-mode)))
