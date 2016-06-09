;;; packages.el --- Theming Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst theming-packages
  '((theming :location local)))

(defun theming/init-theming ()
  ;; Apply theme customizations after any call to load-theme
  (advice-add 'load-theme :after 'spacemacs//theming)
  ;; Apply the initial customizations now, because load-theme has already been called
  (spacemacs//theming spacemacs--cur-theme))
