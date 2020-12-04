;;; packages.el --- Theming Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst theming-packages
  '((theming :location local)))

(defun theming/init-theming ()
  ;; Apply theme customizations after any call to load-theme
  (advice-add 'load-theme :after 'space-macs//theming)
  ;; Apply the initial customizations now, because load-theme has already been called
  (space-macs//theming space-macs--cur-theme))


