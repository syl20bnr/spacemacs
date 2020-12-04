;;; config.el --- Agda2 Layer config File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: FreeSalad <freesalad@noreply.git>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defvar agda-mode-path 'use-helper
  "Indicates the location of the agda-mode package (the file
`agda2.el') If `nil', it is assumed to be already available by
e-macs. If `use-helper', the `agda-mode' executable is used to
find its location.")

(space-macs|define-jump-handlers agda2-mode agda2-goto-definition-keyboard)


