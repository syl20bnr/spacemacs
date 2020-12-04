;;; config.el --- ESS Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(space-macs|define-jump-handlers ess-r-mode)

;; Variables

(defvar ess-r-backend 'nil
  "The backend to use for IDE features. Possible values are `ess' and `lsp'.")

(defvar ess-assign-key nil
  "Call `ess-insert-assign'.")


