;;; config.el --- ESS Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers ess-r-mode)

;; Variables

(defvar ess-r-backend 'nil
  "The backend to use for IDE features. Possible values are `ess' and `lsp'.")

(defvar ess-assign-key nil
  "Call `ess-insert-assign'.")
