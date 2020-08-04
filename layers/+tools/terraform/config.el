;;; config.el -- terraform Layer configuration file for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Harry Hull <harry.hull1@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar terraform-auto-format-on-save nil
  "If non-nil then call `terraform fmt' before saving the terraform buffer.")

(defvar terraform-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-terraform'.
If `nil' then 'company-terraform` is the default backend unless `lsp' layer is used")
