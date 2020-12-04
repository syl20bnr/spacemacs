;;; config.el --- Crystal Layer Configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; variables

(space-macs|define-jump-handlers crystal-mode crystal-def-jump)

(defvar crystal-enable-auto-format nil
  "If non-nil then auto-format on save.")

(defvar crystal-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-crystal'.
If `nil' then 'company-crystal` is the default backend unless `lsp' layer is used")


