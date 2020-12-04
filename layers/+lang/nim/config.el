;;; config.el --- Nim Configuration Layer for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Variables

(space-macs|define-jump-handlers nim-mode)

(defvar nim-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-nim'.
If `nil' then 'company-nim` is the default backend unless `lsp' layer is used")


