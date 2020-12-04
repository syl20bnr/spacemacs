;;; config.el --- Java configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; variables

(space-macs|define-jump-handlers java-mode)

(defvar java-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `meghanada'.
If `nil' then `meghanada' is the default backend unless `lsp' layer is used.")


