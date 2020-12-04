;;; config.el --- Vimscript Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; variables

(space-macs|define-jump-handlers vimrc-mode)

(defvar vimscript-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-vimscript'.
If `nil' then 'company-vimscript` is the default backend unless `lsp' layer is used")


