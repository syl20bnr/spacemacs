;;; config.el --- Rust Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Variables

(space-macs|define-jump-handlers rust-mode)

(defvar rust-backend nil
  "The backend to use for completion.
Possible values are `lsp' `racer'.
If `nil' then `racer' is the default backend unless `lsp' layer is used.")


