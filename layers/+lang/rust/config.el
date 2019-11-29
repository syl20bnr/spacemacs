;;; config.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|define-jump-handlers rust-mode)

(defvar rust-backend nil
  "The backend to use for completion.
Possible values are `lsp' `racer'.
If `nil' then `racer' is the default backend unless `lsp' layer is used.")
