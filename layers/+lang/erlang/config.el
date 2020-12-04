;;; config.el --- Erlang Layer packages File for Space-macs
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

(space-macs|define-jump-handlers erlang-mode)

(defvar erlang-fill-column 80
  "Column beyond which automatic line-wrapping should happen.")

;; lsp - erlang_ls

(defvar erlang-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' or `company-erlang'.
If `nil' then `company-erlang' is the default backend unless `lsp' layer is used.")


