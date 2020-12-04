;;; config.el --- fsharp Layer config File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(space-macs|define-jump-handlers fsharp-mode fsharp-ac/gotodefn-at-point)

(defvar fsharp-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `eglot'.
If `nil' then 'eglot` is the default backend unless `lsp' layer is used")


