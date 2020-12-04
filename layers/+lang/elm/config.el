;;; config.el --- Elm Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; variables

(space-macs|define-jump-handlers elm-mode)

(defvar elm-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-elm'.
If `nil' then 'company-elm` is the default backend unless `lsp' layer is used")


