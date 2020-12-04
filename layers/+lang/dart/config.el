;;; config.el --- dart Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Bruno Tavares <connect+space-macs@bltavares.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Variables
(space-macs|define-jump-handlers dart-mode)

(defvar dart-backend 'lsp
  "The backend to use for IDE features.
Possible values are `analyzer' and `lsp'.")


