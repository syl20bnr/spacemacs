;;; config.el --- dart Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Bruno Tavares <connect+spacemacs@bltavares.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar dart-sdk-path "~/flutter/bin/cache/dart-sdk"
  "The path to dart sdk which is used by dart-server or lsp features.
Default to flutter's build in sdk")

(defvar dart-backend nil
  "The backend to use for IDE features.
Possible values are `analyzer' `lsp'.
If `nil' then `analyzer' is the default backend unless `lsp' layer is used.")

(spacemacs|define-jump-handlers dart-mode)
