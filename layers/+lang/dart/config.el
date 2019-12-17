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

(defvar dart-backend nil
  "The backend to use for IDE features.
Possible values are `analyzer' `lsp'.
If `nil' then `analyzer' is the default backend unless `lsp' layer is used.")

(spacemacs|define-jump-handlers dart-mode)
