;;; config.el --- Nim Configuration Layer for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|define-jump-handlers nim-mode)

(defvar nim-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-nim'.
If `nil' then 'company-nim` is the default backend unless `lsp' layer is used")
