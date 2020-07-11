;;; config.el --- kotlin Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers kotlin-mode)

(defvar kotlin-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-kotlin'.
If `nil' then 'company-kotlin` is the default backend unless `lsp' layer is used")

(defvar kotlin-lsp-jar-path "~/install/server/bin/kotlin-language-server"
  "The path to the lsp jar file")
