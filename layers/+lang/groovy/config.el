;;; config.el --- Groovy layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar groovy-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-groovy'.
If `nil' then 'company-groovy` is the default backend unless `lsp' layer is used")

(defvar groovy-lsp-jar-path "~/groovy-lsp-all.jar"
  "The path to the lsp jar file")
