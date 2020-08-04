;;; config.el --- json layer configuration file for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar json-fmt-tool 'web-beautify
  "The formatter to format a JSON file. Possible values are `web-beautify' and `prettier'.")

(defvar json-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar json-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-json'.
If `nil' then 'company-json` is the default backend unless `lsp' layer is used")
