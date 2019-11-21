;;; config.el --- json layer configuration file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar json-fmt-tool 'web-beautify
  "The formatter to format a JSON file. Possible values are `jq', `web-beautify',
`prettier', `fixjson'.")

(defvar json-fixjson-args '()
  "Additional arguments to be supplied to `fixjson'.")

(defvar json-fmt-on-save nil
  "Run formatter on buffer save.")
