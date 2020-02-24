;;; config.el --- Tide Layer config file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar tide-managed-modes '(
                            ;; typescript-mode
                            ;; typescript-tsx-mode
                            ;; js2-mode
                            ;; js-mode
                            ;; rjsx-mode
                            )
  "List of major modes that `tide layer` can manage.
Client layers must add its major-mode to this list")

(defvar tide-jsconfig-content
  "{\n\
    \"compilerOptions\": {\n\
      \"target\": \"es2017\",\n\
      \"allowSyntheticDefaultImports\": true,\n\
      \"noEmit\": true,\n\
      \"checkJs\": true,\n\
      \"jsx\": \"react\",\n\
      \"lib\": [ \"dom\", \"es2017\" ]\n\
    }\n\
  }"
  "Content of jsconfig.json file.")
