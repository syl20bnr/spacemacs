;;; config.el --- vue layer config file for Space-macs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


(defvar vue-backend 'dumb
  "The backend to use for IDE features. Possible values are `dumb' and `lsp'.")

(space-macs|define-jump-handlers vue-mode)


