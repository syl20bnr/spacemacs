;;; config.el --- vue layer config file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defvar vue-backend 'dumb
  "The backend to use for IDE features. Possible values are `dumb' and `lsp'.")

(spacemacs|define-jump-handlers vue-mode)
