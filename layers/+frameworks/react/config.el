;;; config.el --- react Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Prerequisites

(configuration-layer/declare-layers '(html javascript))

;; Variables

(spacemacs|defvar-company-backends react-mode)
