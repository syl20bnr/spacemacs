;;; funcs.el --- restclient Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


(defun space-macs/restclient-http-send-current-raw-stay-in-window ()
  (interactive)
  (restclient-http-send-current t t))


