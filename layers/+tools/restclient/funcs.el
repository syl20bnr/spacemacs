;;; funcs.el --- restclient Layer functions File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun spacemacs/restclient-http-send-current-raw-stay-in-window ()
  (interactive)
  (restclient-http-send-current t t))
