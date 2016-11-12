;;; funcs.el --- rust Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: NJBS <DoNotTrackMeUsingThis@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point and switch to
it."
  (interactive)
  (select-window (racer-describe)))
