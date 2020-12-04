;;; funcs.el --- slack layer functions file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; persp

(defun space-macs//slack-persp-filter-save-buffers-function (buffer)
  "Filter for slack layout."
  (with-current-buffer buffer
    (eq major-mode 'slack-mode)))

(defun space-macs//slack-buffer-to-persp ()
  "Add buffer to slack layout."
  (persp-add-buffer (current-buffer)))


