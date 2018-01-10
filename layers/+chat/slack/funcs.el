;;; funcs.el --- slack layer functions file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; persp

(defun spacemacs//slack-persp-filter-save-buffers-function (buffer)
  "Filter for slack layout."
  (with-current-buffer buffer
    (eq major-mode 'slack-mode)))

(defun spacemacs//slack-buffer-to-persp ()
  "Add buffer to slack layout."
  (persp-add-buffer (current-buffer)))
