;;; funcs.el --- Hy Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; REPL

(defun space-macs/hy-shell-eval-buffer-and-go ()
  "Send current buffer to REPL and focus it."
  (interactive)
  (hy-shell-eval-buffer)
  (run-hy))

(defun space-macs/hy-shell-eval-current-form-and-go ()
  "Send current form to REPL and focus it."
  (interactive)
  (hy-shell-eval-current-form)
  (run-hy))

(defun space-macs/hy-shell-eval-region-and-go ()
  "Send region to REPL and focus it."
  (interactive)
  (hy-shell-eval-region)
  (run-hy))


