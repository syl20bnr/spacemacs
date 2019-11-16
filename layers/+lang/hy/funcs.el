;;; funcs.el --- Hy Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; REPL

(defun spacemacs/hy-shell-eval-buffer-and-go ()
  "Send current buffer to REPL and focus it."
  (interactive)
  (hy-shell-eval-buffer)
  (run-hy))

(defun spacemacs/hy-shell-eval-current-form-and-go ()
  "Send current form to REPL and focus it."
  (interactive)
  (hy-shell-eval-current-form)
  (run-hy))

(defun spacemacs/hy-shell-eval-region-and-go ()
  "Send region to REPL and focus it."
  (interactive)
  (hy-shell-eval-region)
  (run-hy))
