;;; funcs.el --- Javascript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun javascript//tern-detect ()
  "Detect tern binary and warn if not found."
  (if (executable-find "tern")
      t
    (unless javascript-disable-tern-missing-warning
      (warn "tern binary not found"))))
