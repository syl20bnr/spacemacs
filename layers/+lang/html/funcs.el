;;; funcs.el --- HTML Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/emmet-expand ()
  (interactive)
  (if (bound-and-true-p yas-minor-mode)
      (call-interactively 'emmet-expand-yas)
    (call-interactively 'emmet-expand-line)))

;; see https://github.com/osv/company-web/issues/4
(defun spacemacs//company-web-minimum-prefix-length ()
  "Set company minimum prefix length to 0 for the current buffer only."
  (set (make-local-variable 'company-minimum-prefix-length) 0))
