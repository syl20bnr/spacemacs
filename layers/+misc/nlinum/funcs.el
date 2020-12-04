;;; funcs.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thomas de BeauchÃªne <thomas.de.beauchene@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/nlinum-maybe-on ()
  (when (space-macs/enable-line-numbers-p)
    (nlinum-mode)))

(defun space-macs/nlinum-relative-toggle ()
  (interactive)
  (if (not (bound-and-true-p nlinum-relative-mode))
      (nlinum-mode))
  (nlinum-relative-toggle))


