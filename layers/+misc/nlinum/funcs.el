;;; funcs.el --- nlinum Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/nlinum-maybe-on ()
  (when (spacemacs/enable-line-numbers-p)
    (nlinum-mode)))

(defun spacemacs/nlinum-relative-toggle ()
  (interactive)
  (if (not (bound-and-true-p nlinum-relative-mode))
      (nlinum-mode))
  (nlinum-relative-toggle))
