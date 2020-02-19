;;; funcs.el --- rebox layer function file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Christian E. Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun rebox-enable-hook ()
  "Until rebox is fixed for C style comments we have to disable"
  (and (not (or (eq major-mode 'c-mode)
                (eq major-mode 'c++-mode)
                (eq major-mode 'objc-mode)))
       (rebox-mode)))

(defun spacemacs/rebox-dwim-previous ()
  "Cycle to previous style in `rebox-style-loop'."
  (interactive)
  (rebox-dwim '(4)))
