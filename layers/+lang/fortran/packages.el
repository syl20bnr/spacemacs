;;; packages.el --- fortran layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Halvor Lund <halvor.lund@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst fortran-packages '(fortpy))

(defun fortran/init-fortpy ()
  (use-package fortpy
    :defer t
    :init
    (add-hook 'f90-mode-hook 'fortpy-setup)
    :config
    (progn
      (setq fortpy-complete-on-percent t)
      (setq fortpy-complete-on-bracket t))))
