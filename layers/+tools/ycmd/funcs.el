;;; funcs.el --- Ycmd Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ycmd/manual-semantic-company-completer ()
  "A useful function that can be bound, if users prefer to trigger company
completion manually"

  (interactive)
  (company-cancel)
  (let ((ycmd-force-semantic-completion (not (company-ycmd--in-include))))
    (setq company-backend 'company-ycmd)
    (company-manual-begin)))
