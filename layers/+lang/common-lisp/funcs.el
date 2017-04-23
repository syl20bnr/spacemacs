;;; funcs.el --- Slime Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; Helm integration

(defun spacemacs//slime-helm-source (&optional table)
  (or table (setq table slime-lisp-implementations))
  `((name . "Slime")
    (candidates . ,(mapcar #'car table))
    (action . (lambda (candidate)
                (car (helm-marked-candidates))))))

(defun spacemacs/helm-slime ()
  (interactive)
  (let ((command (helm :sources (spacemacs//slime-helm-source))))
    (and command (slime (intern command)))))


;; Evil integration

(defun spacemacs/slime-eval-sexp-end-of-line ()
  "Evaluate current line."
  (interactive)
  (move-end-of-line 1)
  (slime-eval-last-expression))
