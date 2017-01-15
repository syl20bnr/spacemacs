;;; layers.el --- MongoDB layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq inf-mongo-packages '(inf-mongo))

(defun inf-mongo/init-inf-mongo ()
  (use-package inf-mongo
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'inf-mongo-mode
        "'" 'inf-mongo
        "b" 'mongo-send-buffer
        "B" 'mongo-send-buffer-and-go
        "s" 'mongo-send-last-sexp
        "S" 'mongo-send-last-sexp-and-go
        "r" 'mongo-send-region
        "R" 'mongo-send-region-and-go
        ))))
