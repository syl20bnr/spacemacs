;;; packages.el --- Thrift Layer packages File for Spacemacs
;;; License: GPLv3

(setq thrift-packages '(thrift))

(defun thrift/init-thrift ()
  (use-package thrift
               :defer t
               :mode ("\\.thrift\\'" . thrift-mode)))
