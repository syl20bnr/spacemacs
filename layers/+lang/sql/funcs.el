;;; funcs.el --- SQL Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/sql-populate-products-list (&rest args)
  "Update Spacemacs list of sql products"
  (setq
   spacemacs-sql-highlightable sql-product-alist
   spacemacs-sql-startable (remove-if-not
                            (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                            sql-product-alist)))
