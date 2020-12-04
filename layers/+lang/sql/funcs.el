;;; funcs.el --- SQL Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/sql-populate-products-list (&rest args)
  "Update Space-macs list of sql products"
  (setq
   space-macs-sql-highlightable sql-product-alist
   space-macs-sql-startable (remove-if-not
                            (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                            sql-product-alist)))

(defun space-macs//sql-backend ()
  "Returns selected backend."
  (if sql-backend
      sql-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-sql))))

(defun space-macs//sql-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//sql-backend)
    ('company-sql (space-macs|add-company-backends
                    :backends company-capf
                    :modes sql-mode))
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes sql-mode))))

(defun space-macs//sql-setup-backend ()
  "Conditionally setup sql backend."
  (pcase (space-macs//sql-backend)
    (`lsp (lsp))))


