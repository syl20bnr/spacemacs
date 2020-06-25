;;; funcs.el --- SQL Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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

(defun spacemacs//sql-backend ()
  "Returns selected backend."
  (if sql-backend
      sql-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-sql))))

(defun spacemacs//sql-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//sql-backend)
    ('company-sql (spacemacs|add-company-backends
                    :backends company-capf
                    :modes sql-mode))
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes sql-mode))))

(defun spacemacs//sql-setup-backend ()
  "Conditionally setup sql backend."
  (pcase (spacemacs//sql-backend)
    (`lsp (lsp))))
