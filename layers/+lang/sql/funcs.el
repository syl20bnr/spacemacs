;;; funcs.el --- SQL Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs/sql-populate-products-list (&rest args)
  "Update Spacemacs list of sql products"
  (setq
   spacemacs-sql-highlightable sql-product-alist
   spacemacs-sql-startable (cl-remove-if-not
                            (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                            sql-product-alist)))

(defun spacemacs//sql-setup-company ()
  "Conditionally setup company based on backend."
  (pcase sql-backend
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
  (when (eq sql-backend 'lsp)
    (lsp-deferred)))
