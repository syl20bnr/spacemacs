;;; funcs.el --- Terraform Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//terraform-backend ()
  "Returns selected backend."
  (if terraform-backend
      terraform-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-terraform))))

(defun space-macs//terraform-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//terraform-backend)
    ('company-terraform (space-macs|add-company-backends
                          :backends company-terraform
                          :modes terraform-mode))
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes terraform-mode))))


(defun space-macs//terraform-setup-backend ()
  "Conditionally setup terraform backend."
  (pcase (space-macs//terraform-backend)
    (`lsp (lsp))))


