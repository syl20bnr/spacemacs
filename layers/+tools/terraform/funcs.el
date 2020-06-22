;;; funcs.el --- Terraform Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//terraform-backend ()
  "Returns selected backend."
  (if terraform-backend
      terraform-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-terraform))))

(defun spacemacs//terraform-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//terraform-backend)
    ('company-terraform (spacemacs|add-company-backends
                          :backends company-terraform
                          :modes terraform-mode))
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes terraform-mode))))


(defun spacemacs//terraform-setup-backend ()
  "Conditionally setup terraform backend."
  (pcase (spacemacs//terraform-backend)
    (`lsp (lsp))))
