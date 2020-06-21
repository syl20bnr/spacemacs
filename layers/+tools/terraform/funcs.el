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
    nil))

(defun spacemacs//terraform-setup-backend ()
  "Conditionally setup terraform backend."
  (pcase (spacemacs//terraform-backend)
    (`lsp (spacemacs//terraform-setup-lsp))))

;; lsp

(defun spacemacs//terraform-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (when (eq terraform-lsp-server 'terraform-lsp)
          (require 'lsp-terraform))
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
