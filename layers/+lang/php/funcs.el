;;; funcs.el --- PHP Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//php-backend ()
  "Returns selected backend."
  (if php-backend
      php-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t nil))))

(defun spacemacs//php-setup-backend ()
  "Conditionally setup php backend."
  (pcase (spacemacs//php-backend)
    (`lsp (spacemacs//php-setup-lsp))))


;; lsp

(defun spacemacs//php-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
