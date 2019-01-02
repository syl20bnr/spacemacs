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


;; lsp

(defun spacemacs//php-setup-backend ()
  "Conditionally setup php backend."
  (pcase php-backend
    (`lsp (spacemacs//php-setup-lsp))))

(defun spacemacs//php-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//php-setup-company ()
  "Conditionally setup company based on backend."
  (message "%s setting up company" php-backend)
  (pcase php-backend
    (`lsp (spacemacs//php-setup-lsp-company))))

(defun spacemacs//php-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes php-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
