;;; funcs.el --- Dart Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend

(defun spacemacs//dart-backend ()
  "Returns selected backend."
  (if dart-backend
      dart-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'analyzer))))

(defun spacemacs//dart-setup-backend ()
  (pcase (spacemacs//dart-backend)
    (`analyzer (spacemacs//dart-setup-analyzer))
    (`lsp (spacemacs//dart-setup-lsp))))

(defun spacemacs//dart-setup-company ()
  (pcase (spacemacs//dart-backend)
    (`lsp (spacemacs//dart-setup-company-lsp))))


;; analyzer

(defun spacemacs//dart-setup-analyzer ()
  (dart-server))


;; lsp

(defun spacemacs//dart-setup-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//dart-setup-company-lsp ()
  "Setup lsp auto-completion"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes dart-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
