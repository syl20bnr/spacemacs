;;; funcs.el --- PureScript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: nobv <6e6f6276@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//purescript-backend ()
  "Returns selected backend."
  (if purescript-backend
      purescript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'psc-ide))))

(defun spacemacs//purescript-setup-backend()
  "Conditionally setup purescript backend."
  (pcase (spacemacs//purescript-backend)
    ('lsp (lsp))))

(defun spacemacs//purescript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//purescript-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    ('lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes purescript-mode))
    ('psc-ide (spacemacs|add-company-backends
                :backends company-psc-ide-backend
                :modes purescript-mode))))
