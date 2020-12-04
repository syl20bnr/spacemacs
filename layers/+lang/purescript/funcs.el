;;; funcs.el --- PureScript Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: nobv <6e6f6276@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//purescript-backend ()
  "Returns selected backend."
  (if purescript-backend
      purescript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'psc-ide))))

(defun space-macs//purescript-setup-backend()
  "Conditionally setup purescript backend."
  (pcase (space-macs//purescript-backend)
    ('lsp (lsp))))

(defun space-macs//purescript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//purescript-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    ('lsp (space-macs|add-company-backends
            :backends company-capf
            :modes purescript-mode))
    ('psc-ide (space-macs|add-company-backends
                :backends company-psc-ide-backend
                :modes purescript-mode))))


