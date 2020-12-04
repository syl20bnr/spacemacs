;;; funcs.el --- kotlin Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//kotlin-backend ()
  "Returns selected backend."
  (if kotlin-backend
      kotlin-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-kotlin))))

(defun space-macs//kotlin-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//kotlin-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes kotlin-mode))))

(defun space-macs//kotlin-setup-backend ()
  "Conditionally setup kotlin backend."
  (pcase (space-macs//kotlin-backend)
    (`lsp (lsp))))


