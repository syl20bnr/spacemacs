;;; funcs.el --- vimscript Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//vimscript-backend ()
  "Returns selected backend."
  (if vimscript-backend
      vimscript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-vimscript))))

(defun space-macs//vimscript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//vimscript-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes vimrc-mode))))

(defun space-macs//vimscript-setup-backend ()
  "Conditionally setup vimscript backend."
  (pcase (space-macs//vimscript-backend)
    (`lsp (lsp))))


