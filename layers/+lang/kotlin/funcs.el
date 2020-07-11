;;; funcs.el --- kotlin Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//kotlin-backend ()
  "Returns selected backend."
  (if kotlin-backend
      kotlin-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-kotlin))))

(defun spacemacs//kotlin-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//kotlin-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes kotlin-mode))))

(defun spacemacs//kotlin-setup-backend ()
  "Conditionally setup kotlin backend."
  (pcase (spacemacs//kotlin-backend)
    (`lsp (lsp))))
