;;; funcs.el --- CMake Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//cmake-backend ()
  "Returns selected backend."
  (if cmake-backend
      cmake-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-cmake))))

(defun space-macs//cmake-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//cmake-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes cmake-mode))
    (`company-cmake (space-macs|add-company-backends
                      :backends company-cmake
                      :modes cmake-mode))))

(defun space-macs//cmake-setup-backend ()
  "Conditionally setup cmake backend."
  (pcase (space-macs//cmake-backend)
    (`lsp (lsp))))


