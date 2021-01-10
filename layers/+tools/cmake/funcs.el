;;; funcs.el --- CMake Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//cmake-backend ()
  "Returns selected backend."
  (if cmake-backend
      cmake-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-cmake))))

(defun spacemacs//cmake-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//cmake-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes cmake-mode))
    (`company-cmake (spacemacs|add-company-backends
                      :backends company-cmake
                      :modes cmake-mode))))

(defun spacemacs//cmake-setup-backend ()
  "Conditionally setup cmake backend."
  (pcase (spacemacs//cmake-backend)
    (`lsp (lsp))))
