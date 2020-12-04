;;; packages.el --- import-js Layer packages file for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq import-js-packages '(import-js))

(defun import-js/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (dolist (x space-macs--import-js-modes)
      (add-hook (cdr x) #'run-import-js)
      (space-macs/declare-prefix-for-mode (car x) "mi" "import")
      (space-macs/set-leader-keys-for-major-mode (car x)
        "if" #'space-macs/import-js-fix
        "ii" #'space-macs/import-js-import
        "ig" #'import-js-goto))))


