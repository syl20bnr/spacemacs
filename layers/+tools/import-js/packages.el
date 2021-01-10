;;; packages.el --- import-js Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq import-js-packages '(import-js))

(defun import-js/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (dolist (x spacemacs--import-js-modes)
      (add-hook (cdr x) #'run-import-js)
      (spacemacs/declare-prefix-for-mode (car x) "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode (car x)
        "if" #'spacemacs/import-js-fix
        "ii" #'spacemacs/import-js-import
        "ig" #'import-js-goto))))
