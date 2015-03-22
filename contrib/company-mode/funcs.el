;;; funcs.el --- Company Layer functions File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/company-backend-with-yas (backend)
  "Return BACKEND with support for yasnippet candidates."
  (if (and (configuration-layer/package-declaredp 'yasnippet)
           company-mode-enable-yas
           (not (eq 'company-semantic backend)))
      (unless (and (listp backend) (member 'company-yasnippet backend))
        (append (if (listp backend) backend (list backend))
                (list :with 'company-yasnippet)))
        ;; (cons backend '(company-yasnippet)))
    backend))

