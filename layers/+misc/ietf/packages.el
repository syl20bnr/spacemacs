;;; packages.el --- ietf layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst ietf-packages '(ietf-docs
                          (irfc :location local)
                          (ox-rfc :requires org)))

(defun ietf/init-ietf-docs ()
  (use-package ietf-docs
    :commands ietf-docs-open-at-point
    :init
    (progn
      (spacemacs/set-leader-keys "f I" 'ietf-docs-open-at-point)
      (setq ietf-docs-cache-directory ietf-docs-cache))))

(defun ietf/init-irfc ()
  (use-package irfc
    :commands irfc-mode
    :init
    (progn
      (setq irfc-directory ietf-docs-cache)
      (setq irfc-assoc-mode t)
      (add-to-list 'auto-mode-alist
                   '("/draft-\\([a-z0-9_]+-\\)+[0-9]+.txt" . irfc-mode))
      (add-to-list 'auto-mode-alist
                   '("/draft-\\([a-z0-9_]+-\\)+[a-z0-9_]+.txt" . irfc-mode))
      (add-to-list 'auto-mode-alist
                   '("/rfc-\\([a-z0-9_]+-\\).txt" . irfc-mode)))))



(defun ietf/pre-init-ox-rfc ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-rfc)))
(defun ietf/init-ox-rfc ())


;;; packages.el ends here
