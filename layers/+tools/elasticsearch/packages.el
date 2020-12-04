;;; packages.el --- elasticsearch layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Jean Rigotti <gmgotti@pm.me>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst elasticsearch-packages
  '(
    company
    org
    es-mode
    ))

(defun elasticsearch/init-es-mode ()
  (use-package es-mode
    :defer t
    :mode ("\\.es\\'" . es-mode)))

(defun elasticsearch/post-init-company ()
  (space-macs|add-company-backends :backends es-company-backend :modes es-mode))

(defun elasticsearch/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(elasticsearch . t))))


