;;; packages.el --- graphql layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst graphql-packages
  '(company
    prettier-js
    graphql-mode))

(defun graphql/init-graphql-mode ()
  (use-package graphql-mode
    :defer t
    :init
    (progn
      (add-to-list 'space-macs-jump-handlers-graphql-mode 'ahs-backward-definition)
      (when (configuration-layer/layer-used-p 'prettier)
        (space-macs/declare-prefix-for-mode 'graphql-mode "m=" "format"))
      (space-macs/declare-prefix-for-mode 'graphql-mode "mg" "goto")
      (space-macs/set-leader-keys-for-major-mode 'graphql-mode
        "s" 'graphql-send-query
        "e" 'graphql-select-endpoint
        "h" 'graphql-edit-headers))))

(defun graphql/post-init-company ()
  (space-macs|add-company-backends
    :backends company-dabbrev
    :modes graphql-mode))

(defun graphql/pre-init-prettier-js ()
  (add-to-list 'space-macs--prettier-modes 'graphql-mode))


