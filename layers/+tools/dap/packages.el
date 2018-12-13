;;; packages.el --- DAP mode functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Ivan Yonchovski (yyoncho@gmail.com)
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst dap-packages
  '(dap-mode))

(defun dap/init-dap-mode ()
  (use-package dap-mode
    :after (lsp-mode)
    :config
    (dap-mode 1)
    (dap-ui-mode 1)))
