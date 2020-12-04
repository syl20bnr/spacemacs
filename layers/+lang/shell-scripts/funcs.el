;;; funcs.el --- Shell Scripts Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//shell-scripts-backend ()
  "Returns selected backend."
  ;; backend must be choosed explicitly with this layer
  shell-scripts-backend)

(defun space-macs//shell-scripts-setup-backend ()
  "Conditionally setup shell-scripts backend."
  (pcase (space-macs//shell-scripts-backend)
    (`lsp (space-macs//shell-scripts-setup-lsp))))


;; lsp

(defun space-macs//shell-scripts-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; shebang

(defun space-macs/insert-shebang ()
  "Insert shebang line at the top of the file."
  (interactive)
  (require 'insert-shebang)
  (insert-shebang-get-extension-and-insert
   (file-name-nondirectory (buffer-file-name))))



