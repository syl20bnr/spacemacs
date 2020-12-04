;;; funcs.el --- PHP Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//php-setup-backend ()
  "Conditionally setup php backend."
  (pcase php-backend
    (`lsp (lsp))))

(defun space-macs//php-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase php-backend
    (`lsp (space-macs//php-setup-lsp-dap))))

(defun space-macs//php-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-php))


