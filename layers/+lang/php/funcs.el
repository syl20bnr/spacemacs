;;; funcs.el --- PHP Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//php-setup-backend ()
  "Conditionally setup php backend."
  (pcase php-backend
    (`lsp (lsp))))

(defun spacemacs//php-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase php-backend
    (`lsp (spacemacs//php-setup-lsp-dap))))

(defun spacemacs//php-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-php))
