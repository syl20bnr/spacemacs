;;; funcs.el --- Shell Scripts Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//shell-scripts-backend ()
  "Returns selected backend."
  ;; backend must be choosed explicitly with this layer
  shell-scripts-backend)

(defun spacemacs//shell-scripts-setup-backend ()
  "Conditionally setup shell-scripts backend."
  (pcase (spacemacs//shell-scripts-backend)
    (`lsp (spacemacs//shell-scripts-setup-lsp))))


;; lsp

(defun spacemacs//shell-scripts-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; shebang

(defun spacemacs/insert-shebang ()
  "Insert shebang line at the top of the file."
  (interactive)
  (require 'insert-shebang)
  (insert-shebang-get-extension-and-insert
   (file-name-nondirectory (buffer-file-name))))

