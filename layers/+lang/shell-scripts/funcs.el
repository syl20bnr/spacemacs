;;; funcs.el --- Shell Scripts Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; shebang

(defun spacemacs/insert-shebang ()
  "Insert shebang line at the top of the file."
  (interactive)
  (require 'insert-shebang)
  (insert-shebang-get-extension-and-insert
   (file-name-nondirectory (buffer-file-name))))


;; lsp

(defun spacemacs//shell-scripts-setup-backend ()
  "Conditionally setup shell-scripts backend."
  (pcase shell-scripts-backend
    (`lsp (spacemacs//shell-scripts-setup-lsp))))

(defun spacemacs//shell-scripts-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//shell-scripts-setup-company ()
  "Conditionally setup company based on backend."
  (message "%s setting up company" shell-scripts-backend)
  (pcase shell-scripts-backend
    (`lsp (spacemacs//shell-scripts-setup-lsp-company))))

(defun spacemacs//shell-scripts-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes sh-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
