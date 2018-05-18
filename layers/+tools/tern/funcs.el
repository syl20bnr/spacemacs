;;; funcs.el --- Tern Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/tern-setup-tern ()
  "Setup tern backend.
Must be called by a layer using tern."
  (require 'tern)
  (when tern-disable-port-files
    (add-to-list 'tern-command "--no-port-file" 'append))
  (tern-mode))

(defun spacemacs/tern-setup-tern-company (&rest modes)
  "Setup tern auto-completion for given MODES.
Must be called by a layer using tern."
  (eval `(spacemacs|add-company-backends
           :backends company-tern
           :modes ,@modes
           :append-hooks nil
           :call-hooks t))
  (company-mode)
  (tern-mode))
