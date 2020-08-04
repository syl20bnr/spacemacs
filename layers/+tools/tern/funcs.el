;;; funcs.el --- Tern Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
