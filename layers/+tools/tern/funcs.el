;;; funcs.el --- Tern Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/tern-setup-tern ()
  "Setup tern backend.
Must be called by a layer using tern."
  (require 'tern)
  (when tern-disable-port-files
    (add-to-list 'tern-command "--no-port-file" 'append))
  (tern-mode))


