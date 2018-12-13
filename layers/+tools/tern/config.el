;;; config.el --- Tern Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar tern-disable-port-files t
  "Stops tern from creating tern port files.")

(defvar tern--key-bindings-modes nil
  "List of major modes where Tern key-bindings must be defined.")
