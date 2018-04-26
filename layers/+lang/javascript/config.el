;;; config.el --- Javascript Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|define-jump-handlers js2-mode)

(defvar javascript-disable-tern-port-files t
  "Stops tern from creating tern port files.")

(defvar javascript-backend 'tern
  "The backend to use for IDE features. Possible values are `tern'
and `lsp'.")
