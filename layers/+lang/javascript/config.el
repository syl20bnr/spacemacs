;;; config.el --- Javascript Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|defvar-company-backends js2-mode)

(spacemacs|define-jump-handlers js2-mode)

(defvar javascript-disable-tern-port-files t
  "Stops tern from creating tern port files.")
