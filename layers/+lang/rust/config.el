;;; config.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;

;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

;; Define the buffer local company backend variable
(spacemacs|defvar-company-backends rust-mode)

(defvar rust-enable-racer nil
  "If non-nil, load the racer package (this has an external dependency).")
