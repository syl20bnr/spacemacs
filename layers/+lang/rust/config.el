;;; config.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(defvar rust-enable-rustfmt-on-save nil
  "If non-nil, automatically format code with rustfmt on save.")
