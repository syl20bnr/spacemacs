;;; config.el --- Colors Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar colors-enable-rainbow-identifiers nil
  "if non nil the `rainbow-identifers' package is enabled.")

(defvar colors-enable-nyan-cat-progress-bar nil
  "if non nil all nyan cat packges are enabled (for now only `nyan-mode').")

;; Command prefixes

(when colors-enable-rainbow-identifiers
  (setq colors/key-binding-prefixes '(("Ci" . "colors-identifiers")))
  (mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
        colors/key-binding-prefixes))
