;;; config.el --- Evernote Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(setq evernote/key-binding-prefixes '(("ae" . "applications-evernote")))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      evernote/key-binding-prefixes)
