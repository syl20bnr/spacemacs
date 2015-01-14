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

;; ---------------------------------------------------------------------------
;; Prefixes 
;; ---------------------------------------------------------------------------

(setq spacemacs/key-binding-prefixes '(("C" .  "colors")
                                       ("Ci" . "colors-identifiers")
                                       ("tC" . "toggles-colors")))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      spacemacs/key-binding-prefixes)
