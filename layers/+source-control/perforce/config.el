;;; config.el --- Perforce Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq perforce/key-binding-prefixes '(("p4" . "perforce")))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      perforce/key-binding-prefixes)
