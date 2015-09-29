;;; config.el --- Github configuration File for Spacemacs
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

;; Command prefixes

(setq github/key-binding-prefixes '(("gf" . "github/file")
                                    ("gg" . "github/gist")))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      github/key-binding-prefixes)
