;;; config.el --- elm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|defvar-company-backends elm-mode)

(setq elm/key-binding-prefixes '(("mR" . "reactor")
                                 ("mc" . "compile")
                                 ("mh" . "help")
                                 ("mp" . "package")
                                 ("ms" . "repl")))

(mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                   'elm-mode (car x) (cdr x)))
      elm/key-binding-prefixes)
