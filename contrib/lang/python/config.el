;;; packages.el --- Python Layer packages File for Spacemacs
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

(setq python/key-binding-prefixes
      '(; django/pony-mode
        ("mj" . "django")
        ("mjr" . "runserver")
        ("mji" . "shells")
        ("mjs" . "south/syncdb")
        ("mjt" . "test")
        ("mjf" . "files")
        ("mja" . "fabric")
        ; pyenv/pyenv-mode
        ("mp" . "pyenv")))

(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      python/key-binding-prefixes)
