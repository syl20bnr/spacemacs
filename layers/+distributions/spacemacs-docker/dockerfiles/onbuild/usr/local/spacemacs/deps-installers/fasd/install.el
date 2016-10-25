#!/usr/bin/emacs --script
;;; install.el --- fasd layer dependencies installation script
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(load "/usr/local/spacemacs/lib/deps-dsl.el" nil t)

(with-build-dir (tfasd "/tmp/fasd/")
  (with-installed (make git)
    ($ "git clone https://github.com/clvv/fasd ."
       "make install")))
