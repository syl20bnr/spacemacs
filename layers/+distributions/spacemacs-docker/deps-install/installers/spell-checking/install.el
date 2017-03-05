#!/usr/bin/emacs --script
;;; install.el --- spell-checking layer dependencies installation script
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(load (expand-file-name "../../lib/deps-install-helpers.el"
                        (file-name-directory
                         load-file-name)) nil t)

(install ispell)
