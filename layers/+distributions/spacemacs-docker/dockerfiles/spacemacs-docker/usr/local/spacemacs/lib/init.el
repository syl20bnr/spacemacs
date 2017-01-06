;;; init.el --- Dependency initialization for Spacemacs-docker
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'package)
(package-initialize)
(let ((package-archives '(("melpa" . "https://melpa.org/packages/")))
      (package-user-dir "usr/local/spacemacs/lib"))
  (package-refresh-contents)
  (package-install 's)
  (package-install 'dash)
  (package-install 'f))
