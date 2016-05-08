;;; packages.el --- prose layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Ewald <chrisewald@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar prose-proselint-enable-by-default t
  "Enable proselint feedback by default")

(defvar-local prose-proselint-enabled prose-proselint-enable-by-default
  "Buffer local proselint enabled status")

;; (length flycheck-checkers)
