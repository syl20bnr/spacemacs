;;; config.el --- common-lisp Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers lisp-mode slime-inspect-definition)

;; Company integration
(spacemacs|defvar-company-backends lisp-mode)
(defcustom enable-slime-company t
  "If non nil enables the slime-company package. DEFAULT: t")
