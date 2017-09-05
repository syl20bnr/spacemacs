;;; config.el --- common-lisp Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers lisp-mode slime-edit-definition)
(spacemacs|define-jump-handlers common-lisp-mode)

(defvar common-lisp-enable-smartparens-mode nil
  "If non nil, smartparens mode will be enabled in SLIME REPL.
Possible values are `strict' or `normal'.")
