;;; extensions.el --- Clojure Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Pre extensions are loaded *before* the packages
(defvar clojure-pre-extensions '())

;; Post extensions are loaded *after* the packages
(defvar clojure-post-extensions '(cider-eval-sexp-fu))

(defun clojure/init-cider-eval-sexp-fu ()
  (eval-after-load 'eval-sexp-fu
    '(require 'cider-eval-sexp-fu)))
