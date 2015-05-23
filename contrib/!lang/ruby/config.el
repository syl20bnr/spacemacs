;;; config.el --- Ruby Layer configuration File for Spacemacs
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

;; Variables

(spacemacs|defvar-company-backends enh-ruby-mode)

(defvar ruby-version-manager nil
  "If non nil defines the Ruby version manager (i.e. rbenv, rvm)")

(defvar ruby-enable-ruby-on-rails-support nil
  "If non nil we'll load support for Rails (haml, features, navigation)")

;; not supported for now
;; (setq ruby/key-binding-prefixes
;;       '(("mr" . "RoR")
;;         ("mrc" . "RoR-compile/generate")
;;         ("mrg" . "RoR-goto")
;;         ("mrf" . "RoR-find")
;;         ("mrr" . "RoR-rake")
;;         ("mrR" . "RoR-refactoring")
;;         ("mrs" . "RoR-REPL")
;;         ("mrx" . "RoR-run")))
;; (mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
;;       ruby/key-binding-prefixes)
