;;; config.el --- Ruby Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|defvar-company-backends enh-ruby-mode)
(spacemacs|defvar-company-backends ruby-mode)

(defvar ruby-enable-enh-ruby-mode nil
  "If non-nil, use `enh-ruby-mode' package instead of the built-in Ruby Mode.")

(defvar ruby-version-manager nil
  "If non nil, defines the Ruby version manager.
Possible values are `rbenv', `rvm' or `chruby'.)")

(defvar ruby-test-runner 'ruby-test
  "Test runner to use. Possible values are `ruby-test' or `rspec'.")

;; Command prefixes

(spacemacs/declare-prefix-for-mode 'ruby-mode "mt" "ruby/test")
