;;; config.el --- Ruby Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|define-jump-handlers enh-ruby-mode)
(spacemacs|define-jump-handlers ruby-mode)

(defvar ruby-backend 'robe
  "Defines the backend for IDE features, defaulting to robe.
Possible values are `robe', and `lsp'.
If `nil' then `robe' is the default backend unless `lsp' layer is used.")

(defvar ruby-enable-enh-ruby-mode nil
  "If non-nil, use `enh-ruby-mode' package instead of the built-in Ruby Mode.")

(defvar ruby-version-manager nil
  "If non nil, defines the Ruby version manager.
Possible values are `rbenv', `rvm' or `chruby'.)")

(defvar ruby-test-runner 'ruby-test
  "Test runner to use. Possible values are `ruby-test', `minitest' or `rspec'.")

(defvar ruby-highlight-debugger-keywords t
  "If non-nil, enable highlight for debugger keywords.")
