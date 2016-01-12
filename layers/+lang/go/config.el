;;; packages.el --- Go Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|defvar-company-backends go-mode)

(defvar go-use-gocheck-for-testing nil
  "If using gocheck for testing when running the tests -check.f will be used instead of -run to specify the test that will be ran. Gocheck is mandatory for testing suites.")
