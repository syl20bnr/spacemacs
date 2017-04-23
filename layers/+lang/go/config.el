;;; config.el --- Go Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers go-mode godef-jump)

(defvar go-use-gocheck-for-testing nil
  "If using gocheck for testing when running the tests -check.f will be used instead of -run to specify the test that will be ran. Gocheck is mandatory for testing suites.")

(defvar go-tab-width 8
  "Set the `tab-width' in Go mode. Default is 8.")

(defvar go-use-gometalinter nil
  "Use gometalinter if the variable has non-nil value.")

(defvar go-test-buffer-name "*go test*"
  "Name of the buffer for go test output. Default is *go test*.")
