;;; core-release-management-ftest.el --- Space-macs Functional Test File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Alberto Zaccagni <me@lazywithclass.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(require 'core-space-macs)

;; ---------------------------------------------------------------------------
;; space-macs//git-has-remote
;; ---------------------------------------------------------------------------

(ert-deftest test-git-has-remote--command-is-working ()
  (should (numberp (space-macs//git-has-remote "origin"))))

(ert-deftest test-git-has-remote--input-is-not-a-remote ()
  (should (equal (space-macs//git-has-remote "clearly-not-a-R3M0T3!") nil)))

;; ---------------------------------------------------------------------------
;; space-macs//git-fetch-tags
;; ---------------------------------------------------------------------------

(ert-deftest test-git-fetch-tags--command-is-working ()
  (should (equal t (space-macs//git-fetch-tags "origin" "master"))))

(ert-deftest test-git-fetch-tags--input-is-not-a-remote ()
  (should (equal nil (space-macs//git-fetch-tags "qwerty" "master"))))


