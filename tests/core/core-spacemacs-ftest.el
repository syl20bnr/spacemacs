;;; core-spacemacs-ftest.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Alberto Zaccagni <me@lazywithclass.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'core-spacemacs)

(ert-deftest assertion-library-should-work ()
  "the assertion library should works"
  (mocker-let ((foo (n)
                    ((:input '(1) :output 1)))
               (bar (n m)
                    ((:input '(2 2) :output 2))))
              (should (equal (foo 1) 1))
              (should (equal (bar 2 2) 2))))

