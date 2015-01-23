;;; spacemacs-mode-test.el --- Spacemacs Functional Test File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Alberto Zaccagni & Contributors
;;
;; Author: Alberto Zaccagni <me@lazywithclass.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'spacemacs-mode)
 
(ert-deftest assertion-library-should-work ()
  "the assertion library should works"
  (mocker-let ((foo (n)
                    ((:input '(1) :output 1)))
               (bar (n m)
                    ((:input '(2 2) :output 2))))
              (should (equal (foo 1) 1))
              (should (equal (bar 2 2) 2))))
 
(ert-deftest git-has-remote ()
  (should (equal (spacemacs/git-has-remote "clearly-not-a-R3M0T3!") nil))
  (should (numberp (spacemacs/git-has-remote "origin"))))
 
(ert-deftest git-fetch-tags ()
  (mocker-let ((process-file (git infile buffer display action &rest args)
                             :ordered nil
                             ((:input '("git" nil "git-fetch-tags" nil "fetch" "--tags" "origin" "master")
                                      :output 0)
                              (:input '("git" nil "git-fetch-tags" nil "fetch" "origin" "master")
                                      :output 0)))
               (kill-buffer (buffer)
                            ((:input '("git-fetch-tags")
                                     :output 0))))
              (should (equal (spacemacs/git-fetch-tags "origin" "master") t))))
