;;; spacebind-utest.el --- Spacemacs Unit Test File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-spacebind)
(require 'cl-lib)

;;;; Helpers:
(defmacro test-spacebind|process-bind-stack-called-p (&rest body)
  "Mocks `spacebind//process-bind-stack' and returns true if it was called.
NOTE: `spacebind--eager-bind' set to true. "
  `(cl-letf* ((spacebind--eager-bind t)
              (spacebind--bs-minor-mode-replacements '())
              (spacebind--bs-major-mode-replacements '())
              (spacebind--bs-declare-prefix '())
              (spacebind--bs-declare-prefix-for-mode '())
              (spacebind--bs-leader-keys '())
              (spacebind--bs-leader-keys-for-major-mode '())
              (spacebind--bs-leader-keys-for-minor-mode '())
              (spacebind--bs-global-replacements '())
              (spacebind--bs-fn-key-seq-override '())
              (spacebind--timer [t])
              (called nil)
              ((symbol-function 'spacebind//process-bind-stack)
               (lambda () (setq called t))))
     ,@body
     called))

;;;; Tests:
(ert-deftest test-spacebind-sanity-check ()
  (thread-last (spacemacs|spacebind
                :major
                (python-mode
                 "with a description"
                 ("c" "compile/execute"
                  ("c" spacemacs/python-execute-file "execute file"))))
    (test-spacebind|process-bind-stack-called-p)
    (eq t)
    (should)))

(ert-deftest test-spacebind-use-package-integration-works ()
  (thread-last (use-package use-package
                 :spacebind
                 (:major
                  (python-mode
                   "with a description"
                   ("c" "compile/execute"
                    ("c" spacemacs/python-execute-file "execute file")))))
    (test-spacebind|process-bind-stack-called-p)
    (eq t)
    (should)))
