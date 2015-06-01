;;; core-configuration-layer-utest.el --- Spacemacs Unit Test File
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
(require 'mocker)
(require 'core-configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--result-order-is-not-reversed ()
  (mocker-let ((configuration-layer//declare-layer
                (x)
                ((:input '(layer3) :output 'layer3)
                 (:input '(layer2) :output 'layer2)
                 (:input '(layer1) :output 'layer1))))
    (let* ((input '(layer1 layer2 layer3))
           (result (configuration-layer//declare-layers input)))
      (should (equal result input)))))

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-layer
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--input-is-a-symbol ()
  (let ((input 'testlayer))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,input) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer//declare-layer input))
            (expected '(testlayer :dir "/a/dummy/path/testlayer/"
                                  :ext-dir "/a/dummy/path/testlayer/extensions/")))
        (should (equal result expected))))))

(ert-deftest test-declare-layers--input-is-a-list ()
  (let ((input '(testlayer :variables
                           var1 t
                           var2 nil
                           :excluded-packages
                           excludedlayer)))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,(car input)) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer//declare-layer input))
            (expected '(testlayer :dir "/a/dummy/path/testlayer/"
                                  :ext-dir "/a/dummy/path/testlayer/extensions/"
                                  :variables
                                  var1 t
                                  var2 nil
                                  :excluded-packages
                                  excludedlayer
                                  )))
        (should (equal result expected))))))

;; ---------------------------------------------------------------------------
;; configuration-layer//set-layers-variables
;; ---------------------------------------------------------------------------

(ert-deftest test-set-layers-variables--none ()
  (let ((input '((testlayer :dir "/a/path/"
                            :ext-dir "/a/path/extensions/")))
        (var 'foo))
    (configuration-layer//set-layers-variables input)
    (should (eq var 'foo))))

(ert-deftest test-set-layers-variables--one-value ()
  (let ((input '((testlayer :dir "/a/path/"
                            :ext-dir "/a/path/extensions/"
                            :variables
                            var1 'bar))))
    (setq var1 'foo)
    (configuration-layer//set-layers-variables input)
    (should (eq var1 'bar))))

(ert-deftest test-set-layers-variables--multiple-values ()
  (let ((input '((testlayer :dir "/a/path/"
                            :ext-dir "/a/path/extensions/"
                            :variables
                            var1 'bar1
                            var2 'bar2
                            var3 'bar3))))
    (setq var1 'foo)
    (setq var2 'foo)
    (setq var3 'foo)
    (configuration-layer//set-layers-variables input)
    (should (eq var1 'bar1))
    (should (eq var2 'bar2))
    (should (eq var3 'bar3))))

(ert-deftest test-set-layers-variables--odd-number-of-values ()
  (let ((input '((testlayer :dir "/a/path/"
                            :ext-dir "/a/path/extensions/"
                            :variables
                            var1 'bar
                            var2))))
    (mocker-let
     ((spacemacs-buffer/warning
       (msg &rest args)
       ((:record-cls 'mocker-stub-record :output nil :occur 1))))
     (setq var1 'foo)
     (setq var2 'foo)
     (configuration-layer//set-layers-variables input)
     (should (eq var1 'bar))
     (should (eq var2 'foo)))))
