;;; configuration-layer.el --- Spacemacs Unit Test File
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
(require 'configuration-layer)

;; ---------------------------------------------------------------------------
;; configuration-layer//declare-layers
;; ---------------------------------------------------------------------------

(ert-deftest test-declare-layers--result-order-is-reversed ()
  (mocker-let ((configuration-layer//declare-layer
                (x)
                ((:input '(layer1) :output 'layer1)
                 (:input '(layer2) :output 'layer2)
                 (:input '(layer3) :output 'layer3))))
    (let* ((input '(layer1 layer2 layer3))
           (result (configuration-layer//declare-layers input)))
      (should (equal (reverse result) input)))))

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
  (let ((input '(testlayer :variables ((var1 t)
                                       (var2 nil))
                           :excluded (excludedlayer))))
    (mocker-let ((configuration-layer/get-layer-path
                  (x)
                  ((:input `(,(car input)) :output "/a/dummy/path/"))))
      (let ((result (configuration-layer//declare-layer input))
            (expected '(testlayer :dir "/a/dummy/path/testlayer/"
                                  :ext-dir "/a/dummy/path/testlayer/extensions/"
                                  :variables ((var1 t) (var2 nil))
                                  :excluded (excludedlayer)
                                  )))
        (should (equal result expected))))))
