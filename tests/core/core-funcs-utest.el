;;; core-funcs-utest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'core-funcs)

;; ---------------------------------------------------------------------------
;; spacemacs/mplist-get-values
;; ---------------------------------------------------------------------------

(ert-deftest test-mplist-get-values--one-property ()
  (let* ((input '(dummy :property sym1 sym2 sym3))
         (result (spacemacs/mplist-get-values input :property)))
    (should (equal '(sym1 sym2 sym3) result))))

(ert-deftest test-mplist-get-values--multiple-properties-get-first ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 :prop3 sym5 sym6))
         (result (spacemacs/mplist-get-values input :prop1)))
    (should (equal '(sym1 sym2 sym3) result))))

(ert-deftest test-mplist-get-values--multiple-properties-get-middle ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 :prop3 sym5 sym6))
         (result (spacemacs/mplist-get-values input :prop2)))
    (should (equal '(sym4) result))))

(ert-deftest test-mplist-get-values--multiple-properties-get-last ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 :prop3 sym5 sym6))
         (result (spacemacs/mplist-get-values input :prop3)))
    (should (equal '(sym5 sym6) result))))

(ert-deftest test-mplist-get-values--one-property-no-value ()
  (let* ((input '(dummy :property))
         (result (spacemacs/mplist-get-values input :property)))
    (should (null result))))

(ert-deftest test-mplist-get-values--multiple-same-poperty-ignore-all-but-first ()
  (let* ((input '(dummy :property val1 :property val2))
         (result (spacemacs/mplist-get-values input :property)))
    (should (equal '(val1) result))))

;; ---------------------------------------------------------------------------
;; spacemacs/mplist-get-value
;; ---------------------------------------------------------------------------

(ert-deftest test-mplist-get-value--one-property ()
  (let* ((input '(dummy :property sym1))
         (result (spacemacs/mplist-get-value input :property)))
    (should (equal 'sym1 result))))

(ert-deftest test-mplist-get-value--one-property-multiple-values-returns-first ()
  (let* ((input '(dummy :property sym1 sym2 sym3))
         (result (spacemacs/mplist-get-value input :property)))
    (should (equal 'sym1 result))))

(ert-deftest test-mplist-get-value--multiple-properties-get-first ()
  (let* ((input '(dummy :prop1 sym1 :prop2 sym4 :prop3 sym5))
         (result (spacemacs/mplist-get-value input :prop1)))
    (should (equal 'sym1 result))))

(ert-deftest test-mplist-get-value--multiple-properties-get-middle ()
  (let* ((input '(dummy :prop1 sym1 :prop2 sym4 :prop3 sym5))
         (result (spacemacs/mplist-get-value input :prop2)))
    (should (equal 'sym4 result))))

(ert-deftest test-mplist-get-value--multiple-properties-get-last ()
  (let* ((input '(dummy :prop1 sym1 :prop2 sym4 :prop3 sym5))
         (result (spacemacs/mplist-get-value input :prop3)))
    (should (equal 'sym5 result))))

(ert-deftest test-mplist-get-value--one-property-no-value ()
  (let* ((input '(dummy :property))
         (result (spacemacs/mplist-get-value input :property)))
    (should (null result))))

(ert-deftest test-mplist-get-value--one-property-no-value-next-is-another-property ()
  (let* ((input '(dummy :property :property2 sym2))
         (result (spacemacs/mplist-get-value input :property)))
    (should (null result))))

(ert-deftest test-mplist-get-value--multiple-same-poperty-ignore-all-but-first ()
  (let* ((input '(dummy :property val1 :property val2))
         (result (spacemacs/mplist-get-value input :property)))
    (should (equal 'val1 result))))

;; ---------------------------------------------------------------------------
;; spacemacs/mplist-remove
;; ---------------------------------------------------------------------------

(ert-deftest test-mplist-remove--one-property ()
  (let* ((input '(dummy :property sym1 sym2 sym3))
         (result (spacemacs/mplist-remove input :property)))
    (should (equal '(dummy) result))))

(ert-deftest test-mplist-remove--one-property-no-value ()
  (let* ((input '(dummy :property))
         (result (spacemacs/mplist-remove input :property)))
    (should (equal '(dummy) result))))

(ert-deftest test-mplist-remove--multiple-properties-remove-first ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 sym5 :prop3 sym6))
         (result (spacemacs/mplist-remove input :prop1)))
    (should (equal '(dummy :prop2 sym4 sym5 :prop3 sym6) result))))

(ert-deftest test-mplist-remove--multiple-properties-remove-middle ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 sym5 :prop3 sym6))
         (result (spacemacs/mplist-remove input :prop2)))
    (should (equal '(dummy :prop1 sym1 sym2 sym3 :prop3 sym6) result))))

(ert-deftest test-mplist-remove--multiple-properties-remove-last ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 sym5 :prop3 sym6))
         (result (spacemacs/mplist-remove input :prop3)))
    (should (equal '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 sym5) result))))

(ert-deftest test-mplist-remove--multiple-same-property-remove-only-first ()
  (let* ((input '(dummy :prop1 sym1 sym2 sym3 :prop2 sym4 sym5 :prop1 sym6))
         (result (spacemacs/mplist-remove input :prop1)))
    (should (equal '(dummy :prop2 sym4 sym5 :prop1 sym6) result))))
