(require 'elk-test)

(deftest "window-numbering-assign"
    (let ((window-numbering-windows (make-vector 10 nil))
          (window-numbering-numbers (make-hash-table :size 10))
          (window-numbering-left '(1 2 3)))
      (assert-nonnil (window-numbering-assign 'xx 7))
      (assert-nil (window-numbering-assign 'yy 7))
      (assert-nonnil (window-numbering-assign 'zz 8))
      (assert-equal 8 (gethash 'zz window-numbering-numbers))
      (assert-equal 7 (gethash 'xx window-numbering-numbers))
      (assert-equal 'zz (aref window-numbering-windows 8))
      (assert-equal 'xx (aref window-numbering-windows 7))
      ))

(deftest "window-numbering-assign auto"
    (let ((window-numbering-windows (make-vector 10 nil))
          (window-numbering-numbers (make-hash-table :size 10))
          (window-numbering-left '(1 2 3 4)))
      (assert-eq 1 (window-numbering-assign 'xx))
      (assert-nonnil (window-numbering-assign 'yy 3))
      (assert-eq 2 (window-numbering-assign 'zz))
      (assert-eq 4 (window-numbering-assign 'aa))
))


(deftest "window-numbering-calculate-left"
  (assert-equal '(6) (window-numbering-calculate-left
                      [t t t t t nil t t t t]))
  (assert-equal '(1 2 3) (window-numbering-calculate-left
                          [nil nil nil t t t t t t t]))
  (assert-equal '(1 2 3 4 5 6 7 8 9 0)
                (window-numbering-calculate-left
                 [nil nil nil nil nil nil nil nil nil nil]))
  )
