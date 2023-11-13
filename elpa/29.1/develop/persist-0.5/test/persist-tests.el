(require 'persist)
(require 'seq)


(defmacro with-local-temp-persist (&rest body)
  (declare (debug body))
  `(unwind-protect
       (let ((persist--directory-location "./persist/")
             (persist--symbols nil))
         ,@body)
     (delete-directory "./persist" t)))

(ert-deftest test-persist-symbol ()
  (should
   (let ((persist--symbols nil)
         (sym (cl-gensym)))
     (persist-symbol sym 10)
     (seq-contains persist--symbols sym))))

(ert-deftest test-persist-save-only-persistant ()
  ;; do not save not persist variables
  (should-error
   (with-local-temp-persist
    (persist-save (cl-gensym)))
   :type 'error
   :exclude-subtypes t))

(ert-deftest test-persist-save ()
  (with-local-temp-persist
   (let ((sym (cl-gensym)))
     ;; precondition
   (should-not (file-exists-p (persist--file-location sym)))
     (set sym 10)
     (persist-symbol sym 10)
     (persist-save sym)
     (should t)
     (should-not (file-exists-p (persist--file-location sym)))
     (set sym 20)
     (persist-save sym)
     (should (file-exists-p (persist--file-location sym)))
     (should
      (string-match-p
       "20"
       (with-temp-buffer
         (insert-file-contents (persist--file-location sym))
         (buffer-string))))
     (set sym 10)
     (persist-save sym)
     (should-not (file-exists-p (persist--file-location sym)))
     (should-error
      (persist-save 'fred)))))

(ert-deftest test-persist-save-non-number ()
  "Test saving something that is not a number.

`test-persist-save' missed "
  (with-local-temp-persist
   (let ((sym (cl-gensym)))
     (set sym "fred")
     (persist-symbol sym "fred")
     (persist-save sym)
     (should t)
     (should-not (file-exists-p (persist--file-location sym)))
     (set sym "george")
     (persist-save sym)
     (should (file-exists-p (persist--file-location sym)))
     (should
      (string-match-p
       "george"
       (with-temp-buffer
         (insert-file-contents (persist--file-location sym))
         (buffer-string)))))))

(ert-deftest test-persist-load ()
  (with-local-temp-persist
   (let ((sym (cl-gensym)))
     (set sym 10)
     ;; set this different to force save
     (persist-symbol sym 1)
     (persist-save sym)
     (should (equal 10 (symbol-value sym)))
     (set sym 30)
     (should (equal 30 (symbol-value sym)))
     (persist-load sym)
     (should (equal 10 (symbol-value sym))))))

(ert-deftest test-persist-remove ()
  (with-local-temp-persist
   (let ((sym (cl-gensym)))
     (should-not (persist--persistant-p sym))
     (persist-symbol sym 10)
     (should (persist--persistant-p sym))
     (persist-unpersist sym)
     (should-not (persist--persistant-p sym)))))

(ert-deftest test-persist-defvar ()
  (with-local-temp-persist
   (defvar test-no-persist-variable 10 "docstring")
   (persist-defvar test-persist-variable 20 "docstring")
   (should-not (persist--persistant-p 'test-no-persist-variable))
   (should (persist--persistant-p 'test-persist-variable))
   (should (= 20
              (persist-default 'test-persist-variable)))))

(ert-deftest test-persist-location ()
  (unwind-protect
      (let ((sym (cl-gensym)))
        (delete-directory "./persist-defined-location" t)
        (set sym 10)
        (persist-symbol sym 10)
        (persist-location sym "./persist-defined-location")
        (should
         (equal (expand-file-name
                 (symbol-name sym)
                 "./persist-defined-location/")
                (persist--file-location sym)))
        (persist-save sym)
        (should-not (file-exists-p (persist--file-location sym)))
        (set sym 20)
        (persist-save sym)
        (should (file-exists-p (persist--file-location sym)))
        (should
         (string-match-p
          "20"
          (with-temp-buffer
            (insert-file-contents (persist--file-location sym))
            (buffer-string))))
        (should-error
         (persist-save 'fred)))
    (delete-directory "./persist-defined-location" t)))
