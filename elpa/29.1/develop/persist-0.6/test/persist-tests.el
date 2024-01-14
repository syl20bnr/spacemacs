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

(defmacro persist-test-persist-save (init default change printed-changed)
  "Test persisting symbols.
- symbol is not persisted when value is set to INIT and default
  value is set to DEFAULT.
- symbol is persisted when value is changed according to CHANGE.
- persisted file contents match PRINTED-CHANGED.
- symbol is not persisted after value is set back to DEFAULT."
  `(with-local-temp-persist
    (let ((sym (cl-gensym)))
      (should-not (file-exists-p (persist--file-location sym)))
      (set sym ,init)
      (persist-symbol sym ,default)
      (persist-save sym)
      (should t)
      (should-not (file-exists-p (persist--file-location sym)))
      ,change
      (persist-save sym)
      (should (file-exists-p (persist--file-location sym)))
      (should
       (string-match-p
        ,printed-changed
        (with-temp-buffer
          (insert-file-contents (persist--file-location sym))
          (buffer-string))))
      (set sym ,default)
      (persist-save sym)
      (should-not (file-exists-p (persist--file-location sym))))))

(ert-deftest test-persist-save-number ()
  "Test saving number."
  (persist-test-persist-save 1 1 (set sym 2) "2"))

(ert-deftest test-persist-save-string ()
  "Test saving string."
  (persist-test-persist-save "foo" "foo" (set sym "bar") "bar"))

(ert-deftest test-persist-save-hash ()
  "Test saving hash table."
  (let* ((hash (make-hash-table))
         (default (copy-hash-table hash)))
    (persist-test-persist-save hash default
                               (puthash 'foo "bar" (symbol-value sym))
                               "#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data (foo \"bar\"))")))

(ert-deftest test-persist-save-record ()
  "Test saving record."
  (let* ((rec (record 'foo 'a 'b))
         (default (copy-sequence rec)))
    (persist-test-persist-save rec default
                               (setf (aref (symbol-value sym) 2) 'quux)
                               "#s(foo a quux)")))

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
