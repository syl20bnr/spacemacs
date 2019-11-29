;;; spacebind-utest.el --- Spacemacs Unit Test File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-spacebind)
(require 'cl-lib)

(defconst test-spacebind-moked-fns-sig
  '((spacemacs/add-key-based-replacements-for-minor-mode
     (MODE KEY-SEQUENCE REPLACEMENT &rest MORE))
    (spacemacs/declare-prefix
      (PREFIX NAME &optional LONG-NAME))
    (spacemacs/declare-prefix-for-mode
      (MODE PREFIX NAME &optional LONG-NAME))
    (spacemacs/set-leader-keys
      (KEY DEF &rest BINDINGS))
    (spacemacs/set-leader-keys-for-major-mode
      (MODE KEY DEF &rest BINDINGS))
    (spacemacs/set-leader-keys-for-minor-mode
      (MODE KEY DEF &rest BINDINGS))
    (which-key-add-key-based-replacements
      (KEY-SEQUENCE REPLACEMENT &rest MORE))
    (which-key-add-major-mode-key-based-replacements
      (MODE KEY-SEQUENCE REPLACEMENT &rest MORE)))
  "Signature of the functions that we will mock for the `spacebind' tests.")

;;;; Helpers:
(defmacro test-spacebind/subst-eval (rep-fun &rest body)
  "Substitute calls to binding functions with calls to REP-FUN.
REP-FUN applied to the form: (fn-sym (args)).
Binding functions are listed in `test-spacebind-moked-fns-sig'. "
  `(cl-labels
       ((spacemacs/leader-key () "SPC")
        (spacemacs/major-mode-prefix () "m")
        ,@(mapcar
           (lambda (seg)
             (let* ((f-s (car seg))
                    (args (cadr seg))
                    (pl-args (cl-set-difference args '(&rest &optional))))
               `(,f-s ,args (funcall ,rep-fun ',f-s (list ,@pl-args)))))
           test-spacebind-moked-fns-sig))
     ,@body))

(defmacro test-spacebind/log-calls (&rest body)
  "Evaluate BODY while mocking and logging calls to the binding functions.
The log is returned.
Binding functions are listed in `test-spacebind-moked-fns-sig'."
  `(let ((acc nil))
     (test-spacebind/subst-eval
      (lambda (fn args) (push (cons fn args) acc))
      ,@body)
     acc))

;; Example:
(thread-last (spacebind
              :major
              (major-foo-mode
               ("a" "section a"
                ("a" foo-fn "execute foo-fn"))))
  (test-spacebind/log-calls)
  (format "%S")
  (insert)
  ;; Prevents execution
  (declare))

;;;; Tests:

(ert-deftest test-spacebind-sanity-check ()
  (should
   (eq '()
       (cl-set-exclusive-or
        (test-spacebind/log-calls
         (spacebind
          :major
          (python-mode
           "with a description"
           ("c" "compile/execute"
            ("c" spacemacs/python-execute-file "execute file")))))

        '((spacemacs/set-leader-keys-for-major-mode
            python-mode "mcc" spacemacs/python-execute-file nil)
          (which-key-add-major-mode-key-based-replacements
            python-mode "SPC m c c" "execute file" nil)
          (spacemacs/declare-prefix-for-mode
            python-mode "mc" "compile/execute" nil))
        :test 'equal))))

(ert-deftest test-spacebind-use-package-integration-works ()
  (should
   (eq '()
       (cl-set-exclusive-or
        (test-spacebind/log-calls
         (use-package use-package
           :spacebind
           (:major
            (python-mode
             "with a description"
             ("c" "compile/execute"
              ("c" spacemacs/python-execute-file "execute file"))))))

        '((spacemacs/set-leader-keys-for-major-mode
            python-mode "mcc" spacemacs/python-execute-file nil)
          (which-key-add-major-mode-key-based-replacements
            python-mode "SPC m c c" "execute file" nil)
          (spacemacs/declare-prefix-for-mode
            python-mode "mc" "compile/execute" nil))
        :test 'equal))))
