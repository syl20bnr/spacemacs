;;; collect-macro-forms.lisp -- helper macros for slynk-macrostep.lisp
;;
;; Authors: Luís Oliveira <luismbo@gmail.com>
;;          Jon Oddie <j.j.oddie@gmail.com>
;;          João Távora <joaotavora@gmail.com>
;;
;; License: Public Domain

(in-package #:slynk-macrostep)

;;; JT: These definitions brought into this contrib from SLIME's
;;; backend.lisp. They could/should go into SLY if they prove to be useful
;;; enough for writing other contribs, meanwhile keep them here.
;;; 
(defmacro with-collected-macro-forms
    ((forms &optional result) instrumented-form &body body)
  "Collect macro forms by locally binding *MACROEXPAND-HOOK*.
Evaluates INSTRUMENTED-FORM and collects any forms which undergo
macro-expansion into a list.  Then evaluates BODY with FORMS bound to
the list of forms, and RESULT (optionally) bound to the value of
INSTRUMENTED-FORM."
  (assert (and (symbolp forms) (not (null forms))))
  (assert (symbolp result))
  ;; JT: Added conditional ignore spec
  ;; 
  (let ((result-var (or result
                        (gensym))))
    `(call-with-collected-macro-forms
      (lambda (,forms ,result-var)
        (declare (ignore ,@(unless result
                             `(,result-var))))
        ,@body)
      (lambda () ,instrumented-form))))

(defun call-with-collected-macro-forms (body-fn instrumented-fn)
  (let ((return-value nil)
        (collected-forms '()))
    (let* ((real-macroexpand-hook *macroexpand-hook*)
           (*macroexpand-hook*
            (lambda (macro-function form environment)
              (let ((result (funcall real-macroexpand-hook
                                     macro-function form environment)))
                (unless (eq result form)
                  (push form collected-forms))
                result))))
      (setf return-value (funcall instrumented-fn)))
    (funcall body-fn collected-forms return-value)))

(defun collect-macro-forms (form &optional env)
  "Collect subforms of FORM which undergo (compiler-)macro expansion.
Returns two values: a list of macro forms and a list of compiler macro
forms."
  (with-collected-macro-forms (macro-forms expansion)
      (ignore-errors (macroexpand-all form env))
    (with-collected-macro-forms (compiler-macro-forms)
        (handler-bind ((warning #'muffle-warning))
          (ignore-errors
            (compile nil `(lambda () ,expansion))))
      (values macro-forms compiler-macro-forms))))
