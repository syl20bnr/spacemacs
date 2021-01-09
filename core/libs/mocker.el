;;; mocker.el --- mocking framework for emacs

;; Copyright (C) 2011  Yann Hodique.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, testing
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(require 'eieio)

(defvar mocker-mock-default-record-cls 'mocker-record)

(put 'mocker-mock-error 'error-conditions '(mocker-mock-error error))
(put 'mocker-mock-error 'error-message "Mocker mock error")

(put 'mocker-record-error 'error-conditions '(mocker-record-error error))
(put 'mocker-record-error 'error-message "Mocker record error")

(defun mocker--plist-remove (plist key)
  ;; courtesy of pjb
  (if (eq (car plist) key) (cdr (cdr plist))
    (cons (car plist)
          (cons (cadr plist)
                (mocker--plist-remove (cddr plist) key)))))

;;; Mock object
(defclass mocker-mock ()
  ((function :initarg :function :type symbol)
   (orig-def :initarg :orig-def :initform nil)
   (argspec :initarg :argspec :initform nil :type list)
   (ordered :initarg :ordered :initform t)
   (records :initarg :records :initform nil :type list)))

(cl-defmethod make-instance ((mock (subclass mocker-mock)) newname &rest args)
  (let* ((obj (cl-call-next-method))
         (recs (oref obj :records))
         (func (oref obj :function)))
    (oset obj :orig-def (when (fboundp func) (symbol-function func)))
    (oset obj :records nil)
    (mapc #'(lambda (r)
              (apply 'mocker-add-record obj r))
          recs)
    obj))

(cl-defmethod mocker-add-record ((mock mocker-mock) &rest args)
  (object-add-to-list mock :records
                      (let ((cls mocker-mock-default-record-cls)
                            (tmp (plist-get args :record-cls)))
                        (when tmp
                          (setq cls tmp
                                args (mocker-read-record cls
                                                         (mocker--plist-remove
                                                          args :record-cls))))
                        (apply 'make-instance cls :-mock mock
                               :-sym (make-symbol "unique") args))
                      t))

(cl-defmethod mocker-fail-mock ((mock mocker-mock) args)
  (signal 'mocker-mock-error
          (list (format (concat "Unexpected call to mock `%s'"
                                " with input `%s'")
                        (oref mock :function) args))))

(defvar mocker-inhibit nil)

(cl-defmethod mocker-run ((mock mocker-mock) &rest args)
  (if (not mocker-inhibit)
      (let* ((mocker-inhibit t)
             (rec (mocker-find-active-record mock args))
             (ordered (oref mock :ordered)))
        (cond ((null rec)
               (mocker-fail-mock mock args))
              ((or (not ordered) (mocker-test-record rec args))
               (mocker-run-record rec args))
              (t
               (mocker-fail-record rec args))))
    (apply (oref mock :orig-def) args)))

(cl-defmethod mocker-find-active-record ((mock mocker-mock) args)
  (let ((first-match (lambda (pred seq)
                       (let ((x nil))
                         (while (and seq
                                     (not (setq x (funcall pred (pop seq))))))
                         x))))
    (let* ((ordered (oref mock :ordered))
           rec)
      (if ordered
          (setq rec (funcall
                     first-match
                     #'(lambda (r)
                         (when (oref r :-active)
                           (if (mocker-test-record r args)
                               (progn
                                 (mocker-use-record r)
                                 r)
                             (mocker-skip-record r args))))
                     (oref mock :records)))
        (setq rec (funcall
                   first-match
                   #'(lambda (r)
                       (and
                        (oref r :-active)
                        (mocker-test-record r args)
                        (progn
                          (mocker-use-record r)
                          r)))
                   (oref mock :records))))
      rec)))

(cl-defmethod mocker-verify ((mock mocker-mock))
  (mapc #'(lambda (r) (when (and (oref r :-active)
                                 (< (oref r :-occurrences)
                                    (oref r :min-occur)))
                        (signal 'mocker-record-error
                                (list (format
                                       (concat "Expected call to mock `%s',"
                                               " with input like %s,"
                                               " was not run.")
                                       (oref mock :function)
                                       (mocker-get-record-expectations r))))))
        (oref mock :records)))

;;; Mock record base object
(defclass mocker-record-base ()
  ((min-occur :initarg :min-occur :initform 1 :type number)
   (max-occur :initarg :max-occur :initform nil :type (or null number))
   (-occur :initarg :occur :initform nil :type (or null number))
   (-occurrences :initarg :-occurrences :initform 0 :type number
                 :protection :protected)
   (-mock :initarg :-mock)
   (-active :initarg :-active :initform t :protection :protected)
   (-sym :initarg :-sym)))

(cl-defmethod make-instance ((rec (subclass mocker-record-base)) newname &rest args)
  (let* ((obj (cl-call-next-method))
         (occur (oref obj :occur)))
    (when occur
      (oset obj :min-occur (max (oref obj :min-occur)
                                occur))
      (oset obj :max-occur (if (oref obj :max-occur)
                               (min (oref obj :max-occur) occur)
                             occur)))
    obj))

(cl-defmethod mocker-read-record ((rec (subclass mocker-record-base)) spec)
  spec)

(cl-defmethod mocker-use-record ((rec mocker-record-base))
  (let ((max (oref rec :max-occur))
        (n (1+ (oref rec :-occurrences))))
    (oset rec :-occurrences n)
    (when (and (not (null max))
               (= n max))
      (oset rec :-active nil))))

(cl-defmethod mocker-skip-record ((rec mocker-record-base) args)
  (if (>= (oref rec :-occurrences)
          (oref rec :min-occur))
      (oset rec :-active nil)
    (mocker-fail-record rec args)))

(cl-defmethod mocker-test-record ((rec mocker-record-base) args)
  (error "not implemented in base class"))

(cl-defmethod mocker-run-record ((rec mocker-record-base) args)
  (error "not implemented in base class"))

(cl-defmethod mocker-get-record-expectations ((rec mocker-record-base)))

(cl-defmethod mocker-fail-record ((rec mocker-record-base) args)
  (signal 'mocker-record-error
          (list (format (concat "Violated record while mocking `%s'."
                                " Expected input like: %s, got: `%s' instead")
                        (oref (oref rec :-mock) :function)
                        (mocker-get-record-expectations rec)
                        args))))

;;; Mock input recognizer
(defclass mocker-input-record (mocker-record-base)
  ((input :initarg :input :initform nil :type list)
   (input-matcher :initarg :input-matcher :initform nil)))

(cl-defmethod make-instance ((rec (subclass mocker-input-record)) newname &rest args)
  (let* ((obj (cl-call-next-method)))
    (when (or (not (slot-boundp obj :max-occur))
              (and (oref obj :max-occur)
                   (< (oref obj :max-occur)
                      (oref obj :min-occur))))
      (oset obj :max-occur (oref obj :min-occur)))
    obj))

(cl-defmethod mocker-test-record ((rec mocker-input-record) args)
  (let ((matcher (oref rec :input-matcher))
        (input (oref rec :input)))
    (cond (matcher
           (apply matcher args))
          (t
           (equal input args)))))

(cl-defmethod mocker-get-record-expectations ((rec mocker-input-record))
  (format "`%s'" (or (oref rec :input-matcher) (oref rec :input))))

;;; Mock record default object
(defclass mocker-record (mocker-input-record)
  ((output :initarg :output :initform nil)
   (output-generator :initarg :output-generator :initform nil)))

(cl-defmethod mocker-run-record ((rec mocker-record) args)
  (let ((generator (oref rec :output-generator))
        (output (oref rec :output)))
    (cond (generator
           (apply generator args))
          (t
           output))))

;;; Mock simple stub object
(defclass mocker-stub-record (mocker-record-base)
  ((output :initarg :output :initform nil)))

(cl-defmethod make-instance ((rec (subclass mocker-stub-record)) newname &rest args)
  (let* ((obj (cl-call-next-method)))
    (unless (slot-boundp obj :min-occur)
      (oset obj :min-occur 0))
    (unless (slot-boundp obj :max-occur)
      (oset obj :max-occur nil))
    obj))

(cl-defmethod mocker-test-record ((rec mocker-stub-record) args)
  t)

(cl-defmethod mocker-run-record ((rec mocker-stub-record) args)
  (oref rec :output))

(cl-defmethod mocker-get-record-expectations ((rec mocker-stub-record))
  "anything")

;;; Mock passthrough record
(defclass mocker-passthrough-record (mocker-input-record)
  ())

(cl-defmethod mocker-run-record ((rec mocker-passthrough-record) args)
  (let* ((mock (oref rec :-mock))
         (def (oref mock :orig-def)))
    (when def
      (apply def args))))

;;; Helpers
(defun mocker-gen-mocks (mockspecs)
  "helper to generate mocks from the input of `mocker-let'"
  (mapcar #'(lambda (m)
              (let* ((func (car m))
                     (argspec (cadr m))
                     (rest (cddr m))
                     (sym (make-symbol (concat (symbol-name func) "--mock"))))
                (list sym
                      (apply 'make-instance 'mocker-mock
                             :function func
                             :argspec argspec
                             (let* ((order (if (plist-member rest :ordered)
                                               (prog1
                                                   (plist-get rest :ordered)
                                                 (setq rest
                                                       (mocker--plist-remove
                                                        rest :ordered)))
                                             (oref-default 'mocker-mock
                                                           :ordered))))
                               (list :ordered order)))
                      (if (plist-member rest :records)
                          (plist-get rest :records)
                        (car rest)))))
          mockspecs))

;;;###autoload
(defmacro mocker-let (mockspecs &rest body)
  "Generate temporary bindings according to MOCKSPECS then eval
BODY. The value of the last form in BODY is returned.
Each element of MOCKSPECS is a list (FUNC ARGS [OPTIONS]
RECORDS).

FUNC is the name of the function to bind, whose original
 definition must accept arguments compatible with ARGS.
OPTIONS can be :ordered nil if the records can be executed out of
order (by default, order is enforced).
RECORDS is a list ([:record-cls CLASS] ARG1 ARG2...).

Each element of RECORDS will generate a record for the
corresponding mock. By default, records are objects of the
`mocker-record' class, but CLASS is used instead if specified.
The rest of the arguments are used to construct the record
object. They will be passed to method `mocker-read-record' for
the used CLASS. This method must return a valid list of
parameters for the CLASS constructor. This allows to implement
specialized mini-languages for specific record classes.
"
  (declare (indent 1) (debug t))
  (let* ((mocks (mocker-gen-mocks mockspecs))
         (vars (mapcar #'(lambda (m)
                           `(,(car m) ,(cadr m)))
                       mocks))
         (specs (mapcar
                 #'(lambda (m)
                     (let* ((mock-sym (car m))
                            (mock (cadr m))
                            (func (oref mock :function))
                            (spec (oref mock :argspec))
                            (call (or (and (member '&rest spec) 'apply)
                                      'funcall))
                            (args (cl-loop for el in spec
                                        if (or (not (symbolp el))
                                               (not (equal
                                                     (elt (symbol-name el) 0)
                                                     ?&)))
                                        collect el)))
                       (list `(symbol-function ',func)
                             `(lambda ,spec
                                (,call #'mocker-run ,mock-sym ,@args)))))
                 mocks))
         (inits (mapcar #'(lambda (m)
                            (cons 'progn
                                  (mapcar #'(lambda (rec)
                                              `(mocker-add-record ,(car m)
                                                              ,@rec))
                                          (nth 2 m))))
                        mocks))
         (verifs (mapcar #'(lambda (m)
                             `(mocker-verify ,(car m)))
                         mocks)))
    `(let (,@vars)
       ,@inits
       (prog1
           ,(macroexpand `(cl-letf (,@specs) ,@body))
         ,@verifs))))

(provide 'mocker)
;;; mocker.el ends here
