(defpackage :slynk-trace-dialog
  (:use :cl :slynk-api)
  (:export #:clear-trace-tree
           #:dialog-toggle-trace
           #:dialog-trace
           #:dialog-traced-p
           #:dialog-untrace
           #:dialog-untrace-all
           #:inspect-trace-part
           #:report-partial-tree
           #:report-specs
           #:report-total
           #:report-specs
           #:trace-format
           #:still-inside
           #:exited-non-locally
           #:*record-backtrace*
           #:*traces-per-report*
           #:*dialog-trace-follows-trace*
           #:instrument

           #:pprint-trace-part
           #:describe-trace-part
           #:trace-part-or-lose
           #:inspect-trace
           #:trace-or-lose
           #:trace-arguments-or-lose
           #:trace-location))

(in-package :slynk-trace-dialog)

(defparameter *record-backtrace* nil
  "Record a backtrace of the last 20 calls for each trace.

Beware that this may have a drastic performance impact on your
program.")

(defparameter *traces-per-report* 150
  "Number of traces to report to emacs in each batch.")

(defparameter *dialog-trace-follows-trace* nil)

(defvar *traced-specs* '())

(defparameter *visitor-idx* 0)

(defparameter *visitor-key* nil)

(defvar *unfinished-traces* '())


;;;; `trace-entry' model
;;;;
(defvar *traces* (make-array 1000 :fill-pointer 0
                                  :adjustable t))

(defvar *trace-lock* (slynk-backend:make-lock :name "slynk-trace-dialog lock"))

(defvar *current-trace-by-thread* (make-hash-table))

(defclass trace-entry ()
  ((id         :reader   id-of)
   (children   :accessor children-of :initform nil)
   (backtrace  :accessor backtrace-of :initform (when *record-backtrace*
                                                  (useful-backtrace)))

   (spec       :initarg  :spec      :accessor spec-of
               :initform (error "must provide a spec"))
   (function   :initarg  :function  :accessor function-of)
   (args       :initarg  :args      :reader args-of
               :initform (error "must provide args"))
   (printed-args)
   (parent     :initarg  :parent    :reader   parent-of
               :initform (error "must provide a parent, even if nil"))
   (retlist    :initarg  :retlist   :accessor retlist-of
               :initform 'still-inside)
   (printed-retlist :initform ":STILL-INSIDE")))

(defmethod initialize-instance :after ((entry trace-entry) &key)
  (with-slots (parent id printed-args args) entry
    (if parent
        (nconc (children-of parent) (list entry)))
    (setf printed-args
          (mapcar (lambda (arg)
                    (present-for-emacs arg #'slynk-pprint-to-line))
                  args))
    (slynk-backend:call-with-lock-held
     *trace-lock*
     #'(lambda ()
         (setf (slot-value entry 'id) (fill-pointer *traces*))
         (vector-push-extend entry *traces*)))))

(defmethod print-object ((entry trace-entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "~a=~a" (id-of entry) (spec-of entry))))

(defun completed-p (trace) (not (eq (retlist-of trace) 'still-inside)))

(defun trace-arguments (trace-id)
  (values-list (args-of (trace-or-lose trace-id))))

(defun useful-backtrace ()
  (slynk-backend:call-with-debugging-environment
   #'(lambda ()
       (loop for i from 0
             for frame in (slynk-backend:compute-backtrace 0 20)
             collect (list i (slynk::frame-to-string frame))))))

(defun current-trace ()
  (gethash (slynk-backend:current-thread) *current-trace-by-thread*))

(defun (setf current-trace) (trace)
  (setf (gethash (slynk-backend:current-thread) *current-trace-by-thread*)
        trace))


;;;; Helpers
;;;;
(defun describe-trace-for-emacs (trace)
  (with-slots (id args parent spec printed-args retlist printed-retlist) trace
    `(,id
      ,(and parent (id-of parent))
      ,(cons (string-downcase (present-for-emacs spec)) spec)
      ,(loop for arg in args
             for printed-arg in printed-args
             for i from 0
             collect (list i printed-arg))
      ,(loop for retval in (slynk::ensure-list retlist)
             for printed-retval in (slynk::ensure-list printed-retlist)
             for i from 0
             collect (list i printed-retval)))))


;;;; slyfuns
;;;;
(defslyfun trace-format (format-spec &rest format-args)
  "Make a string from FORMAT-SPEC and FORMAT-ARGS and as a trace."
  (let* ((line (apply #'format nil format-spec format-args)))
    (make-instance 'trace-entry :spec line
                                :args format-args
                                :parent (current-trace)
                                :retlist nil)))

(defslyfun trace-or-lose (id)
  (when (<= 0 id (1- (length *traces*)))
    (or (aref *traces* id)
        (error "No trace with id ~a" id))))

(defslyfun report-partial-tree (key)
  (unless (equal key *visitor-key*)
    (setq *visitor-idx* 0
          *visitor-key* key))
  (let* ((recently-finished
           (loop with i = 0
                 for trace in *unfinished-traces*
                 while (< i *traces-per-report*)
                 when (completed-p trace)
                   collect trace
                   and do
                     (incf i)
                     (setq *unfinished-traces*
                           (remove trace *unfinished-traces*))))
         (new (loop for i
                    from (length recently-finished)
                      below *traces-per-report*
                    while (< *visitor-idx* (length *traces*))
                    for trace = (aref *traces* *visitor-idx*)
                    collect trace
                    unless (completed-p trace)
                      do (push trace *unfinished-traces*)
                    do (incf *visitor-idx*))))
    (list
     (mapcar #'describe-trace-for-emacs
             (append recently-finished new))
     (- (length *traces*) *visitor-idx*)
     key)))

(defslyfun report-specs ()
  (mapcar (lambda (spec)
            (cons (string-downcase (present-for-emacs spec))
                  spec))
          (sort (copy-list *traced-specs*)
                #'string<
                :key #'princ-to-string)))

(defslyfun report-total ()
  (length *traces*))

(defslyfun clear-trace-tree ()
  (setf *current-trace-by-thread* (clrhash *current-trace-by-thread*)
        *visitor-key* nil
        *unfinished-traces* nil)
  (slynk-backend:call-with-lock-held
   *trace-lock*
   #'(lambda () (setf (fill-pointer *traces*) 0)))
  nil)

(defslyfun trace-part-or-lose (id part-id type)
  (let* ((trace (trace-or-lose id))
         (l (ecase type
              (:arg (args-of trace))
              (:retval (slynk::ensure-list (retlist-of trace))))))
    (or (nth part-id l)
        (error "Cannot find a trace part with id ~a and part-id ~a"
               id part-id))))

(defslyfun trace-arguments-or-lose (trace-id)
  (values-list (args-of (trace-or-lose trace-id))))

(defslyfun inspect-trace-part (trace-id part-id type)
  (slynk::inspect-object
   (trace-part-or-lose trace-id part-id type)))

(defslyfun pprint-trace-part (trace-id part-id type)
  (slynk::slynk-pprint (list (trace-part-or-lose trace-id part-id type))))

(defslyfun describe-trace-part (trace-id part-id type)
  (slynk::describe-to-string (trace-part-or-lose trace-id part-id type)))

(defslyfun inspect-trace (trace-id)
  (slynk::inspect-object (trace-or-lose trace-id)))

(defslyfun trace-location (trace-id)
  (slynk-backend:find-source-location (function-of (trace-or-lose trace-id))))

(defslyfun dialog-trace (spec)
  (let ((function nil))
    (flet ((before-hook (args)
             (setf (current-trace) (make-instance 'trace-entry
                                                  :spec      spec
                                                  :function  (or function
                                                                 spec)
                                                  :args      args
                                                  :parent    (current-trace))))
           (after-hook (returned-values)
             (let ((trace (current-trace)))
               (when trace
                 (with-slots (retlist parent printed-retlist) trace
                   ;; the current trace might have been wiped away if the
                   ;; user cleared the tree in the meantime. no biggie,
                   ;; don't do anything.
                   ;;
                   (setf retlist returned-values
                         printed-retlist
                         (mapcar (lambda (obj)
                                   (present-for-emacs obj #'slynk-pprint-to-line))
                                 (slynk::ensure-list retlist))
                         (current-trace) parent))))))
      (when (dialog-traced-p spec)
        (warn "~a is apparently already traced! Untracing and retracing." spec)
        (dialog-untrace spec))
      (setq function
            (slynk-backend:wrap spec 'trace-dialog
                                :before #'before-hook
                                :after #'after-hook))
      (pushnew spec *traced-specs*)
      (format nil "~a is now traced for trace dialog" spec))))

(defslyfun dialog-untrace (spec)
  (with-simple-restart
      (continue "Never mind, i really want this trace to go away")
    (slynk-backend:unwrap spec 'trace-dialog))
  (setq *traced-specs* (remove spec *traced-specs* :test #'equal))
  (format nil "~a is now untraced for trace dialog" spec))

(defslyfun dialog-toggle-trace (spec)
  (if (dialog-traced-p spec)
      (dialog-untrace spec)
      (dialog-trace spec)))

(defslyfun dialog-traced-p (spec)
  (find spec *traced-specs* :test #'equal))

(defslyfun dialog-untrace-all ()
  (let ((regular (length (trace)))
        (dialog (length *traced-specs*)))
    (untrace)
    (mapcar #'dialog-untrace *traced-specs*)
    (cons regular dialog)))




;;;; Hook onto emacs
;;;;
(setq slynk:*after-toggle-trace-hook*
      #'(lambda (spec traced-p)
          (when *dialog-trace-follows-trace*
            (cond (traced-p
                   (dialog-trace spec)
                   "traced for trace dialog as well")
                  (t
                   (dialog-untrace spec)
                   "untraced for the trace dialog as well")))))


;;;; Instrumentation
;;;;
(defmacro instrument (x &optional (id (gensym "EXPLICIT-INSTRUMENT-")) )
  (let ((values-sym (gensym)))
    `(let ((,values-sym (multiple-value-list ,x)))
       (trace-format (format nil "~a: ~a" ',id "~a => ~{~a~^, ~}") ',x
                     ,values-sym)
       (values-list ,values-sym))))

(provide :slynk/trace-dialog)
