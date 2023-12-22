(defpackage :slynk-profiler
  (:use :cl)
  (:import-from :slynk :defslyfun :from-string :to-string)
  (:export #:toggle-timing
           #:untime-spec
           #:clear-timing-tree
           #:untime-all
           #:timed-spec-p
           #:time-spec))

(in-package :slynk-profiler)

(defvar *timing-lock* (slynk-backend:make-lock :name "slynk-timings lock"))

(defvar *current-timing* nil)

(defvar *timed-spec-lists* (make-array 10
                                       :fill-pointer 0
                                       :adjustable t))

(defun started-timing ())

(defmethod timed-specs ()
  (aref *timed-spec-lists* (1- (fill-pointer *timed-spec-lists*))))

(defmethod (setf timed-specs) (value)
  (setf (aref *timed-spec-lists* (1- (fill-pointer *timed-spec-lists*))) value))

(defclass timing ()
  ((parent :reader parent-of :initform *current-timing* )
   (origin :initarg :origin :reader origin-of
           :initform (error "must provide an ORIGIN for this TIMING"))
   (start  :reader start-of :initform (get-internal-real-time))
   (end    :accessor end-of :initform nil)))

(defclass timed-spec ()
  ((spec       :initarg  :spec      :accessor spec-of
               :initform (error "must provide a spec"))
   (stats      :accessor stats-of)
   (total      :accessor total-of)
   (subtimings :accessor subtimings-of)
   (owntimings :accessor owntimings-of)))

(defun get-singleton-create (spec)
  (let ((existing (find spec (timed-specs) :test #'equal :key #'spec-of)))
    (if existing
        (reinitialize-instance existing)
        (let ((new (make-instance 'timed-spec :spec spec)))
          (push new (timed-specs))
          new))))

(defmethod shared-initialize :after ((ts timed-spec) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (stats-of ts) (make-hash-table)
        (total-of ts) 0
        (subtimings-of ts) nil
        (owntimings-of ts) nil)
  (loop for otherts in (remove ts (timed-specs))
        do (setf (gethash ts (stats-of otherts)) 0)
           (setf (gethash otherts (stats-of ts)) 0)))

(defmethod initialize-instance :after ((tm timing) &rest initargs)
  (declare (ignore initargs))
  (push tm (owntimings-of (origin-of tm)))
  (let ((parent (parent-of tm)))
    (when parent
      (push tm (subtimings-of (origin-of parent))))))

(defmethod (setf end-of) :after (value (tm timing))
  (let* ((parent (parent-of tm))
         (parent-origin (and parent (origin-of parent)))
         (origin (origin-of tm))
         (tm1 (pop (owntimings-of origin)))
         (tm2 (and parent
                   (pop (subtimings-of parent-origin))))
         (delta (- value (start-of tm))))
    (assert (eq tm tm1) nil "Hmm something's gone wrong in the owns")
    (assert (or (null tm2)
                (eq tm tm2)) nil "Something's gone wrong in the subs")
    (when (null (owntimings-of origin))
      (incf (total-of origin) delta))
    (when (and parent-origin
               (null (subtimings-of parent-origin)))
      (incf (gethash origin (stats-of parent-origin))
            delta))))

(defmethod duration ((tm timing))
  (/ (- (or (end-of tm)
            (get-internal-real-time))
        (start-of tm))
     internal-time-units-per-second))

(defmethod print-object ((tm timing) stream)
  (print-unreadable-object (tm stream :type t :identity t)
    (format stream "~a: ~f~a"
            (spec-of (origin-of tm))
            (duration tm)
            (if (not (end-of tm)) "(unfinished)" ""))))

(defmethod print-object ((e timed-spec) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~a ~fs" (spec-of e)
            (/ (total-of e)
               internal-time-units-per-second))))

(defslyfun time-spec (spec)
  (when (timed-spec-p spec)
    (warn "~a is apparently already timed! Untiming and retiming." spec)
    (untime-spec spec))
  (let ((timed-spec (get-singleton-create spec)))
    (flet ((before-hook (args)
             (declare (ignore args))
             (setf *current-timing*
                   (make-instance 'timing :origin timed-spec)))
           (after-hook (retlist)
             (declare (ignore retlist))
             (let* ((timing *current-timing*))
               (when timing
                 (setf (end-of timing) (get-internal-real-time))
                 (setf *current-timing* (parent-of timing))))))
      (slynk-backend:wrap spec 'timings
                          :before #'before-hook
                          :after #'after-hook)
      (format nil "~a is now timed for timing dialog" spec))))

(defslyfun untime-spec (spec)
  (slynk-backend:unwrap spec 'timings)
  (let ((moribund (find spec (timed-specs) :test #'equal :key #'spec-of)))
    (setf (timed-specs) (remove moribund (timed-specs)))
    (loop for otherts in (timed-specs)
          do (remhash moribund (stats-of otherts))))
  (format nil "~a is now untimed for timing dialog" spec))

(defslyfun toggle-timing (spec)
  
  (if (timed-spec-p spec)
      (untime-spec spec)
      (time-spec spec)))

(defslyfun timed-spec-p (spec)
  (find spec (timed-specs) :test #'equal :key #'spec-of))

(defslyfun untime-all ()
  (mapcar #'untime-spec (timed-specs)))


;;;; Reporting to emacs
;;;
(defun describe-timing-for-emacs (timed-spec)
  (declare (ignore timed-spec))
  `not-implemented)

(defslyfun report-latest-timings ()
  (loop for spec in (timed-specs)
        append (loop for partial being the hash-values of (stats-of spec)
                     for path being the hash-keys of (stats-of spec)
                     collect (list (slynk-api:slynk-pprint-to-line spec) partial
                                   (slynk-api:slynk-pprint-to-line path)))))

(defun print-tree ()
  (loop for ts in (timed-specs)
        for total = (total-of ts)
        do (format t "~%~a~%~%" ts)
           (when (plusp total)
             (loop for partial being the hash-values of (stats-of ts)
                   for path being the hash-keys of (stats-of ts)
                   when (plusp partial)
                     sum partial into total-partials
                     and
                       do (format t "  ~8fs ~4f%  ~a ~%"
                                  (/ partial
                                     internal-time-units-per-second)
                                  (* 100 (/ partial
                                            total))
                                  (spec-of path))
                   finally
                      (format t "  ~8fs ~4f%  ~a ~%"
                              (/ (- total total-partials)
                                 internal-time-units-per-second)
                              (* 100 (/ (- total total-partials)
                                        total))
                              'other)))))

(defslyfun clear-timing-tree ()
  (setq *current-timing* nil)
  (loop for ts in (timed-specs)
        do (reinitialize-instance ts)))

(provide :slynk/profiler)
