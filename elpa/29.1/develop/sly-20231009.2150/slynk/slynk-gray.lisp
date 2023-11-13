;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; slynk-gray.lisp --- Gray stream based IO redirection.
;;;
;;; Created 2003
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(in-package slynk-backend)

#.(progn
    (defvar *gray-stream-symbols*
    '(fundamental-character-output-stream
      stream-write-char
      stream-write-string
      stream-fresh-line
      stream-force-output
      stream-finish-output

      fundamental-character-input-stream
      stream-read-char
      stream-peek-char
      stream-read-line
      stream-listen
      stream-unread-char
      stream-clear-input
      stream-line-column
      stream-read-char-no-hang))
    nil)

(defpackage slynk-gray
  (:use cl slynk-backend)
  (:import-from #.(gray-package-name) . #.*gray-stream-symbols*)
  (:export . #.*gray-stream-symbols*))

(in-package slynk-gray)

(defclass sly-output-stream (fundamental-character-output-stream)
  ((output-fn :initarg :output-fn)
   (buffer :initform (make-string 8000))
   (fill-pointer :initform 0)
   (column :initform 0)
   (lock :initform (make-lock :name "buffer write lock"))
   (flush-thread :initarg :flush-thread
                 :initform nil
                 :accessor flush-thread)
   (flush-scheduled :initarg :flush-scheduled
                    :initform nil
                    :accessor flush-scheduled)))

(defun maybe-schedule-flush (stream)
  (when (and (flush-thread stream)
             (not (flush-scheduled stream)))
    (setf (flush-scheduled stream) t)
    (send (flush-thread stream) t)))

(defmacro with-sly-output-stream (stream &body body)
  `(with-slots (lock output-fn buffer fill-pointer column) ,stream
     (call-with-lock-held lock (lambda () ,@body))))

(defmethod stream-write-char ((stream sly-output-stream) char)
  (with-sly-output-stream stream
    (setf (schar buffer fill-pointer) char)
    (incf fill-pointer)
    (incf column)
    (when (char= #\newline char)
      (setf column 0))
    (if (= fill-pointer (length buffer))
        (finish-output stream)
        (maybe-schedule-flush stream)))
  char)

(defmethod stream-write-string ((stream sly-output-stream) string
                                &optional start end)
  (with-sly-output-stream stream
    (let* ((start (or start 0))
           (end (or end (length string)))
           (len (length buffer))
           (count (- end start))
           (free (- len fill-pointer)))
      (when (>= count free)
        (stream-finish-output stream))
      (cond ((< count len)
             (replace buffer string :start1 fill-pointer
                                    :start2 start :end2 end)
             (incf fill-pointer count)
             (maybe-schedule-flush stream))
            (t
             (funcall output-fn (subseq string start end))))
      (let ((last-newline (position #\newline string :from-end t
                                                     :start start :end end)))
        (setf column (if last-newline
                         (- end last-newline 1)
                         (+ column count))))))
  string)

(defmethod stream-line-column ((stream sly-output-stream))
  (with-sly-output-stream stream column))

(defmethod reset-stream-line-column ((stream sly-output-stream))
  (with-sly-output-stream stream (setf column 0)))

#+sbcl
(defmethod reset-stream-line-column ((stream sb-sys:fd-stream))
  (with-slots (sb-impl::output-column) stream
    (setf sb-impl::output-column 0)))

#+cmucl
(defmethod reset-stream-line-column ((stream system:fd-stream))
  (with-slots (lisp::char-pos) stream
    (setf lisp::char-pos 0)))

(defmethod stream-finish-output ((stream sly-output-stream))
  (with-sly-output-stream stream
    (unless (zerop fill-pointer)
      (funcall output-fn (subseq buffer 0 fill-pointer))
      (setf fill-pointer 0))
    (setf (flush-scheduled stream) nil))
  nil)

#+(and sbcl sb-thread)
(defmethod stream-force-output :around ((stream sly-output-stream))
  ;; Workaround for deadlocks between the world-lock and auto-flush-thread
  ;; buffer write lock.
  ;;
  ;; Another alternative would be to grab the world-lock here, but that's less
  ;; future-proof, and could introduce other lock-ordering issues in the
  ;; future.
  (handler-case
      (sb-sys:with-deadline (:seconds 0.1)
        (call-next-method))
    (sb-sys:deadline-timeout ()
      nil)))

(defmethod stream-force-output ((stream sly-output-stream))
  (stream-finish-output stream))

(defmethod stream-fresh-line ((stream sly-output-stream))
  (with-sly-output-stream stream
    (cond ((zerop column) nil)
          (t (terpri stream) t))))

(defclass sly-input-stream (fundamental-character-input-stream)
  ((input-fn :initarg :input-fn)
   (buffer :initform "") (index :initform 0)
   (lock :initform (make-lock :name "buffer read lock"))))

(defmethod stream-read-char ((s sly-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index input-fn) s
       (when (= index (length buffer))
         (let ((string (funcall input-fn)))
           (cond ((zerop (length string))
                  (return-from stream-read-char :eof))
                 (t
                  (setf buffer string)
                  (setf index 0)))))
       (assert (plusp (length buffer)))
       (prog1 (aref buffer index) (incf index))))))

(defmethod stream-listen ((s sly-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (< index (length buffer))))))

(defmethod stream-unread-char ((s sly-input-stream) char)
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (decf index)
       (cond ((eql (aref buffer index) char)
              (setf (aref buffer index) char))
             (t
              (warn "stream-unread-char: ignoring ~S (expected ~S)"
                    char (aref buffer index)))))))
  nil)

(defmethod stream-clear-input ((s sly-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (setf buffer ""
             index 0))))
  nil)

(defmethod stream-line-column ((s sly-input-stream))
  nil)

(defmethod stream-read-char-no-hang ((s sly-input-stream))
  (call-with-lock-held
   (slot-value s 'lock)
   (lambda ()
     (with-slots (buffer index) s
       (when (< index (length buffer))
         (prog1 (aref buffer index) (incf index)))))))


;;;

(defimplementation make-auto-flush-thread (stream)
  (if (typep stream 'sly-output-stream)
      (setf (flush-thread stream)
            (spawn (lambda () (auto-flush-loop stream 0.08 t))
                   :name "auto-flush-thread"))
      (spawn (lambda () (auto-flush-loop stream *auto-flush-interval*))
             :name "auto-flush-thread")))

(defimplementation make-output-stream (write-string)
  (make-instance 'sly-output-stream :output-fn write-string))

(defimplementation make-input-stream (read-string)
  (make-instance 'sly-input-stream :input-fn read-string))
