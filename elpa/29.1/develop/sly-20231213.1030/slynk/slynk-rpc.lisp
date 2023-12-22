;;; -*- indent-tabs-mode: nil; coding: latin-1-unix -*-
;;;
;;; slynk-rpc.lisp  -- Pass remote calls and responses between lisp systems.
;;;
;;; Created 2010, Terje Norderhaug <terje@in-progress.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage #:slynk-rpc
  (:use :cl)
  (:export 
   #:read-message
   #:read-packet
   #:slynk-reader-error
   #:slynk-reader-error.packet
   #:slynk-reader-error.cause
   #:write-message
   #:*translating-swank-to-slynk*))

(in-package :slynk-rpc)


;;;;; Input

(define-condition slynk-reader-error (reader-error)
  ((packet :type string :initarg :packet 
           :reader slynk-reader-error.packet)
   (cause :type reader-error :initarg :cause 
          :reader slynk-reader-error.cause)))

(defun read-message (stream package)
  (let ((packet (read-packet stream)))
    (handler-case (values (read-form packet package))
      (reader-error (c)
        (error 'slynk-reader-error 
               :packet packet :cause c)))))

(defun read-packet (stream)
  (let* ((length (parse-header stream))
         (octets (read-chunk stream length)))
    (handler-case (slynk-backend:utf8-to-string octets)
      (error (c) 
        (error 'slynk-reader-error 
               :packet (asciify octets)
               :cause c)))))

(defun asciify (packet)
  (with-output-to-string (*standard-output*)
    (loop for code across (etypecase packet 
                            (string (map 'vector #'char-code packet))
                            (vector packet))
          do (cond ((<= code #x7f) (write-char (code-char code)))
                   (t (format t "\\x~x" code))))))

(defun parse-header (stream)
  (parse-integer (map 'string #'code-char (read-chunk stream 6))
                 :radix 16))

(defun read-chunk (stream length)
  (let* ((buffer (make-array length :element-type '(unsigned-byte 8)))
         (count (read-sequence buffer stream)))
    (cond ((= count length)
           buffer)
          ((zerop count)
           (error 'end-of-file :stream stream))
          (t
           (error "Short read: length=~D  count=~D" length count)))))

(defparameter *translating-swank-to-slynk* t
  "Set to true to ensure SWANK*::SYMBOL is interned SLYNK*::SYMBOL.
Set by default to T to ensure that bootstrapping can occur from
clients sending strings like this on the wire. 

   (:EMACS-REX (SWANK:CONNECTION-INFO) NIL T 1)

*before* the slynk-retro.lisp contrib kicks in and renames SLYNK*
packages to SWANK*. After this happens, this variable is set to NIL,
since the translation is no longer necessary.

The user that is completely sure that Slynk will always be contacted
by SLY clients **without** the sly-retro.el contrib, can also set this
to NIL in her ~/.swankrc. Generally best left alone.")

(defun read-form (string package)
  (with-standard-io-syntax
    (let ((*package* package))
      (if *translating-swank-to-slynk*
          (with-input-from-string (*standard-input* string)
            (translating-read))
          (read-from-string string)))))

(defun maybe-convert-package-designator (string)
  (let ((colon-pos (position #\: string))
        (search (search "SWANK" string :test #'char-equal)))
    (if (and search colon-pos)
        (nstring-upcase (replace string "SLYNK"))
        string)))
 
(defun translating-read ()
  "Read a form that conforms to the protocol, otherwise signal an error."
  (flet ((chomp ()
           (loop for ch = (read-char nil t)
                 while (eq ch #\space)
                 finally (unread-char ch))))
    (chomp)
    (let ((c (read-char)))
      (case c 
        (#\" (with-output-to-string (*standard-output*)
               (loop for c = (read-char) do
                 (case c
                   (#\" (return))
                   (#\\ (write-char (read-char)))
                   (t (write-char c))))))
        (#\(
         (chomp)
         (loop with dotread = nil
               with retval = nil
               for read = (read-char)
               while (case read
                       (#\) nil)
                       (#\. (setq dotread t) t)
                       (t (progn (unread-char read) t)))
                              
               when (eq dotread 'should-error)
                 do (error 'reader-error :format-arguments "Too many things after dot")
               when dotread
                 do (setq dotread 'should-error)
               do (setq retval (nconc retval
                                      (if dotread
                                          (translating-read)
                                          (list (translating-read)))))
                  (chomp)
               finally (return retval)))
        (#\' `(quote ,(translating-read)))
        (t (let ((string (with-output-to-string (*standard-output*)
                           (loop for ch = c then (read-char nil nil) do
                             (case ch
                               ((nil) (return))
                               (#\\ (write-char (read-char)))
                               ((#\" #\( #\space #\)) (unread-char ch)(return))
                               (t (write-char ch)))))))
             (read-from-string
              (maybe-convert-package-designator string))))))))


;;;;; Output

(defun write-message (message package stream)
  (let* ((string (prin1-to-string-for-emacs message package))
         (octets (handler-case (slynk-backend:string-to-utf8 string)
                   (error (c) (encoding-error c string))))
         (length (length octets)))
    (write-header stream length)
    (write-sequence octets stream)
    (finish-output stream)))

;; FIXME: for now just tell emacs that we and an encoding problem.
(defun encoding-error (condition string)
  (slynk-backend:string-to-utf8
   (prin1-to-string-for-emacs
    `(:reader-error
      ,(asciify string)
      ,(format nil "Error during string-to-utf8: ~a"
               (or (ignore-errors (asciify (princ-to-string condition)))
                   (asciify (princ-to-string (type-of condition))))))
    (find-package :cl))))

(defun write-header (stream length)
  (declare (type (unsigned-byte 24) length))
  ;;(format *trace-output* "length: ~d (#x~x)~%" length length)
  (loop for c across (format nil "~6,'0x" length)
        do (write-byte (char-code c) stream)))

(defun switch-to-double-floats (x)
  (typecase x
    (double-float x)
    (float (coerce x 'double-float))
    (null x)
    (list (loop for (x . cdr) on x
                collect (switch-to-double-floats x) into result
                until (atom cdr)
                finally (return (append result (switch-to-double-floats cdr)))))
    (t x)))

(defun prin1-to-string-for-emacs (object package)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package)
          ;; Emacs has only double floats.
          (*read-default-float-format* 'double-float))
      (prin1-to-string (switch-to-double-floats object)))))


#| TEST/DEMO:

(defparameter *transport*
  (with-output-to-string (out)
    (write-message '(:message (hello "world")) *package* out)
    (write-message '(:return 5) *package* out)
    (write-message '(:emacs-rex NIL) *package* out)))

*transport*
                 
(with-input-from-string (in *transport*)
  (loop while (peek-char T in NIL)
        collect (read-message in *package*)))

|#
