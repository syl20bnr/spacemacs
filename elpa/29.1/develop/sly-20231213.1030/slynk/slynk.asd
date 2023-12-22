;;; -*- lisp -*-
(in-package :asdf)

;; ASDF system definition for loading the Slynk server independently
;; of Emacs.
;;
;; Usage:
;;
;;   (push #p"/path/to/this/file/" asdf:*central-registry*)
;;   (asdf:load-system :slynk)
;;   (slynk:create-server :port PORT) => ACTUAL-PORT
;;
;; (PORT can be zero to mean "any available port".)
;; Then the Slynk server is running on localhost:ACTUAL-PORT. You can
;; use `M-x sly-connect' to connect Emacs to it.
;;
;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

(defsystem :slynk
  :serial t
  ;; See commit message and GitHub#502, GitHub#501 for the reason
  ;; for this dedicated sbcl muffling.
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    (handler-bind (((and warning (not style-warning))
                     (lambda (c)
                       (format *error-output* "~&~@<~S: ~3i~:_~A~:>~%"
                               (class-name (class-of c)) c)
                       (muffle-warning c))))
      (let ((sb-ext:*on-package-variance* '(:warn t)))
        (funcall thunk))))
  :components
  ((:file "slynk-match")
   (:file "slynk-backend")
   ;; If/when we require ASDF3, we shall use :if-feature instead
   #+(or cmu sbcl scl)
   (:file "slynk-source-path-parser")
   #+(or cmu ecl sbcl scl)
   (:file "slynk-source-file-cache")
   #+clisp
   (:file "xref")
   #+(or clisp clozure clasp)
   (:file "metering")
   (:module "backend"
    :serial t
    :components (#+allegro
                 (:file "allegro")
                 #+armedbear
                 (:file "abcl")
                 #+clisp
                 (:file "clisp")
                 #+clozure
                 (:file "ccl")
                 #+cmu
                 (:file "cmucl")
                 #+cormanlisp
                 (:file "corman")
                 #+ecl
                 (:file "ecl")
                 #+lispworks
                 (:file "lispworks")
                 #+sbcl
                 (:file "sbcl")
                 #+clasp
                 (:file "clasp")
                 #+scl
                 (:file "scl")
                 #+mkcl
                 (:file "mkcl")))
   #-armedbear
   (:file "slynk-gray")
   (:file "slynk-rpc")
   (:file "slynk")
   (:file "slynk-completion")
   (:file "slynk-apropos")))

(defmethod perform :after ((o load-op) (c (eql (find-system :slynk))))
  (format *debug-io* "~&SLYNK's ASDF loader finished.")
  (funcall (with-standard-io-syntax (read-from-string "slynk::init"))))


;;; Contrib systems (should probably go into their own file one day)
;;;
(defsystem :slynk/arglists
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-arglists")))

(defsystem :slynk/fancy-inspector
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-fancy-inspector")))

(defsystem :slynk/package-fu
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-package-fu")))

(defsystem :slynk/mrepl
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-mrepl")))

(defsystem :slynk/trace-dialog
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-trace-dialog")))

(defsystem :slynk/profiler
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-profiler")))

(defsystem :slynk/stickers
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-stickers")))

(defsystem :slynk/indentation
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-indentation")))

(defsystem :slynk/retro
  :depends-on (:slynk)
  :components ((:file "../contrib/slynk-retro")))

