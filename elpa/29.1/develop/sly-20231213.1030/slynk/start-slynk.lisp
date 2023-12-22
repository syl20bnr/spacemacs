;;; This file is intended to be loaded by an implementation to
;;; get a running slynk server
;;; e.g. sbcl --load start-slynk.lisp
;;;
;;; Default port is 4005

;;; For additional slynk-side configurations see
;;; 6.2 section of the Slime user manual.

(load (make-pathname :name "slynk-loader" :type "lisp"
                     :defaults *load-truename*))

(slynk-loader:init
 :delete nil         ; delete any existing SLYNK packages
 :reload nil)        ; reload SLYNK, even if the SLYNK package already exists
 

(slynk:create-server :port 4005
                     ;; if non-nil the connection won't be closed
                     ;; after connecting
                     :dont-close t)
