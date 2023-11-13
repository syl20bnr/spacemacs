;;; -*- lisp -*-
(in-package :asdf)

(defsystem :slynk-macrostep
    :author "João Távora <https://github.com/capitaomorte>"
    :depends-on (#:slynk)
  :description "MACROSTEP support for Slynk"
  :components ((:file "package")
               (:file "collect-macro-forms")
               (:file "slynk-macrostep")))

;; Local Variables:
;; coding: utf-8
;; End:
