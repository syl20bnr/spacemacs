;;; package.lisp -- package definition for slynk-macrostep.lisp
;;
;; Authors: Luís Oliveira <luismbo@gmail.com>
;;          Jon Oddie <j.j.oddie@gmail.com>
;;          João Távora <joaotavora@gmail.com>
;;
;; License: Public Domain

(defpackage slynk-macrostep
  (:use #:cl #:slynk-api)
  (:import-from slynk
		#:*macroexpand-printer-bindings*
                #:with-buffer-syntax
		#:with-bindings
                #:to-string
                #:macroexpand-all
                #:compiler-macroexpand-1
                #:debug-on-slynk-error
                #:defslyfun)
  (:export #:macrostep-expand-1
           #:macro-form-p))

