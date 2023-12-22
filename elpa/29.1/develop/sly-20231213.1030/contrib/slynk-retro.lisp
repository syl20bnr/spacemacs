(defpackage :slynk-retro
  (:use :cl :slynk :slynk-api))

(in-package :slynk-retro)

(defun ensure-slynk-package-nicknames (&rest ignored)
  "Nickname all SLYNK-* package to SWANK-*"
  (declare (ignore ignored))
  (loop for package in (list-all-packages)
      for package-name = (package-name package)
      when (search "SLYNK" package-name :test #'char-equal)
        do (rename-package package
                           package-name
                           (remove-duplicates
                            (cons
                             (format nil "SWANK~a"
                                     (subseq package-name 5))
                             (package-nicknames package))
                            :test #'string-equal))))

(defun load-swankrcs-maybe ()
  (find-if (lambda (homedir-file)
             (load (merge-pathnames (user-homedir-pathname)
                                    homedir-file)
                   :if-does-not-exist nil))
           (list (make-pathname :name ".swank" :type "lisp")
                 (make-pathname :name ".swankrc"))))

(setq slynk-rpc:*translating-swank-to-slynk* nil)
(push #'ensure-slynk-package-nicknames
      slynk-api:*slynk-require-hook*)

(ensure-slynk-package-nicknames)
;;; Take this chance to load ~/.swank.lisp and ~/.swankrc if no
;;; ~/.slynk.lisp or ~/.slynkrc have already been loaded.
;;;
(unless slynk-api:*loaded-user-init-file*
  (setq slynk-api:*loaded-user-init-file*
        (load-swankrcs-maybe)))

(provide :slynk/retro)


