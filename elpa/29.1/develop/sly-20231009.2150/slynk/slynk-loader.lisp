;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; slynk-loader.lisp --- Compile and load the Sly backend.
;;;
;;; Created 2003, James Bielman <jamesjb@jamesjb.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

;; If you want customize the source- or fasl-directory you can set
;; slynk-loader:*source-directory* resp. slynk-loader:*fasl-directory*
;; before loading this files.
;; E.g.:
;;
;;   (load ".../slynk-loader.lisp")
;;   (setq slynk-loader::*fasl-directory* "/tmp/fasl/")
;;   (slynk-loader:init)

(cl:defpackage :slynk-loader
  (:use :cl)
  (:export #:init
           #:dump-image
           #:*source-directory*
           #:*fasl-directory*
           #:*load-path*))

(cl:in-package :slynk-loader)

(defvar *source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *default-pathname-defaults*))
  "The directory where to look for the source.")

(defvar *load-path* (list *source-directory*)
  "A list of directories to search for modules.")

(defparameter *sysdep-files*
  #+cmu '(slynk-source-path-parser slynk-source-file-cache (backend cmucl))
  #+scl '(slynk-source-path-parser slynk-source-file-cache (backend scl))
  #+sbcl '(slynk-source-path-parser slynk-source-file-cache
           (backend sbcl))
  #+clozure '(metering (backend ccl))
  #+lispworks '((backend lispworks))
  #+allegro '((backend allegro))
  #+clisp '(xref metering (backend clisp))
  #+armedbear '((backend abcl))
  #+cormanlisp '((backend corman))
  #+ecl '(slynk-source-path-parser slynk-source-file-cache
          (backend ecl))
  #+clasp '(metering (backend clasp))
  #+mkcl '((backend mkcl)))

(defparameter *implementation-features*
  '(:allegro :lispworks :sbcl :clozure :cmu :clisp :ccl :corman :cormanlisp
    :armedbear :gcl :ecl :scl :mkcl :clasp))

(defparameter *os-features*
  '(:macosx :linux :windows :mswindows :win32 :solaris :darwin :sunos :hpux
    :unix))

(defparameter *architecture-features*
  '(:powerpc :ppc :ppc64 :x86 :x86-64 :x86_64 :amd64 :i686 :i586 :i486 :pc386 :iapx386
    :sparc64 :sparc :hppa64 :hppa :arm :armv5l :armv6l :armv7l :arm64 :aarch64
    :pentium3 :pentium4
    :mips :mipsel
    :java-1.4 :java-1.5 :java-1.6 :java-1.7))

(defun q (s) (read-from-string s))

#+ecl
(defun ecl-version-string ()
  (format nil "~A~@[-~A~]"
          (lisp-implementation-version)
          (when (find-symbol "LISP-IMPLEMENTATION-VCS-ID" :ext)
            (let ((vcs-id (funcall (q "ext:lisp-implementation-vcs-id"))))
              (when (>= (length vcs-id) 8)
                (subseq vcs-id 0 8))))))

#+clasp
(defun clasp-version-string ()
  (format nil "~A~@[-~A~]"
          (lisp-implementation-version)
          (core:lisp-implementation-id)))

(defun lisp-version-string ()
  #+(or clozure cmu) (substitute-if #\_ (lambda (x) (find x " /"))
                                    (lisp-implementation-version))
  #+(or cormanlisp scl mkcl) (lisp-implementation-version)
  #+sbcl (format nil "~a~:[~;-no-threads~]"
                 (lisp-implementation-version)
                 #+sb-thread nil
                 #-sb-thread t)
  #+lispworks (lisp-implementation-version)
  #+allegro   (format nil "~@{~a~}"
                      excl::*common-lisp-version-number*
                      (if (string= 'lisp "LISP") "A" "M")     ; ANSI vs MoDeRn
                      (if (member :smp *features*) "s" "")
                      (if (member :64bit *features*) "-64bit" "")
                      (excl:ics-target-case
                       (:-ics "")
                       (:+ics "-ics")))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+ecl (ecl-version-string) )

(defun unique-dir-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (loop for f in features
                 when (find f *features*) return it))
         (maybe-warn (value fstring &rest args)
           (cond (value)
                 (t (apply #'warn fstring args)
                    "unknown"))))
    (let ((lisp (maybe-warn (first-of *implementation-features*)
                            "No implementation feature found in ~a."
                            *implementation-features*))
          (os   (maybe-warn (first-of *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch (maybe-warn (first-of *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp ~
                                implementation version.")))
      (format nil "~(~@{~a~^-~}~)" lisp version os arch))))

(defun file-newer-p (new-file old-file)
  "Returns true if NEW-FILE is newer than OLD-FILE."
  (> (file-write-date new-file) (file-write-date old-file)))

(defun sly-version-string ()
  "Return a string identifying the SLY version.
Return nil if nothing appropriate is available."
  (let ((this-file #.(or *compile-file-truename* *load-truename*)))
    (with-open-file (s (make-pathname :name "sly" :type "el"
                                      :directory (butlast
                                                  (pathname-directory this-file)
                                                  1)
                                      :defaults this-file))
      (let ((seq (make-array 200 :element-type 'character :initial-element #\null)))
        (read-sequence seq s :end 200)
        (let* ((beg (search ";; Version:" seq))
               (end (position #\NewLine seq :start beg))
               (middle (position #\Space seq :from-end t :end end)))
          (subseq seq (1+ middle) end))))))

(defun default-fasl-dir ()
  (merge-pathnames
   (make-pathname
    :directory `(:relative ".sly" "fasl"
                 ,@(if (sly-version-string) (list (sly-version-string)))
                 ,(unique-dir-name)))
   (let ((uhp (user-homedir-pathname)))
     (make-pathname
      :directory (or (pathname-directory uhp)
                     '(:absolute))
      :defaults uhp))))

(defvar *fasl-directory* (default-fasl-dir)
  "The directory where fasl files should be placed.")

(defun binary-pathname (src-pathname binary-dir)
  "Return the pathname where SRC-PATHNAME's binary should be compiled."
  (let ((cfp (compile-file-pathname src-pathname)))
    (merge-pathnames (make-pathname :name (pathname-name cfp)
                                    :type (pathname-type cfp))
                     binary-dir)))

(defun handle-slynk-load-error (condition context pathname)
  (fresh-line *error-output*)
  (pprint-logical-block (*error-output* () :per-line-prefix ";; ")
    (format *error-output*
            "~%Error ~A ~A:~%  ~A~%"
            context pathname condition)))

(defun compile-files (files fasl-dir load quiet)
  "Compile each file in FILES if the source is newer than its
corresponding binary, or the file preceding it was recompiled.
If LOAD is true, load the fasl file."
  (let ((needs-recompile nil)
        (state :unknown))
    (dolist (src files)
      (let ((dest (binary-pathname src fasl-dir)))
        (handler-bind
            ((error (lambda (c)
                      (ecase state
                        (:compile (handle-slynk-load-error c "compiling" src))
                        (:load    (handle-slynk-load-error c "loading" dest))
                        (:unknown (handle-slynk-load-error c "???ing" src))))))
          (when (or needs-recompile
                    (not (probe-file dest))
                    (file-newer-p src dest))
            (ensure-directories-exist dest)
            ;; need to recompile SRC, so we'll need to recompile
            ;; everything after this too.
            (setf needs-recompile t
                  state :compile)
            (or (compile-file src :output-file dest :print nil
                                  :verbose (not quiet))
                ;; An implementation may not necessarily signal a
                ;; condition itself when COMPILE-FILE fails (e.g. ECL)
                (error "COMPILE-FILE returned NIL.")))
          (when load
            (setf state :load)
            (load dest :verbose (not quiet))))))))

#+cormanlisp
(defun compile-files (files fasl-dir load quiet)
  "Corman Lisp has trouble with compiled files."
  (declare (ignore fasl-dir))
  (when load
    (dolist (file files)
      (load file :verbose (not quiet)
      (force-output)))))

(defun ensure-list (o)
  (if (listp o) o (list o)))

(defun src-files (files src-dir)
  "Return actual pathnames for each spec in FILES."
  (mapcar (lambda (compound-name)
            (let* ((directories (butlast compound-name))
                   (name (car (last compound-name))))
              (make-pathname :name (string-downcase name) :type "lisp"
                             :directory (append (or (pathname-directory src-dir)
                                                    '(:relative))
                                                (mapcar #'string-downcase directories))
                             :defaults src-dir)))
          (mapcar #'ensure-list files)))

(defvar *slynk-files*
  `(slynk-backend ,@*sysdep-files* #-armedbear slynk-gray slynk-match slynk-rpc
                  slynk slynk-completion slynk-apropos))

(defun load-slynk (&key (src-dir *source-directory*)
                     (fasl-dir *fasl-directory*)
                     quiet)
  (compile-files (src-files *slynk-files* src-dir) fasl-dir t quiet))

(defun delete-stale-contrib-fasl-files (slynk-files contrib-files fasl-dir)
  (let ((newest (reduce #'max (mapcar #'file-write-date slynk-files))))
    (dolist (src contrib-files)
      (let ((fasl (binary-pathname src fasl-dir)))
        (when (and (probe-file fasl)
                   (<= (file-write-date fasl) newest))
          (delete-file fasl))))))

(defun loadup ()
  (load-slynk))

(defun setup ()
  (funcall (q "slynk::init")))

(defun string-starts-with (string prefix)
  (string-equal string prefix :end1 (min (length string) (length prefix))))

(defun list-slynk-packages ()
  (remove-if-not (lambda (package)
                   (let ((name (package-name package)))
                     (and (string-not-equal name "slynk-loader")
                          (string-starts-with name "slynk"))))
                 (list-all-packages)))

(defun delete-packages (packages)
  (dolist (package packages)
    (flet ((handle-package-error (c)
             (let ((pkgs (set-difference (package-used-by-list package)
                                         packages)))
               (when pkgs
                 (warn "deleting ~a which is used by ~{~a~^, ~}."
                       package pkgs))
               (continue c))))
      (handler-bind ((package-error #'handle-package-error))
        (delete-package package)))))

(defun init (&key delete reload (setup t)
                  (quiet (not *load-verbose*))
                  load-contribs)
  "Load SLYNK and initialize some global variables.
If DELETE is true, delete any existing SLYNK packages.
If RELOAD is true, reload SLYNK, even if the SLYNK package already exists.
If SETUP is true, load user init files and initialize some
global variabes in SLYNK."
  (if load-contribs
      (warn
       "LOAD-CONTRIBS arg to SLYNK-LOADER:INIT is deprecated and useless"))
  (when (and delete (find-package :slynk))
    (delete-packages (list-slynk-packages))
    (mapc #'delete-package '(:slynk :slynk-io-package :slynk-backend)))
  (cond ((or (not (find-package :slynk)) reload)
         (load-slynk :quiet quiet))
        (t
         (warn "Not reloading SLYNK.  Package already exists.")))
  (when setup
    (setup)))

(defun dump-image (filename)
  (init :setup nil)
  (funcall (q "slynk-backend:save-image") filename))


;;;;;; Simple *require-module* function for asdf-loader.lisp.


(defun module-binary-dir (src-file)
  (flet ((dir-components (path)
           (cdr (pathname-directory path))))
    (make-pathname :directory
                   (append
                    (pathname-directory *fasl-directory*)
                    (nthcdr (mismatch (dir-components *fasl-directory*)
                                      (dir-components src-file)
                                      :test #'equal)
                            (dir-components src-file))))))

(defun require-module (module)
  (labels ((module () (string-upcase module))
           (provided ()
             (member (string-upcase (module)) *modules* :test #'string=)))
    (unless (provided)
      (let* ((src-file-name (substitute #\- #\/ (string-downcase module)))
             (src-file
               (some #'(lambda (dir)
                         (probe-file (make-pathname
                                      :name src-file-name
                                      :type "lisp"
                                      :defaults dir)))
                     *load-path*)))
        (assert src-file
                nil
                "Required module ~a but no source file ~a found in ~a" module
                src-file-name
                *load-path*)
        (compile-files (list src-file)
                       (module-binary-dir src-file)
                       'load
                       nil)
        (assert (provided)
                nil
                "Compiled and loaded ~a but required module ~s was not
                provided" src-file module)))))
