;; -*- lexical-binding: t; -*-
(require 'sly)
(require 'tramp)
(require 'cl-lib)

(define-sly-contrib sly-tramp
  "Filename translations for tramp"
  (:authors "Marco Baringer <mb@bese.it>")
  (:license "GPL")
  (:on-load 
   (setq sly-to-lisp-filename-function #'sly-tramp-to-lisp-filename)
   (setq sly-from-lisp-filename-function #'sly-tramp-from-lisp-filename)))

(defcustom sly-filename-translations nil
  "Assoc list of hostnames and filename translation functions.  
Each element is of the form (HOSTNAME-REGEXP TO-LISP FROM-LISP).

HOSTNAME-REGEXP is a regexp which is applied to the connection's
sly-machine-instance. If HOSTNAME-REGEXP maches then the
corresponding TO-LISP and FROM-LISP functions will be used to
translate emacs filenames and lisp filenames.

TO-LISP will be passed the filename of an emacs buffer and must
return a string which the underlying lisp understandas as a
pathname. FROM-LISP will be passed a pathname as returned by the
underlying lisp and must return something that emacs will
understand as a filename (this string will be passed to
find-file).

This list will be traversed in order, so multiple matching
regexps are possible.

Example:

Assuming you run emacs locally and connect to sly running on
the machine 'soren' and you can connect with the username
'animaliter':

  (push (list \"^soren$\"
              (lambda (emacs-filename)
                (subseq emacs-filename (length \"/ssh:animaliter@soren:\")))
              (lambda (lisp-filename)
                (concat \"/ssh:animaliter@soren:\" lisp-filename)))
        sly-filename-translations)

See also `sly-create-filename-translator'."
  :type '(repeat (list :tag "Host description"
                       (regexp :tag "Hostname regexp")
                       (function :tag "To   lisp function")
                       (function :tag "From lisp function")))
  :group 'sly-lisp)

(defun sly-find-filename-translators (hostname)
  (cond ((cdr (cl-assoc-if (lambda (regexp) (string-match regexp hostname))
                           sly-filename-translations)))
        (t (list #'identity #'identity))))

(defun sly-make-tramp-file-name (username remote-host lisp-filename)
  "Tramp compatability function.

Handles the signature of `tramp-make-tramp-file-name' changing
over time."
  (cond
   ((>= emacs-major-version 26)
    ;; Emacs 26 requires the method to be provided and the signature of
    ;; `tramp-make-tramp-file-name' has changed.
    (tramp-make-tramp-file-name (tramp-find-method nil username remote-host)
                                username
                                nil
                                remote-host
                                nil
                                lisp-filename))
   ((boundp 'tramp-multi-methods)
    (tramp-make-tramp-file-name nil nil
                                username
                                remote-host
                                lisp-filename))
   (t
    (tramp-make-tramp-file-name nil
                                username
                                remote-host
                                lisp-filename))))

(cl-defun sly-create-filename-translator (&key machine-instance
                                               remote-host
                                               username)
  "Creates a three element list suitable for push'ing onto
sly-filename-translations which uses Tramp to load files on
hostname using username. MACHINE-INSTANCE is a required
parameter, REMOTE-HOST defaults to MACHINE-INSTANCE and USERNAME
defaults to (user-login-name).

MACHINE-INSTANCE is the value returned by sly-machine-instance,
which is just the value returned by cl:machine-instance on the
remote lisp. REMOTE-HOST is the fully qualified domain name (or
just the IP) of the remote machine. USERNAME is the username we
should login with.
The functions created here expect your tramp-default-method or
 tramp-default-method-alist to be setup correctly."
  (let ((remote-host (or remote-host machine-instance))
        (username (or username (user-login-name))))
    (list (concat "^" machine-instance "$")
          (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          `(lambda (lisp-filename)
             (sly-make-tramp-file-name
              ,username
              ,remote-host
              lisp-filename)))))

(defun sly-tramp-to-lisp-filename (filename)
  (funcall (if (let ((conn (sly-current-connection)))
                 (and conn (process-live-p conn)))
               (cl-first (sly-find-filename-translators (sly-machine-instance)))
             'identity)
           (expand-file-name filename)))

(defun sly-tramp-from-lisp-filename (filename)
  (funcall (cl-second (sly-find-filename-translators (sly-machine-instance)))
           filename))

(provide 'sly-tramp)
