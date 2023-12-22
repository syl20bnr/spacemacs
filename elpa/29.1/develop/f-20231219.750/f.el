;;; f.el --- Modern API for working with files and directories -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Version: 0.20.0
;; Package-Requires: ((emacs "24.1") (s "1.7.0") (dash "2.2.0"))
;; Keywords: files, directories
;; Homepage: http://github.com/rejeep/f.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Much inspired by magnar's excellent s.el and dash.el, f.el is a
;; modern API for working with files and directories in Emacs.

;;; Code:



(require 's)
(require 'dash)
(when (version<= "28.1" emacs-version)
  (when (< emacs-major-version 29)
   (require 'f-shortdoc nil t)))

(put 'f-guard-error 'error-conditions '(error f-guard-error))
(put 'f-guard-error 'error-message "Destructive operation outside sandbox")

(defvar f--guard-paths nil
  "List of allowed paths to modify when guarded.

Do not modify this variable.")

(defmacro f--destructive (path &rest body)
  "If PATH is allowed to be modified, yield BODY.

If PATH is not allowed to be modified, throw error."
  (declare (indent 1))
  `(if f--guard-paths
       (if (--any? (or (f-same-p it ,path)
                       (f-ancestor-of-p it ,path)) f--guard-paths)
           (progn ,@body)
         (signal 'f-guard-error (list ,path f--guard-paths)))
     ,@body))


;;;; Paths

(defun f-join (&rest args)
  "Join ARGS to a single path.

Be aware if one of the arguments is an absolute path, `f-join'
will discard all the preceeding arguments and make this absolute
path the new root of the generated path."
  (let (path
        (relative (f-relative-p (car args))))
    (mapc
     (lambda (arg)
       (setq path (cond ((not path) arg)
                        ((f-absolute-p arg)
                         (progn
                           (setq relative nil)
                           arg))
                        (t (f-expand arg path)))))
     args)
    (if relative (f-relative path) path)))

(defun f-split (path)
  "Split PATH and return list containing parts."
  (let ((parts (split-string path (f-path-separator) 'omit-nulls)))
    (if (string= (s-left 1 path) (f-path-separator))
        (push (f-path-separator) parts)
      parts)))

(defun f-expand (path &optional dir)
  "Expand PATH relative to DIR (or `default-directory').
PATH and DIR can be either a directory names or directory file
names.  Return a directory name if PATH is a directory name, and
a directory file name otherwise.  File name handlers are
ignored."
  (let (file-name-handler-alist)
    (expand-file-name path dir)))

(defun f-filename (path)
  "Return the name of PATH."
  (file-name-nondirectory (directory-file-name path)))

(defalias 'f-parent 'f-dirname)

(defun f-dirname (path)
  "Return the parent directory to PATH."
  (let ((parent (file-name-directory
                 (directory-file-name (f-expand path default-directory)))))
    (unless (f-same-p path parent)
      (if (f-relative-p path)
          (f-relative parent)
        (directory-file-name parent)))))

(defun f-common-parent (paths)
  "Return the deepest common parent directory of PATHS."
  (cond
   ((not paths) nil)
   ((not (cdr paths)) (f-parent (car paths)))
   (:otherwise
    (let* ((paths (-map 'f-split paths))
           (common (caar paths))
           (re nil))
      (while (and (not (null (car paths))) (--all? (equal (car it) common) paths))
        (setq paths (-map 'cdr paths))
        (push common re)
        (setq common (caar paths)))
      (cond
       ((null re) "")
       ((and (= (length re) 1) (f-root-p (car re)))
        (f-root))
       (:otherwise
        (concat (apply 'f-join (nreverse re)) "/")))))))

(defalias 'f-ext 'file-name-extension)

(defalias 'f-no-ext 'file-name-sans-extension)

(defun f-swap-ext (path ext)
  "Return PATH but with EXT as the new extension.
EXT must not be nil or empty."
  (if (s-blank-p ext)
      (error "Extension cannot be empty or nil")
    (concat (f-no-ext path) "." ext)))

(defun f-base (path)
  "Return the name of PATH, excluding the extension of file."
  (f-no-ext (f-filename path)))

(defalias 'f-relative 'file-relative-name)

(defalias 'f-short 'abbreviate-file-name)
(defalias 'f-abbrev 'abbreviate-file-name)

(defun f-long (path)
  "Return long version of PATH."
  (f-expand path))

(defalias 'f-canonical 'file-truename)

(defun f-slash (path)
  "Append slash to PATH unless one already.

Some functions, such as `call-process' requires there to be an
ending slash."
  (if (f-dir-p path)
      (file-name-as-directory path)
    path))

(defun f-full (path)
  "Return absolute path to PATH, with ending slash."
  (f-slash (f-long path)))

(defun f--uniquify (paths)
  "Helper for `f-uniquify' and `f-uniquify-alist'."
  (let* ((files-length (length paths))
         (uniq-filenames (--map (cons it (f-filename it)) paths))
         (uniq-filenames-next (-group-by 'cdr uniq-filenames)))
    (while (/= files-length (length uniq-filenames-next))
      (setq uniq-filenames-next
            (-group-by 'cdr
                       (--mapcat
                        (let ((conf-files (cdr it)))
                          (if (> (length conf-files) 1)
                              (--map (cons
                                      (car it)
                                      (concat
                                       (f-filename (s-chop-suffix (cdr it)
                                                                  (car it)))
                                       (f-path-separator) (cdr it)))
                                     conf-files)
                            conf-files))
                        uniq-filenames-next))))
    uniq-filenames-next))

(defun f-uniquify (files)
  "Return unique suffixes of FILES.

This function expects no duplicate paths."
  (-map 'car (f--uniquify files)))

(defun f-uniquify-alist (files)
  "Return alist mapping FILES to unique suffixes of FILES.

This function expects no duplicate paths."
  (-map 'cadr (f--uniquify files)))


;;;; I/O

(defun f-read-bytes (path &optional beg end)
  "Read binary data from PATH.

Return the binary data as unibyte string.  The optional second
and third arguments BEG and END specify what portion of the file
to read."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path nil beg end)
    (buffer-substring-no-properties (point-min) (point-max))))

(defalias 'f-read 'f-read-text)
(defun f-read-text (path &optional coding)
  "Read text with PATH, using CODING.

CODING defaults to `utf-8'.

Return the decoded text as multibyte string."
  (decode-coding-string (f-read-bytes path) (or coding 'utf-8)))

(defalias 'f-write 'f-write-text)
(defun f-write-text (text coding path)
  "Write TEXT with CODING to PATH.

TEXT is a multibyte string.  CODING is a coding system to encode
TEXT with.  PATH is a file name to write to."
  (f-write-bytes (encode-coding-string text coding) path))

(defun f-unibyte-string-p (s)
  "Determine whether S is a unibyte string."
  (not (multibyte-string-p s)))

(defun f-write-bytes (data path)
  "Write binary DATA to PATH.

DATA is a unibyte string.  PATH is a file name to write to."
  (f--write-bytes data path nil))

(defalias 'f-append 'f-append-text)
(defun f-append-text (text coding path)
  "Append TEXT with CODING to PATH.

If PATH does not exist, it is created."
  (f-append-bytes (encode-coding-string text coding) path))

(defun f-append-bytes (data path)
  "Append binary DATA to PATH.

If PATH does not exist, it is created."
  (f--write-bytes data path :append))

(defun f--write-bytes (data filename append)
  "Write binary DATA to FILENAME.
If APPEND is non-nil, append the DATA to the existing contents."
  (f--destructive filename
    (unless (f-unibyte-string-p data)
      (signal 'wrong-type-argument (list 'f-unibyte-string-p data)))
    (let ((coding-system-for-write 'binary)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (write-region data nil filename append :silent)
      nil)))


;;;; Destructive

(defun f-mkdir (&rest dirs)
  "Create directories DIRS.

DIRS should be a successive list of directories forming together
a full path.  The easiest way to call this function with a fully
formed path is using `f-split' alongside it:

    (apply #\\='f-mkdir (f-split \"path/to/file\"))

Although it works sometimes, it is not recommended to use fully
formed paths in the function. In this case, it is recommended to
use `f-mkdir-full-path' instead."
  (let (path)
    (-each
        dirs
      (lambda (dir)
        (setq path (f-expand dir path))
        (unless (f-directory-p path)
          (f--destructive path (make-directory path)))))))

(defun f-mkdir-full-path (dir)
  "Create DIR from a full path.

This function is similar to `f-mkdir' except it can accept a full
path instead of requiring several successive directory names."
  (apply #'f-mkdir (f-split dir)))

(defun f-delete (path &optional force)
  "Delete PATH, which can be file or directory.

If FORCE is t, a directory will be deleted recursively."
  (f--destructive path
    (if (or (f-file-p path) (f-symlink-p path))
        (delete-file path)
      (delete-directory path force))))

(defun f-symlink (source path)
  "Create a symlink to SOURCE from PATH."
  (f--destructive path (make-symbolic-link source path)))

(defun f-move (from to)
  "Move or rename FROM to TO.
If TO is a directory name, move FROM into TO."
  (f--destructive to (rename-file from to t)))

(defun f-copy (from to)
  "Copy file or directory FROM to TO.
If FROM names a directory and TO is a directory name, copy FROM
into TO as a subdirectory."
  (f--destructive to
    (if (f-file-p from)
        (copy-file from to)
      ;; The behavior of `copy-directory' differs between Emacs 23 and
      ;; 24 in that in Emacs 23, the contents of `from' is copied to
      ;; `to', while in Emacs 24 the directory `from' is copied to
      ;; `to'. We want the Emacs 24 behavior.
      (if (> emacs-major-version 23)
          (copy-directory from to)
        (if (f-dir-p to)
            (progn
              (apply 'f-mkdir (f-split to))
              (let ((new-to (f-expand (f-filename from) to)))
                (copy-directory from new-to)))
          (copy-directory from to))))))

(defun f-copy-contents (from to)
  "Copy contents in directory FROM, to directory TO."
  (unless (f-exists-p to)
    (error "Cannot copy contents to non existing directory %s" to))
  (unless (f-dir-p from)
    (error "Cannot copy contents as %s is a file" from))
  (--each (f-entries from)
    (f-copy it (file-name-as-directory to))))

(defun f-touch (path)
  "Update PATH last modification date or create if it does not exist."
  (f--destructive path
    (if (f-file-p path)
        (set-file-times path)
      (f-write-bytes "" path))))


;;;; Predicates

(defalias 'f-exists-p 'file-exists-p)
(defalias 'f-exists? 'file-exists-p)

(defalias 'f-directory-p 'file-directory-p)
(defalias 'f-directory? 'file-directory-p)
(defalias 'f-dir-p 'file-directory-p)
(defalias 'f-dir? 'file-directory-p)


(defalias 'f-file-p 'file-regular-p)
(defalias 'f-file? 'file-regular-p)

(defun f-symlink-p (path)
  "Return t if PATH is symlink, false otherwise."
  (not (not (file-symlink-p path))))

(defalias 'f-symlink? 'f-symlink-p)

(defalias 'f-readable-p 'file-readable-p)
(defalias 'f-readable? 'file-readable-p)

(defalias 'f-writable-p 'file-writable-p)
(defalias 'f-writable? 'file-writable-p)

(defalias 'f-executable-p 'file-executable-p)
(defalias 'f-executable? 'file-executable-p)

(defalias 'f-absolute-p 'file-name-absolute-p)
(defalias 'f-absolute? 'file-name-absolute-p)

(defun f-relative-p (path)
  "Return t if PATH is relative, false otherwise."
  (not (f-absolute-p path)))

(defalias 'f-relative? 'f-relative-p)

(defun f-root-p (path)
  "Return t if PATH is root directory, false otherwise."
  (not (f-parent path)))

(defalias 'f-root? 'f-root-p)

(defun f-ext-p (path &optional ext)
  "Return t if extension of PATH is EXT, false otherwise.

If EXT is nil or omitted, return t if PATH has any extension,
false otherwise.

The extension, in a file name, is the part that follows the last
'.', excluding version numbers and backup suffixes."
  (if ext
      (string= (f-ext path) ext)
    (not (eq (f-ext path) nil))))

(defalias 'f-ext? 'f-ext-p)

(defalias 'f-equal-p 'f-same-p)
(defalias 'f-equal? 'f-same-p)

(defun f-same-p (path-a path-b)
  "Return t if PATH-A and PATH-B are references to same file."
  (equal
   (f-canonical (directory-file-name (f-expand path-a)))
   (f-canonical (directory-file-name (f-expand path-b)))))

(defalias 'f-same? 'f-same-p)

(defun f-parent-of-p (path-a path-b)
  "Return t if PATH-A is parent of PATH-B."
  (--when-let (f-parent path-b)
    (f-same-p path-a it)))

(defalias 'f-parent-of? 'f-parent-of-p)

(defun f-child-of-p (path-a path-b)
  "Return t if PATH-A is child of PATH-B."
  (--when-let (f-parent path-a)
    (f-same-p it path-b)))

(defalias 'f-child-of? 'f-child-of-p)

(defun f-ancestor-of-p (path-a path-b)
  "Return t if PATH-A is ancestor of PATH-B."
  (unless (f-same-p path-a path-b)
    (string-prefix-p (f-full path-a)
                     (f-full path-b))))

(defalias 'f-ancestor-of? 'f-ancestor-of-p)

(defun f-descendant-of-p (path-a path-b)
  "Return t if PATH-A is desendant of PATH-B."
  (unless (f-same-p path-a path-b)
    (let ((path-a (f-split (f-full path-a)))
          (path-b (f-split (f-full path-b)))
          (parent-p t))
      (while (and path-b parent-p)
        (if (string= (car path-a) (car path-b))
            (setq path-a (cdr path-a)
                  path-b (cdr path-b))
          (setq parent-p nil)))
      parent-p)))

(defalias 'f-descendant-of? 'f-descendant-of-p)

(defun f-hidden-p (path &optional behavior)
  "Return t if PATH is hidden, nil otherwise.

BEHAVIOR controls when a path should be considered as hidden
depending on its value.  Beware, if PATH begins with \"./\", the
current dir \".\" will not be considered as hidden.

When BEHAVIOR is nil, it will only check if the path begins with
a dot, as in .a/b/c, and return t if there is one.  This is the
old behavior of f.el left as default for backward-compatibility
purposes.

When BEHAVIOR is ANY, return t if any of the elements of PATH is
hidden, nil otherwise.

When BEHAVIOR is LAST, return t only if the last element of PATH
is hidden, nil otherwise.

TODO: Hidden directories and files on Windows are marked
differently than on *NIX systems.  This should be properly
implemented."
  (let ((split-path (f-split path))
        (check-hidden (lambda (elt)
                        (and (string= (substring elt 0 1) ".")
                             (not (member elt '("." "..")))))))
    (pcase behavior
      ('any  (-any check-hidden split-path))
      ('last (apply check-hidden (last split-path)))
      (otherwise (if (null otherwise)
                     (funcall check-hidden (car split-path))
                   (error "Invalid value %S for argument BEHAVIOR" otherwise))))))

(defalias 'f-hidden? 'f-hidden-p)

(defun f-empty-p (path)
  "If PATH is a file, return t if the file in PATH is empty, nil otherwise.
If PATH is directory, return t if directory has no files, nil otherwise."
  (if (f-directory-p path)
      (equal (f-files path nil t) nil)
    (= (f-size path) 0)))

(defalias 'f-empty? 'f-empty-p)


;;;; Stats

(defun f-size (path)
  "Return size of PATH.

If PATH is a file, return size of that file.  If PATH is
directory, return sum of all files in PATH."
  (if (f-directory-p path)
      (-sum (-map 'f-size (f-files path nil t)))
    (nth 7 (file-attributes path))))

(defun f-depth (path)
  "Return the depth of PATH.

At first, PATH is expanded with `f-expand'.  Then the full path is used to
detect the depth.
'/' will be zero depth,  '/usr' will be one depth.  And so on."
  (- (length (f-split (f-expand path))) 1))

;; For Emacs 28 and below, forward-declare ‘current-time-list’, which was
;; introduced in Emacs 29.
(defvar current-time-list)

(defun f--get-time (path timestamp-p fn)
  "Helper function, get time-related information for PATH.
Helper for `f-change-time', `f-modification-time',
`f-access-time'.  It is meant to be called internally, avoid
calling it manually unless you have to.

If TIMESTAMP-P is non-nil, return the date requested as a
timestamp.  If the value is \\='seconds, return the timestamp as
a timestamp with a one-second precision.  Otherwise, the
timestamp is returned in a (TICKS . HZ) format, see
`current-time' if using Emacs 29 or newer.

Otherwise, if TIMESTAMP-P is nil, return the default style of
`current-time'.

FN is the function specified by the caller function to retrieve
the correct data from PATH."
      (let* ((current-time-list (not timestamp-p))
             (date (apply fn (list (file-attributes path))))
             (emacs29-or-newer-p (version<= "29" emacs-version)))
        (cond
         ((and (eq timestamp-p 'seconds) emacs29-or-newer-p)
          (/ (car date) (cdr date)))
         ((or (and (not (eq timestamp-p 'seconds)) emacs29-or-newer-p)
              (and (not timestamp-p) (not emacs29-or-newer-p)))
          date)
         ((and (eq timestamp-p 'seconds) (not emacs29-or-newer-p))
          (+ (* (nth 0 date) (expt 2 16))
             (nth 1 date)))
         ((and timestamp-p (not emacs29-or-newer-p))
          `(,(+ (* (nth 0 date) (expt 2 16) 1000)
                (* (nth 1 date) 1000)
                (nth 3 date))
            . 1000)))))

(defun f-change-time (path &optional timestamp-p)
  "Return the last status change time of PATH.

The status change time (ctime) of PATH in the same format as
`current-time'.  For details on TIMESTAMP-P and the format of the
returned value, see `f--get-time'."
  (f--get-time path
               timestamp-p
               (if (fboundp 'file-attribute-status-change-time)
                   #'file-attribute-status-change-time
                 (lambda (f) (nth 6 f)))))

(defun f-modification-time (path &optional timestamp-p)
  "Return the last modification time of PATH.
The modification time (mtime) of PATH in the same format as
`current-time'.  For details on TIMESTAMP-P and the format of the
returned value, see `f--get-time'."
  (f--get-time path
               timestamp-p
               (if (fboundp 'file-attribute-modification-time)
                   #'file-attribute-modification-time
                 (lambda (f) (nth 5 f)))))

(defun f-access-time (path &optional timestamp-p)
  "Return the last access time of PATH.
The access time (atime) of PATH is in the same format as
`current-time'.  For details on TIMESTAMP-P and the format of the
returned value, see `f--get-time'."
  (f--get-time path
               timestamp-p
               (if (fboundp 'file-attribute-access-time)
                   #'file-attribute-access-time
                 (lambda (f) (nth 4 f)))))

(defun f--three-way-compare (a b)
  "Three way comparison.

Return -1 if A < B.
Return 0 if A = B.
Return 1 if A > B."
  (cond ((< a b) -1)
        ((= a b) 0)
        ((> a b) 1)))

;; TODO: How to properly test this function?
(defun f--date-compare (file other method)
  "Three-way comparison of the date of FILE and OTHER.

This function can return three values:
* 1 means FILE is newer than OTHER
* 0 means FILE and NEWER share the same date
* -1 means FILE is older than OTHER

The statistics used for the date comparison depends on METHOD.
When METHOD is null, compare their modification time.  Otherwise,
compare their change time when METHOD is \\='change, or compare
their last access time when METHOD is \\='access."
  (let* ((fn-method (cond
                     ((eq 'change method) #'f-change-time)
                     ((eq 'access method) #'f-access-time)
                     ((null method)       #'f-modification-time)
                     (t (error "Unknown method %S" method))))
         (date-file (apply fn-method (list file)))
         (date-other (apply fn-method (list other)))
         (dates      (-zip-pair date-file date-other)))
    (-reduce-from (lambda (acc elt)
                    (if (= acc 0)
                        (f--three-way-compare (car elt) (cdr elt))
                      acc))
                  0
                  dates)))

(defun f-older-p (file other &optional method)
  "Compare if FILE is older than OTHER.

For more info on METHOD, see `f--date-compare'."
  (< (f--date-compare file other method) 0))

(defalias 'f-older? #'f-older-p)

(defun f-newer-p (file other &optional method)
  "Compare if FILE is newer than OTHER.

For more info on METHOD, see `f--date-compare'."
  (> (f--date-compare file other method) 0))

(defalias 'f-newer? #'f-newer-p)

(defun f-same-time-p (file other &optional method)
  "Check if FILE and OTHER share the same access or modification time.

For more info on METHOD, see `f--date-compare'."
  (= (f--date-compare file other method) 0))

(defalias 'f-same-time? #'f-same-time-p)


;;;; Misc

(defun f-this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))

(defvar f--path-separator nil
  "A variable to cache result of `f-path-separator'.")

(defun f-path-separator ()
  "Return path separator."
  (or f--path-separator
      (setq f--path-separator (substring (f-join "x" "y") 1 2))))

(defun f-glob (pattern &optional path)
  "Find PATTERN in PATH."
  (file-expand-wildcards
   (f-join (or path default-directory) pattern)))

(defun f--collect-entries (path recursive)
  (let (result
        (entries
         (-reject
          (lambda (file)
            (member (f-filename file) '("." "..")))
          (directory-files path t))))
    (cond (recursive
           (mapc
            (lambda (entry)
              (if (f-file-p entry)
                  (setq result (cons entry result))
                (when (f-directory-p entry)
                  (setq result (cons entry result))
                  (if (f-readable-p entry)
                      (setq result (append result (f--collect-entries entry recursive)))
                    result))))
            entries))
          (t (setq result entries)))
    result))

(defmacro f--entries (path body &optional recursive)
  "Anaphoric version of `f-entries'."
  `(f-entries
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-entries (path &optional fn recursive)
  "Find all files and directories in PATH.

FN - called for each found file and directory.  If FN returns a thruthy
value, file or directory will be included.
RECURSIVE - Search for files and directories recursive."
  (let ((entries (f--collect-entries path recursive)))
    (if fn (-select fn entries) entries)))

(defmacro f--directories (path body &optional recursive)
  "Anaphoric version of `f-directories'."
  `(f-directories
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-directories (path &optional fn recursive)
  "Find all directories in PATH.  See `f-entries'."
  (let ((directories (-select 'f-directory-p (f--collect-entries path recursive))))
    (if fn (-select fn directories) directories)))

(defmacro f--files (path body &optional recursive)
  "Anaphoric version of `f-files'."
  `(f-files
    ,path
    (lambda (path)
      (let ((it path))
        ,body))
    ,recursive))

(defun f-files (path &optional fn recursive)
  "Find all files in PATH.  See `f-entries'."
  (let ((files (-select 'f-file-p (f--collect-entries path recursive))))
    (if fn (-select fn files) files)))

(defmacro f--traverse-upwards (body &optional path)
  "Anaphoric version of `f-traverse-upwards'."
  `(f-traverse-upwards
    (lambda (dir)
      (let ((it dir))
        ,body))
    ,path))

(defun f-traverse-upwards (fn &optional path)
  "Traverse up as long as FN return nil, starting at PATH.

If FN returns a non-nil value, the path sent as argument to FN is
returned.  If no function callback return a non-nil value, nil is
returned."
  (unless path
    (setq path default-directory))
  (when (f-relative-p path)
    (setq path (f-expand path)))
  (if (funcall fn path)
      path
    (unless (f-root-p path)
      (f-traverse-upwards fn (f-parent path)))))

(defun f-root ()
  "Return absolute root."
  (f-traverse-upwards 'f-root-p))

(defmacro f-with-sandbox (path-or-paths &rest body)
  "Only allow PATH-OR-PATHS and descendants to be modified in BODY."
  (declare (indent 1))
  `(let ((paths (if (listp ,path-or-paths)
                    ,path-or-paths
                  (list ,path-or-paths))))
     (unwind-protect
         (let ((f--guard-paths paths))
           ,@body)
       (setq f--guard-paths nil))))

(provide 'f)

;;; f.el ends here
