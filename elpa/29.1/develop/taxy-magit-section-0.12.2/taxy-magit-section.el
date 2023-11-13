;;; taxy-magit-section.el --- View Taxy structs in a Magit Section buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/taxy.el
;; Version: 0.12.2
;; Package-Requires: ((emacs "26.3") (magit-section "3.2.1") (taxy "0.10"))
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a way to view `taxy' structs in a
;; column-based, `magit-section' buffer.  Columns are defined using
;; simple top-level forms, and new columns may be easily defined by
;; users in their configurations.

;;; Code:

;;;; Requirements

(require 'map)

(require 'taxy)
(require 'magit-section)

;;;; Variables

(defvar taxy-magit-section-level-indent 2
  "Default heading indentation per level.")

(defvar taxy-magit-section-item-indent 2
  "Default item indentation per level.")

(defvar taxy-magit-section-depth nil
  "Bound to current depth around calls to a taxy's format-fn.")

(defvar taxy-magit-section-insert-indent-items t
  ;; NOTE: I hate to use a variable to control this, but it seems like
  ;; the cleanest way for now.
  "Whether to indent items in `taxy-magit-section-insert'.
May be disabled when `taxy-magit-section-insert' should not
indent items itself, e.g. if items are pre-indented.  Note that
this does not disable indentation of section headings.")

;;;; Customization


;;;; Structs

;; NOTE: When making `taxy-magit-section' structs at runtime
;; (e.g. with `taxy-take-keyed'), the struct's `make' slot must be set
;; to a function that returns a new struct with the other slots set as
;; desired; the slots' values do not automatically propagate to
;; structs with the default `make' function.  (Using `cl-labels' to
;; define the `make' function makes this simple.)

;; MAYBE: In `taxy-take-keyed', use `taxy-emptied' to copy structs
;; with inheritance for relevant slots, so defining custom `make'
;; functions wouldn't be necessary.

(cl-defstruct (taxy-magit-section
               (:include taxy
                         (make #'make-taxy-magit-section)))
  ;; MAYBE: Pass parent section to the :make function, would make
  ;; inheritance easier (and/or use EIEIO, but that would reduce
  ;; performance, since slot accessors can't be optimized).
  (visibility-fn #'taxy-magit-section-visibility)
  (heading-face-fn (lambda (_depth) 'magit-section-heading))
  (level-indent 2)
  (item-indent 2)
  (format-fn #'prin1-to-string))

(defclass taxy-magit-section-section (magit-section)
  ;; We define this class so we can use it as the type of section we insert, so we can
  ;; define a method to return identifiers for our section type, so section visibility can
  ;; be cached.
  nil)

(cl-defmethod magit-section-ident-value ((section taxy-magit-section-section))
  ;; FIXME: The name of each taxy could be ambiguous.  Best would be to use the
  ;; hierarchical path, but since the taxys aren't doubly linked, that isn't easily done.
  ;; Could probably be worked around by binding a special variable around the creation of
  ;; the taxy hierarchy that would allow the path to be saved into each taxy.
  (when-let ((taxy (oref section value)))
    (taxy-name taxy)))

;;;; Commands


;;;; Functions

(cl-defun taxy-magit-section-insert
    (taxy &key (items 'first) (initial-depth 0) (blank-between-depth 1))
  "Insert a `magit-section' for TAXY into current buffer.
If ITEMS is `first', insert a taxy's items before its descendant
taxys; if `last', insert them after descendants.  INITIAL-DEPTH
is the initial indentation depth; it may be, e.g. -1 to make the
second level unindented.  BLANK-BETWEEN-DEPTH is the level up to
which blank lines are inserted between sections at that level."
  (declare (indent defun))
  (let* ((magit-section-set-visibility-hook
          (cons #'taxy-magit-section-visibility magit-section-set-visibility-hook)))
    (cl-labels ((insert-item
                 (item taxy depth)
                 (magit-insert-section (magit-section item)
                   (magit-insert-section-body
                     ;; This is a tedious way to give the indent
                     ;; string the same text properties as the start
                     ;; of the formatted string, but no matter where I
                     ;; left point after using `insert-and-inherit',
                     ;; something was wrong about the properties, and
                     ;; `magit-section' didn't navigate the sections
                     ;; properly anymore.
                     (let* ((formatted (funcall (taxy-magit-section-format-fn taxy) item))
                            (indent-size (if (or (not taxy-magit-section-insert-indent-items)
                                                 (< depth 0))
                                             0
                                           (+ (* depth (taxy-magit-section-level-indent taxy))
                                              (taxy-magit-section-item-indent taxy))))
                            (indent-string (make-string indent-size ? )))
                       (add-text-properties 0 (length indent-string)
                                            (text-properties-at 0 formatted)
                                            indent-string)
                       (insert indent-string formatted "\n")))))
                (insert-taxy
                 (taxy depth)
                 (let ((magit-section-set-visibility-hook magit-section-set-visibility-hook)
                       (taxy-magit-section-level-indent (taxy-magit-section-level-indent taxy))
                       (taxy-magit-section-item-indent (taxy-magit-section-item-indent taxy))
                       (taxy-name (copy-sequence (taxy-name taxy))))
                   (add-face-text-property
                    0 (length taxy-name)
                    (funcall (taxy-magit-section-heading-face-fn taxy) depth)
                    t taxy-name)
                   (cl-typecase taxy
                     (taxy-magit-section
                      (when (taxy-magit-section-visibility-fn taxy)
                        (push (taxy-magit-section-visibility-fn taxy)
                              magit-section-set-visibility-hook))))
                   ;; HACK: We set the section's washer to nil to prevent
                   ;; `magit-section--maybe-wash' from trying to wash the section when its
                   ;; visibility is toggled back on.  I'm not sure why this is necessary
                   ;; (maybe an issue in magit-section?).
                   (oset (magit-insert-section (taxy-magit-section-section taxy)
                           (magit-insert-heading
                             (make-string (* (if (< depth 0) 0 depth)
                                             (taxy-magit-section-level-indent taxy))
                                          ? )
                             taxy-name
                             (format " (%s%s)"
                                     (if (taxy-description taxy)
                                         (concat (taxy-description taxy) " ")
                                       "")
                                     (taxy-size taxy)))
                           (magit-insert-section-body
                             (when (eq 'first items)
                               (dolist (item (taxy-items taxy))
                                 (insert-item item taxy depth)))
                             (dolist (taxy (taxy-taxys taxy))
                               (insert-taxy taxy (1+ depth)))
                             (when (eq 'last items)
                               (dolist (item (taxy-items taxy))
                                 (insert-item item taxy depth))))
                           (when (<= depth blank-between-depth)
                             (insert "\n")))
                         washer nil))))
      ;; HACK: See earlier note about washer.
      (oset (magit-insert-section (taxy-magit-section-section)
              (insert-taxy taxy initial-depth))
            washer nil))))

(cl-defun taxy-magit-section-pp (taxy &key (items 'first))
  "Pretty-print TAXY into a buffer with `magit-section' and show it."
  (with-current-buffer (get-buffer-create "*taxy-magit-section-pp*")
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (taxy-magit-section-insert taxy :items items))
    (pop-to-buffer (current-buffer))))

(defun taxy-magit-section-visibility (section)
  "Show SECTION if its taxy is non-empty.
Default visibility function for
`magit-section-set-visibility-hook'."
  (pcase (oref section value)
    ((and (pred taxy-p) taxy)
     (pcase (taxy-size taxy)
       (0 'hide)
       (_ (or (magit-section-cached-visibility section)
              'show))))
    (_ nil)))

;;;; Column-based formatting

;; Column-based, or "table"?

;; MAYBE: Move this to a separate library, since it's not directly
;; related to using taxy or magit-section.  Maybe it could be called
;; something like `flextab' (or, keeping with the theme, `tabley').
;; But see also <https://github.com/kiwanami/emacs-ctable>.

;;;;; Macros

(cl-defmacro taxy-magit-section-define-column-definer
    (prefix &key columns-variable-docstring)
  "Define a column-defining macro.
The macro is named \"PREFIX-define-column\".

These customization options are defined, which are to be used in
a `taxy-magit-section' in its `:level-indent' and `:item-indent'
slots, respectively:

  - PREFIX-level-indent
  - PREFIX-item-indent

As well as these variables, which are to be passed to
`taxy-magit-section-format-items':

  - PREFIX-columns
  - PREFIX-column-formatters"
  ;; TODO: Document this.
  (let* ((definer-name (intern (format "%s-define-column" prefix)))
         (definer-docstring (format "Define a column formatting function with NAME.
NAME should be a string.  BODY should return a string or nil.  In
the BODY, `item' is bound to the item being formatted, and `depth' is
bound to the item's depth in the hierarchy.

PLIST may be a plist setting the following options:

  `:align' may be `left' or `right' to align the column
  accordingly.

  `:face' is a face applied to the string.

  `:max-width' defines a customization option for the column's
  maximum width with the specified value as its default: an
  integer limits the width, while nil does not."))
         (level-indent-variable-name (intern (format "%s-level-indent" prefix)))
         (level-indent-docstring (format "Indentation applied to each level of depth for `%s' columns."
                                         prefix))
         (item-indent-variable-name (intern (format "%s-item-indent" prefix)))
         (item-indent-docstring (format "Indentation applied to each item for `%s' columns."
                                        prefix))
         (columns-variable-name (intern (format "%s-columns" prefix)))
         (columns-variable-docstring (or columns-variable-docstring
                                         (format "Columns defined by `%s'."
                                                 definer-name)))
         (column-formatters-variable-name (intern (format "%s-column-formatters" prefix)))
         (column-formatters-variable-docstring (format "Column formatters defined by `%s'."
                                                       definer-name)))
    `(let ((columns-variable ',columns-variable-name)
           (column-formatters-variable ',column-formatters-variable-name))
       (defcustom ,level-indent-variable-name 2
         ,level-indent-docstring
         :type 'integer)
       (defcustom ,item-indent-variable-name 2
         ,item-indent-docstring
         :type 'integer)
       (defvar ,columns-variable-name nil
         ,columns-variable-docstring)
       (defvar ,column-formatters-variable-name nil
         ,column-formatters-variable-docstring)
       (defmacro ,definer-name (name plist &rest body)
         ,definer-docstring
         (declare (indent defun))
         (cl-check-type name string)
         (pcase-let* ((fn-name (intern (concat ,prefix "-column-format-" (downcase name))))
                      (columns-variable-name ',columns-variable-name)
                      (level-indent-variable-name ',level-indent-variable-name)
                      (item-indent-variable-name ',item-indent-variable-name)
                      ((map (:face face) (:max-width max-width)) plist)
                      (max-width-variable (intern (concat ,prefix "-column-" name "-max-width")))
                      (max-width-docstring (format "Maximum width of the %s column." name)))
           `(progn
              ,(when (plist-member plist :max-width)
                 `(defcustom ,max-width-variable
                    ,max-width
                    ,max-width-docstring
                    :type '(choice (integer :tag "Maximum width")
                                   (const :tag "Unlimited width" nil))))
              (defun ,fn-name (item depth)
                (if-let ((string (progn ,@body)))
                    (progn
                      ,(when max-width
                         `(when ,max-width-variable
                            ;; I don't like having to save a copy of the old string for
                            ;; comparison, but given the way `truncate-string-to-width'
                            ;; calculates widths, I don't see much alternative.  It would
                            ;; be nice if it returned nil when no change was made.
                            (let ((old-string string)
                                  (new-string (truncate-string-to-width
                                               string ,max-width-variable nil nil "…")))
                              (unless (equal old-string new-string)
                                ;; String was elided: add help-echo.
                                (put-text-property 0 (length new-string) 'help-echo old-string new-string)
                                (setf string new-string)))))
                      ,(when face
                         ;; Faces are not defined until load time, while this checks type at expansion
                         ;; time, so we can only test that the argument is a symbol, not a face.
                         (cl-check-type face symbol ":face must be a face symbol")
                         `(setf string (propertize string 'face ',face)))
                      (when (equal ,name (car ,columns-variable-name))
                        ;; First column: apply indentation.
                        (let ((indentation (make-string (+ (* depth ,level-indent-variable-name)
                                                           ,item-indent-variable-name)
                                                        ? )))
                          (setf string (concat indentation string))))
                      string)
                  ""))
              (setf (alist-get 'formatter
                               (alist-get ,name ,column-formatters-variable nil nil #'equal))
                    #',fn-name)
              (setf (alist-get 'align
                               (alist-get ,name ,column-formatters-variable nil nil #'equal))
                    ,(plist-get plist :align))
              ;; Add column to the columns-variable's standard value.
              (unless (member ,name (get ',columns-variable 'standard-value))
                (setf (get ',columns-variable 'standard-value)
                      (append (get ',columns-variable 'standard-value)
                              (list ,name))))
              ;; Add column to the columns-variable's custom type.
              (cl-pushnew ,name (get ',columns-variable 'custom-type)
                          :test #'equal)))))))

;;;;; Functions

;; MAYBE: Consider using spaces with `:align-to', rather than formatting strings with
;; indentation, as used by `epkg' (see
;; <https://github.com/emacscollective/epkg/blob/edf8c009066360af61caedf67a2482eaa19481b0/epkg-desc.el#L363>).
;; I'm not sure which would perform better; I guess that with many lines, redisplay might
;; take longer to use the display properties for alignment than just having pre-aligned
;; lines of text.

(defun taxy-magit-section-format-items (columns formatters taxy)
  ;; TODO: Document this.
  "Return a cons (table . column-sizes) for COLUMNS, FORMATTERS, and TAXY.
COLUMNS is a list of column names, each of which should have an
associated formatting function in FORMATTERS.

Table is a hash table keyed by item whose values are display
strings.  Column-sizes is an alist whose keys are column names
and values are the column width.  Each string is formatted
according to `columns' and takes into account the width of all
the items' values for each column."
  (let ((table (make-hash-table))
        column-aligns column-sizes image-p)
    (cl-labels ((string-width*
                 (string) (if-let (pos (text-property-not-all 0 (length string)
                                                              'display nil string))
                              ;; Text has a display property: check for an image.
                              (pcase (get-text-property pos 'display string)
                                ((and `(image . ,_rest) spec)
                                 ;; An image: try to calcuate the display width.  (See also:
                                 ;; `org-string-width'.)

                                 ;; FIXME: The entire string may not be an image, so the
                                 ;; image part needs to be handled separately from any
                                 ;; non-image part.

                                 ;; TODO: Do we need to specify the frame?  What if the
                                 ;; buffer isn't currently displayed?
                                 (setf image-p t)
                                 (floor (car (image-size spec))))
                                (_
                                 ;; No image: just use `string-width'.
                                 (setf image-p nil)
                                 (string-width string)))
                            ;; No display property.
                            (setf image-p nil)
                            (string-width string)))
                (resize-image-string
                 (string width) (let ((image
                                       (get-text-property
                                        (text-property-not-all 0 (length string)
                                                               'display nil string)
                                        'display string)))
                                  (propertize (make-string width ? ) 'display image)))

                (format-column
                 (item depth column-name)
                 (let* ((column-alist (alist-get column-name formatters nil nil #'equal))
                        (fn (alist-get 'formatter column-alist))
                        (value (funcall fn item depth))
                        (current-column-size (or (map-elt column-sizes column-name) (string-width column-name))))
                   (setf (map-elt column-sizes column-name)
                         (max current-column-size (string-width* value)))
                   (setf (map-elt column-aligns column-name)
                         (or (alist-get 'align column-alist)
                             'left))
                   (when image-p
                     ;; String probably is an image: set its non-image string value to a
                     ;; number of matching spaces.  It's not always pixel-perfect, but
                     ;; this is probably as good as we can do without using pixel-based
                     ;; :align-to's for everything (which might be worth doing in the
                     ;; future).

                     ;; FIXME: This only works properly if the entire string has an image
                     ;; display property (but this is good enough for now).
                     (setf value (resize-image-string value (string-width* value))))
                   value))
                (format-item
                 (depth item) (puthash item
                                       (cl-loop for column in columns
                                                collect (format-column item depth column))
                                       table))
                (format-taxy (depth taxy)
                             (dolist (item (taxy-items taxy))
                               (format-item depth item))
                             (dolist (taxy (taxy-taxys taxy))
                               (format-taxy (1+ depth) taxy))))
      (format-taxy 0 taxy)
      ;; Now format each item's string using the column sizes.
      (let* ((column-sizes (nreverse column-sizes))
             (format-string
              (string-join
               (cl-loop for (name . size) in column-sizes
                        for align = (pcase-exhaustive (alist-get name column-aligns nil nil #'equal)
                                      ((or `nil 'left) "-")
                                      ('right ""))
                        collect (format "%%%s%ss" align size))
               " ")))
        (maphash (lambda (item column-values)
                   (puthash item (apply #'format format-string column-values)
                            table))
                 table)
        (cons table column-sizes)))))

(defun taxy-magit-section-format-header (column-sizes formatters)
  ;; TODO: Document this.
  "Return header string for COLUMN-SIZES and FORMATTERS.
COLUMN-SIZES should be the CDR of the cell returned by
`taxy-magit-section-format-items'.  FORMATTERS should be the
variable passed to that function, which see."
  (let* ((first-column-name (caar column-sizes))
         (first-column-alist (alist-get first-column-name formatters nil nil #'equal))
         (first-column-align (pcase-exhaustive (alist-get 'align first-column-alist)
                               ((or `nil 'left) "-")
                               ('right ""))))
    (concat (format (format " %%%s%ss"
                            ;; FIXME: Why is this 1+ necessary for proper alignment?
                            first-column-align (1+ (cdar column-sizes)))
                    (caar column-sizes))
            (cl-loop for (name . size) in (cdr column-sizes)
                     for column-alist = (alist-get name formatters nil nil #'equal)
                     for align = (pcase-exhaustive (alist-get 'align column-alist)
                                   ((or `nil 'left) "-")
                                   ('right ""))
                     for spec = (format " %%%s%ss" align size)
                     concat (format spec name)))))

;;;; Footer

(provide 'taxy-magit-section)

;;; taxy-magit-section.el ends here
