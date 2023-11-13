;;; esxml.el --- Library for working with xml via esxml and sxml
;; Copyright (C) 2012

;; Author: Vanya Izaksonas-Smith <izak0002 at umn dot edu>
;; Maintainer: Vanya Izaksonas-Smith
;; URL: https://github.com/tali713/esxml
;; Created: 15th August 2012
;; Version: 0.3.7
;; Keywords: tools, lisp, comm
;; Description: A library for easily generating XML/XHTML in elisp
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is XML/XHTML done with S-Expressions in EmacsLisp.  Simply,
;; this is the easiest way to write HTML or XML in Lisp.
;;
;; This library uses the native form of XML representation as used by
;; many libraries already included within emacs.  This representation
;; will be referred to as "esxml" throughout this library.  See
;; `esxml-to-xml' for a concise description of the format.
;;
;; This library is not intended to be used directly by a user, though
;; it certainly could be.  It could be used to generate static html,
;; or use a library like `elnode' to serve dynamic pages.  Or even to
;; extract a form from a site to produce an API.
;;
;; TODO: Better documentation, more convenience.
;;
;; NOTICE: Code base will be transitioning to using pcase instead of
;; destructuring bind wherever possible.  If this leads to hard to
;; debug code, please let me know, and I will do whatever I can to
;; resolve these issues.
;;
;;; Code:
(require 'cl-lib)
(require 'xml)
(require 'pcase)

(defun string-trim-whitespace (string)
  "A simple function, strips the whitespace from beginning and
end of the string.  Leaves all other whitespace untouched."
  (replace-regexp-in-string
   (rx string-start (* whitespace)
       (group (+? anything))
       (* whitespace) string-end)
   "\\1"
   string))

(defun esxml-trim-ws (esxml)
  "This may cause problems, is intended for parsing xml into sxml
but may eroneously delete desirable white space."
  (if (stringp esxml) (string-trim-whitespace esxml)
    (pcase-let ((`(,tag ,attrs . ,body) esxml))
      `(,tag ,attrs
             ,@(mapcar 'esxml-trim-ws body)))))

(defun attrp (attr)
  "Returns t if attr is a an esxml attribute.
An esxml attribute is a cons of the form (symbol . string)"
 (and (consp attr)
       (symbolp (car attr))
       (stringp (cdr attr))))

(defun esxml--convert-pair (attr)
  "Converts from cons cell to attribute pair.  Not intended for
general use."
  (pcase-let ((`(,car . ,cdr) attr))
    (cl-check-type cdr string)
    (concat (symbol-name car)
            "="
            (prin1-to-string (xml-escape-string cdr)))))

(defun attrsp (attrs)
    "Returns t if attrs is a list of esxml attributes.

See: `attrp'"
  (and (listp attrs)
       (cl-every (lambda (attr)
                   (and (consp attr)
                        (symbolp (car attr))
                        (stringp (cdr attr))))
                 attrs)))

(defun esxml-validate-form (esxml)
  "A fast esxml validator.  Will error on invalid subparts making
it suitable for hindsight testing."
  (cond ((stringp esxml) nil)
        ((< (length esxml) 2)
         (error "%s is too short to be a valid esxml expression" esxml))
        (t (pcase-let ((`(,tag ,attrs . ,body) esxml))
             (cl-check-type tag symbol)
             (cl-check-type attrs attrs)
             (mapcar 'esxml-validate-form body)))))

;; While the following could certainly have been written using format,
;; concat makes them easier to read.  Update later if neccesary for
;; efficiency.

;; Though at first glance the recursive nature of this function might
;; give one pause, since xml is a recursive data type, a recursive
;; parser is an optimal strategy.  each node will be visited exactly
;; once during the transformation.
;;
;; Further, since a string is a terminal node and since xml can be
;; represented as a string, non dynamic portions of the page may be
;; precached quite easily.
(defun esxml--to-xml-recursive (esxml)
  (pcase esxml
    ((pred stringp)
     (xml-escape-string esxml))
    (`(raw-string ,string)
     (cl-check-type string stringp)
     string)
    (`(comment nil ,body)
     (concat "<!-- " body " -->"))
    (`(,tag ,attrs . ,body)
     ;; code goes here to catch invalid data.
     (concat "<" (symbol-name tag)
             (when attrs
               (concat " " (mapconcat 'esxml--convert-pair attrs " ")))
             (if body
                 (concat ">" (mapconcat 'esxml--to-xml-recursive body "")
                         "</" (symbol-name tag) ">")
               "/>")))))

(defun esxml-to-xml (esxml)
  "This translates an esxml expression, i.e. that which is returned
by xml-parse-region. The structure is defined as any of the
following forms:

- A string.

 STRING

STRING: the string it is returned with entities escaped

- A list where the first element is the raw-string symbol and the
  second is a string.

 (raw-string STRING)

STRING: the string is returned unchanged. This allows for caching
        of any constant parts, such as headers and footers.

- A list where the first element is the comment symbol and the
  second is a string.

 (comment STRING)

STRING: the string is embedded in a HTML comment.

- A list where the first element is the tag, the second is
an alist of attribute value pairs and the remainder of the list
is 0 or more esxml elements.

 (TAG ATTRS &rest BODY)

TAG: is the tag and must be a symbol.

ATTRS: is an alist of attribute pairs each pair must be of the
       form (KEY . VALUE).

KEY: is the name of the attribute and must be a symbol.

VALUE: is the value of the attribute and must be a string.

BODY: is zero or more esxml expressions.  Having no body forms
      implies that the tag should be self closed.  If there is
      one or more body forms the tag will always be explicitly
      closed, even if they are the empty string."
  (condition-case nil
      (esxml--to-xml-recursive esxml)
    (error (esxml-validate-form esxml))))

(defun pp-esxml-to-xml (esxml)
  "This translates an esxml expresion as `esxml-to-xml' but
indents it for ease of human readability, it is necessarily
slower and will produce longer output."
  (pcase esxml
    ((pred stringp)
     (xml-escape-string esxml))
    (`(raw-string ,string)
     (cl-check-type string stringp)
     string)
    (`(comment nil ,body)
     (concat "<!-- " body " -->"))
    (`(,tag ,attrs . ,body)
     (cl-check-type tag symbol)
     (cl-check-type attrs attrs)
     (concat "<" (symbol-name tag)
             (when attrs
               (concat " " (mapconcat 'esxml--convert-pair attrs " ")))
             (if body
                 (concat ">" (if (cl-every 'stringp body)
                                 (mapconcat 'identity body " ")
                               (concat "\n"
                                       (replace-regexp-in-string
                                        "^" "  "
                                        (mapconcat 'pp-esxml-to-xml body "\n"))
                                       "\n"))
                         "</" (symbol-name tag) ">")
               "/>")))
    (_
     (error "%s is not a valid esxml expression" esxml))))

(defun sxml-to-esxml (sxml)
  "Translates sxml to esxml so the common standard can be used.
See: http://okmij.org/ftp/Scheme/SXML.html. Additionally,
(*RAW-STRING* \"string\") is translated to (raw-string
\"string\")."
  (pcase sxml
    (`(*RAW-STRING* ,body)
     `(raw-string ,body))
    (`(*COMMENT* ,body)
     `(comment nil ,body))
    (`(,tag (@ . ,attrs) . ,body)
     `(,tag ,(mapcar (lambda (attr)
                       (cons (car attr)
                             (or (cadr attr)
                                 (prin1-to-string (car attr)))))
                     attrs)
            ,@(mapcar 'sxml-to-esxml body)))
    (`(,tag . ,body)
     `(,tag nil
            ,@(mapcar 'sxml-to-esxml body)))
    ((and sxml (pred stringp)) sxml)))

(defun sxml-to-xml (sxml)
  "Translates sxml to xml, via esxml, hey it's only a constant
factor. :)"
  (esxml-to-xml (sxml-to-esxml sxml)))



;; TODO: make agnostic with respect to libxml vs xml.el
(defun xml-to-esxml (string &optional trim)
  (with-temp-buffer
    (insert string)
    (let ((parse-tree (libxml-parse-xml-region (point-min)
                                               (point-max))))
      (if trim
          (esxml-trim-ws parse-tree)
        parse-tree))))

;; TODO, move to esxpath when mature
(defun esxml-get-by-key (esxml key value)
  "Returns a list of all elements whose wttribute KEY match
VALUE.  KEY should be a symbol, and VALUE should be a string.
Will not recurse below a match."
  (unless (stringp esxml)
    (pcase-let ((`(,tag ,attrs . ,body) esxml))
      (if (equal value
                 (assoc-default key attrs))
          (list esxml)
        (apply 'append (mapcar (lambda (sexp)
                                 (esxml-get-by-key sexp key value))
                               body))))))

(defun esxml-get-tags (esxml tags)
  "Returns a list of all elements whose tag is a member of TAGS.
TAGS should be a list of tags to be matched against. Will not
recurse below a match."
  (unless (stringp esxml)
    (pcase-let ((`(,tag ,attrs . ,body) esxml))
      (if (member tag tags)
          (list esxml)
        (apply 'append (mapcar (lambda (sexp)
                                 (esxml-get-tags sexp tags))
                               body))))))

(defun esxml-get-forms (esxml)
  "Returns a list of all forms."
  (esxml-get-tags esxml '(form)))

;; taken from kv
(defmacro esxml-destructuring-mapcar (args sexp seq)
  (declare (indent 2))
  (let ((entry (make-symbol "entry")))
    `(mapcar (lambda (,entry)
               (cl-destructuring-bind ,args ,entry ,sexp))
             ,seq)))

(provide 'esxml)
;;; esxml.el ends here
