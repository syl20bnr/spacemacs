;;; citeproc-subbibs.el --- support for subbibliographies -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Support for generating subbibliographies based on filtering items.

;;; Code:

(require 'subr-x)
(require 'compat)
(require 'dash)

(require 'citeproc-proc)
(require 'citeproc-itemdata)

(defun citeproc-sb--match-p (vv filter)
  "Return whether var-vals alist VV matches all conditions in FILTER.
FILTER should be an alist containing symbol keys and string
values, each pair describing an atomic condition to be
satisified. For a list and description of the supported keys
see the documentation of `citeproc-add-subbib-filters'."
  (let* ((csl-type (alist-get 'type vv))
	 (type (or (alist-get 'blt-type vv) csl-type))
	 (keyword (alist-get 'keyword vv))
	 (keywords (and keyword (mapcar #'string-clean-whitespace
					(split-string keyword "[,;]" t)))))
    (--every-p
     (pcase it
       (`(type . ,key) (string= type key))
       (`(nottype . ,key) (not (string= type key)))
       (`(keyword . ,key) (member key keywords))
       (`(notkeyword . ,key) (not (member key keywords)))
       (`(filter . ,key) (funcall (intern key) vv))
       (`(csltype . ,key) (string= csl-type key))
       (`(notcsltype . ,key) (not (string= csl-type key)))
       (`(,key . ,_) (error "Unsupported Citeproc filter keyword `%s'" key)))
     filter)))

(defun citeproc-sb-add-subbib-info (proc)
  "Add subbibliography information to the items in PROC."
  (when (citeproc-proc-filtered-bib-p proc)
    (let ((filters (citeproc-proc-bib-filters proc)))
      (maphash
       (lambda (_ itemdata)
	 (let* ((varvals (citeproc-itemdata-varvals itemdata))
		(subbib-nos
		 (-non-nil
		  (--map-indexed
		   (when (citeproc-sb--match-p varvals it) it-index)
		   filters))))
	   (setf (citeproc-itemdata-subbib-nos itemdata) subbib-nos)))
       (citeproc-proc-itemdata proc)))))

(defun citeproc-sb-prune-unrendered (proc)
  "Remove all itemdata about unrendered items from PROC.
An item is unrendered if
- there are subbibfilters but none of them matches it, and
- it is not cited."
  (when (citeproc-proc-filtered-bib-p proc)
    (let ((itemdata (citeproc-proc-itemdata proc)))
      (maphash
       (lambda (id data)
	 (when (and (citeproc-itemdata-uncited data)
		    (null (citeproc-itemdata-subbib-nos data)))
	   (remhash id itemdata)))
       itemdata))))

(provide 'citeproc-subbibs)

;;; citeproc-subbibs.el ends here
