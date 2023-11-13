;;; citeproc-itemdata.el --- represent and access bibliography items -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

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

;; Structure type and functions to handle data about bibliography items.

;;; Code:

(require 'citeproc-rt)
(require 'citeproc-context)

(cl-defstruct (citeproc-itemdata (:constructor citeproc-itemdata-create))
  "Struct for storing bibliography item data.
VARVALS is an alist containg variable-name symbols as keys and
  their values for the item as values,
RAWCITE is the cached cite of the item in internal rich-text
  format,
RC-UPTODATE is t iff the RAWCITE field is up-to-date,
RAWBIBITEM is the cached bibliograhy item in internal rich-text
  format.
SORT-KEY is the bibliography sort-key of the item,
OCCURRED-BEFORE is used during bibliography generation to
  indicate whether the item was referred to earlier. If the first
  occurrence is in a note then the actual value is the
  note-number,
DISAMB-POS contains the position on which cite disambiguation is
  based. Possible values are 'first, 'ibid and 'subsequent,
SUBBIB-NOS is a list of numeric indexes of sub-bibliographies
  in which the item occurs,
UNCITED is t iff the item has no associated citation."
  varvals rawcite rawbibitem rc-uptodate sort-key occurred-before
  disamb-pos subbib-nos uncited)

(defun citeproc-itd-getvar (itd var)
  "Return itemdata ITD's value for VAR ."
  (alist-get var (citeproc-itemdata-varvals itd)))

(defun citeproc-itd-setvar (itd var val)
  "Set itemdata ITD's value for VAR to VAL."
  (setf (alist-get var (citeproc-itemdata-varvals itd) nil t) val
	(citeproc-itemdata-rc-uptodate itd) nil))

(defun citeproc-itd-rt-cite (itd style)
  "Return the rich-text cite of itemdata ITD using STYLE."
  (if (citeproc-itemdata-rc-uptodate itd)
      (citeproc-itemdata-rawcite itd)
    (let ((rc (citeproc-render-varlist-in-rt
	       (cons (cons 'position (citeproc-itemdata-disamb-pos itd))
		     (citeproc-itemdata-varvals itd))
	       style
	       'cite 'display 'no-links t)))
      (setf (citeproc-itemdata-rawcite itd) rc
	    (citeproc-itemdata-rc-uptodate itd) t)
      rc)))

(defun citeproc-itd-plain-cite (itd style)
  "Return the plain text cite of itemdata ITD using STYLE."
  (citeproc-rt-to-plain (citeproc-itd-rt-cite itd style)))

(defun citeproc-itd-namevars (itd style)
  "Rendered namevars in the cite of itemdata ITD using STYLE."
  (citeproc-rt-rendered-name-vars (citeproc-itd-rt-cite itd style)))

(defun citeproc-itd-nameids (itd style)
  "Rendered name ids in the cite of itemdata ITD using STYLE."
  (citeproc-rt-rendered-name-ids (citeproc-itd-rt-cite itd style)))

(defun citeproc-itd-update-disamb-pos (itd pos)
  "Update the highest position of ITD with position POS."
  (let ((old (citeproc-itemdata-disamb-pos itd)))
    (unless (eq old 'subsequent)
      (let ((new (pcase pos
		   ('first 'first)
		   ((or 'ibid 'ibid-with-locator) 'ibid)
		   (_ 'subsequent))))
	(setf (citeproc-itemdata-disamb-pos itd)
	      (cond ((memq old '(nil first)) new)
		    ((eq new 'subsequent) 'subsequent)
		    (t 'ibid)))))))

(provide 'citeproc-itemdata)

;;; citeproc-itemdata.el ends here
