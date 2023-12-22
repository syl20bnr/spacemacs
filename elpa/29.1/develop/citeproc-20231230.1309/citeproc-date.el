;;; citeproc-date.el --- CSL date rendering -*- lexical-binding: t; -*-

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

;; Structure type and functions to render CSL date elements.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'dash)
(require 'let-alist)
(require 's)

(require 'citeproc-lib)
(require 'citeproc-rt)
(require 'citeproc-context)

(cl-defstruct (citeproc-date (:constructor citeproc-date-create))
  "Struct for representing dates.
Slots YEAR, MONTH, DAY are integers, while SEASON and CIRCA are
booleans. SEASON indicates whether the integer in slot MONTH is
to be interpreted as a season number."
  (year nil) (month nil) (day nil) (season nil) (circa nil))

(defun citeproc-date-parse (date-rep)
  "Parse CSL json date repr. DATE-REP into an internal one."
  (let-alist date-rep
    (--map (citeproc-date--conv it .season .circa) .date-parts)))

(defun citeproc-date--conv (dates &optional season circa)
  "Convert date-part list DATES to a citeproc-date struct.
Set the remaining slots to the values SEASON and CIRCA."
  (-let* ((numeric
	   (--map (if (stringp it) (string-to-number it) it) dates))
	  ((year month day) numeric))
    (citeproc-date-create :year year :month month :day day
			  :season season :circa circa)))

(defun citeproc-date--partattrs-for-sort (part-attrs)
  "Return a sort-key version of PART-ATTRS."
  (let (result)
    (when (assoc 'day part-attrs)
      (push '(day . ((form . "numeric-leading-zeros"))) result))
    (when (assoc 'month part-attrs)
      (push '(month . ((form . "numeric-leading-zeros"))) result))
    (when (assoc 'year part-attrs)
      (push '(year . ((form . "long"))) result))
    result))

(defun citeproc--date (attrs context &rest body)
  "Function corresponding to the date CSL element."
  (-let* (((&alist 'variable var
		   'form form)
	   attrs)
	  (var-sym (intern var))
	  (parsed-dates (citeproc-var-value var-sym context))
	  ((d1 d2) parsed-dates)
	  (result
	   (if d1
	       (progn
		 (when form
		   (let ((localized (citeproc-date--localized-attrs attrs body context)))
		     (setq attrs (car localized)
			   body (cdr localized))))
		 (when (eq (citeproc-context-render-mode context) 'sort)
		   (setq body (citeproc-date--partattrs-for-sort body)))
		 (if (citeproc-date--renders-with-attrs-p d1 body)
		     (progn
		       (push `(rendered-var . ,(intern var)) attrs)
		       (cons (if d2
				 (citeproc-date--render-range d1 d2 attrs body context)
			       (citeproc-date--render d1 attrs body context))
			     'present-var))
		   (cons nil 'empty-vars)))
	     (cons nil 'empty-vars))))
    ;; Handle `year' citation mode by stopping if needed
    (citeproc-lib-maybe-stop-rendering 'issued context result var-sym)))

(defun citeproc--date-part (attrs _context &rest _body)
  "Function corresponding to the date-part CSL element."
  (cons (intern (alist-get 'name attrs))
	attrs))

(defun citeproc-date--renders-with-attrs-p (date part-attrs)
  "Whether DATE contains date-parts corresponding to PART-ATTRS."
  (let ((date-parts (mapcar #'car part-attrs)))
    (or (memq 'year date-parts)		; All dates contain a year
	(and (memq 'month date-parts) (citeproc-date-month date))
	(and (memq 'day date-parts) (citeproc-date-day date)))))

(defun citeproc-date--localized-attrs (attrs part-attrs context)
  "Return the localized date attrs merged with date ATTRS and date PART-ATTRS."
  (-let* (((&alist 'form form
		   'date-parts date-parts)
	   attrs)
	  ((loc-attrs . loc-part-attrs)
	   (if (string= form "text") (citeproc-context-date-text context)
	     (citeproc-context-date-numeric context))))
    (pcase (citeproc-lib-intern date-parts)
      ('year
       (setq loc-part-attrs
	     (--select (eq (car it) 'year) loc-part-attrs)))
      ('year-month
       (setq loc-part-attrs
	     (--select (memq (car it) '(year month)) loc-part-attrs))))
    (cons (-concat attrs loc-attrs)
	  (--map (cons (car it)
		       (-concat (alist-get (car it) part-attrs) (cdr it)))
		 loc-part-attrs))))

(defun citeproc-date--render (d attrs part-attrs context)
  "Render citeproc-date D according to formatting in ATTRS and PART-ATTRS.
Return a rich-text content."
  (if (citeproc-var-value 'suppress-date context)
      (citeproc-rt-format-single attrs "<suppressed-date>" context)
    (let ((rendered-date (citeproc-date--render-parts d part-attrs context)))
      (citeproc-rt-join-formatted attrs rendered-date context))))

(defun citeproc-date--render-parts (d part-attrs context &optional no-last-suffix)
  "Render the parts of citeproc-date D according to PART-ATTRS.
Return a list of rich-text contents. If optional NO-LAST-SUFFIX
is non-nil then remove the suffix attribute of the last rendered
element (used for date range rendering)."
  (let ((result (--map (pcase (car it)
			 ('year (citeproc-date--render-year d (cdr it) context))
			 ('month (citeproc-date--render-month d (cdr it) context))
			 ('day (citeproc-date--render-day d (cdr it) context)))
		       part-attrs)))
    (-if-let* ((n-l-s no-last-suffix)
	       (last (car (last result)))
	       (wo-suffix (and (consp last)
			       (cons (--remove (eq 'suffix (car it)) (car last))
				     (cdr last)))))
	(-snoc (butlast result) wo-suffix)
      result)))

(defun citeproc-date--render-range-parts (d1 d2 part-attrs sep context)
  "Render the parts of citeproc-dates D1 and D2 according to PART-ATTRS.
PART-ATTRS is a list containing either part-attrs or lists of part-attrs.
The formers are only rendered for D1, while the latters are rendered for both
D1 and D2. Return a list of rich-text contents."
  (--mapcat (pcase (car it)
	      ('year (list (citeproc-date--render-year d1 (cdr it) context)))
	      ('month (list (citeproc-date--render-month d1 (cdr it) context)))
	      ('day (list (citeproc-date--render-day d1 (cdr it) context)))
	      (_ (-concat (citeproc-date--render-parts d1 it context t)
			  (list sep)
			  (citeproc-date--render-parts d2 it context))))
	    part-attrs))

(defun citeproc-date--render-range (d1 d2 attrs part-attrs context)
  "Render the range given by dates D1 D2 according to attrs."
  (if (citeproc-var-value 'suppress-date context)
      (citeproc-rt-format-single attrs "" context)
    (let* ((gran (min (citeproc-date--gran d1)
		      (citeproc-date--attrs-gran part-attrs)))
	   (range-sep (or (alist-get 'range-delimiter
				     (alist-get (elt '(year month day) gran)
						part-attrs))
			  "–"))
	   (range-p-attrs
	    (cond ((/= (citeproc-date-year d1) (citeproc-date-year d2))
		   (list part-attrs))
		  ((/= (citeproc-date-month d1) (citeproc-date-month d2))
		   (let ((year-part (--find (eq 'year (car it))
					    part-attrs))
			 (attrs-wo-year
			  (--remove (eq 'year (car it))
				    part-attrs)))
		     (cond ((eq (caar part-attrs) 'year)
			    (list year-part attrs-wo-year))
			   ((eq (caar (last part-attrs)) 'year)
			    (list attrs-wo-year year-part))
			   (t (list attrs-wo-year)))))
		  (t (--map (if (eq (car it) 'day) (list it) it)
			    part-attrs))))
	   (rendered-range (citeproc-date--render-range-parts d1 d2 range-p-attrs range-sep
							      context)))
      (citeproc-rt-join-formatted attrs rendered-range context))))

(defun citeproc-date--attrs-gran (d-attrs)
  "Return the granularity (smallest unit) of date-attrs alist D-ATTRS.
The returned value is 0, 1 or 2, corresponding to a year, month
or day granularity."
  (cond ((assoc 'day d-attrs) 2)
	((assoc 'month d-attrs) 1)
	(t 0)))

(defun citeproc-date--gran (date)
  "Return the granularity (smallest unit) in citeproc-date struct DATE.
The returned value is 0, 1 or 2, corresponding to a year, month
or day granularity."
  (cond ((citeproc-date-day date) 2)
	((citeproc-date-month date) 1)
	(t 0)))

(defun citeproc-date--render-year (d attrs context)
  "Render the year in date D according to formatting in ATTRS.
D is a citeproc-date structure. Return a rich-text content."
  (-let* ((form (alist-get 'form attrs))
	  (year (citeproc-date-year d))
	  (s (number-to-string (abs year)))
	  (era
	   (cond ((> year 999) "")
		 ((> year 0) (citeproc-term-get-text "ad" context))
		 (t (citeproc-term-get-text "bc" context)))))

    (citeproc-rt-format-single attrs (concat (if (string= form "short")
						 (s-right 2 s)
					       s)
					     era)
			       context)))

(defun citeproc-date--render-month (d attrs context)
  "Render the month in date D according to formatting in ATTRS.
D is a citeproc-date structure. Return a rich-text content."
  (-if-let (month (citeproc-date-month d))
      (let ((form (alist-get 'form attrs))
	    (term-pref (if (citeproc-date-season d)
			   "season-" "month-")))
	(citeproc-rt-format-single
	 attrs
	 (pcase (citeproc-lib-intern form)
	   ('numeric (number-to-string month))
	   ('numeric-leading-zeros (format "%02d" month))
	   ('short (citeproc-term-inflected-text
		    (concat term-pref (format "%02d" month))
		    'short nil context))
	   (_ (citeproc-term-inflected-text
	       (concat term-pref (format "%02d" month))
	       'long nil context)))
	 context))
    nil))

(defun citeproc-date--render-day (d attrs context)
  "Render the day in date D according to formatting in ATTRS.
D is a citeproc-date structure. Return a rich-text content."
  (-if-let (day (citeproc-date-day d))
      (let ((form (alist-get 'form attrs))
	    (month (citeproc-date-month d)))
	(citeproc-rt-format-single
	 attrs
	 (cond
	  ((string= form "numeric-leading-zeros")
	   (format "%02d" day))
	  ((and (string= form "ordinal")
		(or (= day 1)
		    (not (string= "true"
				  (alist-get 'limit-day-ordinals-to-day-1
					     (citeproc-context-locale-opts context))))))
	   (citeproc-number--format-as-ordinal (number-to-string day)
					       (concat "month-" (format "%02d" month))
					       context))
	  (t (number-to-string day)))
	 context))
    nil))

(provide 'citeproc-date)

;;; citeproc-date.el ends here
