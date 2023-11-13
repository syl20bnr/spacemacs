;;; citeproc-disamb.el --- disambiguate ambiguous cites -*- lexical-binding: t; -*-

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

;; Functions to disambiguate cites of different bibliography items that would
;; have the same rendering. Disambiguation steps (e.g., adding more names,
;; expanding names, adding year-suffixes) are performed according to the
;; disambiguation rules specified by the CSL style in use.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)

(require 'citeproc-itemdata)

(defun citeproc-itd-inc-disamb-level (key itd type)
  "Increment the disambiguation level of KEY in itemdata ITD.
TYPE is either `add-names' or `show-given-names.'"
  (let ((vv (citeproc-itemdata-varvals itd)))
    (if (alist-get type vv)
	(let* ((cur-level (alist-get key (alist-get type vv)))
	       (new-level (if cur-level (1+ cur-level) 1)))
	  (setf (alist-get key (alist-get type vv)) new-level))
      (push `(,type . ((,key . 1))) (citeproc-itemdata-varvals itd))))
  (setf (citeproc-itemdata-rc-uptodate itd) nil))

(defun citeproc-itd-add-name (itd style &optional _first-step)
  "Perform an add-name disambig. step on itemdata ITD with STYLE.
FIRST-STEP is ignored -- it is there to get the same signature as
the other two disamb. step functions. Return nil if no disambiguation
could be performed and t otherwise. Disambiguation is performed
from left to right: an item is attempted to be expanded only if
no previous items to the left could be."
  (let* ((vars (citeproc-itd-namevars itd style))
	 (cite (citeproc-itd-plain-cite itd style))
	 (vv (citeproc-itemdata-varvals itd))
	 (levels (alist-get 'add-names vv))
	 (remaining-vars (if levels (memq (caar levels) vars) vars))
	 (success nil))
    (while (and (not success) remaining-vars)
      (citeproc-itd-inc-disamb-level (car remaining-vars) itd 'add-names)
      (if (string= cite (citeproc-itd-plain-cite itd style))
	  (pop remaining-vars)
	(setq success t)))
    success))

(defun citeproc-itd-add-given (itd style &optional first-step)
  "Perform an add-given disambig. step on itemdata ITD with STYLE.
Unless FIRST-STEP is non-nil, remove the last previously added
given name if the last added given name is shown in its entirety.
Return nil if no disambig. step could be performed and t
otherwise."
  (let* ((nids (citeproc-itd-nameids itd style))
	 (cite (citeproc-itd-plain-cite itd style))
	 (vv (citeproc-itemdata-varvals itd))
	 (levels (alist-get 'show-given-names vv))
	 (remaining-nids (if levels (memq (caar levels) nids) nids))
	 (success nil))
    (while (and (not success) remaining-nids)
      (let* ((current-nid (car remaining-nids))
	     (vv (citeproc-itemdata-varvals itd))
	     (levels (alist-get 'show-given-names vv))
	     (current-level (alist-get current-nid levels)))
	(if (and current-level (>= current-level 2))
	    (pop remaining-nids)
	  (citeproc-itd-inc-disamb-level current-nid itd 'show-given-names)
	  (unless (string= cite (citeproc-itd-plain-cite itd style))
	    (setq success t)
	    (unless (or first-step current-level)
	      (let ((ls (alist-get 'show-given-names vv)))
		(setf (alist-get 'show-given-names vv)
		      (cons (car ls) (cddr ls))))
	      (setf (citeproc-itemdata-rc-uptodate itd) nil))))))
    success))

(defun citeproc-itd-addgiven-with-addname (itd style &optional first-step)
  "Perform a combined disambig. step on itemdata ITD with STYLE.
If FIRST-STEP is non-nil then (i) add a new name even if the last
add-given step showed only initials, (ii) don't remove the
previously added given name. Return nil if no disambig. step
could be performed and t otherwise."
  (let* ((vv (citeproc-itemdata-varvals itd))
	 (gn (alist-get 'show-given-names vv))
	 (success nil)
	 (remaining-names t))
    (if (and (not first-step) gn (= 1 (cdar gn)) (citeproc-itd-add-given itd style))
	t
      (while (and (not success) remaining-names)
	(let ((nids (citeproc-itd-nameids itd style)))
	  (if (citeproc-itd-add-name itd style)
	      (let* ((new-nids (citeproc-itd-nameids itd style))
		     (new-nid (car (cl-set-difference new-nids nids))))
		;;next sexp is to direct the add-given function to the just added name
		(when first-step
		  (setf (alist-get new-nid
				   (alist-get 'show-given-names
					      (citeproc-itemdata-varvals itd)))
			0))
		(when (citeproc-itd-add-given itd style first-step)
		  (setq success t)))
	    (setq remaining-names nil))))
      success)))

(defun citeproc-disamb--different-cites-p (itds style)
  "Whether some itemdata in ITDS have different cites with STYLE."
  (--any-p (not (string= (citeproc-itd-plain-cite it style)
			 (citeproc-itd-plain-cite (car itds) style)))
	   (cdr itds)))

(defun citeproc-disamb--with-method (itds style disamb-fun)
  "Disambiguate itemdatas in ITDS for STYLE with DISAMB-FUN.
Return t if the disambiguation was (at least partially)
successful and nil otherwise."
  (let ((orig-settings (copy-tree (citeproc-disamb--settings itds)))
	success
	(first-step t))
    (while (and (not success)
		(--all-p (funcall disamb-fun it style first-step) itds))
      (setq first-step nil)
      (when (citeproc-disamb--different-cites-p itds style)
	(setq success t)))
    (unless success
      (citeproc-disamb--restore-settings itds orig-settings))
    success))

(defun citeproc-disamb--settings (itds)
  "Return a list with the disamb settings of ITDS."
  (--map (cons (citeproc-itd-getvar it 'add-names)
	       (citeproc-itd-getvar it 'show-given-names))
	 itds))

(defun citeproc-disamb--restore-settings (itds settings)
  "Restore the disamb settings of ITDS from SETTINGS.
SETTINGS should have the structure produced by citeproc--disamb-settings."
  (cl-loop for itd in itds
	   for (add-names-val . show-given-val) in settings do
	   (citeproc-itd-setvar itd 'add-names add-names-val)
	   (citeproc-itd-setvar itd 'show-given-names show-given-val)))

(defun citeproc-disamb--num-to-yearsuffix (n)
  "Return year-suffix no. N (starting from 0)."
  (cond ((< n 26)
	 (char-to-string (+ 97 n)))
	((< n 702)
	 (let* ((rem (% n 26))
		(d (/ (- n rem) 26)))
	   (concat (char-to-string (+ 96 d))
		   (char-to-string (+ 97 rem)))))
	(t (error "Number too large to convert into a year-suffix"))))

(defun citeproc-disamb--add-yearsuffix (itds _style)
  "Disambiguate itemdata in ITDS by adding year suffices.
Return t (for the sake of consistency with other disamb methods)."
  (--each-indexed (--sort (< (string-to-number (citeproc-itd-getvar it 'citation-number))
			     (string-to-number (citeproc-itd-getvar other 'citation-number)))
			  itds)
    (citeproc-itd-setvar it 'year-suffix (citeproc-disamb--num-to-yearsuffix it-index))
    (setf (citeproc-itemdata-rc-uptodate it) nil))
  t)

(defun citeproc-disamb--set-fields (itds)
  "Disambiguate ITDS by setting their disambiguate fields."
  (--each itds
    (citeproc-itd-setvar it 'disambiguate t)))

(defun citeproc-disamb-amb-itds (itds style name given yearsuff)
  "Disambiguate ITDS ambigous for STYLE with the given methods.
NAME, GIVEN and YEARSUFF are generalized booleans specifying
whether or not the add-name, show-given or add-year-suffix
disambiguation methods should be used. Return t if the
disambiguation was (at least partially) successful, nil
otherwise."
  (or (and name (citeproc-disamb--with-method itds style 'citeproc-itd-add-name))
      (and given (citeproc-disamb--with-method itds style 'citeproc-itd-add-given))
      (and name given
	   (citeproc-disamb--with-method itds style 'citeproc-itd-addgiven-with-addname))
      (progn
	(citeproc-disamb--set-fields itds)
	(citeproc-disamb--different-cites-p itds style))
      (and yearsuff
	   (citeproc-disamb--add-yearsuffix itds style)
	   (citeproc-disamb--different-cites-p itds style))))

(defun citeproc-amb-itds (itds style)
  "Return a list of ambigous sets in ITDS for STYLE.
The returned value is a (possibly empty) list of lists."
  (let* ((sorted (-sort (lambda (x y)
			  (string< (citeproc-itd-plain-cite x style)
				   (citeproc-itd-plain-cite y style)))
			itds))
	 (result nil)
	 (remaining (cdr sorted))
	 (act (car sorted))
	 (act-list (list act))
	 (ambig nil))
    (while remaining
      (let ((next (car remaining)))
	(if (string= (citeproc-itd-plain-cite act style)
		     (citeproc-itd-plain-cite next style))
	    (progn
	      (push next act-list)
	      (setq ambig t))
	  (when ambig (push act-list result))
	  (setq act-list (list next)
		act next
		ambig nil))
	(pop remaining)))
    (when ambig (push act-list result))
    result))

(defun citeproc-disamb-itds (itds style name given yearsuff)
  "Disambiguate itemdatas in list ITDS for STYLE.
NAME, GIVEN and YEARSUFF are generalized booleans specifying
whether or not the add-name, show-given or add-year-suffix
disambiguation methods should be used."
  (let ((amb-itds (citeproc-amb-itds itds style)))
    (while amb-itds
      (let ((act-set (pop amb-itds)))
	(citeproc-disamb-amb-itds act-set style name given yearsuff)
	(when (citeproc-disamb--different-cites-p act-set style)
	  (-when-let (new-ambs (citeproc-amb-itds act-set style))
	    (setq amb-itds (nconc new-ambs amb-itds))))))))

(provide 'citeproc-disamb)

;;; citeproc-disamb.el ends here
