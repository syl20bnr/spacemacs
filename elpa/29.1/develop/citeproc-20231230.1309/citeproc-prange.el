;;; citeproc-prange.el --- page-range rendering -*- lexical-binding: t; -*-

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

;; Functions to render CSL page-ranges.

;;; Code:

(defun citeproc-prange--end-significant (start end len)
  "Return the significant digits of the end in page range START END.
START and END are strings of equal length containing integers. If
the significant part of END is shorter than LEN then add
insignificant digits from END until the string is LEN long or
there are no more digits left."
  (let ((first (min (max 1 (1+ (- (length end) len)))
		    (abs (compare-strings start nil nil end nil nil)))))
    (substring end (1- first))))

(defun citeproc-prange--end-complete (start end len)
  "Complete the closing form of a START END pagerange to LEN."
  (citeproc-prange--end-significant start (citeproc-s-fill-copy end start) len))

(defun citeproc-prange--end-expanded (_start end end-pref)
  "Render the end of range _START END in `expanded' format.
END-PREF is an optional non-numeric prefix preceding END. All
arguments are strings, END has the same length as START."
  (concat end-pref end))

(defun citeproc-prange--end-minimal (start end _end-pref)
  "Render the end of range START END in `minimal' format.
END-PREFIX is an optional non-numeric prefix preceding END. All
arguments are strings, END has the same length as START."
  (citeproc-prange--end-significant start end 1))

(defun citeproc-prange--end-minimal-two (start end _end-pref)
  "Render the end of range START END in `minimal-two' format.
END-PREFIX is an optional non-numeric prefix preceding END. All
arguments are strings, END has the same length as START."
  (citeproc-prange--end-significant start end 2))

(defun citeproc-prange--end-chicago (start end _end-pref &optional 15th-ed)
  "Render the end of range START END in `chicago' format.
END-PREFIX is an optional non-numeric prefix preceding END. All
arguments are strings, END has the same length as START. If
optional 15TH-ED is non-nil then use the special 4digit rule of
the 15th edition."
  (let ((len (length start))) 
    (cond ((or (< len 3) (string= (substring start -2) "00"))
	   end)
	  ((string= (substring start -2 -1) "0")
	   (citeproc-prange--end-significant start end 1))
	  ((and 15th-ed (= 4 (length start)))
	   (let ((min-two (citeproc-prange--end-significant start end 2)))
	     (if (> (length min-two) 2) end min-two)))
	  (t (citeproc-prange--end-significant start end 2)))))

(defconst citeproc-prange-formatters-alist
  `((chicago . ,(lambda (start end end-pref)
		  (citeproc-prange--end-chicago start end end-pref t)))
    (chicago-15 . ,(lambda (start end end-pref)
		     (citeproc-prange--end-chicago start end end-pref t)))
    (chicago-16 . citeproc-prange--end-chicago)
    (minimal . citeproc-prange--end-minimal)
    (minimal-two . citeproc-prange--end-minimal-two)
    (expanded . citeproc-prange--end-expanded))
  "Alist mapping page range formats to formatter functions.")

(defun citeproc-prange-render (p format sep)
  "Render page range P in FORMAT with separator SEP."
  (with-temp-buffer
    (insert p)
    (goto-char 0)
    (while (search-forward-regexp
	    "\\([[:digit:]]*[[:alpha:]]\\)?\\([[:digit:]]+\\)\\( ?\\)\\([-–—]+\\)\\( ?\\)\\([[:digit:]]*[[:alpha:]]\\)?\\([[:digit:]]+\\)"
	    nil t)
      (let* ((start-pref (match-string 1))
	     (start-num (match-string 2))
	     (orig-dash (match-string 4))
	     (orig-sep (concat (match-string 3) orig-dash (match-string 5)))
	     (end-pref (match-string 6))
	     (end-num (match-string 7))
	     (end (concat end-pref end-num))
	     (old-sep-w-end (concat orig-sep end))
	     ;; Note: To conform with the official CSL tests we don't replace the separating
	     ;; dash with SEP if collapse cannot be applied because of incompatible prefixes
	     ;; but we still remove spaces surrounding the dash. It would make far more
	     ;; sense to replace the dash as well.
	     (new-sep-w-end (cond ((not (string= start-pref end-pref))
				   (concat orig-dash end))
				  ;; Deal with degenerate single page "ranges"
				  ((string= start-num end-num)
				   "")
				  ((or (not format) (> (length end-num) (length start-num)))
				   (concat sep end))
				  (t (concat
				      sep
				      (funcall (alist-get format
							  citeproc-prange-formatters-alist)
					       start-num
					       (citeproc-s-fill-copy end-num start-num)
					       end-pref))))))
	(unless (string-equal new-sep-w-end old-sep-w-end)
	  (delete-char (- (length old-sep-w-end)))
	  (insert new-sep-w-end))))
    (buffer-string)))

(provide 'citeproc-prange)

;;; citeproc-prange.el ends here
