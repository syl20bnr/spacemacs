;;; citeproc-s.el --- citeproc-el string functions -*- lexical-binding: t; -*-

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

;; String utility functions for citeproc-el.

;;; Code:

(require 'thingatpt)
(require 's)

;; Handle the unavailability of `string-replace' in early Emacs versions
(if (fboundp 'string-replace)
    (defalias 'citeproc-s-replace #'string-replace)
  (defalias 'citeproc-s-replace #'s-replace))

(defun citeproc-s-camelcase-p (s)
  "Return whether string S is in camel case."
  (let ((case-fold-search nil))
    (and (< 1 (length s))
	 (s-matches-p "[[:upper:]]" (substring s 1))
	 (s-matches-p "[[:lower:]]" s))))

(defun citeproc-s-fill-copy (s1 s2)
  "Return a copy of string S1 filled by characters from string S2.
If string S1 is shorter than string S2 then prepend enough
characters from to beginning of S2 to the beginning of a copy of
S2 to make their length equal, and return the filled copy."
  (if (>= (length s1) (length s2)) s1
    (concat (substring s2 0 (- (length s2) (length s1))) s1)))

(defun citeproc-s-fill-left-to-len (s len &optional char)
  "Fill string S to length LEN with CHAR from left."
  (let ((len-s (length s))
	(char (or char ?0)))
    (if (>= len-s len) s
      (concat (make-string (- len len-s) char) s))))

(defun citeproc-s-nil-or-s-to-num (s)
  "Convert S, which is a string or nil to a number.
A nil value is converted to 0."
  (if s (string-to-number s) 0))

(defun citeproc-s-from-num-or-s (x)
  "Return the content of string or number X as a number."
  (if (numberp x) (number-to-string x) x))

(defun citeproc-s-content (s)
  "Return the string content of string or symbol or nil S.
The string content of nil is defined as \"\"."
  (pcase s
    ((\` nil) "")
    ((pred symbolp) (symbol-name s))
    (_ s)))

(defun citeproc-s-slice-by-matches (s regexp &optional start annot)
  "Slice S up at the boundaries of each REGEXP match.
Optionally start from index START. Matches are also included
among the slices, but all zero-length slices are omitted. If
optional ANNOT is non-nil then slices are given as (S . B) cons
cells where S is the slice string, while B is nil if the S slice
is a separating REGEXP match and t otherwise."
  (unless start (setq start 0))
  (save-match-data
    (let ((begin (string-match regexp s start)))
      (if begin
	  (let ((end (match-end 0)))
	    (if (and (= begin start) (= end start))
		(citeproc-s-slice-by-matches s regexp (1+ start) annot)
	      (let ((result (citeproc-s-slice-by-matches (substring s end)
							 regexp 0 annot)))
		(unless (= begin end)
		  (let ((slice (substring s begin end)))
		    (push (if annot (list slice) slice) result)))
		(unless (= 0 begin)
		  (let ((slice (substring s 0 begin)))
		    (push (if annot (cons slice t) slice) result)))
		result)))
	(list (if annot (cons s t) s))))))

(defun citeproc-s-tail (s length)
  "Return the closing substring of string S with length LENGTH.
Return S if LENGTH is nil or not less than the length of S."
  (let ((l (length s)))
    (if (and length (< length l))
	(substring s (- l length))
      s)))

(defun citeproc-s-capitalize-first (s)
  "Capitalize the first word in string S.
Return the capitalized version of S. If S contains no word or the
first word is not in lowercase then return S."
  (if (s-present-p s)
      (with-temp-buffer
	(insert s)
	(goto-char 1)
	(forward-word)
	(backward-word)
	(let ((word (word-at-point)))
	  (when (s-lowercase-p word)
	    (capitalize-word 1)))
	(buffer-string))
    s))

(defun citeproc-s-capitalize-all (s)
  "Capitalize all lowercase words in string S.
Return the capitalized version of S. If S contains no word or the
first word is not in lowercase then return S."
  (if (s-present-p s)
      (with-temp-buffer
	(insert s)
	(goto-char 1)
	(while (forward-word)
	  (let ((word (word-at-point)))
	    (when (s-lowercase-p word)
	      (capitalize-word -1))))
	(buffer-string))
    s))

(defun citeproc-s-sentence-case (s)
  "Return a sentence-cased version of string S."
  (if (s-present-p s)
      (with-temp-buffer
	(insert s)
	(goto-char 1)
	(let ((first t))
	  (while (forward-word)
	    (let ((word (word-at-point)))
	      (cond ((s-uppercase-p word) (capitalize-word -1))
		    ((and first (s-lowercase-p word)) (capitalize-word -1))))
	    (when first (setq first nil))))
	(buffer-string))
    s))

(defun citeproc-s-sentence-case-title (s omit-nocase)
  "Return a sentence-cased version of title string S.
If optional OMIT-NOCASE is non-nil then omit the nocase tags from the output."
  (if (s-blank-p s) s
    (let ((sliced (citeproc-s-slice-by-matches
		   s "\\(<span class=\"nocase\">\\|</span>\\|: +\\w\\)"))
	  (protect-level 0)
	  (first t)
	  result)
      (dolist (slice sliced)
	(push
	 (pcase slice
	   ("<span class=\"nocase\">" (cl-incf protect-level) (if omit-nocase nil slice))
	   ("</span>" (cl-decf protect-level) (if omit-nocase nil slice))
	   ;; Don't touch the first letter after a colon since it is probably a subtitle.
	   ((pred (string-match-p "^:")) slice)
	   (_ (cond ((< 0 protect-level) (setq first nil) slice)
		    ((not first) (downcase slice))
		    (t (setq first nil)
		       (concat (upcase (substring slice 0 1))
			       (downcase (substring slice 1)))))))
	 result))
      (apply #'concat (nreverse result)))))

(defconst citeproc-s-english-stopwords
  '("a" "according to" "across" "afore" "after" "against" "ahead of" "along" "alongside"
    "amid" "amidst" "among" "amongst" "an" "and" "anenst" "apart from" "apropos"
    "around" "as" "as regards" "aside" "astride" "at" "athwart" "atop" "back to"
    "barring" "because of" "before" "behind" "below" "beneath" "beside" "besides"
    "between" "beyond" "but" "by" "c" "ca" "circa" "close to" "d'" "de" "despite" "down"
    "due to" "during" "et" "except" "far from" "for" "forenenst" "from" "given" "in"
    "inside" "instead of" "into" "lest" "like" "modulo" "near" "next" "nor" "of" "off"
    "on" "onto" "or" "out" "outside of" "over" "per" "plus" "prior to" "pro" "pursuant
    to" "qua" "rather than" "regardless of" "sans" "since" "so" "such as" "than"
    "that of" "the" "through" "throughout" "thru" "thruout" "till" "to" "toward" "towards"
    "under" "underneath" "until" "unto" "up" "upon" "v." "van" "versus" "via" "vis-à-vis"
    "von" "vs." "where as" "with" "within" "without" "yet"))

(defun citeproc-s-title-case (s)
  "Return a title-cased version of string S."
  (if (s-present-p s)
      (with-temp-buffer
	(insert s)
	(goto-char 1)
	(let ((first t)
	      after-colon)
	  (while (forward-word)
	    (let ((word (word-at-point)))
	      (cond ((and (not (or first after-colon))
			  (member (downcase word) citeproc-s-english-stopwords)
			  ;; Don't downcase A before a period:
			  (or (not (string= word "A"))
			      (= (point) (point-max))
			      (/= (char-after) ?.)))
		     (downcase-word -1))
		    ((s-lowercase-p word)
		     (capitalize-word -1))))
	    (when first (setq first nil))
	    (when (< (point) (point-max))
	      (setq after-colon (or (= (char-after) ?:)
				    (= (char-after) ?.))))))
	(buffer-string))
    s))

(defun citeproc-s-smart-quotes (s oq cq)
  "Replace dumb quotes in string S with smart ones OQ and CQ.
OQ is the opening quote, CQ is the closing quote to use."
  (with-temp-buffer
    (insert s)
    (goto-char 1)
    (while (search-forward "\"" nil 1)
      (let ((w-a-p (word-at-point)))
	(delete-char -1)
	(insert (if w-a-p oq cq))))
    (buffer-string)))

(defun citeproc-s-replace-all-seq (s replacements)
  "Make replacements in S according to REPLACEMENTS sequentially.
REPLACEMENTS is an alist with (FROM . TO) elements."
  (let ((result s))
    (pcase-dolist (`(,from . ,to) replacements)
      (setq result (citeproc-s-replace from to result)))
    result))

(defun citeproc-s-replace-all-sim (s regex replacements)
  "Replace all matches of REGEX in S according to REPLACEMENTS simultaneously.
REPLACEMENTS is an alist with (FROM . TO) elements."
  (replace-regexp-in-string regex
			    (lambda (match) (cdr (assoc-string match replacements)))
			    s t t))

(defun citeproc-s-smart-apostrophes (s)
  "Replace dumb apostophes in string S with smart ones.
The replacement character used is the unicode character `modifier
letter apostrophe'."
  (subst-char-in-string ?' ?ʼ (subst-char-in-string ?’ ?ʼ s t) t))

(defconst citeproc-s--cull-spaces-alist
  '(("  " . " ") (";;" . ";") ("..." . ".") (",," . ",") (".." . "."))
  "Alist describing replacements for space and punct culling.")

(defconst citeproc-s--cull-spaces-alist-rx
  (regexp-opt (mapcar #'car citeproc-s--cull-spaces-alist)))

(defun citeproc-s-cull-spaces-puncts (s)
  "Replace unnecessary characters with delete chars in string S."
  (let ((result (citeproc-s-replace-all-seq s citeproc-s--cull-spaces-alist)))
    (dolist (match-rep '(("\\([:;!?]\\):" . "\\1")
			 ("\\([:.;!?]\\)\\." . "\\1")
			 ("\\([:;!]\\)!" . "!")
			 ("\\([:;?]\\)\\?" . "?")
			 ("\\.\\([”’‹›«»]\\)\\." . ".\\1")
			 (",\\([”’‹›«»]\\)," . ",\\1"))
		       result)
      (setq result (replace-regexp-in-string (car match-rep)
					     (cdr match-rep)
					     result)))))

(provide 'citeproc-s)

;;; citeproc-s.el ends here
