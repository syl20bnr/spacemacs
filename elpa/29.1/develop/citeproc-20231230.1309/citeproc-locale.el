;; citeproc-locale.el --- CSL locale related functions -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2022 András Simonyi

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

;; In addition to some locale handling helpers, this file provides the function
;; `citeproc-locale-getter-from-dir', which constructs locale getter functions
;; that retrieve locales from a given directory.

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(require 'citeproc-lib)
(require 'citeproc-term)

(defconst citeproc-locale--default-variants-alist
  '(("af" . "ZA") ("ca" . "AD") ("cs" . "CZ") ("cy" . "GB")
    ("da" . "DK") ("en" . "US") ("el" . "GR") ("et" . "EE")
    ("fa" . "IR") ("he" . "IR") ("ja" . "JP") ("km" . "KH")
    ("ko" . "KR") ("nb" . "NO") ("nn" . "NO") ("sl" . "SI")
    ("sr" . "RS") ("sv" . "SE") ("uk" . "UA") ("vi" . "VN")
    ("zh" . "CN"))
  "Alist mapping locales to their default variants.
Only those locales are given for which the default variant is not
simply the result of upcasing.")

(defconst citeproc-locale--simple-locales
  '("la" "eu" "ar")
  "List of simple locale names (without dash).")

(defun citeproc-locale--extend (loc)
  "Extend simple locale LOC to default variant."
  (let ((variant (assoc-default loc citeproc-locale--default-variants-alist)))
    (concat loc "-" (or variant (s-upcase loc)))))

(defun citeproc-locale--compatible-p (l1 l2)
  "Whether locales L1 and L2 are compatible."
  (or (not (and l1 l2))
      (s-prefix-p l1 l2)
      (s-prefix-p l2 l1)))

(defun citeproc-locale-getter-from-dir (dir)
  "Return a locale getter getting parsed locales from a local DIR.
If the requested locale couldn't be read then return the parsed
en-US locale, which must exist."
  (let ((default-loc-file (f-join dir "locales-en-US.xml")))
    (lambda (loc)
      (let* ((ext-loc (if (or (member loc citeproc-locale--simple-locales)
			      (s-contains-p "-" loc))
			  loc
			(citeproc-locale--extend loc)))
	     (loc-file (concat dir "/locales-" ext-loc ".xml"))
	     (loc-available (f-readable-p loc-file)))
	(citeproc-lib-remove-xml-comments
	 (citeproc-lib-parse-xml-file
	  (if loc-available loc-file
	   (if (not (f-readable-p default-loc-file))
	       (error
		"The default CSL locale file %s doesn't exist or is unreadable"
		      default-loc-file)   
	     default-loc-file))))))))

(defun citeproc-locale-termlist-from-xml-frag (frag)
  "Transform xml FRAG representing citeproc--terms into a citeproc-term list."
  (--mapcat (if (eq 'term (car it))
		(citeproc-term--from-xml-frag (cdr it))
	      nil)
	    frag))

(provide 'citeproc-locale)

;;; citeproc-locale.el ends here
