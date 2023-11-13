;; citeproc-formatters.el --- output formatters -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021 András Simonyi

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

;; Provides a general framework for defining citeproc-el output formatters, and,
;; using this framework, defines formatters for the formats `raw' (the rich-text
;; internal representation), `plain' (plain-text), `html', `org' and `latex'.
;; Further output formatters can easily be added.

;;; Code:

(require 'let-alist)
(require 'subr-x)
(require 's)
(require 'cl-lib)

  (cl-defstruct (citeproc-formatter (:constructor citeproc-formatter-create))
    "Output formatter struct with slots RT, CITE, BIB-ITEM and BIB.
RT is a one-argument function mapping a rich-text to its
  formatted version,
CITE is a one-argument function mapping the output of RT for a
  citation rich-text to a fully formatted citation,
BIB-ITEM is a two-argument function mapping the output of RT for
  a bibliography-item rich-text and a BIB-FORMAT alist (see
  below) to a fully formatted bibliography item,
BIB is a two-argument function mapping a list of formatted
  bibliography items and a FORMATTING-PARAMETERS alist (see
  `citeproc-render-bib' for details) to a fully formatted
  bibliography,
NO-EXTERNAL-LINKS is non-nil if the formatter doesn't support
  external linking."
    rt (cite #'identity) (bib-item (lambda (x _) x))
    (bib (lambda (x _) (mapconcat #'identity x "\n\n")))
    (no-external-links nil))

(defun citeproc-formatter-fun-create (fmt-alist)
  "Return a rich-text formatter function based on FMT-ALIST.
FMT-ALIST is an alist with some or all of the following symbols
as keys:
- `unformatted',
- `font-style-italic', `font-style-oblique', `font-style-normal',
- `font-variant-small-caps', `font-variant-normal',
- `font-weight-bold', `font-weight-light', `font-weight-normal',
- `text-decoration-underline', `text-decoration-normal',
- `vertical-align-sub', `vertical-align-sup', `vertical-align-baseline',
- `display-block', `display-left-margin', `display-right-inline',
  `display-indent',
- `href',
- `cited-item-no', `bib-item-no'.

With the exceptions listed below the values should be
one-argument formatting functions that format the input string
according to the attribute-value pair specified by the key.

The exceptions are the keys
`unformatted', for which the value should be a one-argument
  function converting unformatted text into the required
  format (e.g., by escaping);
`href', here the value should be a two-argument function
  mapping the first argument as anchor text and the second as
  target URI to a hyperlink representation; and
`cited-item-no' and `bib-item-no' whose associated values
  should be two-argument functions, which are called with the
  already formatted cites/bibliography item text and the number of
  the bibliography item as a string."
  (cl-labels
      ((rt-fmt
	(rt)
	(pcase rt
	  ((pred stringp) (funcall (alist-get 'unformatted fmt-alist) rt))
	  ((pred consp)
	   (let ((attrs (car rt))
		 (result (mapconcat #'rt-fmt (cdr rt) "")))
	     (dolist (attr attrs)
	       (let ((key (car attr)))
		 (if (memq key '(href cited-item-no bib-item-no))
		     (when-let ((fmt-fun (alist-get key fmt-alist)))
		       (setq result (funcall fmt-fun result (cdr attr))))
		   (when-let
		       ((fmt-fun
			 (alist-get
			  (pcase attr
			    ('(font-style . "italic") 'font-style-italic)
			    ('(font-weight . "bold") 'font-weight-bold)
			    ('(display . "indent") 'display-indent)
			    ('(display . "left-margin") 'display-left-margin)
			    ('(display . "right-inline") 'display-right-inline)
			    ('(display . "block") 'display-block)
			    ('(vertical-align . "sup") 'vertical-align-sup)
			    ('(vertical-align . "baseline") 'vertical-align-baseline)
			    ('(font-variant . "small-caps") 'font-variant-small-caps)
			    ('(text-decoration . "underline") 'text-decoration-underline)
			    ('(font-style . "oblique") 'font-style-oblique)
			    ('(font-style . "normal") 'font-style-normal)
			    ('(font-variant . "normal") 'font-variant-normal)
			    ('(font-weight . "light") 'font-weight-light)
			    ('(font-weight . "normal") 'font-weight-normal)
			    ('(text-decoration . "normal") 'text-decoration-normal)
			    ('(vertical-align . "sub") 'vertical-align-sub))
			  fmt-alist)))
		     (setq result (funcall fmt-fun result))))))
	     result))
	  (_ rt))))
    #'rt-fmt))

;;;; Specific formatters

;; Org

(defun citeproc-fmt--org-link (anchor target)
  "Return an Org link with ANCHOR and TARGET.
If ANCHOR is string= to TARGET then return ANCHOR."
  (if (string= anchor target)
      anchor
    (concat "[[" target "][" anchor "]]")))

(defconst citeproc-fmt--org-alist
  `((unformatted . identity)
    (href . ,#'citeproc-fmt--org-link)
    (cited-item-no . ,(lambda (x y) (concat "[[citeproc_bib_item_" y "][" x "]]")))
    (bib-item-no . ,(lambda (x y) (concat "<<citeproc_bib_item_" y ">>" x)))
    ;; Warning: The next four formatter lines put protective zero-width spaces
    ;; around the Org format characters ('/' etc.).
    (font-style-italic . ,(lambda (x) (concat "​/" x "/​")))
    (font-style-oblique . ,(lambda (x) (concat "​/" x "/​")))
    (font-weight-bold . ,(lambda (x) (concat "​*" x "*​")))
    (text-decoration-underline . ,(lambda (x) (concat "​_" x "_​")))
    ;; End of zero-width space protected formatters.
    (font-variant-small-caps . ,(lambda (x) (upcase x)))
    (vertical-align-sub . ,(lambda (x) (concat "_{" x "}")))
    (vertical-align-sup . ,(lambda (x) (concat "^{" x "}")))
    (display-left-margin . ,(lambda (x) (concat x " ")))))

(defvar citeproc-fmt--org-format-rt-1
  (citeproc-formatter-fun-create citeproc-fmt--org-alist)
  "Recursive rich-text Org formatter.
Doesn't do finalization by removing zero-width spaces.")

(defun citeproc-fmt--org-format-rt (rt)
  "Convert rich-text RT into Org format.
Performs finalization by removing unnecessary zero-width spaces."
  (let ((result (funcall citeproc-fmt--org-format-rt-1 rt)))
    (when (> (length result) 2)
      ;; First we remove z-w spaces around spaces and before punctuation.
      (setq result (citeproc-s-replace-all-seq
		    result '((" ​" . " ") ("​ " . " ") ("​," . ",") ("​;" . ";")
			     ("​:" . ":") ("​." . "."))))
      ;; Starting and ending z-w spaces are also removed, but not before an
      ;; asterisk to avoid creating an Org heading.
      (when (and (= (aref result 0) 8203)
		 (not (= (aref result 1) ?*)))
	(setq result (substring result 1)))
      (when (= (aref result (- (length result) 1)) 8203)
	(setq result (substring result 0 -1))))
    result))

;; HTML

(defun citeproc-fmt--xml-escape (s)
  "Return the xml-escaped version of string S.
Only '&', '<' and '>' are escaped to keep compatibility with the
CSL tests."
  (citeproc-s-replace-all-seq s '(("&" . "&#38;") ("<" . "&#60;") (">" . "&#62;"))))

(defconst citeproc-fmt--html-alist
  `((unformatted . citeproc-fmt--xml-escape)
    (href . ,(lambda (x y) (concat "<a href=\"" y "\">" x "</a>")))
    (cited-item-no . ,(lambda (x y) (concat "<a href=\"#citeproc_bib_item_" y "\">"
					    x "</a>")))
    (bib-item-no . ,(lambda (x y) (concat "<a id=\"citeproc_bib_item_" y "\"></a>"
					  x)))
    (font-style-italic . ,(lambda (x) (concat "<i>" x "</i>")))
    (font-style-oblique . ,(lambda (x)
			     (concat "<span style=\"font-style:oblique;\"" x "</span>")))
    (font-variant-small-caps . ,(lambda (x)
				  (concat
				   "<span style=\"font-variant:small-caps;\">" x "</span>")))
    (font-weight-bold . ,(lambda (x) (concat "<b>" x "</b>")))
    (text-decoration-underline .
			       ,(lambda (x)
				  (concat
				   "<span style=\"text-decoration:underline;\">" x "</span>")))
    (vertical-align-sub . ,(lambda (x) (concat "<sub>" x "</sub>")))
    (vertical-align-sup . ,(lambda (x) (concat "<sup>" x "</sup>")))
    (vertical-align-baseline . ,(lambda (x) (concat "<span style=\"baseline\">" x "</span>")))
    (display-left-margin . ,(lambda (x) (concat "\n    <div class=\"csl-left-margin\">"
						x "</div>")))
    (display-right-inline . ,(lambda (x) (concat "<div class=\"csl-right-inline\">"
						 x "</div>\n  ")))
    (display-block . ,(lambda (x) (concat "\n\n    <div class=\"csl-block\">"
					  x "</div>\n")))
    (display-indent . ,(lambda (x) (concat "<div class=\"csl-indent\">" x "</div>\n  ")))))

(defconst citeproc-fmt--csl-test-alist
  `((unformatted . citeproc-fmt--xml-escape)
    (cited-item-no . ,(lambda (x y) (concat "<a href=\"#citeproc_bib_item_" y "\">"
					    x "</a>")))
    (bib-item-no . ,(lambda (x y) (concat "<a id=\"citeproc_bib_item_" y "\"></a>"
					  x)))
    (font-style-italic . ,(lambda (x) (concat "<i>" x "</i>")))
    (font-style-oblique . ,(lambda (x)
			     (concat "<span style=\"font-style:oblique;\"" x "</span>")))
    (font-variant-small-caps . ,(lambda (x)
				  (concat
				   "<span style=\"font-variant:small-caps;\">" x "</span>")))
    (font-weight-bold . ,(lambda (x) (concat "<b>" x "</b>")))
    (text-decoration-underline .
     ,(lambda (x)
	(concat
	 "<span style=\"text-decoration:underline;\">" x "</span>")))
    (vertical-align-sub . ,(lambda (x) (concat "<sub>" x "</sub>")))
    (vertical-align-sup . ,(lambda (x) (concat "<sup>" x "</sup>")))
    (vertical-align-baseline . ,(lambda (x) (concat "<span style=\"baseline\">" x "</span>")))
    (display-left-margin . ,(lambda (x) (concat "\n    <div class=\"csl-left-margin\">"
						x "</div>")))
    (display-right-inline . ,(lambda (x) (concat "<div class=\"csl-right-inline\">"
						 x "</div>\n  ")))
    (display-block . ,(lambda (x) (concat "\n\n    <div class=\"csl-block\">"
					  x "</div>\n")))
    (display-indent . ,(lambda (x) (concat "<div class=\"csl-indent\">" x "</div>\n  ")))))

(defun citeproc-fmt--html-bib-formatter (items _bib-format)
  "Return a html bibliography from already formatted ITEMS."
  (concat "<div class=\"csl-bib-body\">\n"
	  (mapconcat (lambda (i)
		       (concat "  <div class=\"csl-entry\">" i "</div>\n"))
		     items
		     "")
	  "</div>"))

;; LaTeX

(defconst citeproc-fmt--latex-esc-regex
  (regexp-opt '("_" "&" "#" "%" "$"))
  "Regular expression matching characters to be escaped in LaTeX output.")

(defun citeproc-fmt--latex-escape (s)
  "Return the LaTeX-escaped version of string S."
  (replace-regexp-in-string citeproc-fmt--latex-esc-regex "\\\\\\&" s))

(defun citeproc-fmt--latex-href (text uri)
  (let ((escaped-uri (replace-regexp-in-string "%" "\\\\%" uri)))
   (if (string-prefix-p "http" text)
       (concat "\\url{" escaped-uri "}")
     (concat "\\href{" escaped-uri "}{" text "}"))))

(defconst citeproc-fmt--latex-alist
  `((unformatted . ,#'citeproc-fmt--latex-escape)
    (href . ,#'citeproc-fmt--latex-href)
    (font-style-italic . ,(lambda (x) (concat "\\textit{" x "}")))
    (font-weight-bold . ,(lambda (x) (concat "\\textbf{" x "}")))
    (cited-item-no . ,(lambda (x y) (concat "\\citeprocitem{" y "}{" x "}")))
    (bib-item-no . ,(lambda (x y) (concat "\\hypertarget{citeproc_bib_item_" y "}{"
					  x "}")))
    (font-variant-small-caps . ,(lambda (x) (concat "\\textsc{" x "}")))
    (text-decoration-underline . ,(lambda (x) (concat "\\underline{" x "}")))
    (vertical-align-sup . ,(lambda (x) (concat "\\textsuperscript{" x "}")))
    (display-left-margin . ,(lambda (x) (concat x " ")))
    (vertical-align-sub . ,(lambda (x) (concat "\\textsubscript{" x "}")))
    (font-style-oblique . ,(lambda (x) (concat "\\textsl{" x "}")))))

;; Org-LaTeX

(defconst citeproc-fmt--org-latex-alist
  `((unformatted . ,#'citeproc-fmt--latex-escape)
    (href . ,#'citeproc-fmt--latex-href)
    (font-style-italic . ,(lambda (x) (concat "\\textit{" x "}")))
    (font-weight-bold . ,(lambda (x) (concat "\\textbf{" x "}")))
    (cited-item-no . ,(lambda (x y) (concat "\\cslcitation{" y "}{" x "}")))
    (bib-item-no . ,(lambda (x y) (concat "\\cslbibitem{" y "}{" x "}")))
    (font-variant-small-caps . ,(lambda (x) (concat "\\textsc{" x "}")))
    (text-decoration-underline . ,(lambda (x) (concat "\\underline{" x "}")))
    (vertical-align-sup . ,(lambda (x) (concat "\\textsuperscript{" x "}")))
    (display-left-margin . ,(lambda (x) (concat "\\cslleftmargin{" x "}")))
    (display-right-inline . ,(lambda (x) (concat "\\cslrightinline{" x "}")))
    (display-block . ,(lambda (x) (concat "\\cslblock{" x "}")))
    (display-indent . ,(lambda (x) (concat "\\cslindent{" x "}")))
    (vertical-align-sub . ,(lambda (x) (concat "\\textsubscript{" x "}")))
    (font-style-oblique . ,(lambda (x) (concat "\\textsl{" x "}")))))

(defun citeproc-fmt--org-latex-bib-formatter (items bib-format)
  "Return an Org LaTeX bibliography of ITEMS formatted in BIB-FORMAT."
  (let-alist bib-format
    (let ((hanging-indent (if .hanging-indent "1" "0"))
	  (entry-spacing (if (and .entry-spacing (<= 1 .entry-spacing))
			     (number-to-string (- .entry-spacing 1))
			   "0")))
      (concat "\\begin{cslbibliography}{" hanging-indent "}{" entry-spacing "}\n"
	      (mapconcat #'identity items "\n\n")
	      "\n\n\\end{cslbibliography}\n"))))

;; Org-ODT

(defconst citeproc-fmt--org-odt-alist
  `((unformatted . citeproc-fmt--xml-escape)
    (href . ,(lambda (x y) (concat "<text:a xlink:type=\"simple\" xlink:href=\""
				   y "\">" x "</text:a>")))
    (cited-item-no
     . ,(lambda (x y) (concat "<text:a xlink:type=\"simple\" xlink:href=\"#citeproc_bib_item_"
			      y "\">" x "</text:a>")))
    (bib-item-no
     . ,(lambda (x y)
	  (concat "<text:bookmark-start text:name=\"OrgXref.citeproc_bib_item_" y "\"/>"
		  "<text:bookmark text:name=\"citeproc_bib_item_" y "\"/>"
		  "<text:bookmark-end text:name=\"OrgXref.citeproc_bib_item_" y "\"/>" x)))
    (font-style-italic
     . ,(lambda (x) (concat "<text:span text:style-name=\"Emphasis\">" x "</text:span>")))
    (font-style-oblique
     . ,(lambda (x) (concat "<text:span text:style-name=\"Emphasis\">" x "</text:span>")))
    ;; NOTE: small caps support requires the availability of the OrgSmallcaps ODT style
    ;; this requires an addition to or replacement of the default OrgOdtStyles.xml
    (font-variant-small-caps
     . ,(lambda (x) (concat "<text:span text:style-name=\"OrgSmallCaps\">" x "</text:span>")))
    (font-weight-bold
     . ,(lambda (x) (concat "<text:span text:style-name=\"Bold\">" x "</text:span>")))
    (text-decoration-underline
     . ,(lambda (x) (concat "<text:span text:style-name=\"Underline\">" x "</text:span>")))
    (vertical-align-sub
     . ,(lambda (x) (concat "<text:span text:style-name=\"OrgSubscript\">" x "</text:span>")))
    (vertical-align-sup
     . ,(lambda (x)
	  (concat "<text:span text:style-name=\"OrgSuperscript\">" x "</text:span>")))
    ;; TODO:
    ;; - display-left-margin 
    ;; - display-right-inline 
    ;; - display-block 
    ;; - display-indent 
    ))

(defun citeproc-fmt--org-odt-bib-formatter (items _bib-format)
  "Return a html bibliography from already formatted ITEMS."
  (mapconcat (lambda (i)
	       (concat "<text:p text:style-name=\"Text_20_body\">" i "</text:p>"))
	     items
	     "\n"))

;; Define the formatters alist

(defvar citeproc-fmt--formatters-alist
  `((org-odt . ,(citeproc-formatter-create
		 :rt (citeproc-formatter-fun-create citeproc-fmt--org-odt-alist)
		 :bib #'citeproc-fmt--org-odt-bib-formatter
		 ))
    (html . ,(citeproc-formatter-create
	      :rt (citeproc-formatter-fun-create citeproc-fmt--html-alist)
	      :bib #'citeproc-fmt--html-bib-formatter))
    (csl-test . ,(citeproc-formatter-create
		  :rt (citeproc-formatter-fun-create citeproc-fmt--csl-test-alist)
		  :bib #'citeproc-fmt--html-bib-formatter
		  :no-external-links t))
    (raw . ,(citeproc-formatter-create :rt #'identity :bib (lambda (x _) x)))
    (org . ,(citeproc-formatter-create :rt #'citeproc-fmt--org-format-rt))
    (org-latex . ,(citeproc-formatter-create
		   :rt (citeproc-formatter-fun-create citeproc-fmt--org-latex-alist)
		   :bib #'citeproc-fmt--org-latex-bib-formatter))
    (latex . ,(citeproc-formatter-create
	       :rt (citeproc-formatter-fun-create citeproc-fmt--latex-alist)
	       :bib (lambda (x _) (concat (mapconcat #'identity x "\n\n")
					  "\\bigskip"))))
    (plain . ,(citeproc-formatter-create :rt #'citeproc-rt-to-plain
					 :no-external-links t)))
  "Alist mapping supported output formats to formatter structs.")

(defun citeproc-formatter-for-format (format)
  "Return the formatter struct belonging to FORMAT.
FORMAT is a symbol"
  (if-let ((formatter (alist-get format citeproc-fmt--formatters-alist)))
      formatter
    (error "No formatter for citeproc format `%s'" format)))

(provide 'citeproc-formatters)

;;; citeproc-formatters.el ends here
