;;; ox-bibtex.el --- Export bibtex fragments

;; Copyright (C) 2009-2014, 2021 Taru Karttunen

;; Author: Taru Karttunen <taruti@taruti.net>
;;      Nicolas Goaziou <n dot goaziou at gmail dot com>
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is an utility to handle BibTeX export to LaTeX, html and ascii
;; exports.  For HTML and ascii it uses the bibtex2html software from:
;;
;;   https://www.lri.fr/~filliatr/bibtex2html/
;;
;; For ascii it uses the pandoc software from:
;;
;;   https://pandoc.org/
;;
;; It also introduces "cite" syntax for Org links.
;;
;; The usage is as follows:
;;
;;   #+BIBLIOGRAPHY: bibfilename stylename optional-options
;;
;; e.g. given foo.bib and using style plain:
;;
;;   #+BIBLIOGRAPHY: foo plain option:-d
;;
;; "stylename" can also be "nil", in which case no style will be used.
;;
;; Full filepaths are also possible:
;;
;;   #+BIBLIOGRAPHY: /home/user/Literature/foo.bib plain option:-d
;;
;; Optional options are of the form:
;;
;;   option:-foobar pass '-foobar' to bibtex2html
;;
;; e.g.,
;;
;;   option:-d    sort by date
;;   option:-a    sort as BibTeX (usually by author) *default*
;;   option:-u    unsorted i.e. same order as in .bib file
;;   option:-r    reverse the sort
;;
;; See the bibtex2html man page for more.  Multiple options can be
;; combined like:
;;
;;   option:-d option:-r
;;
;; Limiting to only the entries cited in the document:
;;
;;   limit:t
;;
;; For LaTeX export this simply inserts the lines
;;
;;   \bibliographystyle{plain}
;;   \bibliography{foo}
;;
;; into the TeX file when exporting.
;;
;; For HTML export it:
;; 1) converts all \cite{foo} and [[cite:foo]] to links to the
;;    bibliography,
;; 2) creates a foo.html and foo_bib.html,
;; 3) includes the contents of foo.html in the exported HTML file.
;;
;; For ascii export it:
;; 1) converts all \cite{foo} and [[cite:foo]] to links to the
;;    bibliography,
;; 2) creates a foo.txt and foo_bib.html,
;; 3) includes the contents of foo.txt in the exported ascii file.
;;
;; For LaTeX export it:
;; 1) converts all [[cite:foo]] to \cite{foo}.

;; Initialization

(require 'cl-lib)

;;; Internal Functions

(defun org-bibtex-get-file (keyword)
  "Return bibliography file as a string.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no file is found,
return nil instead."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (match-string 1 value))))

(defun org-bibtex-get-style (keyword)
  "Return bibliography style as a string.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no style is found,
return nil instead."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (match-string 2 value))))

(defun org-bibtex-get-arguments (keyword)
  "Return \"bibtex2html\" arguments specified by the user.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. Return value is a plist
containing `:options' and `:limit' properties.  The former
contains a list of strings to be passed as options to
\"bibtex2html\" process.  The latter contains a boolean."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (let (options limit)
           (dolist (arg (split-string (match-string 3 value))
                        ;; Return value.
                        (list :options (nreverse options) :limit limit))
             (let* ((s (split-string arg ":"))
                    (key (car s))
                    (value (nth 1 s)))
               (cond ((equal "limit" key)
                      (setq limit (not (equal "nil" value))))
                     ((equal "option" key) (push value options)))))))))

(defun org-bibtex-citation-p (object)
  "Non-nil when OBJECT is a citation."
  (cl-case (org-element-type object)
    (link (equal (org-element-property :type object) "cite"))
    (latex-fragment
     (string-match "\\`\\\\cite{" (org-element-property :value object)))))

(defun org-bibtex-get-citation-key (citation)
  "Return key for a given citation, as a string.
CITATION is a `latex-fragment' or `link' type object satisfying
to `org-bibtex-citation-p' predicate."
  (if (eq (org-element-type citation) 'link)
      (org-element-property :path citation)
    (let ((value (org-element-property :value citation)))
      (and (string-match "\\`\\\\cite{" value)
	   (substring value (match-end 0) -1)))))


;;; Follow cite: links

(defvar org-bibtex-file nil
  "Org file of BibTeX entries.")

(defun org-bibtex-goto-citation (&optional citation)
  "Visit a citation given its ID."
  (interactive)
  (let ((citation (or citation (completing-read "Citation: " (obe-citations)))))
    (find-file (or org-bibtex-file
		   (error "`org-bibtex-file' has not been configured")))
    (let ((position (org-find-property "CUSTOM_ID" citation)))
      (and position (progn (goto-char position) t)))))

(let ((jump-fn (car (cl-remove-if-not #'fboundp '(ebib org-bibtex-goto-citation)))))
  (org-add-link-type "cite" jump-fn))



;;; Filters

(defun org-bibtex-process-bib-files (tree backend info)
  "Send each bibliography in parse tree to \"bibtex2html\" process.
Return new parse tree."
  (when (org-export-derived-backend-p backend 'ascii 'html)
    ;; Initialize dynamically scoped variables.  The first one
    ;; contain an alist between keyword objects and their HTML
    ;; translation.  The second one will contain an alist between
    ;; citation keys and names in the output (according to style).
    (setq org-bibtex-html-entries-alist nil
	  org-bibtex-html-keywords-alist nil)
    (org-element-map tree 'keyword
      (lambda (keyword)
	(when (equal (org-element-property :key keyword) "BIBLIOGRAPHY")
	  (let ((arguments (org-bibtex-get-arguments keyword))
		(file (org-bibtex-get-file keyword))
		temp-file
		out-file)
	    ;; Test if filename is given with .bib-extension and strip
    	    ;; it off. Filenames with another extensions will be
	    ;; untouched and will finally rise an error in bibtex2html.
	    (setq file (if (equal (file-name-extension file) "bib")
			   (file-name-sans-extension file) file))
	    ;; Outpufiles of bibtex2html will be put into current working directory
	    ;; so define a variable for this.
	    (setq out-file (file-name-sans-extension
			    (file-name-nondirectory file)))
	    ;; limit is set: collect citations throughout the document
	    ;; in TEMP-FILE and pass it to "bibtex2html" as "-citefile"
	    ;; argument.
	    (when (plist-get arguments :limit)
	      (let ((citations
		     (org-element-map tree '(latex-fragment link)
		       (lambda (object)
			 (and (org-bibtex-citation-p object)
			      (org-bibtex-get-citation-key object))))))
		(with-temp-file (setq temp-file (make-temp-file "ox-bibtex"))
		  (insert (mapconcat 'identity citations "\n")))
		(setq arguments
		      (plist-put arguments
				 :options
				 (append (plist-get arguments :options)
					 (list "-citefile" temp-file))))))
	    ;; Call "bibtex2html" on specified file.
	    (unless (eq 0 (apply
			   'call-process
			   (append '("bibtex2html" nil nil nil)
				   '("-a" "-nodoc" "-noheader" "-nofooter")
				   (let ((style
					  (org-not-nil
					   (org-bibtex-get-style keyword))))
				     (and style (list "--style" style)))
				   (plist-get arguments :options)
				   (list (concat file ".bib")))))
	      (error "Executing bibtex2html failed"))
	    (and temp-file (delete-file temp-file))
	    ;; Open produced HTML file, and collect Bibtex key names
	    (with-temp-buffer
	      (insert-file-contents (concat out-file ".html"))
	      ;; Update `org-bibtex-html-entries-alist'.
	      (goto-char (point-min))
	      (while (re-search-forward
		      "a name=\"\\([-_a-zA-Z0-9:]+\\)\">\\([^<]+\\)" nil t)
		(push (cons (match-string 1) (match-string 2))
		      org-bibtex-html-entries-alist)))
	    ;; Open produced HTML file, wrap references within a block and
	    ;; return it.
	    (with-temp-buffer
	      (cond
	       ((org-export-derived-backend-p backend 'html)
		(insert (format "<div id=\"bibliography\">\n<h2>%s</h2>\n"
				(org-export-translate "References" :html info)))
		(insert-file-contents (concat out-file ".html"))
		(goto-char (point-max))
		(insert "\n</div>"))
	       ((org-export-derived-backend-p backend 'ascii)
		;; convert HTML references to text w/pandoc
		(unless (eq 0 (call-process "pandoc" nil nil nil
					    (concat out-file ".html")
					    "-o"
					    (concat out-file ".txt")))
		  (error "Executing pandoc failed"))
		(insert
		 (format
		  "%s\n==========\n\n"
		  (org-export-translate
		   "References"
		   (intern (format ":%s" (plist-get info :ascii-charset)))
		   info)))
		(insert-file-contents (concat out-file ".txt"))
		(goto-char (point-min))
		(while (re-search-forward
			"\\[ \\[bib\\][^ ]+ \\(\\]\\||[\n\r]\\)" nil t)
		  (replace-match ""))
		(goto-char (point-min))
		(while (re-search-forward "\\( \\]\\| \\]\\| |\\)" nil t)
		  (replace-match ""))
		(goto-char (point-min))
		(while (re-search-forward "[\n\r]\\([\n\r][\n\r]\\)" nil t)
		  (replace-match "\\1"))))
	      ;; Update `org-bibtex-html-keywords-alist'.
	      (push (cons keyword (buffer-string))
		    org-bibtex-html-keywords-alist)))))))
  ;; Return parse tree unchanged.
  tree)

(defun org-bibtex-merge-contiguous-citations (tree backend info)
  "Merge all contiguous citation in parse tree.
As a side effect, this filter will also turn all \"cite\" links
into \"\\cite{...}\" LaTeX fragments and will extract options.
Cite options are placed into square brackets at the beginning of
the \"\\cite\" command for the LaTeX backend, and are removed for
the HTML and ASCII backends."
  (when (org-export-derived-backend-p backend 'html 'latex 'ascii)
    (org-element-map tree '(link latex-fragment)
      (lambda (object)
	(when (org-bibtex-citation-p object)
	  (let ((new-citation (list 'latex-fragment
				    (list :value ""
					  :post-blank (org-element-property
						       :post-blank object))))
		option)
	    ;; Insert NEW-CITATION right before OBJECT.
	    (org-element-insert-before new-citation object)
	    ;; Remove all subsequent contiguous citations from parse
	    ;; tree, keeping only their citation key.
	    (let ((keys (list (org-bibtex-get-citation-key object)))
		  next)
	      (while (and (setq next (org-export-get-next-element object info))
			  (or (and (stringp next)
				   (not (string-match-p "\\S-" next)))
			      (org-bibtex-citation-p next)))
		(unless (stringp next)
		  (push (org-bibtex-get-citation-key next) keys))
		(org-element-extract-element object)
		(setq object next))
	      ;; Find any options in keys, e.g., "(Chapter 2)key" has
	      ;; the option "Chapter 2".
	      (setq keys
		    (mapcar
		     (lambda (k)
		       (if (string-match "^(\\([^)]\+\\))\\(.*\\)" k)
			   (progn
			     (when (org-export-derived-backend-p backend 'latex)
			       (setq option (format "[%s]" (match-string 1 k))))
			     (match-string 2 k))
			 k))
		     keys))
	      (org-element-extract-element object)
	      ;; Eventually merge all keys within NEW-CITATION.  Also
	      ;; ensure NEW-CITATION has the same :post-blank property
	      ;; as the last citation removed.
	      (org-element-put-property
	       new-citation
	       :post-blank (org-element-property :post-blank object))
	      (org-element-put-property
	       new-citation
	       :value (format "\\cite%s{%s}"
			      (or option "")
			      (mapconcat 'identity (nreverse keys) ",")))))))))
  tree)

(eval-after-load 'ox
  '(progn (add-to-list 'org-export-filter-parse-tree-functions
		       'org-bibtex-process-bib-files)
	  (add-to-list 'org-export-filter-parse-tree-functions
		       'org-bibtex-merge-contiguous-citations)))



;;; LaTeX Part

(defadvice org-latex-keyword (around bibtex-keyword)
  "Translate \"BIBLIOGRAPHY\" keywords into LaTeX syntax.
Fallback to `latex' back-end for other keywords."
  (let ((keyword (ad-get-arg 0)))
    (if (not (equal (org-element-property :key keyword) "BIBLIOGRAPHY"))
        ad-do-it
      (let ((file (org-bibtex-get-file keyword))
            (style (org-not-nil (org-bibtex-get-style keyword))))
        (setq ad-return-value
              (when file
                (concat (and style (format "\\bibliographystyle{%s}\n" style))
                        (format "\\bibliography{%s}" file))))))))

(ad-activate 'org-latex-keyword)



;;; HTML Part

(defvar org-bibtex-html-entries-alist nil)  ; Dynamically scoped.
(defvar org-bibtex-html-keywords-alist nil) ; Dynamically scoped.


;;;; Advices

(defadvice org-html-keyword (around bibtex-keyword)
  "Translate \"BIBLIOGRAPHY\" keywords into HTML syntax.
Fallback to `html' back-end for other keywords."
  (let ((keyword (ad-get-arg 0)))
    (if (not (equal (org-element-property :key keyword) "BIBLIOGRAPHY"))
        ad-do-it
      (setq ad-return-value
            (cdr (assq keyword org-bibtex-html-keywords-alist))))))

(defadvice org-html-latex-fragment (around bibtex-citation)
  "Translate \"\\cite\" LaTeX fragments into HTML syntax.
Fallback to `html' back-end for other keywords."
  (let ((fragment (ad-get-arg 0)))
    (if (not (org-bibtex-citation-p fragment)) ad-do-it
      (setq ad-return-value
            (format "[%s]"
		    (mapconcat
		     (lambda (key)
		       (format "<a href=\"#%s\">%s</a>"
			       key
			       (or (cdr (assoc key org-bibtex-html-entries-alist))
				   key)))
		     (org-split-string
		      (org-bibtex-get-citation-key fragment) ",") ","))))))

(ad-activate 'org-html-keyword)
(ad-activate 'org-html-latex-fragment)


;;; Ascii Part
(defadvice org-ascii-keyword (around bibtex-keyword)
  "Translate \"BIBLIOGRAPHY\" keywords into ascii syntax.
Fallback to `ascii' back-end for other keywords."
  (let ((keyword (ad-get-arg 0)))
    (if (not (equal (org-element-property :key keyword) "BIBLIOGRAPHY"))
        ad-do-it
      (setq ad-return-value
            (cdr (assq keyword org-bibtex-html-keywords-alist))))))

(defadvice org-ascii-latex-fragment (around bibtex-citation)
  "Translate \"\\cite\" LaTeX fragments into ascii syntax.
Fallback to `ascii' back-end for other keywords."
  (let ((fragment (ad-get-arg 0)))
    (if (not (org-bibtex-citation-p fragment)) ad-do-it
      (setq ad-return-value
            (format "[%s]"
		    (mapconcat
		     (lambda (key)
		       (or (cdr (assoc key org-bibtex-html-entries-alist))
			   key))
		     (org-split-string
		      (org-bibtex-get-citation-key fragment) ",") ","))))))

(ad-activate 'org-ascii-keyword)
(ad-activate 'org-ascii-latex-fragment)

(provide 'ox-bibtex)

;;; ox-bibtex.el ends here
