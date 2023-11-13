;;; org-ref-natbib-bbl-citeproc.el --- A bibtex + natbib BBL-based citeproc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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
;;
;;; Commentary:
;;
;; This is a citation processor that uses the bbl from bibtex and natbib.
;;
;; It is kind of hacky. I parse the bbl file into entries, and then replace the
;; in-text citations and the bibliography with data from the parsed bbl.
;;
;; The CSL exporter is probably better all around than this, but this does have
;; the advantage of using LaTeX/bibtex for formatting.
;;
;; TODO: pre/post notes are not handled in the bbl file. They have to be handled
;; separately
;;
;; This seems to work ok for
;; See https://gking.harvard.edu/files/natnotes2.pdf
;;
;; Basically supports stylying options in natbib including [numbers, super, authoryear] for style,
;; and [round, square, curly, angle] for in-text citations
;; and [comma, semicolon; colon] for citation separators
;; and [sort, compress, sort&compress] if you want to sort and/or compress citation numbers.
;;
;; longnamesfirst is not currently supported.
;;
;; To use this for HTML export you do something like this:
;; (let ((org-export-before-parsing-hook '(org-ref-bbl-preprocess)))
;;   (org-open-file (org-html-export-to-html)))
;;
;;
;;; Code:

(defvar org-ref-natmove)  			;silence compiler

(defun org-bbl-clean-string (s)
  "Clean S of markups.
This replaces some LaTeX markup and bibtex markups including:
\\emph{}, \\doi{}, \\url{},\\citename, and {\\em ...} {\\bf ...},
removes things like \\protect and \\penalty0, and replaces ~ with
a space. This function is surely not complete, and it does not
cover any math (yet)."
  (with-temp-buffer
    (insert s)
    ;; I convert these to org syntax.
    ;; \emph... -> org italics
    ;; TODO This list is surely not complete.
    ;; convert \cmd{stuff} to the format string
    (let (p1 p2 p3 p4 ss cmd-p)
      (cl-loop for (cmd . repl-format) in '(("emph" . "/%s/")
					    ("doi" . "%s")
					    ("url" . "%s")
					    ("citename" . "%s")
					    ;; natexlab seems to be for differentiating similar authors.
					    ("natexlab" . "(%s)")
					    ;; I use \ce{} a lot, and just strip it here.
					    ("ce" . "%s"))
	       do
	       (goto-char (point-min))
	       (while (search-forward (format "\\%s{" cmd) nil t)
		 (setq p1 (match-beginning 0)
		       p2 (point))
		 (backward-char)
		 (forward-list)
		 (setq p4 (point)
		       p3 (- (point) 1))
		 (setq s (buffer-substring p2 p3))
		 (cl--set-buffer-substring p1 p4 (format repl-format s))))

      ;; some times we have these markups TODO I bet there are more Note I use
      ;; non-breaking spaces here to make the org syntax ok. It doesn't seem to
      ;; do quite the right thing in the bib-items though.
      (cl-loop for (markup . fmt) in '(("\\em" . " /%s/ ")
				       ("\\it" . " /%s/ ")
				       ("\\bf" . " *%s* ")
				       ("\\tt" . " =%s= "))
	       do
	       (goto-char (point-min))
	       (while (search-forward (concat "{" markup) nil t)
		 (setq p1 (match-beginning 0)
		       p2 (point))
		 (goto-char (match-beginning 0))
		 (forward-list)
		 (setq p4 (point)
		       p3 (- (point) 1))
		 (setq ss (string-trim (buffer-substring p2 p3)))
		 (setf (buffer-substring p1 p4) (format fmt ss))))

      ;; {text} for protecting case. This is tricky to do reliably. I try to check
      ;; if this is not part of a command, and skip it if so. This leaves
      ;; un-cleaned commands in, which is desirable to me so you can see if there
      ;; are ones we might handle in the future.
      (goto-char (point-min))
      (while (search-forward "{" nil t)
	;; I think if we go back a word, and are then looking back at \\, we are in a command.
	;; this would fail on a latex command like \word-word{} but I can't think of any right now.
	(save-excursion
	  (backward-word)
	  (setq cmd-p (looking-back "\\\\" 1)))
	;; This looks back for a \cmd, basically things not a closing }
	(unless cmd-p
	  (setq p1 (match-beginning 0)
		p2 (point))
	  (backward-char)
	  (forward-list)
	  (setq p4 (point)
		p3 (- (point) 1))
	  (setq s (buffer-substring p2 p3))
	  (cl--set-buffer-substring p1 p4 s))))


    (let ((result (buffer-string)))
      (cl-loop for (s . repl) in '(("~" . " ")
				   ("\\\\&" . "&")
				   ("\\\\protect" . "")
				   ("\\\\penalty0" . "")
				   ("\\\\ " . " "))
	       do
	       (setq result (replace-regexp-in-string s repl result)))
      result)))


(defun org-ref-bbl-get-natbib-options ()
  "Return natbib-options (including the brackets).
Defaults to [numbers,super,sort]"
  (goto-char (point-min))
  (if (re-search-forward "\\usepackage\\(?1:\\[.*\\]\\)?{natbib}")
      (match-string 1)
    "[numbers,super,sort]"))



(defun org-ref-bbl-entry (entry)
  "ENTRY is a string containing the contents of a bibitem from a bbl file.
Return the bibliography string associated with the entry. This is
done in a temp-buffer so we don't actually modify the bbl file."
  (with-temp-buffer
    (insert (org-bbl-clean-string entry))
    (goto-char (point-min))
    (let (p1
	  p2 
	  authors
	  blocks
	  entry)
      (setq p1 (point))
      (search-forward "\\newblock")
      (setq p2 (match-beginning 0))
      (setq authors (string-join
		     (mapcar 'string-trim
			     (split-string (buffer-substring p1 p2) "\n"))
		     " "))
      (goto-char (- p2 1))
      (while (search-forward "\\newblock" nil t)
	(setq p1 (point))
	(setq p2 (save-excursion
		   (if (search-forward "\\newblock" nil t)
		       (match-beginning 0)
		     (point-max))))

	(cl-pushnew (string-join
		     (mapcar 'string-trim
			     (split-string (buffer-substring p1 p2) "\n"))
		     " ")
		    blocks))

      (setq entry (string-join (append (list authors) blocks) " "))
      entry)))


(defun org-ref-bbl-bibliography-data ()
  "Get an p-list for each entry in the buffer.
Assumes you are in a bbl file.
Returns a plist (list bibitem-key :entry (org-ref-bbl-entry bibitem-entry)
			  :index counter :bracket-data bibitem-bracket)"
  (let ((data '())
	N ss
	p1 p2 bbl-max
	bibitem-bracket
	bibitem-key
	bibitem-entry
	(counter 0))

    (goto-char (point-min))
    (search-forward "\\end{thebibliography}")
    ;; this is the character that the last line starts on.
    (setq bbl-max (match-beginning 0))

    (goto-char (point-min))
    (when (looking-at "\\\\begin{thebibliography}{\\(?1:[0-9]*\\)}")
      (setq N (string-to-number (match-string 1))))

    ;; This might only work for numeric types
    (if (> 0 N)
	(cl-loop for i from 1 to N do
		 (search-forward "\\bibitem[")
		 ;; get text in [...]
		 (setq p1 (point))
		 (forward-list)
		 (setq p2 (point))
		 ;; the bracket usually contains [stuff (year) more stuff]
		 (setq ss (org-bbl-clean-string
			   (buffer-substring-no-properties p1 p2)))

		 (let* ((s (string-match "(" ss))
			(e (string-match ")" ss s)))
		   ;; in numerical mode I think this is ignored, but we get it
		   ;; just in case for some later day.
		   (setq bibitem-bracket (format "%s%s"
						 (substring ss 0 s)
						 (if (string= "()" (substring ss s (+ e 1)))
						     ""
						   (concat " " (substring ss s (+ e 1)))))))

		 ;; Now get the key. From the last
		 (search-forward "{")
		 (goto-char (match-beginning 0))
		 (setq p1 (+ 1 (point)))
		 (forward-list)
		 (setq p2 (- (point) 1)
		       bibitem-key (buffer-substring p1 p2))

		 ;; Now get up to the next bibitem
		 (setq p1 (point))
		 (save-excursion
		   (setq p2 (or (when (search-forward "\\bibitem[" nil t)
				  (match-beginning 0))
				bbl-max)))
		 (setq bibitem-entry (string-trim (buffer-substring p1 p2)))

		 (cl-pushnew (list bibitem-key :entry (org-ref-bbl-entry bibitem-entry)
				   :index i :bracket-data bibitem-bracket)
			     data))
      ;; no number found probably author year
      (while (search-forward "\\bibitem[" nil t)
	(cl-incf counter)
	;; get text in [...]
	(setq p1 (point))
	(search-forward "]")
	(backward-char)
	(setq p2 (point))

	(setq ss (org-bbl-clean-string
		  (buffer-substring-no-properties p1 p2)))

	(let* ((s (string-match "(" ss))
	       (e (string-match ")" ss s)))

	  (setq bibitem-bracket (format "%s%s"
					(substring ss 0 s)
					(if (string= "()" (substring ss s (+ e 1)))
					    ""
					  (concat " " (substring ss s (+ e 1)))))))


	;; Now get the key. From the last
	(search-forward "{")
	(goto-char (match-beginning 0))
	(setq p1 (+ 1 (point)))
	(forward-list)
	(setq p2 (- (point) 1)
	      bibitem-key (buffer-substring p1 p2))

	;; Now get up to the next bibitem
	(setq p1 (point))
	(save-excursion
	  (setq p2 (or (when (search-forward "\\bibitem[" nil t)
			 (match-beginning 0))
		       bbl-max)))
	(setq bibitem-entry (string-trim (buffer-substring p1 p2)))

	(cl-pushnew (list bibitem-key :entry (org-ref-bbl-entry bibitem-entry)
			  :index counter :bracket-data bibitem-bracket)
		    data)))
    (reverse data)))


(defun org-ref-bbl-compress-numbers (lst)
  "Take a list like (1 2 3 6) and return \"1-3,6\"."
  (let* ((a (pop lst))
	 b
	 (sequence (list a))
	 (sequentials '()))
    (while lst
      (setq b (pop lst))
      (if (= (- b a) 1)
	  ;; we have a sequence, add it, set a=b and continue
	  (setq sequence (append sequence (list b))
		a b)
	;; lost sequence, store the sequence
	(setq sequentials (append sequentials (list sequence))
	      sequence (list b)
	      a b)))
    ;; store last one
    (setq sequentials (append sequentials (list sequence)))

    ;; Now construct strings for each group
    (cl-loop for group in sequentials collect
	     (pcase (length group)
	       (1 (format "[[%s]]" (car group)))
	       (2 (format "[[%s]],[[%s]]" (cl-first group) (cl-second group)))
	       (_ (format "[[%s]]-[[%s]]" (cl-first group) (car (last group))))))))


(defun org-ref-replace-cite-link (link bibdata NATBIB-OPTIONS backend)
  "NATBIB-OPTIONS is the string to options.
Argument LINK is an org link for a citation.
Argument BIBDATA the data parsed from a bbl file.
Argument BACKEND is the export format."
  (let* ((refs (plist-get (org-ref-parse-cite-path (org-element-property :path link)) :references))
	 (keys (cl-loop for ref in refs collect (plist-get ref :key)))
	 items replacements replacement joiner p1 p2)

    ;; Numeric types
    (cond
     ((or (string-match-p "numbers" NATBIB-OPTIONS)
	  (string-match-p "super" NATBIB-OPTIONS))
      (setq items (cl-loop for key in keys
			   collect (plist-get (cdr (assoc key bibdata)) :index)))

      (when (string-match-p "sort" NATBIB-OPTIONS)
	(setq items (sort items #'<)))

      (setq replacements (if (string-match-p "compress" NATBIB-OPTIONS)
			     (org-ref-bbl-compress-numbers items)
			   ;; these are fuzzy org links to the target in item.
			   (cl-loop for item in items collect (format "[[%s]]" item)))))

     ;; authoryear types
     ;; This html provides an anchor that is named. Targets are usually replaced with numbers, not the text.
     ((string-match-p "authoryear" NATBIB-OPTIONS)
      (setq replacements (cl-loop for key in keys
				  collect
				  (cond
				   ((eq backend 'html)
				    (format "@@html:<a href=\"#%s\">%s</a>@@"
					    (plist-get (cdr (assoc key bibdata)) :bracket-data) 
					    (plist-get (cdr (assoc key bibdata)) :bracket-data)))
				   (t
				    (format "[[%s]]" (plist-get (cdr (assoc key bibdata)) :bracket-data)))))))
     (t
      (error "%s not supported yet" NATBIB-OPTIONS)))

    ;; Now join all the replacements for each reference
    (setq joiner (cond
		  ((string-match-p "comma" NATBIB-OPTIONS)
		   ",")
		  ((or (string-match-p "semicolon" NATBIB-OPTIONS)
		       (string-match-p "colon" NATBIB-OPTIONS))
		   ";")
		  (t
		   ";")))

    ;; Finally create the overall replacement
    (setq replacement (cond
		       ((string-match-p "super" NATBIB-OPTIONS)
			(format "^{%s}" (string-join replacements joiner)))

		       ((string-match-p "square" NATBIB-OPTIONS)
			(format "[%s]" (string-join replacements joiner)))

		       ((string-match-p "round" NATBIB-OPTIONS)
			(format "(%s)" (string-join replacements joiner)))

		       ((string-match-p "curly" NATBIB-OPTIONS)
			(format "{%s}" (string-join replacements joiner)))

		       ((string-match-p "angle" NATBIB-OPTIONS)
			(format "<%s>" (string-join replacements joiner)))

		       ((string-match-p "authoryear" NATBIB-OPTIONS)
			(string-join replacements joiner))

		       (t
			(error "Cannot compute replacement for %s" NATBIB-OPTIONS))))

    ;; When super is the style, we need to replace any blanks back to the last
    ;; non-space. We just look back and move point if needed here.
    (if (not (string-match-p "super" NATBIB-OPTIONS))
	(setq p1 (org-element-property :begin link))
      (goto-char (org-element-property :begin link))
      (while (looking-back " " 1)
	(backward-char))
      (setq p1 (point)))

    (setq p2 (org-element-property :end link))

    ;; org-ref-natmove is dynamically bound here
    (when org-ref-natmove
      (save-excursion
	(goto-char (org-element-property :end link))
	(skip-chars-backward " ")

	(when (string-match-p "[[:punct:]]" (buffer-substring (point) (+ (point) 1)))
	  ;; Get the character
	  (setq replacement (concat (buffer-substring (org-element-property :end link)
						      (+ 1 (org-element-property :end link)))
				    replacement)
		p2 (+ 1 (org-element-property :end link))))))

    (setf (buffer-substring p1 p2)
	  (concat replacement (make-string (org-element-property :post-blank link) ? )))))


(defun org-ref-bbl-replace-bibliography (bib-link bibdata NATBIB-OPTIONS backend)
  "Get a replacement bibliography string for BIBDATA and NATBIB-OPTIONS.
BIBDATA comes from `org-ref-bbl-bibliography-data'.
Argument BIB-LINK an org link for a bibliography.
Argument BACKEND is the export format."
  (cl--set-buffer-substring (org-element-property :begin bib-link)
			    (org-element-property :end bib-link)
			    (cond
			     ((or (string-match-p "numbers" NATBIB-OPTIONS)
				  (string-match-p "super" NATBIB-OPTIONS))
			      (concat "\n* Bibliography\n\n"
				      (string-join
				       (cl-loop for entry in bibdata collect
						(format "%s. <<%s>> %s"
							(plist-get (cdr entry) :index)
							(plist-get (cdr entry) :index)
							(plist-get (cdr entry) :entry)))
				       "\n\n")))


			     ((string-match-p "authoryear" NATBIB-OPTIONS)
			      (concat "\n* Bibliography\n\n"
				      (string-join
				       (cl-loop for entry in bibdata collect
						(cond
						 ((eq backend 'html)
						  (format "- @@html:<a id=\"%s\"></a>@@(%s) %s\n"
							  (plist-get (cdr entry) :bracket-data)
							  (plist-get (cdr entry) :bracket-data)
							  (plist-get (cdr entry) :entry)))
						 (t
						  (format "- <<%s>> %s"
							  (plist-get (cdr entry) :bracket-data)
							  (plist-get (cdr entry) :entry)))))
				       
				       
				       "\n")))
			     (t
			      (error "%s not supported yet" NATBIB-OPTIONS)))))



(defun org-ref-bbl-preprocess (&optional backend)
  "Should work on a copy of the buffer.
Meant to be used in `org-export-before-parsing-hook'.
Optional argument BACKEND The export backend.

You need a LaTeX file and a bbl file for it. This hook generates
those, then gets the data, replaces the citations and the
bibliography.
"
  (let* ((org-export-before-parsing-hook nil)
	 (tex-file (org-latex-export-to-latex))
	 (bbl-file (concat (file-name-sans-extension tex-file) ".bbl"))
	 natbib-options bibdata org-ref-natmove
	 buf)

    (when-let (buf (find-buffer-visiting tex-file))
      (kill-buffer buf))

    (when-let (buf (find-buffer-visiting bbl-file))
      (kill-buffer buf))

    ;; refresh these
    (call-process-shell-command (format "latex -shell-escape %s" tex-file))
    (call-process-shell-command (format "bibtex %s" (file-name-sans-extension tex-file)))

    (setq buf (find-file-noselect tex-file))
    (with-current-buffer buf
      (goto-char (point-min))
      (setq natbib-options (org-ref-bbl-get-natbib-options))
      (goto-char (point-min))
      (setq org-ref-natmove (search-forward "\\usepackage{natmove}" nil t)))
    (kill-buffer buf)

    (setq buf (find-file-noselect bbl-file))
    (with-current-buffer buf
      (goto-char (point-min))
      (setq bibdata (org-ref-bbl-bibliography-data)))
    (kill-buffer buf)

    ;; Replace all the cite links
    (cl-loop for cl in (reverse (org-ref-get-cite-links)) do
	     (org-ref-replace-cite-link cl bibdata natbib-options backend))

    (org-ref-bbl-replace-bibliography
     (org-element-map (org-element-parse-buffer) 'link
       (lambda (lnk)
	 (when (string= (org-element-property :type lnk) "bibliography")
	   lnk))
       nil
       t)
     bibdata natbib-options backend)))


(provide 'org-ref-natbib-bbl-citeproc)

;;; org-ref-natbib-bbl-citeproc.el ends here
