;;; org-bibtex-extras --- extras for working with org-bibtex entries

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.

;; Author: Eric Schulte <eric dot schulte at gmx dot com>
;; Keywords: outlines, hypermedia, bibtex, d3
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 0.01

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Warning: This should certainly be considered EXPERIMENTAL and still
;;          in development, feedback is welcome, but don't expect it
;;          to work.

;; This file add some extra functionality to your bibtex entries which
;; are stored as Org-mode headlines using org-bibtex.el.  Most
;; features expect that you keep all of your reading notes in a single
;; file, set the `obe-bibtex-file' variable to the path to this file.
;;
;; - d3 view :: d3 is a Javascript library which supports interactive
;;              display of graphs.  To view your citations as a d3
;;              graph, execute the following which will create a .json
;;              export of your references file, then grab a copy of
;;              d3, edit examples/force/force.js to replace
;;
;;                var source`"miserables.json";
;;
;;              with
;;
;;                var source`"your-references.json";
;;
;;              then view examples/force/force.html in your browser.
;;
;; - HTML export :: Customize the `obe-html-link-base' variable so
;;                  that it points to an html export of your
;;                  references, then add the following to your html
;;                  export hook, and citations will be resolved during
;;                  html export.
;;
;;	 (add-hook 'org-export-first-hook
;;	 	  (lambda ()
;;	 	    (when (equal org-export-current-backend 'html)
;;	 	      (obe-html-export-citations))))

;;; Code:
(require 'ol-bibtex)

(declare-function org-trim "org" (s &optional keep-lead))

(defcustom obe-bibtex-file nil "File holding bibtex entries.")

(defcustom obe-html-link-base nil
  "Base of citation links.
For example, to point to your `obe-bibtex-file' use the following.

  (setq obe-html-link-base (format \"file:%s\" obe-bibtex-file))
")

(defvar obe-citations nil)
(defun obe-citations ()
  "Return all citations from `obe-bibtex-file'."
  (or obe-citations
      (save-window-excursion
	(find-file (or obe-bibtex-file
		       (error "`obe-bibtex-file' has not been configured")))
	(goto-char (point-min))
	(while (re-search-forward "  :CUSTOM_ID: \\(.+\\)$" nil t)
	  (push (org-no-properties (match-string 1))
		obe-citations))
	obe-citations)))

(defun obe-html-export-citations ()
  "Convert all \\cite{...} citations in the current file into HTML links."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\cite{\\([^\000}]+\\)}" nil t)
      (replace-match
       (save-match-data
	 (mapconcat (lambda (c) (format "[[%s#%s][%s]]" obe-html-link-base c c))
		    (mapcar #'org-trim
			    (split-string (match-string 1) ",")) ", "))))))

(defun obe-meta-to-json (meta &optional fields)
  "Turn a list of META data from citations into a string of json."
  (let ((counter 1) nodes links)
    (flet ((id (it) (position it nodes :test #'string= :key #'car))
	   (col (k) (mapcar (lambda (r) (cdr (assoc k r))) meta))
	   (add (lst)
		(dolist (el lst) (push (cons el counter) nodes))
		(cl-incf counter)))
      ;; build the nodes of the graph
      (add (col :title))
      (add (cl-remove-if (lambda (author) (string-match "others" author))
		         (remove-duplicates (apply #'append (col :authors))
					    :test #'string=)))
      (dolist (field fields)
	(add (remove-duplicates (col field) :test #'string=)))
      ;; build the links in the graph
      (dolist (citation meta)
        (let ((dest (id (cdr (assq :title citation)))))
          (dolist (author (mapcar #'id (cdr (assq :authors citation))))
            (when author (push (cons author dest) links)))
          (let ((jid (id (cdr (assq :journal citation)))))
            (when jid (push (cons jid dest) links)))
          (let ((cid (id (cdr (assq :category citation)))))
            (when cid (push (cons cid dest) links)))))
      ;; build the json string
      (format "{\"nodes\":[%s],\"links\":[%s]}"
	      (mapconcat
	       (lambda (pair)
		 (format "{\"name\":%S,\"group\":%d}"
			 (car pair) (cdr pair)))
	       nodes ",")
	      (mapconcat
	       (lambda (link)
		 (format "{\"source\":%d,\"target\":%d,\"value\":1}"
			 (car link) (cdr link)))
	       (meta-to-links meta nodes) ",")))))

(provide 'org-bibtex-extras)
;;; org-bibtex-extras ends here
