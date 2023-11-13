;;; org-ref-citation-links.el --- citation links for org-ref -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: convenience

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
;; This library provides hyper-functional citation links. These links can
;; contain common pre/post notes, and multiple citation keys that each have
;; their own pre/postnotes.
;;
;; These links are fontified to indicate if the citation keys are valid, and to
;; indicate the pre/post-note structure. They also have tooltips that show
;; information from the bibtex entry.
;;
;; Each link is functional, and clicking on one will open a hydra menu
;; `org-ref-citation-hydra/body' of actions that range from opening the bibtex
;; entry, notes, pdf or associated URL, to searching the internet for related
;; articles.
;;
;; Each citation link also has a local keymap on it, which provides keyboard
;; shortcuts for some actions like sorting, rearranging and navigating citation
;; links. See `org-ref-cite-keymap' for the key bindings.
;;
;; Each link exports to a corresponding LaTeX citation command, or can be
;; rendered with CSL for other kinds of exports like HTML, markdown, or ODT.
;;
;; This library also provides a minimal set of insertion functions that use
;; completion. You can also use the org link completion mechanism to insert a
;; citation.
;;
;; natmove like preprocessing is provided with `org-ref-cite-natmove'.
;;
;;; Code:

(require 'hydra)
(require 'xref)
(eval-when-compile (require 'subr-x))

(defgroup org-ref-faces nil
  "A group for faces in `org-ref'."
  :group 'org-ref-faces)


(defface org-ref-cite-face
  `((t (:inherit org-link
                 :foreground "forest green")))
  "Color for cite-like links in org-ref."
  :group 'org-ref-faces)


(defface org-ref-bad-cite-key-face
  `((t (:inherit org-ref-cite-face
		 :foreground "red")))
  "Color for bad cite keys in org-ref."
  :group 'org-ref-faces)


(defface org-ref-cite-global-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :weight bold)))
  "Face for global prefix/suffix in a cite link."
  :group 'org-ref-faces)


(defface org-ref-cite-&-face
  `((t (:inherit org-ref-cite-face :weight bold)))
  "Face for the starting & in a cite key."
  :group 'org-ref-faces)


(defface org-ref-cite-local-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :slant italic)))
  "Face for local prefix/suffix in a cite link."
  :group 'org-ref-faces)


(defface org-ref-cite-invalid-local-prefix/suffix-face
  `((t (:inherit org-ref-cite-face :foreground "red")))
  "Face for invalid local prefix/suffix in a cite link.
This is mostly for multicites and natbib."
  :group 'org-ref-faces)


(defcustom org-ref-activate-cite-links t
  "If non-nil use font-lock to activate citations.
In large documents with many citations activation can be slow.
Set this to nil to turn that off, which increase performance."
  :type 'boolean
  :group 'org-ref)


(defcustom org-ref-default-citation-link
  "cite"
  "The default type of citation link to use."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-natbib-types
  '(("cite" "basic citation")
    ("nocite" "add key to bibliography, but do not cite it in the text")
    ("citet" "textual, Jones et al. (1990)")
    ("citet*" "textual, full author list Jones, Baker, and Williams (1990)")
    ("citep" "parenthetical citation (Jones et al. (1990))")
    ("citep*" "parenthetical, full author list, (Jones, Baker, and Williams, 1990)")
    ("citealt" "same as citet, but without parentheses")
    ("citealt*" "same as citet, with full author list but without parentheses")
    ("citealp" "same as citep, but without parentheses")
    ("citealp*" "same as citep, with full author list, but without parentheses")
    ("citenum" "The number of the citation in the bibliography, e.g. 11")
    ("citetext" "text inserted in citation in the document")
    ("citeauthor" "Only the author, Jones et al.")
    ("citeauthor*" "The full author list, Jones, Baker, and Williams")
    ("citeyear" "The year of the citation, 2021")
    ("citeyearpar" "The year in parentheses (2021)")
    ("Citet" "like citet, but with forced capitalization for starting sentences")
    ("Citep" "like citep, but with forced capitalization for starting sentences")
    ("Citealt" "like citet, but with forced capitalization and no parentheses")
    ("Citealp" "like citep, but with forced capitalization and no parentheses")
    ("Citeauthor" "like citeauthor with forced capitalization")
    ("Citet*" "like citet, with full author list and forced capitalization")
    ("Citep*" "like citep, with full author list and forced capitalization")
    ("Citealt*" "like citet, with full author list, forced capitalization and no parentheses")
    ("Citealp*" "like citep, with full author list, forced capitalization and no parentheses")
    ("Citeauthor*" "like citeauthor with forced capitalization"))
  "Natbib commands can have many references, and global prefix/suffix text.
  For natbib cite commands see
  http://tug.ctan.org/macros/latex/contrib/natbib/natnotes.pdf"
  :type '(repeat :tag "List of citation types" (list string string))
  :group 'org-ref)


(defcustom org-ref-biblatex-types
  '(("Cite" "basic citation with capitalization")
    ("parencite" "similar to cite with parentheses")
    ("Parencite" "similar to cite with parentheses and capitalization")
    ("footcite" "Put the citation in a footnote")
    ("footcitetext" "Put the citation in a footnote using \footnotetext")

    ("textcite" "print the authors or editors as a subject of the sentence")
    ("Textcite" "print the authors or editors as a subject of the sentence with capitalization")
    ("smartcite" "like parencite in a footnote, and footcite in the body")
    ("Smartcite" "like parencite in a footnote, and footcite in the body with capitalization")
    ("cite*" "similar to cite, but prints the year or title")
    ("parencite*" "similar to parencite, but prints the year or title")
    ("supercite" "superscripted numeric citation (only in numeric styles)")

    ("autocite" "handles some punctuation nuances")
    ("Autocite" "handles some punctuation nuances with punctuation")
    ("autocite*" "same as autocite but * is passed to the backend")
    ("Autocite*" "same as Autocite but * is passed to the backend")

    ("citetitle" "the shorttitle or title field")
    ("citetitle*" "the full title")

    ("citeyear" "the year field")
    ("citeyear*" "the year field and extradate information if available")

    ("citedate" "the full date or year")
    ("citedate*" "the full date or year, including extradate information if available")

    ("citeurl" "the url field")

    ("fullcite" "create a full citation similar to what is in the bibliography")
    ("footfullcite" "create a full citation as a footnote")
    ;; "volcite" "Volcite" cannot support the syntax
    ("notecite" "print prenote and postnote, but no citation")
    ("Notecite" "print prenote and postnote, but no citation with capitalization")

    ("pnotecite" "similar to notecite with parentheses")
    ("Pnotecite" "similar to Notecite with parentheses")
    ("fnotecite" "similar to notecite in a footnote"))
  "biblatex commands.
  Biblatex commands
  https://mirrors.ibiblio.org/CTAN/macros/latex/contrib/biblatex/doc/biblatex.pdf"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-biblatex-multitypes
  '(("cites" "multicite version of cite")
    ("Cites" "multicite version of Cite")
    ("parencites" "multicite version of parencite")
    ("Parencites" "multicite version of Parencite")
    ("footcites" "multicite version of footcite")
    ("footcitetexts" "multicite version of footcitetext")
    ("smartcites" "multicite version of smartcite")
    ("Smartcites" "multicite version of Smartcite")
    ("textcites" "multicite version of textcite")
    ("Textcites" "multicite version of Textcite")
    ("supercites" "multicite version of supercite")
    ("autocites" "multicite version of autocite")
    ("Autocites" "multicite version of Autocite"))
  "Multicite link types"
  :type '(repeat :tag "List of citation types" string)
  :group 'org-ref)


(defcustom org-ref-cite-types
  (append
   org-ref-natbib-types
   org-ref-biblatex-types
   org-ref-biblatex-multitypes
   ;; for the bibentry package
   '(("bibentry" "Insert the bibtex entry")))
  "List of citation types known in `org-ref'."
  :type '(repeat :tag "List of citation types (type description)" (list string string))
  :group 'org-ref)


(defvar org-ref-insert-cite-function)

(defcustom org-ref-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    ;; Navigation keys
    (define-key map (kbd "C-<left>") 'org-ref-previous-key)
    (define-key map (kbd "C-<right>") 'org-ref-next-key)

    ;; rearrangement keys
    (define-key map (kbd "S-<left>") (lambda () (interactive) (org-ref-swap-citation-link -1)))
    (define-key map (kbd "S-<right>") (lambda () (interactive) (org-ref-swap-citation-link 1)))
    (define-key map (kbd "S-<up>") 'org-ref-sort-citation-link)
    (define-key map (kbd "<tab>") (lambda ()
				    (interactive)
				    (funcall org-ref-insert-cite-function)))

    ;; xref navigation
    (define-key map (kbd "M-.") (lambda ()
				  (interactive)
				  (xref-push-marker-stack)
				  (org-ref-open-citation-at-point)))
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-ref)


(defcustom org-ref-cite-insert-version 3
  "Default version to insert citations with.
  The default is 3. In legacy documents you might prefer 2 though,
  so this variable can be buffer- or directory local if you want.

  version 2 means the links are not bracketed, and comma-separated keys.

  version 3 means the links are bracketed, with semicolon-separated
  &keys."
  :type 'number
  :group 'org-ref)


(defvar org-ref-citation-key-re
  (rx-to-string
   '(seq "&" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~")))))
  "Numbered regular expression for a version 3 cite key.
  Key is in group 1.
  Adapted from the expression in org-cite.")


(defun org-ref-cite-version (path)
  "Return the version for PATH.
  PATH is from a cite link.
  Version 2 is separated by commas and uses plain keys.
  Version 3 is separated by semicolons and uses &keys.
  I think that if there is a & in path, it must be version 3."
  (if (string-match "&" path)
      3
    2))


;; * Parsing/interpreting a citation path

(defun org-ref-parse-cite-path (path)
  "Return a data structure representing the PATH.
  the data structure is a plist with (:version :prefix :suffix :references).
  Each reference is a plist with (:key :prefix :suffix)."
  (pcase (org-ref-cite-version path)
    (2
     ;; This will not have any prefix or suffix, since that was previously done in the desc.
     (list :version 2 :references (cl-loop for key in (split-string path ",") collect
					   (list :key (string-trim key)))))
    (3 (let ((citation-references (split-string path ";"))
	     (these-results '(:version 3)))
	 ;; if the first ref doesn't match a key, it must be a global prefix
	 ;; this pops the reference off.
	 (when (null (string-match org-ref-citation-key-re (cl-first citation-references)))
	   (setq these-results (append these-results (list :prefix (cl-first citation-references)))
		 citation-references (cdr citation-references)))

	 ;; if the last ref doesn't match a key, then it is a global suffix
	 ;; we remove the last one if this is true after getting the suffix.
	 (when (null (string-match org-ref-citation-key-re (car (last citation-references))))
	   (setq these-results (append these-results (list :suffix (car (last citation-references))))
		 citation-references (butlast citation-references)))

	 (setq these-results
	       (append these-results
		       (list
			:references
			(cl-loop for s in citation-references collect
				 (if (null (string-match org-ref-citation-key-re s))
				     (error "No matching key found in %s" s)
				   (let* ((key (match-string-no-properties 1 s))
					  (key-start (match-beginning 0))
					  (key-end (match-end 0))
					  (prefix (let ((p (substring s 0 key-start)))
						    (if (string= "" (string-trim p))
							nil
						      p)))
					  (suffix (let ((s (substring s key-end)))
						    (if (string= "" (string-trim s))
							nil
						      s))))
				     (list :key key :prefix prefix :suffix suffix)))))))))))


(defun org-ref-interpret-cite-data (data)
  "Interpret the DATA structure from `org-ref-parse-cite-path' back
to a path string."
  (pcase (plist-get data :version)
    (2
     (string-join (cl-loop for ref in (plist-get data :references) collect (plist-get ref :key)) ","))
    (3
     (concat
      (when-let (prefix (plist-get data :prefix)) (concat prefix ";"))
      (string-join (cl-loop for ref in (plist-get data :references) collect
			    (concat
			     (plist-get ref :prefix)
			     "&" (plist-get ref :key)
			     (plist-get ref :suffix)))
		   ";")
      (when-let (suffix (plist-get data :suffix)) (concat ";" suffix))))))


;; * Activating citation links
;;
;; We use the activate-func for fontification of pieces of each link.


(declare-function bibtex-completion-candidates "bibtex-completion")
(declare-function bibtex-completion-init "bibtex-completion")
(defvar bibtex-completion-bibliography)
(defvar bibtex-completion-display-formats-internal)

;; (defun org-ref-valid-keys ()
;;   "Return a list of valid bibtex keys for this buffer.
;; This is used a lot in `org-ref-cite-activate' so it needs to be
;; fast, but also up to date."
;;   ;; this seems to be needed, but we don't want to do this every time
;;   (unless bibtex-completion-display-formats-internal
;;     (bibtex-completion-init))

;;   (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
;;     (cl-loop for entry in (bibtex-completion-candidates)
;; 	     collect
;; 	     (cdr (assoc "=key=" (cdr entry))))))


(defun org-ref-valid-keys ()
  "Return a list of valid bibtex keys for this buffer.
This is used a lot in `org-ref-cite-activate' so it needs to be
fast, but also up to date."

  ;; this seems to be needed, but we don't want to do this every time
  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))

  (let* ((files (org-ref-find-bibliography)))
    (if (seq-every-p 'identity
		     (cl-loop for file in files
			      collect (assoc file bibtex-completion-cache)))
	;; We have a cache for each file
	;; bibtex-completion-cache contains (filename md5hash entries)
	(cl-loop for entry in
		 (cl-loop
		  for file in files
		  append (cddr (assoc file bibtex-completion-cache)))
		 collect (cdr (assoc "=key=" (cdr entry))))
      ;; you need to get a cache because one or more of the files was not in the cache.
      (let ((bibtex-completion-bibliography files))
	(cl-loop for entry in (bibtex-completion-candidates)
		 collect
		 (cdr (assoc "=key=" (cdr entry))))))))


(defvar-local org-ref-valid-keys-hashes nil)
(defvar-local org-ref-valid-keys-cache  nil)

(defun org-ref-valid-keys-cached ()
  "Update `org-ref-valid-keys-cache` only when files changed."

  (let ((local-hashes (cons bibtex-completion-bibliography
                            (mapcar 'cadr bibtex-completion-cache))))
    (when (not (equal local-hashes org-ref-valid-keys-hashes))
      (setq-local org-ref-valid-keys-hashes local-hashes)
      (setq-local org-ref-valid-keys-cache (make-hash-table :test 'equal))
      (cl-loop for entry in (org-ref-valid-keys)
               do
               (puthash entry t org-ref-valid-keys-cache))))
  org-ref-valid-keys-cache)


(defun org-ref-cite-activate (start end path _bracketp)
  "Activation function for a cite link.
START and END are the bounds of the link.
PATH has the citations in it."
  (when (and org-ref-activate-cite-links
	     ;; Try avoid fontifying org-cite elements. this is based on the
	     ;; path containing @ which makes it likely to be an org-cite. Maybe
	     ;; a text property is better, in case this is an issue in the
	     ;; future.
	     (not (s-contains-p "@" path)))
    (let* ((valid-keys (org-ref-valid-keys))
	   valid-key
	   substrings)
      (goto-char start)
      (pcase (org-ref-cite-version path)
	(2
	 ;; This makes the brackets visible, but we only need it when there is a
	 ;; description.
	 (when (looking-at "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]")
	   (remove-text-properties start end '(invisible nil)))
	 (setq substrings (split-string path ","))
	 (cl-loop for key in substrings
		  do
		  ;; get to the substring
		  (search-forward key end)
		  (put-text-property (match-beginning 0)
				     (match-end 0)
				     'keymap
				     org-ref-cite-keymap)
		  (put-text-property (match-beginning 0)
				     (match-end 0)
				     'cite-key
				     key)
		  (unless (member (string-trim key) valid-keys)
		    (put-text-property (match-beginning 0)
				       (match-end 0)
				       'face 'org-ref-bad-cite-key-face)
		    (put-text-property (match-beginning 0)
				       (match-end 0)
				       'help-echo "Key not found"))))
	(3
	 (setq substrings (split-string path ";"))
	 (cl-loop for i from 0 for s in substrings
		  do
		  ;; get to the substring
		  (search-forward s end)
		  (put-text-property (match-beginning 0)
				     (match-end 0)
				     'keymap
				     org-ref-cite-keymap)
		  (let* (key-begin
			 key-end
			 key)

		    ;; Look for a key. common pre/post notes do not have keys in them.
		    (save-match-data
		      (when (string-match org-ref-citation-key-re s)
			(setq key (match-string-no-properties 1 s)
			      valid-key (member key valid-keys))))

		    ;; these are global prefix/suffixes
		    (when (and (or (= i 0)
				   (= i (- (length substrings) 1)))
			       (null key))
		      (put-text-property (match-beginning 0) (match-end 0)
					 'face 'org-ref-cite-global-prefix/suffix-face)
		      (put-text-property (match-beginning 0) (match-end 0)
					 'help-echo "Global prefix/suffix"))

		    ;; we have a key. we have to re-search to get its position
		    (when key
		      (save-excursion
			(save-match-data
			  (search-backward (concat "&" key))
			  (setq key-begin (match-beginning 0)
				key-end (match-end 0))))
		      ;; mark the &
		      (put-text-property key-begin (+ 1 key-begin)
					 'face 'org-ref-cite-&-face)
		      ;; store key on the whole thing
		      (put-text-property (match-beginning 0)
					 (match-end 0)
					 'cite-key
					 key)

		      ;; fontify any prefix /suffix text
		      (put-text-property (match-beginning 0) key-begin
					 'face 'org-ref-cite-local-prefix/suffix-face)

		      (put-text-property key-end (match-end 0)
					 'face 'org-ref-cite-local-prefix/suffix-face)

		      ;; bad key activation
		      (unless valid-key
			(put-text-property key-begin key-end
					   'face 'font-lock-warning-face)
			(put-text-property key-begin key-end
					   'help-echo "Key not found"))))))))))


;; * Following citation links

(declare-function org-ref-get-bibtex-key-and-file "org-ref-core")

(defhydra org-ref-citation-hydra (:color blue :hint nil)
  "Citation actions
"
  ("o" org-ref-open-citation-at-point  "Bibtex" :column "Open")
  ("p" org-ref-open-pdf-at-point "PDF" :column "Open")
  ("n" org-ref-open-notes-at-point "Notes" :column "Open")
  ("u" org-ref-open-url-at-point "URL" :column "Open")

  ;; WWW actions
  ("ww" org-ref-wos-at-point "WOS" :column "WWW")
  ("wr" org-ref-wos-related-at-point "WOS related" :column "WWW")
  ("wc" org-ref-wos-citing-at-point "WOS citing" :column "WWW")
  ("wg" org-ref-google-scholar-at-point "Google Scholar" :column "WWW")
  ("wp" org-ref-pubmed-at-point "Pubmed" :column "WWW")
  ("wf" org-ref-crossref-at-point "Crossref" :column "WWW")
  ("wb" org-ref-biblio-at-point "Biblio" :column "WWW")
  ("e" org-ref-email-at-point "Email" :column "WWW")

  ;; Copyish actions
  ("K" (save-window-excursion
	 (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	   (bibtex-completion-show-entry (list (org-ref-get-bibtex-key-under-cursor)))
	   (bibtex-copy-entry-as-kill)
	   (kill-new (pop bibtex-entry-kill-ring))))
   "Copy bibtex" :column "Copy")
  ("a" org-ref-add-pdf-at-point "add pdf to library" :column "Copy")
  ("k" (kill-new (car (org-ref-get-bibtex-key-and-file))) "Copy key" :column "Copy")
  ("f" (kill-new (bibtex-completion-apa-format-reference
		  (org-ref-get-bibtex-key-under-cursor)))
   "Copy formatted" :column "Copy")
  ("h" (kill-new
	(format "* %s\n\n cite:&%s"
		(bibtex-completion-apa-format-reference
		 (org-ref-get-bibtex-key-under-cursor))
		(car (org-ref-get-bibtex-key-and-file))))
   "Copy org heading"
   :column "Copy")

  ;; Editing actions
  ("<left>" org-ref-cite-shift-left "Shift left" :color red :column "Edit")
  ("<right>" org-ref-cite-shift-right "Shift right" :color red :column "Edit")
  ("<up>" org-ref-sort-citation-link "Sort by year" :column "Edit")
  ("i" (funcall org-ref-insert-cite-function) "Insert cite" :column "Edit")
  ("t" org-ref-change-cite-type "Change cite type" :column "Edit")
  ("d" org-ref-delete-citation-at-point "Delete at point" :column "Edit")
  ("r" org-ref-replace-citation-at-point "Replace cite" :column "Edit")
  ("P" org-ref-edit-pre-post-notes "Edit pre/suffix" :column "Edit")

  ;; Navigation
  ("[" org-ref-previous-key "Previous key" :column "Navigation" :color red)
  ("]"  org-ref-next-key "Next key" :column "Navigation" :color red)
  ("v" org-ref-jump-to-visible-key "Visible key" :column "Navigation" :color red)
  ("q" nil "Quit"))


(defun org-ref-cite-follow (_path)
  "Follow a cite link."
  (org-ref-citation-hydra/body))


;; * Citation links tooltips
(defvar bibtex-completion-bibliography)
(defvar bibtex-completion-pdf-symbol)
(defvar bibtex-completion-notes-symbol)
(defvar bibtex-completion-find-note-functions)

(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function bibtex-completion-find-pdf "bibtex-completion")
(declare-function bibtex-completion-apa-format-reference "bibtex-completion")

(defun org-ref-cite-tooltip (_win _obj position)
  "Get a tooltip for the cite at POSITION."
  (let ((key (get-text-property position 'cite-key)))
    (when key
      (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	     (has-pdf (when (bibtex-completion-find-pdf key) bibtex-completion-pdf-symbol))
	     (has-notes (when (cl-some #'identity
				       (mapcar (lambda (fn)
						 (funcall fn key))
					       bibtex-completion-find-note-functions))
			  bibtex-completion-notes-symbol)))
	(format "%s%s %s" (or has-pdf "") (or has-notes "")
		(bibtex-completion-apa-format-reference key))))))


;; * Exporting citation links

(defun org-ref-cite-export (cmd path desc backend)
  "Export a cite link.
This supports the syntax:  \\cmd[optional prefix][optional suffix]{keys}
The prefix and suffix must be the global version. Local prefix/suffixes are ignored.
PATH contains the link path.
BACKEND is the export backend.
Use with apply-partially."
  (pcase backend
    ('latex
     (let* ((cite (org-ref-parse-cite-path path))
	    (references (plist-get cite :references))
	    (keys (cl-loop for ref in references collect
			   (plist-get ref :key))))
       (pcase (org-ref-cite-version path)
	 (2
	  (let* ((prefix-suffix (split-string (or desc "") "::"))
		 (prefix (cond
			  ((and (cl-first prefix-suffix) (not (string= "" (cl-first prefix-suffix))))
			   (format "[%s]" (cl-first prefix-suffix)))
			  ((cl-second prefix-suffix)
			   "[]")
			  (t
			   "")))
		 (suffix (cond
			  ((cl-second prefix-suffix)
			   (format "[%s]" (cl-second prefix-suffix)))
			  (t
			   ""))))
	    (s-format "\\${cmd}${prefix}${suffix}{${keys}}" 'aget
		      `(("cmd" . ,cmd)
			("prefix" . ,(string-trim prefix))
			("suffix" . ,(string-trim suffix))
			("keys" . ,(string-join keys ","))))))
	 (3
	  (s-format "\\${cmd}${prefix}${suffix}{${keys}}" 'aget
		    `(("cmd" . ,cmd)
		      ;; if there is more than one key, we only do global
		      ;; prefix/suffix But for one key, we should allow local
		      ;; prefix and suffix or the global one.
		      ("prefix" . ,(if (= 1 (length references))
				       ;; single reference
				       (cond
					;; global and common prefixes exist, combine them
					((and (plist-get cite :prefix)
					      (plist-get (car references) :prefix))

					 (concat "["
						 (plist-get cite :prefix)
						 ";" ;; add this back as a separator
						 (plist-get (car references) :prefix)
						 "]"))
					;; local prefix is not empty, we use it.
					((plist-get (car references) :prefix)
					 (concat "["
						 (string-trim (plist-get (car references) :prefix))
						 "]"))
					;; local prefix is empty, but global one
					;; is not, so we use it
					((plist-get cite :prefix)
					 (concat "["
						 (string-trim (plist-get cite :prefix))
						 "]"))
					;; if you have a suffix, you need an empty prefix
					((plist-get cite :suffix)
					 "[]")
					(t
					 ""))
				     ;; Multiple references
				     (cond
				      ;; Check the common prefix
				      ((plist-get cite :prefix)
				       (concat "["
					       (string-trim (plist-get cite :prefix))
					       "]"))
				      ;; Check the prefix in the first cite
				      ((plist-get (car references) :prefix)
				       (concat "["
					       (string-trim (plist-get (car references) :prefix))
					       "]"))
				      ;; if you get here, the prefix is empty.
				      ;; if you have a suffix, you need an empty prefix placeholder
				      ((plist-get cite :suffix)
				       "[]")
				      (t
				       ""))))
		      ("suffix" . ,(if (= 1 (length references))
				       ;; Single reference
				       (cond
					;; local suffix is not empty, so use it
					((plist-get (car references) :suffix)
					 (format "[%s]"
						 (string-trim (plist-get (car references) :suffix))))
					;; global suffix is not empty
					((plist-get cite :suffix)
					 (format "[%s]" (string-trim (plist-get cite :suffix))))
					(t
					 ;; If there is a prefix, then this should
					 ;; be an empty bracket, and if not it
					 ;; should am empty string. You need an
					 ;; empty bracket, at least for biblatex
					 ;; commands. With just one set of
					 ;; brackets it is interpreted as a
					 ;; suffix.
					 (if (or (plist-get cite :prefix)
						 (plist-get (car references) :prefix))
					     "[]"
					   "")))
				     ;; Multiple references
				     (cond
				      ;; this is a common suffix
				      ((plist-get cite :suffix)
				       (format "[%s]" (string-trim (plist-get cite :suffix))))
				      ;; last reference has a suffix
				      ((plist-get (car (last references)) :suffix)
				       (format "[%s]" (string-trim (plist-get (car (last references)) :suffix))))
				      (t
				       ;; If there is a prefix, then this should
				       ;; be an empty bracket, and if not it
				       ;; should am empty string. You need an
				       ;; empty bracket, at least for biblatex
				       ;; commands. With just one set of
				       ;; brackets it is interpreted as a
				       ;; suffix.
				       (if (or (plist-get cite :prefix)
					       (plist-get (car references) :prefix))
					   "[]"
					 "")))))
		      ("keys" . ,(string-join keys ","))))))))))


(defun org-ref-multicite-export (cmd path _desc backend)
  "Export a multicite link.
This supports the syntax:  \\cmd(multiprenote)(multipostnote)[prenote][postnote]{key1}...[prenote][postnote]{key}
PATH contains the link path.
BACKEND is the export backend.
Use with apply-partially."
  (pcase backend
    ('latex
     (let ((cite (org-ref-parse-cite-path path)))
       (s-format "\\${cmd}${global-prefix}${global-suffix}${keys}" 'aget
		 `(("cmd" . ,cmd)
		   ("global-prefix" . ,(cond
					((plist-get cite :prefix)
					 (concat "(" (plist-get cite :prefix) ")"))
					;; if you have a suffix, you need an empty prefix
					((plist-get cite :suffix)
					 "()")
					(t
					 "")))
		   ("global-suffix" . ,(if (not (string= "" (or (plist-get cite :suffix) "")))
					   (format "(%s)" (plist-get cite :suffix))
					 ""))
		   ("keys" . ,(string-join
			       (cl-loop for ref in (plist-get cite :references)
					collect
					(format "%s%s{%s}"
						(cond
						 ;; we have a prefix, stick it in
						 ((not (string= ""
								(or (plist-get ref :prefix) "")))
						  (concat "[" (plist-get ref :prefix) "]"))
						 ;; no prefix, but a suffix, so
						 ;; empty prefix for placeholder
						 ((not (string= ""
								(or (plist-get ref :suffix) "")))
						  "[]")
						 (t
						  ""))
						(cond
						 ((not (string= ""
								(or (plist-get ref :suffix) "")))
						  (concat "[" (plist-get ref :suffix) "]"))
						 (t
						  ""))
						(plist-get ref :key)))))))))))

;; * Completion for citation links
;;
;; This allows you to type C-c l, choose a cite link type, and then insert a key.

(defun org-ref-cite-link-complete (cmd &optional _arg)
  "Cite link completion for CMD."
  (concat
   cmd ":"
   "&" (org-ref-read-key)))

;; * Generate all the links
;;
;; We loop on the three categories because there are some differences between
;; them, mostly in the multitypes.

(cl-loop for (cmd _desc) in (append org-ref-natbib-types
				    org-ref-biblatex-types)
	 do
	 (org-link-set-parameters
	  cmd
	  :complete (apply-partially #'org-ref-cite-link-complete cmd)
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-cite-export  cmd)
	  :activate-func #'org-ref-cite-activate))


(cl-loop for (cmd _desc) in org-ref-biblatex-multitypes do
	 (org-link-set-parameters
	  cmd
	  :complete (apply-partially #'org-ref-cite-link-complete cmd)
	  :follow #'org-ref-cite-follow
	  :face 'org-ref-cite-face
	  :help-echo #'org-ref-cite-tooltip
	  :export (apply-partially 'org-ref-multicite-export cmd)
	  :activate-func #'org-ref-cite-activate))


(org-link-set-parameters
 "bibentry"
 :complete (apply-partially #'org-ref-cite-link-complete "bibentry")
 :follow #'org-ref-cite-follow
 :face 'org-ref-cite-face
 :help-echo #'org-ref-cite-tooltip
 :export (apply-partially 'org-ref-cite-export  "bibentry")
 :activate-func #'org-ref-cite-activate)


;; * Cite link utilities

;;;###autoload
(defun org-ref-delete-citation-at-point ()
  "Delete the citation or reference at point."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
	 (cp (point))
         key i)
    ;;   We only want this to work on citation links
    (when (assoc type org-ref-cite-types)
      (setq key (org-ref-get-bibtex-key-under-cursor))
      (if (null key)
	  ;; delete the whole cite
	  (cl--set-buffer-substring begin end "")
	(setq i (seq-position references key (lambda (el key)
					       (string= key (plist-get el :key)))))
	;; delete i'th reference
	(setq references (-remove-at i references))
	(setq data (plist-put data :references references))
	(save-excursion
	  (goto-char begin)
	  (re-search-forward link-string)
	  (replace-match (org-ref-interpret-cite-data data)))
	(goto-char cp)))))


;;;###autoload
(defun org-ref-replace-citation-at-point ()
  "Replace the citation at point."
  (interactive)
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
	 (cp (point))
         key i)
    ;;   We only want this to work on citation links
    (when (assoc type org-ref-cite-types)
      (setq key (org-ref-get-bibtex-key-under-cursor))

      (if (null key)
	  ;; delete the whole cite
	  (cl--set-buffer-substring begin end "")
	(setq i (seq-position references key (lambda (el key) (string= key (plist-get el :key))))) ;; defined in org-ref
	(setf (plist-get (nth i references) :key)  (org-ref-read-key))

	(setq data (plist-put data :references references))
	(save-excursion
	  (goto-char begin)
	  (re-search-forward link-string)
	  (replace-match (org-ref-interpret-cite-data data)))
	(goto-char cp)))))


;;;###autoload
(defun org-ref-edit-pre-post-notes (&optional common)
  "Edit the pre/post notes at point.

if you are not on a key, or with optional prefix
arg COMMON, edit the common prefixes instead."
  (interactive "P")
  ;; find out what the point is on.
  (let* ((key (get-text-property (point) 'cite-key))
	 (cp (point))
	 (cite (org-element-context))
	 (type (org-element-property :type cite))
	 (data (org-ref-parse-cite-path (org-element-property :path cite)))
	 prefix suffix
	 (delta 0))

    (if (or (null key) common)
	(progn
	  (setq prefix (read-string "prenote: " (plist-get data :prefix))
		suffix (read-string "postnote: " (plist-get data :suffix))
		delta (- (length (plist-get data :prefix)) (length prefix)))

	  (plist-put data :prefix (if (string= "" prefix)
				      nil prefix))

	  (plist-put data :suffix (if (string= "" suffix)
				      nil suffix)))

      ;; On a key
      (let ((index (seq-position (plist-get data :references)
				 key
				 (lambda (el1 key-at-point)
				   (string= key-at-point (plist-get el1 :key))))))
	;; Pad with spaces after prefix and before suffix
	(setq prefix (concat
		      (read-string "prenote: "
				   (string-trim
				    (or
				     (plist-get
				      (nth index (plist-get data :references))
				      :prefix)
				     "")))
		      " ")
	      suffix (concat " "
			     (read-string "postnote: "
					  (string-trim
					   (or
					    (plist-get
					     (nth index (plist-get data :references))
					     :suffix)
					    ""))))
	      delta (- (length (plist-get
				(nth index (plist-get data :references))
				:prefix))
		       (length prefix)))
	(plist-put
	 (nth index (plist-get data :references))
	 :prefix (if (string= "" prefix)
		     nil prefix))

	(plist-put
	 (nth index (plist-get data :references))
	 :suffix (if (string= "" suffix)
		     nil suffix))))


    (cl--set-buffer-substring (org-element-property :begin cite) (org-element-property :end cite)
			      (format "[[%s:%s]]" type (org-ref-interpret-cite-data data)))

    ;; This doesn't exactly save the point. I need a fancier calculation for
    ;; that I think that accounts for the change due to the prefix change. e.g.
    ;; you might add or subtract from the prefix.
    (goto-char (- cp delta))))


(declare-function org-element-create "org-element")

;;;###autoload
(defun org-ref-change-cite-type ()
  "Change the cite type of citation link at point."
  (interactive)
  (let* ((type-annotation (lambda (s)
			    (let ((item (assoc s minibuffer-completion-table)))
			      (when item (concat
					  (make-string (- 12 (length s)) ? )
					  "-- "
					  (cl-second item))))))
	 (completion-extra-properties `(:annotation-function ,type-annotation))
	 (new-type (completing-read "Type: " org-ref-cite-types))
	 (cite-link (org-element-context))
	 (cp (point)))
    (cl--set-buffer-substring
     (org-element-property :begin cite-link)
     (org-element-property :end cite-link)
     (org-element-interpret-data
      (org-element-create 'link
			  `(:type ,new-type
				  :path ,(org-element-property :path cite-link)
				  :contents-begin ,(org-element-property :contents-begin cite-link)
				  :contents-end ,(org-element-property :contents-end  cite-link)))))
    (goto-char cp)))


(defun org-ref-get-bibtex-key-under-cursor ()
  "Return key under the cursor in org-mode.
If not on a key, but on a cite, prompt for key."
  (cond
   (org-ref-activate-cite-links
    (if-let ((key (get-text-property (point) 'cite-key)))
	;; Point is on a key, so we get it directly
	key
      ;; point is not on a key, but may still be on a cite link
      (let ((el (org-element-context))
	    (cp (point))
	    data
	    keys)
	(cond
	 ;; on a cite-link type
	 ((and
	   (eq (org-element-type el) 'link)
	   (assoc (org-element-property :type el) org-ref-cite-types))

	  (goto-char (org-element-property :begin el))
	  (setq data (org-ref-parse-cite-path (org-element-property :path el))
		keys (cl-loop for ref in (plist-get data :references)
			      collect (plist-get ref :key)))
	  (cond
	   ((= 1 (length keys))
	    (search-forward (car keys))
	    (goto-char (match-beginning 0)))
	   ;; multiple keys
	   (t
	    (setq key (completing-read "Key: " keys))
	    (search-forward key)
	    (goto-char (match-beginning 0))))
	  (prog1
	      (get-text-property (point) 'cite-key)
	    (goto-char cp)))

	 ;; somewhere else, but looking at a cite-type see issue #908. links in
	 ;; places like keywords are not parsed as links, but they seem to get
	 ;; activated, so we can just get onto the key, and then open it.
	 ((assoc (thing-at-point 'word) org-ref-cite-types)
	  (save-excursion
	    (when (re-search-forward ":" (line-end-position) t)
	      (prog1
		  (get-text-property (point) 'cite-key)
		(goto-char cp)))))))))
   
   ;; org-ref-activate-cite-links is nil so font-lock does not put
   ;; text-properties on keys. We temporarily activate this
   
   (t
    (let ((el (org-element-context))
	  (cp (point))
	  (org-ref-activate-cite-links t) ;; temporary
	  data
	  keys
	  )
      (and
       (eq (org-element-type el) 'link)
       (assoc (org-element-property :type el) org-ref-cite-types))
      (save-excursion
	;; We activate just this one link
	(org-ref-cite-activate
	 (org-element-property :begin el)
	 (org-element-property :end el)
	 (org-element-property :path el)
	 nil))
      ;; Now we have to handle some cases.
      (cond
       ;; on a key, return a key
       ((get-text-property (point) 'cite-key)
	(get-text-property (point) 'cite-key))
       ;; not on a key, but on a cite. this is lazy, but we just search forward
       ;; to the first key
       (t
	(search-forward ":")
	(get-text-property (point) 'cite-key)))))))


;; ** Shift-arrow sorting of keys in a cite link

(defun org-ref-swap-list-elements (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


(defun org-ref-swap-citation-link (direction)
  "Move citation at point in DIRECTION +1 is to the right, -1 to the left."
  (let* ((object (org-element-context))
         (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         ;; (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
         key i)
    ;;   We only want this to work on citation links
    (when (assoc type org-ref-cite-types)
      (setq key (org-ref-get-bibtex-key-under-cursor))
      (setq i (seq-position references key (lambda (el key) (string= key (plist-get el :key))))) ;; defined in org-ref
      (if (> direction 0) ;; shift right
          (org-ref-swap-list-elements i (min (+ i 1) (- (length references) 1)) references)
        (org-ref-swap-list-elements i (max (- i 1) 0) references))
      (setq data (plist-put data :references references))

      ;; and replace the link with the sorted keys
      (save-excursion
	(goto-char begin)
	(re-search-forward link-string)
	(replace-match (org-ref-interpret-cite-data data)))
      ;; now go forward to key so we can move with the key
      (goto-char begin)
      (re-search-forward key)
      (goto-char (match-beginning 0)))))


(defun org-ref-cite-shift-left ()
  "Shift reference at point to the left."
  (interactive)
  (org-ref-swap-citation-link -1))


(defun org-ref-cite-shift-right ()
  "Shift citation at point to the right."
  (interactive)
  (org-ref-swap-citation-link +1))


;;** Sort cite in cite link

;;;###autoload
(defun org-ref-sort-citation-link ()
  "Replace link at point with sorted link by year."
  (interactive)
  (let* ((object (org-element-context))
         ;; (type (org-element-property :type object))
         (begin (org-element-property :begin object))
         ;; (end (org-element-property :end object))
         (link-string (org-element-property :path object))
	 (data (org-ref-parse-cite-path link-string))
	 (references (plist-get data :references))
	 (bibtex-completion-bibliography (org-ref-find-bibliography))
	 current-point)

    (setq references (cl-sort (cl-loop for ref in references collect
				       (append ref (list :year (bibtex-completion-get-value
								"year"
								(bibtex-completion-get-entry
								 (plist-get ref :key))))))
			      (lambda (x y)
				(< (string-to-number (plist-get x :year))
				   (string-to-number (plist-get y :year))))))
    (setq data (plist-put data :references references)
	  current-point (point))
    (goto-char begin)
    (re-search-forward link-string)
    (replace-match (org-ref-interpret-cite-data data))
    (goto-char current-point)))


;;** C-arrow navigation of cite keys
;;
;; These are a little tricky to understand to me. There are two calls because
;; when you are on a cite, and move, the first change is the boundary of the
;; current cite, and the second is the boundary of next cite.

;;;###autoload
(defun org-ref-next-key ()
  "Move cursor to the next cite key when on a cite link.
Otherwise run `right-word'. If the cursor moves off the link,
move to the beginning of the next cite link after this one."
  (interactive)
  (when-let (next (next-single-property-change (point) 'cite-key))
    (goto-char next))
  (unless (get-text-property (point) 'cite-key)
    (when-let (next (next-single-property-change (point) 'cite-key))
      (goto-char next))))


;;;###autoload
(defun org-ref-previous-key ()
  "Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one."
  (interactive)
  (when-let (prev (previous-single-property-change (point) 'cite-key))
    (goto-char prev))
  (unless (get-text-property (point) 'cite-key)
    (when-let (prev (previous-single-property-change (point) 'cite-key))
      (goto-char prev))))

(defvar avy-goto-key)
(defvar avy-style)
(declare-function avy--style-fn "avy")
(declare-function avy-process "avy")
(declare-function avy-with "avy")
(declare-function org-element-parse-buffer "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-map "org-element")
(declare-function bibtex-completion-get-value "bibtex-completion")
(declare-function bibtex-completion-get-entry "bibtex-completion")

;;;###autoload
(defun org-ref-jump-to-visible-key ()
  "Jump to a visible key with avy."
  (interactive)
  (avy-with avy-goto-key
    (avy-process
     (apply #'append
	    (save-excursion
	      (org-element-map (org-element-parse-buffer) 'link
		(lambda (c)
		  (when (assoc (org-element-property :type c) org-ref-cite-types)
		    (goto-char (org-element-property :begin c))
		    (let* ((path (org-element-property :path c))
			   (data (org-ref-parse-cite-path path))
			   (references (plist-get data :references)))
		      (append (list (org-element-property :begin c))
			      (cl-loop for ref in references collect
				       (progn
					 (search-forward (plist-get ref :key))
					 (match-beginning 0)))))))))))
    (avy--style-fn avy-style)))


;; * Insert links
(declare-function bibtex-completion-format-entry "bibtex-completion")

;; The formatting is adapted from ivy-bibtex-transformer. I feel like it is
;; slower than ivy-bibtex though. It is completion agnostic though...
(defun org-ref-read-key ()
  "Read a key with completion."
  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (candidates (mapcar (lambda (entry)
			       (cons (bibtex-completion-format-entry entry (1- (frame-width)))
				     (cdr entry)))
			     (bibtex-completion-candidates)))
	 (choice (completing-read "org-ref BibTeX entries: " candidates)))
    (cdr (assoc "=key=" (assoc choice candidates)))))


(defun org-ref-insert-cite-key (key &optional set-type)
  "Insert KEY at point as a cite link.
With optional prefix SET-TYPE choose the link type (only on initial insert).
Rules:
1. at beginning of key, insert before it.
2. at middle or end of key, insert after it."
  (let* ((object (org-element-context))
	 (type (org-element-property :type object))
	 ;; (cp (point))
	 link-string version data references key-at-point index
	 (type-annotation (lambda (s)
			    (let ((item (assoc s minibuffer-completion-table)))
			      (when item (concat
					  (make-string (- 12 (length s)) ? )
					  "-- "
					  (cl-second item))))))
	 (completion-extra-properties `(:annotation-function ,type-annotation)))

    (cond
     ((derived-mode-p 'latex-mode)
      (insert (bibtex-completion-format-citation-cite (list key))))
     (t
    (cond
     ;; Not on a link, so we just insert a cite
     ((null (assoc type org-ref-cite-types))
      (insert (format "[[%s:%s%s]]"
		      (if set-type
			  (completing-read "cite type: " org-ref-cite-types)
			org-ref-default-citation-link)
		      (pcase org-ref-cite-insert-version
			(2 "")
			(3 "&"))
		      key)))
     ;; On a link with no path
     ;; See https://github.com/jkitchin/org-ref/issues/951#issuecomment-985998821
     ;; This is an unusual corner case most often triggered by a snippet, although
     ;; perhaps there is some scenario like [[cite:]]
     ((string= "" (org-element-property :path object))
      (unless (looking-back ":" 1)
	(goto-char (org-element-property :end object))
	(skip-chars-backward "]"))
      (insert (concat
	       (pcase org-ref-cite-insert-version
		 (2 "")
		 (3 "&"))
	       key)))

     ;; On a link somewhere, and we need to figure out what to do.
     (t
      (setq link-string (org-element-property :path object)
	    version (org-ref-cite-version link-string)
	    data (org-ref-parse-cite-path link-string)
	    references (plist-get data :references)
	    key-at-point (get-text-property (point) 'cite-key))

      ;; There are two scenarios where key-at-point is null
      ;; 1. on the link-type before the :
      ;; 2. at the end of the link
      ;; Either way we just go to the end.
      (when (null key-at-point)
	;; that failed, so move to the last one This seems weird, but when you
	;; insert several marked candidates the point does weird things.
	(goto-char (org-element-property :end object))
	(skip-chars-backward " ")
	(setq key-at-point (plist-get (car (last references)) :key)))


      ;; this is index of selected key
      (setq index (seq-position references key-at-point
				(lambda (el1 key-at-point)
				  (string= key-at-point (plist-get el1 :key)))))

      (setq data (plist-put data :references
			    (-insert-at
			     (+ index (if (and (= 3 version) (looking-at "&"))
					  0
					1))
			     (list :key key) references)))

      (pcase org-ref-cite-insert-version
	(2
	 (cl--set-buffer-substring
	  (org-element-property :begin object)
	  (org-element-property :end object)
	  (concat type ":" (string-join (cl-loop for ref in (plist-get data :references)
						 collect (plist-get ref :key))
					","))))
	(3 (cl--set-buffer-substring
	    (org-element-property :begin object)
	    (org-element-property :end object)
	    (concat "[["
		    type ":"
		    (org-ref-interpret-cite-data data)
		    "]]"
		    (make-string (org-element-property :post-blank object) ? )))))))

    ;; Now get to the end of the key you just put in.
    (setq object (org-element-context))
    (goto-char (org-element-property :end object))
    (skip-chars-backward " ")))))


(defun org-ref-insert-cite-keys (keys &optional set-type)
  "Insert KEYS as citation links.
Optional SET-TYPE to choose the cite type."
  (cl-loop for key in keys
	   do
	   (org-ref-insert-cite-key key set-type)))


;;;###autoload
(defun org-ref-insert-cite-link (&optional set-type)
  "Insert a cite link with completion.
Optional prefix arg SET-TYPE to choose the cite type."
  (interactive "P")
  (org-ref-insert-cite-key (org-ref-read-key) set-type))


;; * natmove like pre-processing
;;
;; I think that citations belong in the sentence where they are used, which
;; means on the left side of punctuation. However, for some citation styles,
;; especially superscripts, it is nicer if they appear on the right hand side of
;; punctuation. achemso in LaTeX provides natmove
;; (https://ctan.org/pkg/natmove?lang=en) for this. It doesn't seem to work for
;; all LaTeX styles though, and in particular only works on the cite command
;; itself. So, Here is a preprocessor function you can use to move all the
;; cites.

(declare-function org-ref-get-cite-links "org-ref-export")

(defun org-ref-cite-natmove (_backend)
  "Move citations to the right side of punctuation.
Intended for use in `org-export-before-parsing-hook'.

Here is an example use:

  (let ((org-export-before-parsing-hook '(org-ref-cite-natmove)))
    (org-open-file (org-latex-export-to-pdf)))"
  (let ((cites (org-ref-get-cite-links))
	punct)
    (cl-loop for cite in (reverse cites) do
	     (goto-char (org-element-property :end cite))
	     (skip-chars-backward " ")
	     (when (string-match-p "[[:punct:]]" (buffer-substring (point) (+ (point) 1)))
	       (setq punct (buffer-substring (point) (+ (point) 1)))
	       ;; delete the punctuation
	       (cl--set-buffer-substring (point) (+ (point) 1) "")
	       ;; and insert it at the beginning of the link.
	       (goto-char (org-element-property :begin cite))
	       ;; delete spaces backward
	       (skip-chars-backward " ")
	       (cl--set-buffer-substring (point) (org-element-property :begin cite) "")
	       (insert punct)))))

;; * Convert version 2 to version 3

(defun org-ref-v2-cites-to-v3 ()
  "Replace version 2 citation syntax with version 3 citation syntax"
  (interactive)
  (cl-loop for cite in (reverse (org-ref-get-cite-links))
	   collect
	   (let ((data (org-ref-parse-cite-path (org-element-property :path cite)))
		 prefix-suffix)
	     (when (org-element-property :contents-begin cite)
	       (setq prefix-suffix (split-string (buffer-substring (org-element-property :contents-begin cite)
								   (org-element-property :contents-end cite))
						 "::"))
	       (plist-put data :prefix (cl-first prefix-suffix))
	       (plist-put data :suffix (cl-second prefix-suffix)))
	     (plist-put data :version  3)
	     (cl--set-buffer-substring (org-element-property :begin cite)
				       (org-element-property :end cite)
				       (format "[[%s:%s]]" (org-element-property :type cite)
					       (org-ref-interpret-cite-data data))))))


(provide 'org-ref-citation-links)

;;; org-ref-citation-links.el ends here
