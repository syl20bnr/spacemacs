;;; org-ref-glossary.el --- glossary support in org-ref  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  John Kitchin

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

;;; Commentary:

;; Provides Some acronym and glossary support for org-mode. Only export to LaTeX
;; is currently supported. The functionality is based on the LaTeX glossaries
;; package. See https://en.wikibooks.org/wiki/LaTeX/Glossary and
;; http://ctan.math.washington.edu/tex-archive/macros/latex/contrib/glossaries/glossaries-user.pdf

;; Put something like this in your org-file.
;; #+latex_header: \usepackage{glossaries}
;; #+latex_header: \makeglossaries

;; Put this where you want the glossaries to appear in your org-file.
;; [[printglossaries:]]

;; This is the preferred way to add glossary entries to an org-file. The table
;; must be named glossary.
;; 
;; #+name: glossary
;; | label | name  | description   |
;; |-------+-------+---------------|
;; | tree  | Tree  | A woody plant |
;; | shrub | Shrub | A woody bush  |
;;
;; This is the preferred way to add acronyms
;; #+name: acronyms
;; | key  | abbreviation | full form                      |
;; |------+--------------+--------------------------------|
;; | mimo |              | multiple-input multiple output |
;; | qos  | QoS          | quality-of-service             |
;; | bb   | BB           | branch and bound               |
;;
;; Then add `org-ref-glossary-before-parsing' and
;; `org-ref-acronyms-before-parsing' to the `org-export-before-parsing-hook'.

;; See `org-ref-acronym-types' and `org-ref-glossary-gls-commands' for the types defined.

;; Use `org-ref-insert-glossary-link' and `org-ref-insert-acronym-link' to
;; insert a link with completion.

;; * Known limitations
;; 
;; This only works well in a single file (i.e. you cannot include the tables
;; from another file). I tried implementing that once, and it was too slow.



;; * DEPRECATED way of adding entries
;; This is still supported but using the tables is preferred.
;; Add new glossary entries to your org-file like this. Enclose strings
;; containing a comma in {}. Multiline entries are supported.

;; #+latex_header_extra: \newglossaryentry{computer}{name=computer,description={A machine, that computes}}
;; #+latex_header_extra: \newglossaryentry{tree}{name=tree,description=a big plant}

;; #+latex_header_extra: \newglossaryentry{naiive}
;; #+latex_header_extra: {
;; #+latex_header_extra:   name=na\"{\i}ve,
;; #+latex_header_extra:   description={is a French loanword (adjective, form of naïf)
;; #+latex_header_extra:                indicating having or showing a lack of experience,
;; #+latex_header_extra:                understanding or sophistication}
;; #+latex_header_extra: }

;; Here is an example acronym definition
;; #+latex_header_extra: \newacronym{lvm}{LVM}{Logical Volume Manager}

(require 'org-element)
(require 'org-ref-utils)
(require 'ox)

;;; Code:
(defgroup org-ref-glossary nil
  "Customization group for org-ref-glossary."
  :tag "Org Ref glossary"
  :group 'org)


(defcustom org-ref-activate-glossary-links t
  "If non-nil activate acronym and glossary links.
Checks in `org-ref-glossary-face-fn' and `org-ref-acronym-face-fn'.
This is not always fast, so we provide a way to disable it."
  :type 'boolean
  :group 'org-ref-glossary)


(defvar org-ref-glsentries '()
  "Variable to hold locations of glsentries load files.")


(defvar-local org-ref-glossary-cache nil
  "Buffer-local variable for glossary entry cache.")


(defvar-local org-ref-acronym-cache nil
  "Buffer-local variable for acronym entry cache.")


(defun or-find-closing-curly-bracket (&optional limit)
  "Find closing bracket for the bracket at point and move point to it.
Go up to LIMIT or `point-max'. This is a parsing function. I
wrote this because using `forward-list' does not always work if
there is an escaped \" for example. This seems pretty robust."
  (unless (looking-at "{") (error "Not at a curley bracket"))

  (let ((level 1))
    (while (and (not (= 0 level))
		(not (eobp))
		(< (point) (or limit (point-max))))
      (forward-char)
      (when (and (looking-at "{")
		 (not (looking-back "\\\\" (- (point) 2))))
	(cl-incf level))
      (when (and (looking-at "}")
		 (not (looking-back "\\\\" (- (point) 2))))
	(cl-decf level)))
    (point)))


;;* Glossary
(defun or-parse-glossary-entry (entry)
  "Parse a LaTeX glossary ENTRY definition to a p-list of key=value.
ENTRY is the label we are looking for.
Typically returns  (:name name :description description)
but there could be other :key value pairs.

This is a source of performance loss, because this is search
based and it is done on each fontification. It is easy to cache
the results, but not easy to invalidate them, e.g. to reflect
changes."
  (if (and org-ref-glossary-cache (gethash entry org-ref-glossary-cache))
      ;; We have the cache, and an entry and use it
      (gethash entry org-ref-glossary-cache)
    ;; We don't have a cache, or an entry in it, so we find it.
    ;; No cache? we make one
    (unless org-ref-glossary-cache
      (setq-local org-ref-glossary-cache (make-hash-table)))

    ;; Now we search to get the data
    (save-excursion
      (goto-char (point-min))
      (let* (end-of-entry
	     data
	     (external (when (re-search-forward
			      "\\loadglsentries\\(\\[.*\\]\\){\\(?1:.*\\)}" nil t)
			 (match-string 1)))
	     (glsentries (and external
			      (or (cdr (assoc external org-ref-glsentries))
				  (progn
				    (cl-pushnew (cons external (s-trim
								(shell-command-to-string
								 (format "kpsewhich tex %s"
									 external))))
						org-ref-glsentries)
				    (cdr (assoc external org-ref-glsentries))))))
	     key value p1 p2)
	(setq data
	      (catch 'data
		;; look inside first for latex-headers
		(goto-char (point-min))
		(when (re-search-forward
		       (format "\\newglossaryentry{%s}" entry) nil t)
		  (re-search-forward "{")
		  (save-excursion
		    (backward-char)
		    (or-find-closing-curly-bracket)
		    (setq end-of-entry (point)))

		  (while (re-search-forward "\\(\\w+?\\)=" end-of-entry t)
		    (setq key (match-string 1))
		    ;; get value
		    (goto-char (+ 1 (match-end 1)))
		    (setq p1 (point))
		    (if (looking-at "{")
			;; value is wrapped in {}
			(progn
			  (or-find-closing-curly-bracket)
			  (setq p2 (point)
				value (buffer-substring (+ 1 p1) p2)))
		      ;; value is up to the next comma
		      (re-search-forward "," end-of-entry 'mv)
		      (setq value (buffer-substring p1 (- (point) 1))))
		    ;; remove #+latex_header_extra:
		    (setq value (replace-regexp-in-string
				 "#\\+latex_header_extra: " "" value))
		    (setq value (replace-regexp-in-string
				 "\n +" " " value))
		    (setq data (append data
				       (list :label entry)
				       (list (intern (format ":%s" key)))
				       (list value))))
		  (throw 'data data))

		;; check for a glossary table
		(let* ((entries (save-excursion
				  (catch 'found
				    (org-element-map
					(org-element-parse-buffer)
					'table
				      (lambda (el)
					(when (string= "glossary" (org-element-property :name el))
					  (goto-char (org-element-property :contents-begin el))
					  (throw 'found
						 (nthcdr 2 (org-babel-read-table)))))))))
		       (result (assoc entry entries)))
		  (when result
		    (throw 'data (list :label entry :name (cl-second result) :description (cl-third result)))))

		;; then external
		(when (and glsentries
			   (file-exists-p glsentries))

		  (with-current-buffer (find-file-noselect glsentries)
		    (goto-char (point-min))
		    (when (re-search-forward
			   (format "\\newglossaryentry{%s}" entry) nil t)
		      (re-search-forward "{")
		      (save-excursion
			(backward-char)
			(or-find-closing-curly-bracket)
			(setq end-of-entry (point)))

		      (while (re-search-forward "\\(\\w+?\\)=" end-of-entry t)
			(setq key (match-string 1))
			;; get value
			(goto-char (+ 1 (match-end 1)))
			(setq p1 (point))
			(if (looking-at "{")
			    ;; value is wrapped in {}
			    (progn
			      (or-find-closing-curly-bracket)
			      (setq p2 (point)
				    value (buffer-substring (+ 1 p1) p2)))
			  ;; value is up to the next comma
			  (re-search-forward "," end-of-entry 'mv)
			  (setq value (buffer-substring p1 (- (point) 1))))
			(setq data (append data
					   (list :label entry)
					   (list (intern (format ":%s" key)))
					   (list value))))
		      (throw 'data data))))))
	(puthash entry data org-ref-glossary-cache)
	data))))


;;;###autoload
(defun org-ref-add-glossary-entry (label name description)
  "Insert a new glossary entry.
LABEL is how you refer to it with links.
NAME is the name of the entry to be defined.
DESCRIPTION is the definition of the entry.
Entry gets added after the last #+latex_header line.

This is not a preferred way to add entries. It is preferred to
manually add them to the glossary table."
  (interactive "sLabel: \nsName: \nsDescription: ") 
  (save-excursion
    (goto-char (point-max))
    ;; get to the last latex_header line
    (re-search-backward "#\\+latex_header" nil t)
    (forward-line)
    (when (not (looking-at "^$"))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1))
    (insert (format "#+latex_header_extra: \\newglossaryentry{%s}{name={%s},description={%s}}\n"
		    label name description))))


(defun org-ref-glossary-face-fn (label)
  "Return a face for a glossary link."
  (if org-ref-activate-glossary-links
      (save-match-data
	(cond
	 ((or-parse-glossary-entry label)
	  'org-ref-glossary-face)
	 (t
	  'font-lock-warning-face)))
    'org-ref-glossary-face))


;;** Glossary links
(defun or-follow-glossary (entry)
  "Goto beginning of the glossary ENTRY."
  (org-mark-ring-push)
  
  (cond 
   ;; Try finding in the table
   ((progn (goto-char (point-min))
	   (and (re-search-forward "#\\+name: glossary" nil t)
		(re-search-forward entry nil t)))
    nil)

   ((progn (goto-char (point-min)) (re-search-forward (format "\\newglossaryentry{%s}" entry) nil t))
    (goto-char (match-beginning 0)))   

   (t
    (message "no entry found for %s" entry))))


(defvar org-ref-glossary-gls-commands
  '(("gls" "The term associated with the label")
    ("glspl" "The plural term")
    ("Gls" "The capitalized term")
    ("Glspl" "The plural capitalized term")
    ("glssymbol" "The symbol defined (only with latex definition)")
    ("Glssymbol" "The capitalized symbol defined (only with latex definition)")
    ("glsdesc" "The description associated with the label")
    ("Glsdesc" "The capitalized description associated with the label"))
  "An alist of (cmd description).")

(dolist (command org-ref-glossary-gls-commands)
  (org-link-set-parameters (cl-first command)
			   :follow #'or-follow-glossary
			   :face 'org-ref-glossary-face-fn
			   :help-echo 'or-glossary-tooltip
			   :export (lambda (path _ format)
				     (cond
				      ((memq format '(latex beamer))
				       (format "\\%s{%s}" (cl-first command) path))
				      (t
				       (format "%s" path))))))


(org-link-set-parameters "glslink"
			 :follow #'or-follow-glossary
			 :face 'org-ref-glossary-face-fn
			 :help-echo 'or-glossary-tooltip
			 :export (lambda (path desc format)
				   (cond
				    ((memq format '(latex beamer))
				     (format "\\glslink{%s}{%s}" path desc))
				    (t
				     (format "%s" path)))))

;;** Tooltips on glossary entries
(defface org-ref-glossary-face
  `((t (:inherit org-link :foreground "Mediumpurple3")))
  "Face for glossary links.")


(defun or-glossary-tooltip (_window _object position)
  "Return tooltip for the glossary entry.
The entry is in WINDOW and OBJECT at POSITION.
Used in fontification."
  (save-excursion
    (goto-char position)
    (let* ((label (org-element-property :path (org-element-context)))
	   (data (or (or-parse-glossary-entry label)
		     (or-parse-acronym-entry label)))
	   (name (or (plist-get data :name)
		     (plist-get data :abbrv)))
	   (description (or (plist-get data :description)
			    (plist-get data :full))))
      (format
       "%s: %s"
       name
       (with-temp-buffer
	 (insert (concat  description "."))
	 (fill-paragraph)
	 (buffer-string))))))


;; **  printglossaries links
;; There is a printglossary command in LaTeX, but I am not supporting it for now.
(org-link-set-parameters "printglossaries"
			 :export (lambda (path _desc format)
				   (cond
				    ((memq format '(latex beamer))
				     "\\printglossaries")
				    (t
				     (format "%s" path)))))



;; ** exporting with a glossary table
(defun org-ref-glossary-before-parsing (_backend)
  "Function to preprocess a glossary table on export.
This assumes a table like

#+name: glossary
| label | name  | description   |
|-------+-------+---------------|
| tree  | Tree  | A woody plant |
| shrub | Shrub | A woody bush  |

is in the org-buffer, and will add the relevant latex_header
items if there is. The table is deleted in a copy of the buffer
before the export, so you can put it where you want. The column
names are arbitrary, but three columns are expected, and the
hline is expected.

This is intended to be run in `org-export-before-parsing-hook'."
  (save-restriction
    (widen)
    (let* (begin
	   end
	   (entries (save-excursion
		      (catch 'found
			(org-element-map
			    (org-element-parse-buffer)
			    'table
			  (lambda (el)
			    (when (and (org-element-property :name el)
				       (stringp (org-element-property :name el))
				       (string= "glossary" (org-element-property :name el)))
			      (setq begin (org-element-property :begin el)
				    end (org-element-property :end el))
			      (goto-char (org-element-property :contents-begin el))
			      (throw 'found
				     (nthcdr 2 (org-babel-read-table))))))))))
      ;; Delete the table
      (when entries
	(cl--set-buffer-substring begin end ""))

      (goto-char (point-min))
      (cl-loop for (label name description) in entries
	       do
	       (insert (format "#+latex_header_extra: \\newglossaryentry{%s}{name=%s,description={{%s}}}\n"
			       label name description))))))


;;* Acronyms

;;;###autoload
(defun org-ref-add-acronym-entry (label abbrv full)
  "Add an acronym entry with LABEL.
  ABBRV is the abbreviated form.
  FULL is the expanded acronym.

This is not the preferred way to add acronyms, you should add
them manually to the acronyms table."
  (interactive "sLabel: \nsAcronym: \nsFull name: ")
  (save-excursion
    (re-search-backward "#\\+latex_header" nil t)
    (forward-line)
    (when (not (looking-at "^$"))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1))
    (insert (format "#+latex_header_extra: \\newacronym{%s}{%s}{%s}\n"
		    label abbrv full))))


(defun or-parse-acronym-entry (label)
  "Parse an acronym entry LABEL to a plist.
Returns (:abbrv abbrv :full full :label label)
The plist maps to \newacronym{<label>}{<abbrv>}{<full>}"
  (if (and org-ref-acronym-cache (gethash label org-ref-acronym-cache))
      ;; We have the cache, and an entry and use it
      (gethash label org-ref-acronym-cache)
    ;; We don't have a cache, or an label in it, so we find it.
    ;; No cache? we make one
    (unless org-ref-acronym-cache
      (setq-local org-ref-acronym-cache (make-hash-table)))

    ;; Now search for the data
    (save-excursion
      (goto-char (point-min))
      (let* (abbrv
	     full p1
	     (external (when (re-search-forward "\\loadglsentries\\(\\[.*\\]\\){\\(?1:.*\\)}" nil t)
			 (match-string 1)))
	     (glsentries (and external
			      (or (cdr (assoc external org-ref-glsentries))
				  (progn
				    (cl-pushnew (cons external
						      (s-trim (shell-command-to-string
							       (format "kpsewhich tex %s" external))))
						org-ref-glsentries)
				    (cdr (assoc external org-ref-glsentries))))))
	     data)
	(setq data
	      (catch 'data
		(goto-char (point-min))
		;; check in the definitions of newacronym
		(when (re-search-forward (format "\\newacronym{%s}" label) nil t)
		  (setq p1 (+ 1 (point)))
		  (forward-list)
		  (setq abbrv (buffer-substring p1 (- (point) 1)))
		  (setq p1 (+ 1 (point)))
		  (forward-list)
		  (setq full (buffer-substring p1 (- (point) 1)))
		  (throw 'data
			 (list :label label :abbrv abbrv :full full)))

		;; look for acronyms table
		(let* ((entries (save-excursion
				  (catch 'found
				    (org-element-map
					(org-element-parse-buffer)
					'table
				      (lambda (el)
					(when (string= "acronyms" (org-element-property :name el))
					  (goto-char (org-element-property :contents-begin el))
					  (throw 'found
						 (nthcdr 2 (org-babel-read-table)))))))))
		       (result (assoc label entries)))
		  (when result
		    (throw 'data (list :label label
				       :abbrv (cl-second result) :full (cl-third result)))))

		;; look external
		(when (and glsentries
			   (file-exists-p glsentries))
		  (with-current-buffer (find-file-noselect glsentries)
		    (goto-char (point-min))
		    (when (re-search-forward (format "\\newacronym{%s}" label) nil t)
		      (setq p1 (+ 1 (point)))
		      (forward-list)
		      (setq abbrv (buffer-substring p1 (- (point) 1)))
		      (setq p1 (+ 1 (point)))
		      (forward-list)
		      (setq full (buffer-substring p1 (- (point) 1)))
		      (throw 'data
			     (list :label label :abbrv abbrv :full full )))))))
	(puthash label data org-ref-acronym-cache)
	data))))


(defun org-ref-glossary-invalidate-caches ()
  "Function to invalidate the caches."
  (interactive)
  (setq-local org-ref-acronym-cache (make-hash-table))
  (setq-local org-ref-glossary-cache (make-hash-table)))

;;** Acronym links
(defun or-follow-acronym (label)
  "Go to the definition of the acronym LABEL."
  (org-mark-ring-push)
  (cond
   ;; table first
   ((progn (goto-char (point-min))
	   (and (re-search-forward "#\\+name: acronyms" nil t)
		(re-search-forward label nil t)))
    nil)
   
   ((progn (goto-char (point-min)) (re-search-forward (format "\\newacronym{%s}" label) nil t))
    (goto-char (match-beginning 0)))

   (t
    (message "no entry found for %s" label))))


(defvar org-ref-acronym-types
  '(("acrshort" "The acronym for label")
    ("acrshortpl" "The acronym for label in plural")
    ("Acrshort" "Capitalized acronym")
    ("Acrshortpl" "Plural capitalized acronym")
    ("ACRshort" "ALL-CAPS acronym")
    ("ACRshortpl" "ALL-CAPS plural acronym")
    
    ("acrlong" "The label definition")
    ("acrlongpl" "The plural definition")
    ("Acrlong" "Capitalized definition")
    ("Acrlongpl" "Plural capitalized definition")
    ("ACRlong" "ALL-CAPS definition")
    ("ACRlongpl" "plural ALL-CAPS definition")
    
    ("acrfull" "Both the acronym and its definition")
    ("acrfullpl" "Both the acronym and its definition in plural")
    ("Acrfull" "Capitalized both the acronym and its definition")
    ("Acrfullpl" "Capitalized both the acronym and its definition in plural")
    ("ACRfull" "Both the acronym and its definition in ALL-CAPS")
    ("ACRfullpl" "Both the acronym and its definition in plural ALL-CAPS"))
  "list of acronym types (type description)")


(cl-dolist (mapping org-ref-acronym-types)
  (org-link-set-parameters (cl-first mapping)
			   :follow #'or-follow-acronym
			   :face 'org-ref-acronym-face-fn
			   :help-echo 'or-acronym-tooltip
			   :export (lambda (path _ format)
				     (cond
				      ((memq format '(latex beamer))
				       (format "\\%s{%s}" (cl-first mapping) path))
				      (t
				       (format "%s" (upcase path)))))))


;;** Tooltips on acronyms
(defface org-ref-acronym-face
  `((t (:inherit org-link :foreground "Darkorange2")))
  "Face for acronym links.")


(defun org-ref-acronym-face-fn (label)
  "Return a face for an acronym link."
  (if org-ref-activate-glossary-links
      (save-match-data
	(cond
	 ((or-parse-acronym-entry label)
	  'org-ref-acronym-face)
	 (t
	  'font-lock-warning-face)))
    'org-ref-acronym-face))


(defun or-acronym-tooltip (_window _object position)
  "Return tooltip for the acronym entry.
The entry is in WINDOW and OBJECT at POSITION.
Used in fontification.
WINDOW and OBJECT are ignored."
  (save-excursion
    (goto-char position)
    (let* ((label (org-element-property :path (org-element-context)))
	   (acronym-data (or-parse-acronym-entry label))
	   (abbrv (plist-get acronym-data :abbrv))
	   (full (plist-get acronym-data :full)))
      (if acronym-data
	  (format
	   "%s: %s"
	   abbrv full)
	(format "%s is not defined in this file." label)))))


;; ** Exporting with an acronym table
(defun org-ref-acronyms-before-parsing (_backend)
  "Function to preprocess a glossary table on export.
This assumes a table like

#+name: acronyms
| Key  | Short | Long                           |
|------+-------+--------------------------------|
| mimo |       | multiple-input multiple output |
| qos  | QoS   | quality-of-service             |
| bb   | BB    | branch and bound               |

is in the org-buffer, and will add the relevant latex_header items if there is. The table is deleted in a copy of the buffer before the export.

This will run in `org-export-before-parsing-hook'."
  (save-restriction
    (widen)
    (let* (begin
	   end
	   (entries (save-excursion
		      (catch 'found
			(org-element-map
			    (org-element-parse-buffer)
			    'table
			  (lambda (el)
			    (when (and (org-element-property :name el)
				       (stringp (org-element-property :name el))
				       (string= "acronyms" (org-element-property :name el)))
			      (setq begin (org-element-property :begin el)
				    end (org-element-property :end el))
			      (goto-char (org-element-property :contents-begin el))
			      (throw 'found
				     (nthcdr 2 (org-babel-read-table))))))))))
      (when entries
	;; Delete the table
	(cl--set-buffer-substring begin end ""))

      (goto-char (point-min))
      (cl-loop for (label name description) in entries
	       do
	       (insert (format "#+latex_header_extra: \\newacronym{%s}{%s}{%s}\n"
			       label name description))))))


;; * Interactive command to insert acroynm/glossary links

(defun org-ref-insert-glossary-link ()
  "Insert glossary entry as links."
  (interactive)
  ;; gather entries
  (let* ((glossary-candidates '()) 
	 key entry type
	 type-annotation
	 completion-extra-properties
	 choice)

    ;; glossary terms
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "\\\\newglossaryentry{\\([[:ascii:]]+?\\)}" nil t)

	(setq key (match-string 1)
	      entry (or-parse-glossary-entry key))

	(cl-pushnew (cons
		     (propertize (format "%s: %s - glossary."
					 (plist-get entry :name)
					 (plist-get entry :description))
				 'face 'org-ref-glossary-face)
		     entry)
		    glossary-candidates)))

    ;; get entries from the table
    (let ((entries (save-excursion
		     (catch 'found
		       (org-element-map
			   (org-element-parse-buffer)
			   'table
			 (lambda (el)
			   (when (and (org-element-property :name el)
				      (stringp (org-element-property :name el))
				      (string= "glossary"
					       (org-element-property :name el)))
			     (goto-char (org-element-property :contents-begin el))
			     (throw 'found
				    ;; skip header and hline
				    (nthcdr 2 (org-babel-read-table))))))))))
      (cl-loop for (label name description) in entries do
	       (cl-pushnew (cons
			    (propertize (format "%s: %s - glossary."
						label
						description)
					'face 'org-ref-glossary-face)
			    (list :label label :name name :description description))
			   glossary-candidates)))


    (setq choice (completing-read "Choose: " glossary-candidates)
	  entry (cdr (assoc choice glossary-candidates))
	  type-annotation (lambda (s)
			    (let ((item (assoc s minibuffer-completion-table)))
			      (when item (concat
					  (make-string (- 12 (length s)) ? )
					  "-- "
					  (cl-second item)))))
	  completion-extra-properties `(:annotation-function ,type-annotation)
	  type (completing-read "Type: "
				org-ref-glossary-gls-commands
				nil t))

    (insert (format
	     "[[%s:%s][%s]]"
	     type
	     (plist-get entry :label)
	     (pcase type
	       ("gls" (plist-get entry :name))
	       ("glspl" (concat (plist-get entry :name) "s"))
	       ("Gls" (concat (capitalize (substring (plist-get entry :name) 0 1))
			      (substring (plist-get entry :name) 1)))
	       ("Glspl" (concat (capitalize (substring (plist-get entry :name) 0 1))
				(substring (plist-get entry :name) 1)
				"s"))
	       ("glssymbol" (plist-get entry :name))
	       ("Glssymbol" (concat (capitalize (substring (plist-get entry :name) 0 1))
				    (substring (plist-get entry :name) 1)))
	       ("glsdesc" (plist-get entry :description))
	       ("Glsdesc" (concat (capitalize (substring (plist-get entry :description) 0 1))
				  (substring (plist-get entry :description) 1)))
	       ;; fall-through for everything else
	       (_ (plist-get entry :name)))))))


(defun org-ref-insert-acronym-link ()
  "Insert acronym entry as links."
  (interactive)
  ;; gather entries
  (let* ((acronym-candidates '())
	 key entry
	 type
	 type-annotation
	 completion-extra-properties
	 choice
	 (cap1 (lambda (s)
		 "capitalize first letter only"
		 (concat
		  (capitalize (substring s 0 1))
		  (substring s 1)))))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "\\\\newacronym{\\([[:ascii:]]+?\\)}" nil t)
	(setq key (match-string 1)
	      entry (or-parse-acronym-entry key))
	(cl-pushnew (cons (propertize
			   (format "%s (%s) - acronym."
				   (plist-get entry :full)
				   (plist-get entry :abbrv))
			   'face 'org-ref-acronym-face)
			  entry)
		    acronym-candidates)))

    (let ((entries (save-excursion
		     (catch 'found
		       (org-element-map
			   (org-element-parse-buffer)
			   'table
			 (lambda (el)
			   (when (and (org-element-property :name el)
				      (stringp (org-element-property :name el))
				      (string= "acronyms"
					       (org-element-property :name el)))
			     (goto-char (org-element-property :contents-begin el))
			     (throw 'found
				    ;; skip header and hline
				    (nthcdr 2 (org-babel-read-table))))))))))
      (cl-loop for (key abbrv full) in entries do
	       (cl-pushnew (cons (propertize
				  (format "%s (%s) - acronym."
					  full
					  abbrv)
				  'face 'org-ref-acronym-face)
				 (list :label key :abbrv abbrv :full full))
			   acronym-candidates)))

    (setq choice (completing-read "Choose: " acronym-candidates)
	  entry (cdr (assoc choice acronym-candidates))
	  type-annotation (lambda (s)
			    (let ((item (assoc s minibuffer-completion-table)))
			      (when item (concat
					  (make-string (- 12 (length s)) ? )
					  "-- "
					  (cl-second item)))))
	  
	  completion-extra-properties `(:annotation-function ,type-annotation)
	  type (completing-read "Type: "
				org-ref-acronym-types
				nil t))

    (insert (format
	     "[[%s:%s][%s]]"
	     type
	     (plist-get entry :label)
	     (pcase type
	       ("acrshort" (plist-get entry :abbrv))
	       ;; this is a cheap way to pluralize. I don't know what the rules
	       ;; are in LaTeX. this is only for display, so it isn't critical.
	       ("acrshortpl" (concat (plist-get entry :abbrv) "s"))
	       
	       ("Acrshort" (funcall cap1 (plist-get entry :abbrv)))
	       ("Acrshortpl" (concat (capitalize (plist-get entry :abbrv)) "s"))

	       ("ACRshort" (upcase (plist-get entry :abbrv)))
	       ("ACRshortpl" (concat (upcase (plist-get entry :abbrv)) "s"))

	       ("acrlong" (plist-get entry :full))
	       ("acrlongpl" (concat (plist-get entry :full) "s"))
	       
	       ("Acrlong" (funcall cap1 (plist-get entry :full)))
	       ("Acrlongpl" (concat (funcall cap1 (plist-get entry :full)) "s"))
	       
	       ("acrfull" (format "%s (%s)"
				  (plist-get entry :full)
				  (plist-get entry :abbrv)))
	       ("acrfullpl" (format "%ss (%ss)"
				    (plist-get entry :full)
				    (plist-get entry :abbrv)))
	       
	       ("Acrfull" (funcall cap1 (format "%s (%s)"
						(plist-get entry :full)
						(plist-get entry :abbrv))))
	       ("Acrfullpl" (funcall cap1 (format "%ss (%ss)"
						  (plist-get entry :full)
						  (plist-get entry :abbrv))))
	       ("ACRfull" (upcase (format "%s (%s)"
					  (plist-get entry :full)
					  (plist-get entry :abbrv))))
	       ("ACRfullpl" (upcase (format "%ss (%ss)"
					    (plist-get entry :full)
					    (plist-get entry :abbrv))))
	       ;; fall-through in case anything is added later.
	       (_ (plist-get entry :abbrv)))))))


;; * acrossproc - acronym and glossary preprocessor
;;
;; Replace printglossary with the acronym and glossary entries
;;
;; Replace the links with links
;;
;; TODO: support only one or the other? via printglossary:acronyms for example?

(defun org-ref-acrossproc (_backend)
  "Preprocessing function for acronyms and glossary entries.
Meant for non-LaTeX exports."
  (save-excursion
    (let* ((glossary)
	   (acronyms)
	   (printglossary-link)
	   (links))

      (setq glossary (org-element-map
			 (org-element-parse-buffer)
			 'table 
		       (lambda (el)
			 (when (and (org-element-property :name el)
				    (stringp (org-element-property :name el))
				    (string= "glossary" (org-element-property :name el))) 
			   (goto-char (org-element-property :contents-begin el))
			   (prog1
			       ;; skip header and hline
			       (nthcdr 2 (org-babel-read-table))
			     ;; delete the table
			     (cl--set-buffer-substring (org-element-property :begin el)
						       (org-element-property :end el)
						       ""))))
		       nil t))
      (setq acronyms (org-element-map
			 (org-element-parse-buffer)
			 'table 
		       (lambda (el)
			 (when (and (org-element-property :name el)
				    (stringp (org-element-property :name el))
				    (string= "acronyms" (org-element-property :name el))) 
			   (goto-char (org-element-property :contents-begin el))
			   (prog1
			       (nthcdr 2 (org-babel-read-table))
			     ;; delete the table
			     (cl--set-buffer-substring (org-element-property :begin el)
						       (org-element-property :end el)
						       ""))))
		       nil t))
      
      ;; Replace printglossary link
      ;; For each entry, use - name :: description <<label>>
      (setq printglossary-link (org-element-map
				   ;; reparse in case the replacement above changed places
				   (org-element-parse-buffer)
				   'link
				 (lambda (lnk)
				   (when (string= "printglossaries"
						  (org-element-property :type lnk))
				     lnk))
				 nil t))
      (when printglossary-link
	(cl--set-buffer-substring (org-element-property :begin printglossary-link)
				  (org-element-property :end printglossary-link)
				  
				  (concat "*Glossary*\n"
					  (string-join
					   (cl-loop for (label name description) in glossary collect
						    (format "<<%s>>\n- %s :: %s" label name description))
					   "\n")

					  "\n*Acronyms*\n"
					  (string-join
					   (cl-loop for (label name description) in acronyms collect
						    (format "<<%s>>\n- %s :: %s " label name description))
					   "\n"))))


      ;; Replace links
      (setq links (org-element-map
		      ;; reparse in case the replacement above changed places
		      (org-element-parse-buffer)
		      'link
		    (lambda (lnk)
		      (when (assoc (org-element-property :type lnk)
				   (append org-ref-glossary-gls-commands
					   org-ref-acronym-types))
			lnk))))
      ;; For each link, replace with [[label][link description]]
      (cl-loop for lnk in (reverse links) do
	       (cl--set-buffer-substring (org-element-property :begin lnk)
					 (org-element-property :end lnk)
					 (format "[[%s][%s]]%s"
						 (org-element-property :path lnk)
						 (buffer-substring (org-element-property :contents-begin lnk)
								   (org-element-property :contents-end lnk))
						 (make-string (org-element-property :post-blank lnk) ? )))))))


(provide 'org-ref-glossary)
;;; org-ref-glossary.el ends here
