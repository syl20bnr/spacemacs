;;; org-ref-ref-links.el --- cross-reference links for org-ref -*- lexical-binding: t; -*-
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

;;; Code:
(eval-and-compile (require 'org-macs))
(eval-and-compile (require 'ol))
(require 'hydra)

(defcustom org-ref-default-ref-type "ref"
  "Default ref link type to use when inserting ref links."
  :type 'string
  :group 'org-ref)


(defcustom org-ref-activate-ref-links t
  "If non-nil use font lock to activate ref links.
Activation can be slow in large documents with a lot of ref
links. Set this to nil to turn off activation."
  :type 'boolean
  :group 'org-ref)


(defface org-ref-ref-face
  `((t (:inherit org-link :foreground "dark red")))
  "Face for ref links in org-ref."
  :group 'org-ref-faces)


(defvar org-ref-label-re
  (rx-to-string
   '(group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
  "Regexp for labels.")


(defvar org-ref-label-link-re
  (rx-to-string
   `(seq "label:" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~")))))
  "Regexp for label links.")


(defvar org-ref-ref-label-regexps
  (list
   (concat ":ID:\\s-+" org-ref-label-re "\\_>")
   ;; CUSTOM_ID in a heading
   (concat ":CUSTOM_ID:\\s-+" org-ref-label-re "\\_>")
   ;; #+name
   (concat "^\\s-*#\\+name:\\s-+" org-ref-label-re "\\_>")
   ;; labels in latex
   (concat "\\\\label{" org-ref-label-re "}")
   ;; A target, code copied from org-target-regexp and group 1 numbered.
   (let ((border "[^<>\n\r \t]"))
     (format "<<\\(?1:%s\\|%s[^<>\n\r]*%s\\)>>"
	     border border border))
   ;; A label link
   (concat "label:" org-ref-label-re "\\_>")
   "\\\\lstset{.*label=\\(?1:.*?\\),.*}")
  "List of regular expressions to labels.
The label should always be in group 1.")


(defvar org-ref-ref-types
  '(("ref" "A regular cross-reference to a label")
    ("eqref" "A cross-reference to an equation")
    ("pageref" "to the page number a label is on")
    ("nameref" "to the name associated with a label (e.g. a caption)")
    ("autoref" "from hyperref, adds automatic prefixes")
    ("Autoref" "from hyperref, capitalized version of autoref")
    ("cref" "from cleveref, adds automatic prefixes, and condenses multiple refs")
    ("Cref" "from cleveref, capitalized version of cref")
    ("crefrange" "from cleveref, makes a range of refs from two refs with a prefix")
    ("Crefrange" "from cleveref, capitalized prefix version of crefrange"))
  "List of ref link types (type description).")


(defun org-ref-select-ref-type ()
  "Select a ref type with annotated completion."
  (let* ((type-annotation (lambda (s)
			    (let ((item (assoc s minibuffer-completion-table)))
			      (when item (concat
					  (make-string (- 12 (length s)) ? )
					  "-- "
					  (cl-second item))))))
	 (completion-extra-properties `(:annotation-function ,type-annotation)))
    (completing-read "Type: " org-ref-ref-types)))


(defun org-ref-change-ref-type (new-type)
  "Change the ref type to NEW-TYPE."
  (interactive (list (org-ref-select-ref-type)))
  (let* ((cite-link (org-element-context))
	 (old-type (org-element-property :type cite-link))
	 (begin (org-element-property :begin cite-link))
	 (end (org-element-property :end cite-link))
	 (bracketp (eq 'bracket (org-element-property :format cite-link)))
	 (path (org-element-property :path cite-link))
	 (deltap (- (point) begin)))
    ;; note this does not respect brackets
    (cl--set-buffer-substring begin end
			      (concat
			       (if bracketp "[[" "")
			       new-type ":" path
			       (if bracketp "]]" "")))
    ;; try to preserve the character the point is on.
    (goto-char (+ begin deltap (- (length new-type) (length old-type))))))


(defvar-local org-ref-label-cache nil
  "Buffer-local cache variable for labels.")


(defvar-local org-ref-buffer-chars-modified-tick nil
  "Buffer-local variable to hold `buffer-chars-modified-tick'.")


(defun org-ref-get-labels ()
  "Return a list of referenceable labels in the document.
You can reference:
A NAME keyword
A CUSTOM_ID property on a heading
A LaTeX label
A target.
A label link
A setting in lstset

See `org-ref-ref-label-regexps' for the patterns that find these.

Returns a list of cons cells (label . context).

It is important for this function to be fast, since we use it in
font-lock."
  (if (or
       ;; if we have not checked we have to check
       (null org-ref-buffer-chars-modified-tick)
       ;; Now check if buffer has changed since last time we looked. We check
       ;; this with the buffer-chars-modified-tick which keeps track of changes.
       ;; If this hasn't changed, no chars have been modified.
       (not (= (buffer-chars-modified-tick)
	       org-ref-buffer-chars-modified-tick)))
      ;; We need to search for all the labels either because we don't have them,
      ;; or the buffer has changed since we looked last time.
      (let ((case-fold-search t)
	    (rx (string-join org-ref-ref-label-regexps "\\|"))
	    (labels '())
	    oe ;; org-element
	    context
	    data)
	(save-excursion
	  (org-with-wide-buffer
	   (goto-char (point-min))
	   (while (re-search-forward rx nil t)
	     (save-match-data
	       ;; Here we try to get some relevant context for different things you
	       ;; might reference.
	       (setq oe (org-element-context)
		     context (string-trim
			      (pcase (car oe)
				('latex-environment (buffer-substring
						     (org-element-property :begin oe)
						     (org-element-property :end oe)))
				;; figure
				('paragraph (buffer-substring
					     (org-element-property :begin oe)
					     (org-element-property :end oe)))
				('table (buffer-substring
					 (org-element-property :begin oe)
					 (org-element-property :end oe)))
				;; Headings fall here.
				(_ (buffer-substring (line-beginning-position)
						     (line-end-position)))))))
	     (cl-pushnew (cons (match-string-no-properties 1) context)
			 labels))))
	
	;; reverse so they are in the order we find them.
	(setq
	 org-ref-buffer-chars-modified-tick (buffer-chars-modified-tick)
	 org-ref-label-cache (delete-dups (reverse labels))))

    ;; retrieve the cached data
    org-ref-label-cache))


(defun org-ref-ref-jump-to (&optional path)
  "Jump to the target for the ref link at point."
  (interactive)
  (let ((case-fold-search t)
	(label (get-text-property (point) 'org-ref-ref-label))
	(labels (split-string path ","))
	(rx (string-join org-ref-ref-label-regexps "\\|")))
    (when (null label)
      (pcase (length labels)
	(1
	 (setq label (cl-first labels)))
	(_
	 (setq label (completing-read "Label: " labels)))))
    (when label
      (org-mark-ring-push)
      (widen)
      (goto-char (point-min))
      (catch 'found
	(while (re-search-forward rx)
	  (when (string= label (match-string-no-properties 1))
	    (save-match-data (org-mark-ring-push))
	    (goto-char (match-beginning 1))
	    (org-show-entry)
	    (substitute-command-keys
	     "Go back with (org-mark-ring-goto) \`\\[org-mark-ring-goto]'.")
	    (throw 'found t)))))))


(defun org-ref-ref-help-echo (_win _obj position)
  "Tooltip for context on a ref label.
POSITION is the point under the mouse I think."
  (cdr (assoc (get-text-property position 'org-ref-ref-label) (org-ref-get-labels))))


(defun org-ref-ref-activate (start _end path _bracketp)
  "Activate a ref link.
The PATH should be a comma-separated list of labels.
Argument START is the start of the link.
Argument END is the end of the link."
  (when org-ref-activate-ref-links
    (let ((labels (mapcar 'car (org-ref-get-labels))))
      (goto-char start)
      (cl-loop for label in (split-string path ",") do
	       (search-forward label)
	       ;; store property so we can follow it later.
	       (put-text-property (match-beginning 0)
				  (match-end 0)
				  'org-ref-ref-label
				  label)

	       (unless (member label labels)
		 
		 (put-text-property (match-beginning 0)
				    (match-end 0)
				    'face
				    'font-lock-warning-face)
		 (put-text-property (match-beginning 0)
				    (match-end 0)
				    'help-echo
				    "Label not found"))))))


(defun org-ref-ref-export (cmd keyword _desc backend)
  "An export function for ref links.
Argument CMD is the LaTeX command to export to.
Argument KEYWORD is the path of the ref link.
Argument BACKEND is the export backend.
This is meant to be used with `apply-partially' in the link definitions."
  (cond
   ((eq backend 'latex)
    (format "\\%s{%s}" cmd keyword))))


(defun org-ref-complete-link (refstyle &optional _arg)
  "Complete a ref link to an existing label."
  (concat refstyle ":" (completing-read "Label: " (org-ref-get-labels))))


(defun org-ref-store-ref-link (&optional reftype)
  "Store a ref link to a label.  The output will be a ref to that label."
  ;; First we have to make sure we are on a label link.
  (unless reftype (setq reftype "ref"))
  (let* ((object (and (eq major-mode 'org-mode) (org-element-context)))
	 (label (cond
		 ;; here literally on a label link.
		 ((and
		   (equal (org-element-type object) 'link)
		   (equal (org-element-property :type object) "label"))
		  (org-element-property :path object))

		 ;; here on a file link. if it has a caption with a label in it, we store
		 ;; it.
		 ((and
		   (equal (org-element-type object) 'link)
		   (equal (org-element-property :type object) "file")
		   (org-file-image-p (org-element-property :path object)))

		  (if (org-element-property :name object)
		      (org-element-property :name object)
		    ;; maybe we have a caption to get it from.
		    (let* ((parent (org-element-property :parent object)))
		      (when (and parent
				 (equal (org-element-type parent) 'paragraph))
			(if (org-element-property :name parent)
			    ;; caption paragraph may have a name which we use if it is there
			    (org-element-property :name parent)
			  ;; else search caption
			  (let ((caption (s-join
					  ""
					  (mapcar 'org-no-properties
						  (org-export-get-caption parent))))) 
			    (when (string-match org-ref-label-re caption)
			      (match-string 1 caption))))))))

		 ;; here on a paragraph (eg in a caption of an image). it is a paragraph with a caption
		 ;; in a caption, with no name, but maybe a label
		 ((equal (org-element-type object) 'paragraph)
		  (if (org-element-property :name object)
		      (org-element-property :name object)

		    ;; See if it is in the caption name
		    (let ((caption (s-join "" (mapcar 'org-no-properties
						      (org-export-get-caption object)))))
		      (when (string-match org-ref-label-re caption)
			(match-string 1 caption)))))

		 ;; If you are in a table, we need to be at the beginning to
		 ;; make sure we get the name. Note when in a caption it appears
		 ;; you are in a table but org-at-table-p is nil there.
		 ((or (equal (org-element-type object) 'table) (org-at-table-p))
		  (save-excursion
		    (goto-char (org-table-begin))
		    (let* ((table (org-element-context))
			   (label (org-element-property :name table))
			   (caption (s-join "" (mapcar 'org-no-properties
						       (org-export-get-caption table)))))
		      (when (null label)
			;; maybe there is a label in the caption?
			(when (string-match org-ref-label-link-re caption)
			  (match-string 1 caption))))))

		 ;; and to #+namel: lines
		 ((and (equal (org-element-type object) 'paragraph)
		       (org-element-property :name object))
		  (org-element-property :name object))

		 ;; in a latex environment
		 ((equal (org-element-type object) 'latex-environment)
		  (let ((value (org-element-property :value object))
			label)
		    (when (string-match "\\\\label{\\(?1:[+a-zA-Z0-9:\\._-]*\\)}" value)
		      (match-string-no-properties 1 value))))

		 ;; Match targets, like <<label>>
		 ((equal (org-element-type object) 'target)
		  (org-element-property :value object))

		 (t
		  nil))))
    
    (when label
      (org-link-store-props
       :type reftype
       :link (concat reftype ":" label)) 
      t)))


;; ** ref link

(org-link-set-parameters "ref"
			 :store (defun org-ref-store-ref () (org-ref-store-ref-link "ref"))
			 :complete (apply-partially #'org-ref-complete-link "ref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "ref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


;;** pageref link

(org-link-set-parameters "pageref"
			 :store (defun org-ref-store-pageref () (org-ref-store-ref-link "pageref"))
			 :complete (apply-partially #'org-ref-complete-link "pageref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "pageref")
			 :face 'org-ref-ref-face
			 :complete (lambda (&optional arg) (org-ref-complete-link arg "pageref"))
			 :help-echo #'org-ref-ref-help-echo)


;;** nameref link

(org-link-set-parameters "nameref"
			 :store (defun org-ref-store-nameref () (org-ref-store-ref-link "nameref"))
			 :complete (apply-partially #'org-ref-complete-link "nameref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "nameref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;;** eqref link

(org-link-set-parameters "eqref"
			 :store (defun org-ref-store-eqref () (org-ref-store-ref-link "eqref"))
			 :complete (apply-partially #'org-ref-complete-link "eqref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "eqref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;;** autoref link

(org-link-set-parameters "autoref"
			 :store (defun org-ref-store-autoref () (org-ref-store-ref-link "autoref"))
			 :complete (apply-partially #'org-ref-complete-link "autoref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "autoref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(org-link-set-parameters "Autoref"
			 :store (defun org-ref-store-Autoref () (org-ref-store-ref-link "Autoref"))
			 :complete (apply-partially #'org-ref-complete-link "Autoref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "Autoref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)

;;** cref link
;; for LaTeX cleveref package:
;; https://www.ctan.org/tex-archive/macros/latex/contrib/cleveref


(org-link-set-parameters "cref"
			 :store (defun org-ref-store-cref () (org-ref-store-ref-link "cref"))
			 :complete (apply-partially #'org-ref-complete-link "cref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "cref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(org-link-set-parameters "Cref"
			 :store (defun org-ref-store-Cref () (org-ref-store-ref-link "Cref"))
			 :complete (apply-partially #'org-ref-complete-link "Cref")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export (apply-partially #'org-ref-ref-export "Cref")
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(defun org-ref-crefrange-export (path _desc backend)
  (pcase backend
    ('latex
     (let ((labels (split-string path ",")))
       (format "\\crefrange{%s}{%s}" (cl-first labels) (cl-second labels))))))


(defun org-ref-Crefrange-export (path _desc backend)
  (pcase backend
    ('latex
     (let ((labels (split-string path ",")))
       (format "\\Crefrange{%s}{%s}" (cl-first labels) (cl-second labels))))))


(defun org-ref-crefrange-complete (cmd &optional _arg)
  "Completing function for the c/Crefrange links."
  (concat cmd ":"
	  (completing-read "Label 1: " (org-ref-get-labels))
	  ","
	  (completing-read "Label 2: " (org-ref-get-labels))))


(org-link-set-parameters "crefrange"
			 :complete (apply-partially #'org-ref-crefrange-complete "crefrange")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export #'org-ref-crefrange-export
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


(org-link-set-parameters "Crefrange"
			 :complete (apply-partially #'org-ref-crefrange-complete "Crefrange")
			 :activate-func #'org-ref-ref-activate
			 :follow #'org-ref-ref-jump-to
			 :export #'org-ref-Crefrange-export
			 :face 'org-ref-ref-face
			 :help-echo #'org-ref-ref-help-echo)


;; ** remove store functions
(defun org-ref-ref-remove-store ()
  "Remove the store functions from ref links.
These tend to clobber the org store links. You can use C-u C-u
C-c C-l to not use them, but if you prefer not to use them, this
function removes the store functions from the links."
  (interactive)
  (cl-loop for reflink in '("ref" "pageref" "nameref" "eqref" "autoref" "Autoref"
			    "cref" "Cref" "crefrange" "Crefrange")
	   do
	   (setf (cdr (assoc reflink org-link-parameters))
		 (org-plist-delete (cdr (assoc reflink org-link-parameters)) :store))))

;; * Insert link
(defvar org-ref-equation-environments
  '("equation"
    "equation*"
    "align"
    "align*"
    "multline"
    "multline*")
  "LaTeX environments that should be treated as equations when referencing.")


(defvar org-ref-ref-type-inference-alist
  '((org-ref-equation-label-p . "eqref"))
  "Alist of predicate functions taking a label name and the
  desired reference type if the predicate returns true.")


(defun org-ref-enclosing-environment (label)
  "Returns the name of the innermost LaTeX environment containing
the first instance of the label, or nil of there is none."
  (or
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (let ((label-point (search-forward (format "\\label{%s}" label) nil t)))
	 (when label-point
           (catch 'return
             (let (last-begin-point last-env)
               (while (setq
                       last-begin-point (re-search-backward "\\\\begin{\\([^}]+\\)}" nil t)
                       last-env (match-string-no-properties 1))
		 (let ((env-end-point
			(search-forward (format "\\end{%s}" last-env) nil t)))
                   (if (and env-end-point
                            (> env-end-point label-point))
                       (throw 'return last-env)
                     (goto-char last-begin-point))))))))))
   ;; Check latex-environments for names, and matching environment
   (org-element-map (org-element-parse-buffer) 'latex-environment
     (lambda (le)
       (when (and (string= label (org-element-property :name le))
		  (string-match
		   (concat "begin{\\("
			   (regexp-opt org-ref-equation-environments)
			   "\\)}")
		   (org-element-property :value le)))
	 (match-string 1 (org-element-property :value le))))
     nil t)))


(defun org-ref-equation-label-p (label)
  "Return non-nil if LABEL is an equation label."
  (let ((maybe-env (org-ref-enclosing-environment label)))
    (when maybe-env
      (member maybe-env org-ref-equation-environments))))


(defun org-ref-infer-ref-type (label)
  "Return inferred type for LABEL."
  (or (cl-dolist (pred-pair org-ref-ref-type-inference-alist)
	(when (funcall (car pred-pair) label)
	  (cl-return (eval (cdr pred-pair)))))
      org-ref-default-ref-type))


(defun org-ref-ref-link-p ()
  "Return the link at point if point is on a ref link."
  (let ((el (org-element-context)))
    (and (eq (org-element-type el) 'link)
	 (assoc (org-element-property :type el) org-ref-ref-types)
	 el)))


(defun org-ref-select-label ()
  "Select a label in the buffer with annotated completion."
  (let*  ((type-annotation (lambda (s)
			     (let ((item (assoc s minibuffer-completion-table))) 
			       (when item
				 (with-temp-buffer
				   (insert "\n" (cdr item))
				   (indent-rigidly (point-min) (point-max) 20)
				   (buffer-string))))))
	  (completion-extra-properties `(:annotation-function ,type-annotation)))
    (completing-read "Label: " (org-ref-get-labels))))


(defun org-ref-insert-ref-link (&optional set-type)
  "Insert a ref link.
If on a link, append a label to the end.
With a prefix arg SET-TYPE choose the ref type."
  (interactive "P")
  (let* ((minibuffer-setup-hook '(org-ref-minibuffer-prefix))
	 (label (org-ref-select-label)) 
	 (type (if (or set-type org-ref-prefix-arg)
		   (org-ref-select-ref-type)
		 ;; in general, we prefer to infer the type, so that equations
		 ;; get a better default. However, if you customize
		 ;; `org-ref-default-ref-type', we use that instead. The most
		 ;; common use case for this is setting that variable to cref,
		 ;; which does its own automatic inference of the type.
		 (if (string= "ref" org-ref-default-ref-type)
		     (org-ref-infer-ref-type label)
		   org-ref-default-ref-type))))
    (if-let* ((lnk (org-ref-ref-link-p))
	      (path (org-element-property :path lnk))
	      (beg (org-element-property :begin lnk))
	      (end (org-element-property :end lnk)))
	(progn
	  (setf (plist-get (cadr lnk) :path) (concat path "," label))
	  (cl--set-buffer-substring beg end (org-element-interpret-data lnk)))

      (insert (format  "%s:%s" type label)))
    (goto-char (org-element-property :end (org-element-context)))))


(provide 'org-ref-ref-links)

;;; org-ref-ref-links.el ends here
