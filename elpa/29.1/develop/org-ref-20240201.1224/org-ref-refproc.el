;;; org-ref-refproc.el --- refproc - for exporting ref links in non-LaTeX backends -*- lexical-binding: t; -*-

;; For autoref and cref* links, a clever prefix is added at export.
;; Prefixes are found in `org-ref-refproc-clever-prefixes'
;; Use a keyword like this to set the style of clever prefixes. You can use a full or abbreviated prefix
;;
;; #+refproc: :abbreviate t :capitalize t


;;; Commentary:
;;

;;; Code:
(require 'org-ref-ref-links)


(defcustom org-ref-refproc-clever-prefixes
  '((section :full "section" :abbrv "sec." :org-element headline)
    (figure :full "figure" :abbrv "fig." :org-element link)
    (table :full "table" :abbrv "tab." :org-element table)
    (equation :full "equation" :abbrv "eq." :org-element latex-environment)
    (proof :full "proof" :abbrv "pf." :org-element latex-environment)
    (listing :full "listing" :abbrv "lst." :org-element src-block)
    (lemma :full "lemma" :abbrv "lem." :org-element special-block)
    (theorem :full "theorem" :abbrv "thm." :org-element special-block)
    (corollary :full "corollary" :abbrv "cor." :org-element special-block))
  "Prefixes for cleveref links.
plist with :full and :abbrv forms for each type.
:org-element is the element these are defined in."
  :type 'sexp
  :group 'org-ref)



;; * refproc
;;

(defun org-ref-refproc-get-options ()
  "Return a plist of options from a #+refproc keyword.
Supported options are:
:abbreviate (nil or non-nil)
:capitalize (nil or non-nil)

These options only affect the cref* links."
  (read (format "(%s)" (cadr (assoc "REFPROC"
				    (org-collect-keywords
				     '("REFPROC")))))))


(defun org-ref-get-ref-links ()
  "Return a list of ref links in the buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (lnk)
      (when (assoc (org-element-property :type lnk) org-ref-ref-types)
	lnk))))


(defun org-ref-refproc-referenceables ()
  "Return a list of referenceable items, grouped by type.
This will be a list of (type label1 label2 ...).

Not all styles of target are currently supported. This supports
named tables, figures and src-blocks, headlines with a custom_id,
and latex_environments with a latex label or name (these are all
considered equations, which might be wrong for other
environments)."
  (let* ((parse-data (org-element-parse-buffer))
	 type el
	 ;; for each type, we specify what org-element to get the labels from.
	 (referencables (cl-loop for entry in org-ref-refproc-clever-prefixes
				 do
				 (setq type (car entry)
				       el (plist-get (cdr entry) :org-element))
				 collect
				 (cons type
				       (org-element-map parse-data el
					 (lambda (e)
					   (pcase el
					     ;; Headlines
					     ('headline
					      (cond
					       ;; There is a custom_id, we use it.
					       ((org-element-property :CUSTOM_ID e)
						(org-element-property :CUSTOM_ID e))
					       ;; See if there is a target in the heading
					       ((string-match org-target-regexp
							      (org-element-property :raw-value e))
						(match-string 1 (org-element-property :raw-value e)))
					       ;; Check for a label link
					       ((string-match (concat "label:" org-ref-label-re)
							      (org-element-property :raw-value e))
						(match-string 1 (org-element-property :raw-value e)))))

					     ;;  Figures
					     ('link
					      (cond
					       ;; file link with a name property
					       ((and (org-file-image-p (org-element-property :path e))
						     (org-element-property
						      :name (org-element-property :parent e)))
						(org-element-property
						 :name (org-element-property :parent e)))
					       ;; a label link in the caption
					       ((and  (org-file-image-p (org-element-property :path e))
						      (org-element-property
						       :caption (org-element-property :parent e))

						      (string-match (concat "label:" org-ref-label-re)
								    (org-element-interpret-data
								     (org-element-property
								      :caption
								      (org-element-property :parent e)))))
						(match-string 1 (org-element-interpret-data
								 (org-element-property
								  :caption
								  (org-element-property :parent e)))))))

					     ;; Tables
					     ('table
					      (cond
					       ;; A name keyword is preferred
					       ((org-element-property :name e)
						(org-element-property :name e))
					       ;; a label link in a caption also works
					       ((string-match (concat "label:" org-ref-label-re)
							      (or (org-element-interpret-data
								   (org-element-property :caption e))
								  ""))
						(match-string 1 (org-element-interpret-data
								 (org-element-property :caption e))))))

					     ;; equations, proofs, and other latex environments
					     ('latex-environment
					      (when (or (and (eq type 'equation)
							     (string-match "\\\\begin{equation}"
									   (org-element-property :value e)))
							(and (eq type 'equation)
							     (string-match "\\\\begin{align}"
									   (org-element-property :value e)))
							(and (eq type 'proof) (string-match "\\\\begin{proof}"
											    (org-element-property :value e))))

						(cond
						 ;; Name keyword
						 ((org-element-property :name e)
						  (org-element-property :name e))

						 ;; latex label in the value
						 ((string-match "\\\\label{\\(.*\\)}" (org-element-property :value e))
						  (save-match-data
						    (let ((pos 1)
							  matches)
						      (while (string-match "\\\\label{\\(.*\\)}" (org-element-property :value e) pos)
							(push (match-string 1 (org-element-property :value e)) matches)
							(setq pos (match-end 0)))
						      matches))))))

					     ;; special blocks
					     ('special-block
					      (cond
					       ((and (eq type 'lemma)
						     (org-element-property :name e)
						     (string= "lemma" (org-element-property :type e)))
						(org-element-property :name e))
					       
					       ((and (eq type 'corollary)
						     (org-element-property :name e)
						     (string= "corollary" (org-element-property :type e)))
						(org-element-property :name e))

					       ((and (eq type 'theorem)
						     (org-element-property :name e)
						     (string= "theorem" (org-element-property :type e)))
						(org-element-property :name e))))


					     ;; Listings of code blocks
					     ('src-block
					      (when-let (name (org-element-property :name e))
						name)))))))))
    ;; the align equation environment needs to be flattened
    (cl-loop for type in referencables
	     collect (cons (car type) (-flatten (cdr type))))))


(defun org-ref-refproc-get-type (label referenceables)
  "Get the type of reference LABEL refers to.
REFERENCEABLES comes from `org-ref-refproc-referenceables'.
Returns a plist (:type type :label label :index index)"
  (cl-loop for type in referenceables
	   if (member label type)
	   return (list :type (car  type) :label label :index (seq-position (cdr type) label 'equal))))


(defun org-ref-refproc-group-sort-labels (lbls referenceables)
  "Group and sort the labels in LBLS.
REFERENCEABLES comes from `org-ref-refproc-referenceables'.
Returns a list of (list (type l1 i1) (type l2, i2)...) where l1
is label1, i1 is index1, etc.

This data structure has all the references from a single link
grouped by type. For some links there will only be one type, and
one label, because that is all that is allowed, e.g. for ref,
eqref, pageref, nameref and autoref.

For the cref* links though you can have many labels, even to
different kinds of things, and these are why we do the fancy
grouping and sorting here."

  (cl-loop for entry in
	   (seq-group-by (lambda (el)
			   (plist-get el :type))
			 (cl-loop for label in lbls collect
				  (org-ref-refproc-get-type label referenceables)))
	   collect
	   (sort (cdr entry) (lambda (a b)
			       (< (plist-get a :index) (plist-get b :index))))))


;; * Refproc - a reference processor for org-mode
;;
;; The strategy is to compute string replacements that are just org-syntax, then
;; replace each link with the replacement.
(defun org-ref-refproc-replacement (ref-link referenceables backend)
  "Compute a replacement for REF-LINK (an org-element).
REFERENCEABLES comes from `org-ref-refproc-referenceables'.
BACKEND is the export backend."
  (let* ((ref-type (org-element-property :type ref-link))
	 (labels (split-string (org-element-property :path ref-link) ","))
	 (post-blanks (org-element-property :post-blank ref-link))
	 (data (cl-loop for label in labels collect (org-ref-refproc-get-type label referenceables))))

    (pcase ref-type
      ;; These types only allow one label in the reference
      ("ref"
       (when (>  (length data) 1)
	 (error "A ref link can only have one label in it"))
       (setq data (cl-first data))
       (format "[[%s%s]]%s"
	       (if (eq 'section (plist-get data :type))
		   "#"
		 "")
	       (plist-get data :label)
	       ;; add back post blank spaces.
	       (make-string post-blanks ? )))

      ("eqref"
       (when (>  (length data) 1)
	 (error "An eqref link can only have one label in it"))
       (setq data (cl-first data))
       (format (if (eq backend 'html)
		   "[[%s]]%s"
		 "([[%s]])%s")
	       (plist-get data :label)
	       ;; add back post blank spaces.
	       (make-string post-blanks ? )))

      ;; This one doesn't make much sense in unpaginated exports, and I don't
      ;; have a way to get a page number anyway, so we make this a regular ref.
      ("pageref"
       (when (>  (length data) 1)
	 (error "A pageref link can only have one label in it"))
       (setq data (cl-first data))
       (format "[[%s%s]]%s"
	       (if (eq 'section (plist-get data :type))
		   "#"
		 "")
	       (plist-get data :label)
	       ;; add back post blank spaces.
	       (make-string post-blanks ? )))

      ;; this will use the heading name, or caption of the ref
      ("nameref"
       (when (>  (length data) 1)
	 (error "A nameref link can only have one label in it"))
       (setq data (cl-first data))

       ;; Basically I follow the link, and depending on the ref type get a
       ;; "name" for it. Right now there is no error if there is not a heading
       ;; or caption, you would just get an empty string.
       (save-excursion
	 (goto-char (org-element-property :begin ref-link))
	 (search-forward ":")
	 (org-ref-ref-jump-to (org-element-property :path ref-link))
	 (pcase (plist-get data :type)
	   ('section
	    (org-previous-visible-heading 1)
	    (format "[[#%s][%s]]%s"
		    (plist-get data :label)
		    (cl-fifth (org-heading-components))
		    (make-string post-blanks ? )))

	   ('table
	    (format "[[%s][%s]]%s"
		    (plist-get data :label)
		    (org-element-interpret-data (org-element-property :caption (org-element-context)))
		    (make-string post-blanks ? )))

	   ('figure
	    (format "[[%s][%s]]%s"
		    (plist-get data :label)
		    (org-element-interpret-data (org-element-property :caption (org-element-context)))
		    (make-string post-blanks ? )))

	   ('listing
	    (format "[[%s][%s]]%s"
		    (plist-get data :label)
		    (org-element-interpret-data (org-element-property :caption (org-element-context)))
		    (make-string post-blanks ? )))
	   ;; does equation even make sense? is there ever a caption?
	   (_
	    (error "The nameref link is not implemented for %S" data)))))

      ;; autoref is the first place we have to be clever.
      ("autoref"
       (when (>  (length data) 1)
	 (error "A pageref link can only have one label in it"))
       (setq data (cl-first data))

       (let* ((prefix-data (cdr (assoc (plist-get data :type) org-ref-refproc-clever-prefixes)))
	      (options (org-ref-refproc-get-options))
	      (prefix (plist-get prefix-data  (if (plist-get options :abbreviate)
						  :abbrv
						:full))))
	 (when (plist-get options :capitalize) (setq prefix (capitalize prefix)))
	 (format "%s [[%s%s]]%s"
		 prefix
		 (if (eq 'section (plist-get data :type))
		     "#"
		   "")
		 (plist-get data :label)
		 (make-string post-blanks ? ))))

      ;; Now we get to the cref* links. These are complicated, so I will pass
      ;; this out to dedicated functions. At this point, I have the ref-link,
      ("cref"
       (org-ref-refproc-cref-replacement ref-link referenceables nil))

      ("Cref"
       (org-ref-refproc-cref-replacement ref-link referenceables t))

      (_
       (error "%s links are not supported yet" ref-type)))))


(defun org-ref-refproc-cref-replacement (ref-link referenceables capitalize)
  "Calculate a replacement for a REF-LINK.

REFERENCEABLES comes from `org-ref-refproc-referenceables'.

If CAPITALIZE is non-nil, capitalize the first entry (this is for
Cref) and is different than the capitalize option in #+refproc:
which capitalizes each prefix."
  (let* ((options (org-ref-refproc-get-options)) 
	 (labels (split-string (org-element-property :path ref-link) ","))
	 (post-blanks (org-element-property :post-blank ref-link))
	 (data (cl-loop for label in labels collect (org-ref-refproc-get-type label referenceables)))
	 (groups (seq-group-by (lambda (entry)
				 (plist-get entry :type))
			       data))
	 ;; this will be a list of sorted entries.
	 (sorted-groups (cl-loop for group in groups collect
				 (sort (cdr group) (lambda (a b)
						     (< (plist-get a :index) (plist-get b :index))))))
	 ;; temp vars
	 prefix
	 prefix-data
	 ;; Now we have to be clever on each sorted group. One day I should add compression to the groups, e.g. 1,2,3 - > 1 to 3
	 ;; For each group we need a prefix, and it may have to be a plural prefix.
	 ;; If there are two members of a group, they are joined by and
	 ;; if there are more than two members of a group, they are separated by commas with and at the end.
	 (replacements (cl-loop for collection in sorted-groups collect
				(pcase (length collection)
				  ;; One thing in the list
				  (1
				   (setq collection (cl-first collection)
					 prefix-data (cdr (assoc (plist-get collection :type) org-ref-refproc-clever-prefixes))
					 prefix (plist-get  prefix-data (if (plist-get options :abbreviate)
									    :abbrv
									  :full)))
				   (when (plist-get options :capitalize)
				     (setq prefix (capitalize prefix)))
				   (format "%s %s"
					   ;; prefix
					   prefix
					   (concat
					    (cond
					     ((eq 'section (plist-get collection :type))
					      (format "[[#%s]]" (plist-get collection :label)))
					     ((eq 'equation (plist-get collection :type))
					      (format "\\ref{%s}" (plist-get collection :label)))
					     (t
					      (format "[[%s]]" (plist-get collection :label)))))))
				  (2
				   ;; the prefix can be found from the first label, but we have to make it plural.
				   (setq prefix-data (cdr (assoc (plist-get (cl-first collection) :type)
								 org-ref-refproc-clever-prefixes))
					 prefix (plist-get  prefix-data (if (plist-get options :abbreviate)
									    :abbrv
									  :full)))
				   (when (plist-get options :capitalize)
				     (setq prefix (capitalize prefix)))

				   ;; Make it plural (substring "fig." -1)
				   (setq prefix (if (string= "." (substring prefix -1))
						    (concat (substring prefix 0 -1) "s.")
						  (concat prefix "s")))

				   (format "%s [[%s]] and [[%s]]"
					   prefix
					   (concat
					    (if (eq 'section (plist-get (cl-first collection) :type)) "#" "")
					    (plist-get (cl-first collection) :label))
					   (concat
					    (if (eq 'section (plist-get (cl-second collection) :type)) "#" "")
					    (plist-get (cl-second collection) :label))))

				  (_
				   ;; the prefix can be found from the first label, but we have to make it plural.
				   (setq prefix-data (cdr (assoc (plist-get (cl-first collection) :type)
								 org-ref-refproc-clever-prefixes))
					 prefix (plist-get  prefix-data (if (plist-get options :abbreviate)
									    :abbrv
									  :full)))
				   (when (plist-get options :capitalize)
				     (setq prefix (capitalize prefix)))

				   ;; Make it plural. If the prefix ends in .
				   ;; (i.e. it is abbreviated) we remove it and
				   ;; add s.
				   (setq prefix (if (string= "." (substring prefix -1))
						    (concat (substring prefix 0 -1) "s.")
						  (concat prefix "s")))

				   ;; now get
				   (format "%s %s and [[%s]]"
					   prefix
					   (string-join
					    (cl-loop for entry in (butlast collection) collect
						     (format "[[%s]]"
							     (concat
							      (if (eq 'section (plist-get entry :type)) "#" "")
							      (plist-get entry :label))))
					    ", ")
					   ;; the last one
					   (concat
					    (if (eq 'section (plist-get (car (last collection)) :type)) "#" "")
					    (plist-get (car (last collection)) :label))))))))


    (when capitalize (setf (car replacements) (concat (capitalize (substring (car replacements) 0 1)) (substring (car replacements) 1))))

    ;; Finally, join them together with commas, and add post-blank spaces.
    (concat
     (pcase (length replacements)
       (1
	(cl-first replacements))
       (2
	(concat (cl-first replacements) " and " (cl-second replacements)))
       (_
	(concat (string-join (butlast replacements) ", ") " and " (car (last replacements)))))

     (make-string post-blanks ? ))))


(defun org-ref-refproc (&optional backend)
  "Process the buffer replacing all ref links with org syntax.
BACKEND is the
Meant to be used in an `org-export-before-parsing-hook' on a copy
of the buffer."
  (let ((ref-links (org-ref-get-ref-links))
	(referenceables (org-ref-refproc-referenceables)))

    (cl-loop for ref in (reverse ref-links) do
	     (cl--set-buffer-substring (org-element-property :begin ref)
				       (org-element-property :end ref)
				       (org-ref-refproc-replacement ref referenceables backend)))))

(provide 'org-ref-refproc)

;;; org-ref-refproc.el ends here
