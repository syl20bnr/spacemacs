;;; citeproc-name.el --- CSL name and label rendering -*- lexical-binding: t; -*-

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

;; Functions to render CSL name and label elements.

;;; Code:

(require 'subr-x)
(require 'let-alist)
(require 'dash)
(require 's)

(require 'citeproc-lib)
(require 'citeproc-s)
(require 'citeproc-rt)
(require 'citeproc-context)
(require 'citeproc-term)

(defvar citeproc-name-postprocess-functions nil
  "A list of functions to postprocess rendered names.
Each function takes three arguments:
- the rich-text rendering of a name to be post-processed,
- the rendered name as an alist with CSL name-part
  keys (`family', `given' etc.), and
- the rendering context, as a `citeproc-context' structure.
The output of each function should be the post-processed
rich-text, and the functions are applied in the order they appear
in the list.")

;; OPTIMIZE: Name count could be sped up by only counting the names to be
;; rendered without actually rendering them
(defun citeproc-name-render-vars
    (varstring attrs name-attrs name-part-attrs et-al-attrs with-label
	       label-before-names label-attrs context)
  "Render namevars contained in VARSTRING according to the given attrs.
VARSTRING is a string containing variable names separated by
spaces. Labels are also rendered (with formatting according to
LABEL-ATTRS) if WITH-LABEL is t."
  (let* ((vars (-map 'intern (s-split " " varstring)))
	 (present-vars (--filter (citeproc-var-value it context) vars))
	 ed-trans)
    (when (and (memq 'editor present-vars)
	       (memq 'translator present-vars)
	       (= 2 (length present-vars))
	       (equal (--map (alist-get 'name-id it)
			     (citeproc-var-value 'editor context))
		      (--map (alist-get 'name-id it)
			     (citeproc-var-value 'translator context))))
      (setq present-vars '(editor)
	    ed-trans t))
    (unless (alist-get 'delimiter attrs)
      (-when-let (names-delim (alist-get 'names-delimiter (citeproc-context-opts context)))
	(push (cons 'delimiter names-delim) attrs)))
    (if present-vars
	(cons (citeproc-rt-join-formatted
	       attrs
	       (--map (citeproc-name--render-var it name-attrs name-part-attrs
						 et-al-attrs with-label
						 label-before-names
						 label-attrs
						 context
						 ed-trans)
		      present-vars)
	       context)
	      'present-var)
      (cons nil 'empty-vars))))

(defun citeproc-name--render-var (var attrs name-part-attrs et-al-attrs with-label
				      label-before-names
				      label-attrs context &optional ed-trans)
  "Render the value of csl name variable VAR according to given attrs.
VAR is a symbol.

Note: The label (if there is one) is prepended to the name(s) if
the form is verb and also when the label element was before the
names element in the style. The latter is not allowed in the
standard, so can be considered an extension. It's supported
because some styles rely on it, notably that of the journal
Nature."
  ;; Push the current add-names offset for VAR to the ATTRS
  (-when-let (add-names-alist (citeproc-var-value 'add-names context))
    (-when-let (add-names-val (alist-get var add-names-alist))
      (push `(add-names . ,add-names-val) attrs)))
  (let* ((var-value (citeproc-var-value var context))
	 (rendered-names (citeproc-name--render-names
			  var-value attrs et-al-attrs name-part-attrs
			  context)))
    (when (atom rendered-names) (setq rendered-names (list nil rendered-names)))
    (push (list 'rendered-names) (car rendered-names))
    (push `(variable . ,(if ed-trans "editortranslator" (symbol-name var)))
	  label-attrs)
    (let ((plural-val (alist-get 'plural label-attrs)))
      (when (or (not plural-val) (string= plural-val "contextual"))
	(push `(plural . ,(if (> (length var-value) 1) "always" "never"))
	      label-attrs)))
    (if with-label
	(let ((rendered-label (car (citeproc--label label-attrs context))))
	  (citeproc-rt-join-formatted `((rendered-var . ,var))
				      (if label-before-names
					  (list rendered-label rendered-names)
					(list rendered-names rendered-label))
				      context))
      (push (cons 'rendered-var var) (car rendered-names))
      rendered-names)))

(defun citeproc-name--render-names (names attrs et-al-attrs name-part-attrs context)
  "Render NAMES according to the given attrs."
  (let* ((all-attrs (-concat attrs (citeproc-context-opts context)))
	 (rmode (citeproc-context-render-mode context))
	 (sort-o (if (eq rmode 'sort) "all" ;; special setting for sort mode
		   (alist-get 'name-as-sort-order all-attrs)))
	 (names-count (length names))
	 (formatted-first (citeproc-name--render (car names) attrs name-part-attrs
						 sort-o context)))
    (if (= 1 names-count) formatted-first
      (let-alist all-attrs
	(let ((delimiter (or .delimiter .name-delimiter ", "))
	      (add-names (or .add-names 0))
	      (position (citeproc-var-value 'position context)))
	  (unless (or (null position) (eq position 'first))
	    (setq .et-al-min (or .et-al-subsequent-min .et-al-min)
		  .et-al-use-first (or .et-al-subsequent-use-first .et-al-use-first)))
	  (setq .et-al-min (or .names-min .et-al-min)
		.et-al-use-first (or .names-use-first .et-al-use-first)
		.et-al-use-last (or (string= .names-use-last "true")
				    (string= .et-al-use-last "true")))
	  (let* ((et-al-min-val
		  ;; If et-al should be ignored then we set this to an unreachable number.
		  (if (alist-get 'ignore-et-al (citeproc-context-vars context))
		      100
		    (citeproc-s-nil-or-s-to-num .et-al-min)))
		 (et-al-use-first-val (+ add-names
					 (citeproc-s-nil-or-s-to-num .et-al-use-first)))
		 (et-al (and .et-al-min .et-al-use-first
			     (>= names-count et-al-min-val)
			     (< et-al-use-first-val names-count)))
		 (middle-end-pos (if et-al et-al-use-first-val (- names-count 1)))
		 (sort-o-latters (string= sort-o "all"))
		 (formatted-middle
		  (if (< middle-end-pos 2)
		      nil
		    (citeproc-rt-join-formatted
		     `((delimiter . ,delimiter) (prefix . ,delimiter))
		     (--map (citeproc-name--render it attrs name-part-attrs
						   sort-o-latters context)
			    (-slice names 1 middle-end-pos))
		     context)))
		 (last-after-inverted (or sort-o-latters
					  (and (string= sort-o "first")
					       (null formatted-middle))))
		 (last-delim (citeproc-lib-intern (if et-al .delimiter-precedes-et-al
						    .delimiter-precedes-last)))
		 (last-pref (if (or (and (or (not last-delim) (eq last-delim 'contextual))
					 (> middle-end-pos 1))
				    (eq last-delim 'always)
				    (and (eq last-delim 'after-inverted-name)
					 last-after-inverted))
				delimiter
			      " "))
		 (formatted-last
		  (cond (et-al (if .et-al-use-last
				   (citeproc-rt-join-formatted
				    nil
				    (list
				     delimiter "… "
				     (citeproc-name--render (-last-item names)
							    attrs name-part-attrs
							    sort-o-latters context))
				    context)
				 (citeproc-name--render-et-al (cons `(prefix . ,last-pref)
								    et-al-attrs)
							      context)))
			(.and
			 (let ((and-str (if (string= .and "text")
					    (citeproc-term-get-text "and" context)
					  "&")))
			   (citeproc-rt-join-formatted
			    `((prefix . ,last-pref))
			    (list and-str " "
				  (citeproc-name--render
				   (-last-item names) attrs
				   name-part-attrs sort-o-latters context))
			    context)))
			(t
			 (citeproc-rt-join-formatted
			  nil
			  (list delimiter
				(citeproc-name--render (-last-item names)
						       attrs name-part-attrs
						       sort-o-latters context))
			  context)))))
	    (citeproc-rt-join-formatted
	     (--remove (eq 'delimiter (car it)) attrs)
	     (list formatted-first formatted-middle formatted-last)
	     context)))))))

(defun citeproc-name--render (name attrs name-part-attrs sort-o context)
  "Render NAME according to the given attributes."
  (let* ((format-attrs
	  (--filter (memq (car it) (-concat '(prefix suffix) citeproc-rt-format-attrs))
		    attrs))
	 (result (citeproc-rt-format-single
		  (cons `(name-id . ,(alist-get 'name-id name)) format-attrs)
		  (citeproc-name--render-formatted
		   (citeproc-name--format-nameparts name name-part-attrs context)
		   attrs sort-o context)
		  context)))
    (dolist (fn citeproc-name-postprocess-functions)
      (setq result (funcall fn result name context)))
    result)) 

(defun citeproc-name--parts-w-sep (c1 c2 sep context)
  "Join name-parts in lists C1 C2 with spaces and then with SEP."
  (let ((joined-c1 (citeproc-rt-join-formatted '((delimiter . " ")) c1 context)))
    (if (-none-p 'cadr c2)
	joined-c1
      (citeproc-rt-join-formatted
       `((delimiter . ,sep))
       (list joined-c1
	     (citeproc-rt-join-formatted '((delimiter . " ")) c2 context))
       context))))

(defun citeproc-name--render-formatted (name-alist attrs sort-o context)
  "Render formatted name described by NAME-ALIST according to ATTRS.
NAME-ALIST is an alist with symbol keys corresponding to
name-parts like `family' etc. and values are simple rich-text
contents of the form (ATTRS CONTENT) where content must be a
single string. SORT-O is a boolean determining whether to use
sort order."
  (-let* ((global-opts (citeproc-context-opts context))
	  ((&alist 'family f
		   'given g-uninited
		   'suffix s
		   'dropping-particle d
		   'non-dropping-particle n
		   'name-id nid)
	   name-alist)
	  ((&alist 'sort-separator sort-sep
		   'initialize init
		   'initialize-with init-with
		   'form form
		   'name-form name-form)
	   (-concat attrs global-opts))
	  (sort-sep (or sort-sep ", "))
	  (init (not (string= init "false")))
	  (d-n-d (intern (alist-get 'demote-non-dropping-particle global-opts)))
	  (id (cadr nid))
	  (show-given (citeproc-name-show-givenname-level id context))
	  (form (if show-given 'long
		  (intern (or form name-form "long"))))
	  (rmode (citeproc-context-render-mode context)))
    (if (citeproc-name--lat-cyr-greek-p name-alist)
	(let ((g
	       (cond ((or (null g-uninited)
			  (and show-given (= show-given 2)))
		      g-uninited)
		     ((and init-with init)
		      (list (citeproc-rt-attrs g-uninited)
			    (citeproc-name--initialize
			     (citeproc-rt-first-content g-uninited)
			     init-with
			     (string= "false"
				      (alist-get 'initialize-with-hyphen
						 global-opts)))))
		     (init-with
		      (list (citeproc-rt-attrs g-uninited)
			    (citeproc-name--initials-add-suffix
			     init-with
			     (citeproc-rt-first-content g-uninited))))
		     (t g-uninited))))
	  (if (eq form 'long)
	      (if sort-o
		  (if (or (eq d-n-d 'never)
			  (and (eq d-n-d 'sort-only) (eq rmode 'display)))
		      (citeproc-name--parts-w-sep
		       (citeproc-name--conc-nps n f) (list g d s) sort-sep
		       context)
		    (citeproc-name--parts-w-sep (list f) (list g d n s) sort-sep context))
		(citeproc-rt-join-formatted
		 '((delimiter . " ")) `(,g ,@(citeproc-name--conc-nps
					      d n f) ,s)
		 context))
	    (citeproc-rt-join-formatted
	     '((delimiter . " ")) (citeproc-name--conc-nps n f) context)))
      (if (eq form 'long)
	  (citeproc-rt-join-formatted '((delimiter . " ")) (list f g-uninited) context)
	f))))

(defun citeproc-name--conc-nps (&rest nps)
  "Concatenate particles in name-parts NPS if they end with apostrophe."
  (let ((nonnils (delq nil nps)))
    (if (cdr nonnils)
	(let* ((len (length nonnils))
	       (particle (nth (- len 2) nonnils))
	       (particle-str (if (listp particle) (cadr particle) particle)))
	  (if (string= "ʼ" (substring particle-str -1))
	      (let* ((family (car (last nonnils)))
		     (result (list (list nil particle family))))
		(when (> 2 len) (push (car nonnils) result))
		result)
	    nonnils))
      nonnils)))

(defun citeproc-name--lat-cyr-greek-p (name-alist)
  "Return t if NAME-ALIST is cyrillic/latin/greek and nil otherwise.
NAME-ALIST is like in `citeproc-name--render-formatted'"
  (--all-p (or (not (stringp it)) (string-match "^\\(\\cl\\|\\cy\\|\\cg\\|ʼ\\)*$"
						it))
	   (-map (lambda (x)
		   (if (listp (cdr x)) (cl-caddr x)
		     (cdr x)))
		 name-alist)))

(defun citeproc-name--initialize (names suffix &optional remove-hyphens)
  "Initialize NAMES and add SUFFIX.
NAMES is a string containing one or more space-separated names,
while SUFFIX is either nil or a string (e.g. \".\"). If the
optional REMOVE-HYPHENS is non-nil then don't keep hyphens
between initalized given names, e.g., initialize Jean-Paul to
J.P. instead of the default J.-P."
  (let ((trimmed-suffix (s-trim suffix)))
    (concat (s-join
	     suffix
	     (--map
	      (if (s-match "-" it)
		  (citeproc-name--initialize-hyphenated it suffix remove-hyphens)
		(s-left 1 it))
	      (s-split " +" names)))
	    trimmed-suffix)))

(defun citeproc-name--initialize-hyphenated (name suffix &optional remove-hyphens)
  "Initialize space-less but hyphenated NAME with SUFFIX.
If the optional REMOVE-HYPHENS is non-nil then don't keep hyphens
between the initalized given names, e.g., initialize Jean-Paul to
J.P. instead of the default J.-P."
  (let ((inner-suffix (s-trim suffix)))
    (s-join (if remove-hyphens inner-suffix
	      (concat inner-suffix "-"))
	    (--map (s-left 1 it)
		   (s-split "-" name)))))

(defun citeproc-name--initials-add-suffix (suffix names)
  "Add SUFFIX to initials in NAMES.
NAMES is a string containing one or more space-separated names,
while SUFFIX is a string (e.g. \".\")."
  (let ((suffix (s-trim suffix)))
    (mapconcat (lambda (x)
		 (if (and (cdr x) (s-match "^[[:alpha:]]$" (car x)))
		     (concat (car x) suffix)
		   (car x)))
	       (citeproc-s-slice-by-matches names "[ \\-]" 0 t)
	       "")))

(defun citeproc-name--format-nameparts (name-alist name-part-attrs context)
  "Format nameparts in NAME-ALIST according to NAME-PART-ATTRS.
Return a new name alist containg the same keys with formatted
contents."
  (-let (((&alist 'given given-attrs
		  'family family-attrs)
	  name-part-attrs))
    (--map (-let (((n-part . content) it))
	     (cons n-part
		   (cond ((and given-attrs
			       (memq n-part '(given dropping-particle)))
			  (citeproc-rt-format-single given-attrs content context))
			 ((and family-attrs
			       (memq n-part '(family non-dropping-particle)))
			  (citeproc-rt-format-single family-attrs content context))
			 (t (list nil content)))))
	   name-alist)))

(defun citeproc-name--render-et-al (attrs context)
  "Render the `et al' part of a name acc. to ATTRS."
  (let ((rmode (citeproc-context-render-mode context)))
    (if (eq rmode 'sort) ""
      (let ((term (or (alist-get 'term attrs)
		      "et-al")))
	(citeproc-rt-format-single attrs
				   (citeproc-term-get-text term context)
				   context)))))

(defun citeproc-name-show-givenname-level (id context)
  "Return the disambiguation level of name with ID."
  (alist-get id (alist-get 'show-given-names (citeproc-context-vars context))))

(defun citeproc--var-plural-p (var context)
  "Return whether the content of variable VAR is plural.
VAR is a symbol."
  (let ((content (citeproc-rt-to-plain (citeproc-var-value var context))))
    (if (or (string= var "number-of-pages")
	    (string= var "number-of-volumes"))
	(> (string-to-number content) 1)
      (string-match-p
       (concat "[[:digit:]] *\\([-,;–&—―]+\\|[,;]? *"
	       (citeproc-term-get-text "and" context)
	       "\\) *[a-zA-Z]?[[:digit:]]")
       content))))

(defun citeproc--label (attrs context &rest _body)
  "Render a CSL label element with the given ATTRS in CONTEXT."
  (-let* (((&alist 'variable variable
		   'form form
		   'plural plural)
	   attrs)
	  (label (intern variable))
	  (number nil))
    (if (or (eq label 'editortranslator)
	    (and label (citeproc-var-value label context)))
	(progn
	  (if form (setq form (intern form))
	    (setq form 'long))
	  (when (string= variable "locator")
	    (setq variable (citeproc-locator-label context)))
	  (cond ((string= plural "never") (setq number 'single))
		((string= plural "always") (setq number 'multiple))
		(t (setq number
			 (if (citeproc--var-plural-p label context)
			     'multiple
			   'single))))
	  ;; Add rendered locator label info in cite mode.
	  (when (and (eq label 'locator)
		     (eq (citeproc-context-mode context) 'cite))
	   (push '(rendered-locator-label . t) attrs))
	  (cons (citeproc-rt-format-single attrs (citeproc-term-inflected-text
						  variable form number context)
					   context)
		'text-only))
      (cons nil 'text-only))))

(provide 'citeproc-name)

;;; citeproc-name.el ends here
