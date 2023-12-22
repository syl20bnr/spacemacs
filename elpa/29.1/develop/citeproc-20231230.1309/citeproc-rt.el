;; citeproc-rt.el --- citeproc-el rich-text functions -*- lexical-binding: t; -*-

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

;; Functions operating on rich-text contents. In citeproc-el, rich-texts are
;; represented either by strings or by lists of the form (ATTRS RT_1 RT_2...)
;; where ATTRS is an alist consisting of (FORMAT-ATTR . VALUE) pairs and RT_1,
;; RT_2... are all rich-texts. The constants `citeproc-rt-format-attrs' and
;; `citeproc-rt-ext-format-attrs' define the list of normal and extended format
;; attributes, respectively. As a degenerate case, nil is also a legitimate
;; rich-text.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'let-alist)
(require 's)

(require 'citeproc-s)
(require 'citeproc-lib)

(defconst citeproc-rt-format-attrs
  '(font-variant font-style font-weight text-decoration vertical-align font-variant
		 display rendered-var name-id quotes cited-item-no bib-item-no
		 rendered-names href stopped-rendering rendered-locator-label)
  "The rich-text content format attributes (used in raw output).")

(defconst citeproc-rt-ext-format-attrs
  (-concat '(prefix suffix delimiter subst quotes) citeproc-rt-format-attrs)
  "The list of extended format attributes.")

(defun citeproc-rt-to-plain (rt)
  "Return the plain-text content of rich-text RT."
  (if (listp rt)
      (mapconcat 'citeproc-rt-to-plain
		 (cdr rt)
		 "")
    rt))

(defun citeproc-rt-attrs (rt)
  "Return the attrs of rich content RT."
  (if (listp rt) (car rt) nil))

(defun citeproc-rt-first-content (rt)
  "Return the first content element of rich content RT."
  (if (listp rt) (cadr rt) rt))

(defun citeproc-rt-select-attrs (attrs keep)
  "Select attr-val pairs from alist ATTRS whose attr is in KEEP."
  (--filter (memq (car it) keep)
	    attrs))

(defun citeproc-rt-join-strings (rt)
  "Concatenate consecutive strings in rich-text RT."
  (cond ((< (length rt) 2)
	 rt)
	((and (char-or-string-p (car rt))
	      (char-or-string-p (cadr rt)))
	 (citeproc-rt-join-strings (cons (concat (car rt)
						 (cadr rt))
					 (cddr rt))))
	(t (cons (car rt)
		 (citeproc-rt-join-strings (cdr rt))))))

(defun citeproc-rt-splice-unformatted (rt)
  "Splice the body of its unformatted elements into rich-text RT."
  (if (and (consp rt) (not (alist-get 'delimiter (car rt))))
      (cons (car rt)
	    (--mapcat (if (citeproc-rt-formatting-empty-p it)
			  (cdr it)
			(list it))
		      (cdr rt)))
    rt))

(defun citeproc-rt-formatting-empty-p (rt)
  "Is the formatting of rich-text RT empty/redundant?"
  (and (consp rt) (or (not (caar rt))
		      (and (eq (cl-caaar rt) 'delimiter)
			   (= (length (car rt)) 1)
			   (= (length rt) 2)))))

(defun citeproc-rt-reduce-content (rt)
  "Reduce rich-text RT if it has no attributes or body.
Return the original RT if it has non-empty attrs and content."
  (cond ((not (cdr rt)) nil)
	((and (not (car rt)) (= (length rt) 2)) (cadr rt))
	(t rt)))

(defun citeproc-rt-simplify-shallow (rt)
  "Simplify the first level of rich-text RT."
  (citeproc-rt-reduce-content (citeproc-rt-join-strings
			       (citeproc-rt-splice-unformatted rt))))

(defun citeproc-rt-simplify-deep (rt)
  "Simplify all levels of rich-text RT."
  (if (not (consp rt)) rt
    (-let* (((attrs . conts) rt)
	    (simplifieds (--map (citeproc-rt-simplify-deep it)
				conts)))
      (citeproc-rt-reduce-content
       (citeproc-rt-join-strings
	(citeproc-rt-splice-unformatted (cons attrs simplifieds)))))))

(defun citeproc-rt-map-strings (fun rts &optional skip-nocase)
  "Map through FUN all strings in rich-texts RTS.
Return a new rich-text with all S content strings replaced by the
value of FUN applied to S. No formatting is changed. If optional
SKIP-NOCASE is non-nil then skip spans with the `nocase'
attribute set to non-nil."
  (--map (citeproc-rt-format it fun skip-nocase) rts))

(defun citeproc-rt-format (rt fun &optional skip-nocase)
  "Format all plain text in RT with FUN.
If optional SKIP-NOCASE is non-nil then skip spans with the
`nocase' attribute set to non-nil."
  (pcase rt
    (`nil nil)
    ((pred listp) (if (and skip-nocase (alist-get 'nocase (car rt)))
		      rt
		    (cons (car rt) (citeproc-rt-map-strings fun (cdr rt) skip-nocase))))
    (_ (funcall fun rt))))

(defun citeproc-rt-replace-all-sim (replacements regex rts)
  "Make all REPLACEMENTS simultaneously in the strings of rich-texts RTS."
  (citeproc-rt-map-strings (lambda (x) (citeproc-s-replace-all-sim x regex replacements))
			   rts))

(defun citeproc-rt-strip-periods (rts)
  "Remove all periods from rich-texts RTS."
  (citeproc-rt-map-strings (lambda (x) (citeproc-s-replace "." "" x)) rts))

(defun citeproc-rt-length (rt)
  "Return the length of rich-text RT as a string."
  (if (listp rt)
      (-sum (mapcar #'citeproc-rt-length (cdr rt)))
    (length rt)))

(defun citeproc-rt--update-from-plain-1 (rt p start &optional skip-nocase)
  "Update rich-text RT from plain text P from position START in P.
The length of the plain text content of RT must not be less than
the length of P. If optional SKIP-NOCASE is non-nil then skip
spans with the `nocase' attribute set to non-nil.
  Return an (UPDATED . NEXT) pair where UPDATED is the updated
rich-text and NEXT is the first position in P which was not used
for the update."
  (pcase rt
    (`nil nil)
    ((pred listp)
     (if (and skip-nocase (alist-get 'nocase (car rt)))
	 (cons rt (+ start (citeproc-rt-length rt)))
       (let ((act-start start))
	 (cons (cons (car rt)
		     (--map (-let (((updated . next)
				    (citeproc-rt--update-from-plain-1
				     it p act-start skip-nocase)))
			      (setq act-start next)
			      updated)
			    (cdr rt)))
	       act-start))))
    (_ (let ((end (+ start (length rt))))
	 (cons (substring p start end) end)))))

(defun citeproc-rt-update-from-plain (rt p &optional skip-nocase)
  "Update rich-text RT from plain text P.
The length of the plain text content of RT must not be less than
the length of P. Return the updated rich-text. If optional
SKIP-NOCASE is non-nil then skip spans with the `nocase'
attribute set to non-nil."
  (car (citeproc-rt--update-from-plain-1 rt p 0 skip-nocase)))

(defun citeproc-rt-change-case (rt case-fun)
  "Change the case of rich text RT with CASE-FUN.
CASE-FUN is a function taking a string as its argument and
returning a string of the same length."
  (let ((plain (citeproc-rt-to-plain rt)))
    (citeproc-rt-update-from-plain rt (funcall case-fun plain) t)))

(defun citeproc-rt-pred-counts-tree (rt pred)
  "Return a dominated count tree for rich text RT based on PRED.
The returned tree has the same structure as RT but the car of
each subtree is a number indicating the maximal number of nodes
on any dominated branch for which PRED holds."
  (if (consp rt)
      (let ((children-vals (--map (citeproc-rt-pred-counts-tree it pred)
				  (cdr rt))))
	(cons (-max (cl-mapcar (lambda (rich count)
				 (+ (if (listp count) (car count) count)
				    (if (funcall pred rich) 1 0)))
			       (cdr rt)
			       children-vals))
	      children-vals))
    0))

(defun citeproc-rt-flip-italics (rt)
  "Flip the italic attribute of rich text RT."
  (if (listp rt)
      (cons (if (citeproc-rt-in-italics-p rt)
		(--remove (eq (car it) 'font-style) (car rt))
	      (cons '(font-style . "italic") (car rt)))
	    (cdr rt))
    `(((font-style . "italic")) ,rt)))

(defun citeproc-rt-in-italics-p (rt)
  "Whether rich text RT has italic font style as attribute."
  (and (listp rt)
       (string= (alist-get 'font-style (car rt)) "italic")))

(defun citeproc-rt-italics-flipflop (rt)
  "Return a flipflopped italics version of rich text RT."
  (if (and rt (listp rt))
      (let ((italics-counts-tree
	     (citeproc-rt-pred-counts-tree rt 'citeproc-rt-in-italics-p)))
	(if (> (+ (car italics-counts-tree)
		  (if (citeproc-rt-in-italics-p rt) 1 0))
	       1)
	    (citeproc-rt--italics-flipflop-1 rt italics-counts-tree)
	  rt))
    rt))

(defun citeproc-rt--italics-flipflop-1 (rt italics-counts-tree)
  "Flipflop italics in RT using info from ITALICS-COUNTS-TREE."
  (let ((rt-italic (citeproc-rt-in-italics-p rt)))
    (if (or (not (listp rt))
	    (not (listp italics-counts-tree))
	    (< (+ (car italics-counts-tree)
		  (if rt-italic 1 0))
	       2)) rt
      (if rt-italic
	  (cons (--remove (eq (car it) 'font-style) (car rt))
		(cl-mapcar (lambda (r i)
			     (citeproc-rt--italics-flipflop-1
			      (citeproc-rt-flip-italics r) i))
			   (cdr rt)
			   (cdr italics-counts-tree)))
	(cons (car rt)
	      (cl-mapcar (lambda (r i) (citeproc-rt--italics-flipflop-1 r i))
			 (cdr rt)
			 (cdr italics-counts-tree)))))))

(defun citeproc-rt-from-str (s)
  "Parse a html or plain text string S into rich text."
  (if (and s (s-matches-p "</[[:alnum:]]+>" s))
      (let* ((parsed (citeproc-lib-parse-html-frag s))
	     (body (cddr (cl-caddr parsed)))
	     (stripped (if (eq (caar body) 'p) (cl-cddar body) body)))
	(if (= 1 (length stripped))
	    (citeproc-rt-from-html (car stripped))
	  (cons nil (mapcar 'citeproc-rt-from-html stripped))))
    s))

(defconst citeproc-rt-from-html-alist
  '(((i . nil) . (font-style . "italic"))
    ((b . nil) . (font-weight . "bold"))
    ((span . ((style . "font-variant:small-caps;"))) . (font-variant . "small-caps"))
    ((sc . nil) . (font-variant . "small-caps"))
    ((sup . nil) . (vertical-align . "sup"))
    ((sub . nil) . (vertical-align . "sub"))
    ((span . ((class . "nocase"))) . (nocase . t))
    ((span . ((class . "underline"))) . (text-decoration . "underline")))
  "A mapping from html tags and attrs to rich text attrs.")

(defun citeproc-rt-from-html (h)
  "Convert simple html H to rich text."
  (if (listp h)
      (cons (-if-let (attr (assoc-default (cons (car h) (cadr h))
					  citeproc-rt-from-html-alist))
		(list attr)
	      '(nil))
	    (mapcar #'citeproc-rt-from-html (cddr h)))
    h))

(defun citeproc-rt--cquote-pstns-1 (rt offset)
  "Return closing quote positions in rich text RT with OFFSET.
The positions are in the plain text of RT and only those
positions are returned which are associated with a CSL
`quotes'=\"yes\" attribute."
  (if (listp rt)
      (let ((inner (let ((act-offset offset)
			 pstns)
		     (--each (cdr rt)
		       (-let (((p . next)
			       (citeproc-rt--cquote-pstns-1 it act-offset)))
			 (setq pstns (nconc pstns p)
			       act-offset next)))
		     (cons pstns act-offset))))
	(if (string= (alist-get 'quotes (car rt)) "true")
	    (-let (((inner-pstns . inner-offset) inner))
	      (cons (cons (1- inner-offset) inner-pstns)
		    inner-offset))
	  inner))
    (cons nil (+ offset (length rt)))))

(defun citeproc-rt--cquote-pstns (rt)
  "Return a list of closing quote positions in RT.
The positions are in the plain text of RT and only those
positions are returned which are associated with a CSL
`quotes'=\"yes\" attribute. Numbering starts from 1.
The positions are in decreasing order."
  (sort (car (citeproc-rt--cquote-pstns-1 rt 1)) '>))

(defun citeproc-rt-punct-in-quote (rt)
  "Put commas and periods inside quotes in rich text RT."
  (-if-let (pstns (citeproc-rt--cquote-pstns rt))
      (let ((plain (citeproc-rt-to-plain rt)))
	(citeproc-rt-update-from-plain
	 rt
	 (with-temp-buffer
	   (insert plain)
	   (dolist (pos pstns)
	     (goto-char (1+ pos))
	     (when (memq (char-after) '(?, ?.))
	       (call-interactively 'transpose-chars)))
	   (buffer-string))))
    rt))

(defun citeproc-rt-find-first-node (rt pred)
  "Return the first node of RT for which PRED holds.
Return nil if no such node was found."
  (if (funcall pred rt) rt
    (pcase rt
      ;; process further if internal node with content
      (`(,_ . ,body)
       (let (found)
	 (while (and (not found) body)
	   (setq found (citeproc-rt-find-first-node (car body) pred))
	   (pop body))
	 found))
      ;; leaf or node with no content
      (_ nil))))

(defun citeproc-rt-transform-first (rt pred transform)
  "Apply TRANSFORM to the first node of RT for which PRED is non-nil.
PRED and TRANSFORM are functions taking a rich-text node as their
sole argument. Return a (RESULT . SUCCESS) pair where RESULT is
the resulting rich-text and SUCCESS is non-nil iff the
transformation was successfully carried out (i.e., a node
satisfying PRED was found)."
  (if (funcall pred rt) (cons (funcall transform rt) t)
    (pcase rt
      ;; process further if internal node with content
      (`(,attrs . ,body)
       (let* (success
	      (new-body
	       (--map (if success it
			(-let (((it-res . it-success)
				(citeproc-rt-transform-first it pred transform)))
			  (setq success it-success)
			  it-res))
		      body)))
	 (cons (cons attrs new-body) success)))
      ;; leaf or node with no content
      (_ (cons rt nil)))))

(defun citeproc-rt-add-year-suffix (rt ys)
  "Attempt to add year suffix YS to rich-text RT.
Return an (RT . SUCCESS) pair, where RT is the resulting
rich-text, and SUCCESS is non-nil iff the year-suffix has been
successfully added."
  (cl-flet ((rendered-date-var-p
	     (node)
	     (and (consp node)
		  (memq (alist-get 'rendered-var (car node)) citeproc--date-vars)))
	    (add-suffix
	     (node)
	     (let ((content (cadr node)))
	      (if (equal content "<suppressed-date>")
		  (list (car node) ys)
		(let ((full-ys (if (or (not (stringp content))
				       (s-matches-p "[[:digit:]]$" content))
				   ys
				 (concat "-" ys))))
		 (-snoc node `(((rendered-var . year-suffix)) ,full-ys)))))))
    (citeproc-rt-transform-first rt #'rendered-date-var-p #'add-suffix)))

(defun citeproc-rt-replace-first-names (rt replacement)
  "Replace RT's first name-var content with REPLACEMENT.
Return an (RT . SUCCESS) pair, where RT is the resulting
rich-text, and SUCCESS is non-nil iff the replacement has been
successful."
  (cl-flet ((rendered-name-var-p
	     (node)
	     (and (consp node)
		  (assoc 'rendered-names (car node))))
	    (replace (_node) replacement))
    (citeproc-rt-transform-first rt #'rendered-name-var-p #'replace)))

(defun citeproc-rt-count-names (rt)
  "Return a count of the rendered names in RT."
  (if (consp rt)
      (if (alist-get 'name-id (car rt)) 1
	(apply #'+ (mapcar #'citeproc-rt-count-names (cdr rt))))
    0))

(defun citeproc-rt-cull-spaces-puncts (rt)
  "Remove unnecessary characters from rich-text RT."
  (let* ((plain (citeproc-rt-to-plain rt))
	 (updated (citeproc-rt-update-from-plain
		   rt (citeproc-s-cull-spaces-puncts plain))))
    (citeproc-rt-format updated
			(lambda (x) (replace-regexp-in-string "+" "" x)))))

(defun citeproc-rt-render-affixes (rt &optional shallow)
  "Render the affixes in rich-text RT.
If SHALLOW is non-nil then render only the affixes for the first
level."
  (if (not (consp rt))
      rt
    (-let* (((attrs . contents) rt)
	    (rendered
	     (if shallow	 ; We do the recursive call depending on SHALLOW
		 contents
	       (-non-nil (--map (citeproc-rt-render-affixes it) contents)))))
      (if rendered
	  (let-alist attrs
	    (let ((delimited (if .delimiter
				 (cdr (--mapcat (list .delimiter it) rendered))
			       rendered)))
	      (if (or .suffix .prefix)
		  (let (result
			outer-attrs
			(inner-attrs
			 (citeproc-rt-select-attrs attrs citeproc-rt-format-attrs)))
		    (when .display ; The display attribute should encompass affixes
		      (setq outer-attrs (list (cons 'display .display))
			    inner-attrs (--remove (eq (car it) 'display) inner-attrs)))
		    (when .suffix (push .suffix result))
		    (push (cons inner-attrs
				delimited)
			  result)
		    (when .prefix (push .prefix result))
		    (cons outer-attrs result))
		(cons (citeproc-rt-select-attrs attrs citeproc-rt-format-attrs)
		      delimited))))
	nil))))

(defun citeproc-rt-dedup (rt)
  "Remove duplicate substituted renderings from content RT."
  (car (citeproc-rt--dedup-single rt nil)))

(defun citeproc-rt--dedup-single (rt substs)
  "Remove duplicate subst. var renderings from RT.
SUBSTS contains an initial list of vars to be removed. Return
a (<deduplicated content of RT> <substitued vars in RT> <vars in RT>) list."
  (if (not (consp rt))
      (list rt nil nil)
    (-let* (((attrs . cs) rt)
	    ((&alist 'subst subst
		     'rendered-var var)
	     attrs))
      (if (and var (memq var substs))
	  (list nil nil nil)
	(-let (((new-c s v) (citeproc-rt--dedup-multi cs substs)))
	  (list (cons (--reject (memq (car it) '(subst rendered-vars)) attrs)
		      new-c)
		(if subst
		    (-concat v (when var (list var)))
		  s)
		(-concat v (if var
			       (list var)
			     nil))))))))

(defun citeproc-rt--dedup-multi (cs substs)
  (if cs
      (-let* (((c s1 v1) (citeproc-rt--dedup-single (car cs) substs))
	      ((cs s2 v2) (citeproc-rt--dedup-multi (cdr cs) (-concat substs s1))))
	(list (cons c cs)
	      (-concat s1 s2)
	      (-concat v1 v2)))
    (list nil nil nil)))

(defun citeproc-rt-finalize (rt &optional punct-in-quote)
  "Finalize rich text RT.
If the optional PUNCT-IN-QUOTE is non-nil then put punctuation
inside quotes.

Note: Finalization doesn't include culling, because some
rich-text transformations require the state before culling (e.g.
the replacement of subsequent authors)."
  ;; The first step is to replace the internally used `modifier letter
  ;; apostrophe' characters with the normal `right single quotation marks'
  (citeproc-rt-format (citeproc-rt-simplify-deep
		       (citeproc-rt-italics-flipflop
			(if punct-in-quote (citeproc-rt-punct-in-quote rt) rt)))
		      (lambda (x) (s-replace "ʼ" "’" x))))

(defun citeproc-rt--attr-values (r attr)
  "Return the list of ATTR values in raw rich-text content R.
The values are ordered depth-first."
  (if (listp r)
      (let ((val (alist-get attr (car r)))
	    (body-vals (--mapcat (citeproc-rt--attr-values it attr) (cdr r))))
	(if val (cons val body-vals)
	  body-vals))
    nil))

(defun citeproc-rt-rendered-name-ids (r)
  "Return the list of name ids in raw content R."
  (citeproc-rt--attr-values r 'name-id))

(defun citeproc-rt-rendered-vars (r)
  "Return the list of rendered vars in raw content R."
  (citeproc-rt--attr-values r 'rendered-var))

(defun citeproc-rt-rendered-date-vars (r)
  "Return the list of date vars in raw content R."
  (--select (memq it citeproc--date-vars) (citeproc-rt-rendered-vars r)))

(defun citeproc-rt-rendered-name-vars (r)
  "Return the list of name vars in raw content R."
  (--select (memq it citeproc--name-vars) (citeproc-rt-rendered-vars r)))

;;; Helpers for bibliography rendering

(defun citeproc-rt-max-offset (itemdata)
  "Return the maximal first field width in rich-texts RTS."
  (cl-loop for itd being the hash-values of itemdata
	   when (listp (citeproc-itemdata-rawbibitem itd)) maximize
	   (length (citeproc-rt-to-plain (cadr (citeproc-itemdata-rawbibitem itd))))))

(defun citeproc-rt-subsequent-author-substitute (bib s)
  "Substitute S for subsequent author(s) in BIB.
BIB is a list of bib entries in rich-text format. Return the
modified bibliography."
  (let (prev-author)
    (--map
     (let ((author
	    (citeproc-rt-find-first-node
	     it
	     (lambda (x)
	       (and (consp x) (assoc 'rendered-names (car x)))))))
       (if (equal author prev-author)
	   (car (citeproc-rt-replace-first-names it s))
	 (prog1 it (setq prev-author author))))
     bib)))

(defun citeproc-rt-link-title (r target)
  "Link the rendered title var in rich-text R to TARGET."
  (cl-flet ((rendered-var-title-p
	     (node)
	     (and (consp node)
		  (eq (alist-get 'rendered-var (car node)) 'title)))
	    (add-link
	     (node)
	     (push (cons 'href target) (car node))))
    (citeproc-rt-transform-first r #'rendered-var-title-p #'add-link)))

(defun citeproc-rt-locator-p (r)
  "Return whether rich-text R is a rendered locator."
  (and (consp r) (string= (alist-get 'rendered-var (car r)) "locator")))

(defun citeproc-rt-locator-label-p (r)
  "Return whether rich-text R is a rendered locator label."
  (and (consp r) (alist-get 'rendered-locator-label (car r))))

(defun citeproc-rt-add-locator-label-position (r)
  "Add information about locator-label position in rich-text R.
Return value is one of `label', `locator', `label-first',
`locator-first', `label-only', `locator-only' or nil. This
information is also added to the tree node attributes."
  (let ((result
	 (cond
	  ((not (consp r)) nil)
	  ((citeproc-rt-locator-p r) 'locator)
	  ((citeproc-rt-locator-label-p r) 'label)
	  (t (let ((content (cdr r))
		   first second)
	       (while (and content (not (and first second)))
		 (let* ((cur (pop content))
			(cur-order (citeproc-rt-add-locator-label-position cur)))
		   (pcase cur-order
		     ('label-first (setq first 'label second 'locator))
		     ('locator-first (setq first 'locator second 'label))
		     ((or 'label-only 'label)
		      (if first (setq second 'label)
			(setq first 'label)))
		     ((or 'locator-only 'locator)
		      (if first (setq second 'locator)
			(setq first 'locator))))))
	       (cond
		((not first) nil)
		((not second) (if (eq first 'locator) 'locator-only 'label-only))
		(t (if (eq first 'locator) 'locator-first 'label-first))))))))
    (when result (push (cons 'l-l-pos result) (car r)))
    result))

(defun citeproc-rt-locator-w-label (r)
  "Return locator with label if found from rich-text R.
Return R if no locator or locator label was found."
  (let ((l-l-pos (citeproc-rt-add-locator-label-position r)))
    (if l-l-pos
	(citeproc-rt-locator-w-label-1 r l-l-pos)
      ;; We return the full cite if no locator was found.
      r)))

(defun citeproc-rt-locator-w-label-1 (r l-l-pos)
  "Return locator-label span from rich-text fragment R.
L-L-POS is the global position of locator and label, see the
documentation of `citeproc-rt-add-locator-label-position' for the
possible values."
  (if (or (citeproc-rt-locator-label-p r) (citeproc-rt-locator-p r)) r
    (pcase-let* ((`(,attrs . ,content) r)
		 (local-llpos (alist-get 'l-l-pos attrs)))
      (cons attrs
	    (let (result
		  (n-boundaries (if (or (and (eq l-l-pos 'locator-first)
					     (eq local-llpos 'label-only))
					(and (eq l-l-pos 'label-first)
					     (eq local-llpos 'locator-only)))
				    1	; Fragment starts in a between position.
				  0))) 	; Fragment starts in a before position.
	      (while (and content (< n-boundaries 2))
		(let* ((cur-rt (pop content))
		       (cur-rt-llpos (and (consp cur-rt) (alist-get 'l-l-pos (car cur-rt)))))
		  (cond (cur-rt-llpos
			 ;; Element at boundary
			 (cl-incf n-boundaries
				  (if (or (eq l-l-pos 'locator-only)
					  (memq cur-rt-llpos '(label-first locator-first)))
				      2
				    1))
			 (push (citeproc-rt-locator-w-label-1 cur-rt l-l-pos) result))
			;; Element in between position, simply pushing
			((= n-boundaries 1)
			 (push cur-rt result)))))
	      (nreverse result))))))

(provide 'citeproc-rt)

;;; citeproc-rt.el ends here
