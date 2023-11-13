;;; xml-query.el --- query engine complimenting the xml package

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This provides a very rudimentary, jQuery-like, XML selector
;; s-expression language. It operates on the output of the xml
;; package, such as `xml-parse-region' and `xml-parse-file'. It was
;; written to support Elfeed.

;; See the docstring for `xml-query-all'.

;; The macro forms, `xml-query*' and `xml-query-all*', are an order of
;; magnitude faster, but only work on static selectors and need the
;; namespaces to be pre-stripped.

;; Examples:

;; This query grabs the top-level paragraph content from XHTML.

;;  (xml-query-all '(html body p *) xhtml)

;; This query extracts all the links from an Atom feed.

;;  (xml-query-all '(feed entry link [rel "alternate"] :href) xml)

;;; Code:

(require 'cl-lib)

(defun xml-query-strip-ns (tag)
  "Remove the namespace, if any, from TAG."
  (when (symbolp tag)
    (let ((name (symbol-name tag)))
      (if (cl-find ?\: name)
          (intern (replace-regexp-in-string "^.+:" "" name))
        tag))))

(defun xml-query--tag-all (match xml)
  (cl-loop for (tag attribs . content) in (cl-remove-if-not #'listp xml)
           when (or (eq tag match) (eq (xml-query-strip-ns tag) match))
           collect (cons tag (cons attribs content))))

(defun xml-query--attrib-all (attrib value xml)
  (cl-loop for (tag attribs . content) in (cl-remove-if-not #'listp xml)
           when (equal (cdr (assoc attrib attribs)) value)
           collect (cons tag (cons attribs content))))

(defun xml-query--keyword (matcher xml)
  (cl-loop with match = (intern (substring (symbol-name matcher) 1))
           for (tag attribs . content) in (cl-remove-if-not #'listp xml)
           when (cdr (assoc match attribs))
           collect it))

(defun xml-query--symbol (matcher xml)
  (xml-query--tag-all matcher xml))

(defun xml-query--vector (matcher xml)
  (let ((attrib (aref matcher 0))
        (value (aref matcher 1)))
    (xml-query--attrib-all attrib value xml)))

(defun xml-query--list (matchers xml)
  (cl-loop for matcher in matchers
           append (xml-query-all (if (listp matcher)
                                     matcher
                                   (list matcher)) xml)))

(defun xml-query--append (xml)
  (cl-loop for (tag attribs . content) in (cl-remove-if-not #'listp xml)
           append content))

(defun xml-query--stringp (thing)
  "Return non-nil of THING is a non-blank string."
  (and (stringp thing) (string-match "[^ \t\r\n]" thing)))

(defun xml-query-all (query xml)
  "Given a list of tags, XML, apply QUERY and return a list of
matching tags.

A query is a list of matchers.
 - SYMBOL: filters to matching tags
 - LIST: each element is a full sub-query, whose results are concatenated
 - VECTOR: filters to tags with matching attribute, [tag attrib value]
 - KEYWORD: filters to an attribute value (must be last)
 - * (an asterisk symbol): filters to content strings (must be last)

For example, to find all the 'alternate' link URL in a typical
Atom feed:

  (xml-query-all '(feed entry link [rel \"alternate\"] :href) xml)"
  (if (null query)
      xml
    (cl-destructuring-bind (matcher . rest) query
      (cond
       ((keywordp matcher) (xml-query--keyword matcher xml))
       ((eq matcher '*)
        (cl-remove-if-not #'xml-query--stringp (xml-query--append xml)))
       (:else
        (let ((matches
               (cl-etypecase matcher
                 (symbol (xml-query--symbol matcher xml))
                 (vector (xml-query--vector matcher xml))
                 (list   (xml-query--list   matcher xml)))))
          (cond
           ((null rest) matches)
           ((and (or (symbolp (car rest))
                     (listp (car rest)))
                 (not (keywordp (car rest)))
                 (not (eq '* (car rest))))
            (xml-query-all (cdr query) (xml-query--append matches)))
           (:else (xml-query-all rest matches)))))))))

(defun xml-query (query xml)
  "Like `xml-query-all' but only return the first result."
  (let ((result (xml-query-all query xml)))
    (if (xml-query--stringp result)
        result
      (car (xml-query-all query xml)))))

;; Macro alternatives:

;; This is a slightly less capable alternative with significantly
;; better performance (x10 speedup) that requires a static selector.
;; The selector is compiled into Lisp code via macro at compile-time,
;; which is then carried through to byte-code by the compiler. In
;; byte-code form, the macro performs no function calls other than
;; `throw' in the case of `xml-query*', where it's invoked less than
;; once per evaluation (only on success).

;; Queries are compiled tail-to-head with a result handler at the
;; deepest level. The generated code makes multiple bindings of the
;; variable "v" as it dives deeper into the query, using the layers of
;; bindings as a breadcrumb stack.

;; For `xml-query*', which has a single result, the whole expression
;; is wrapped in a catch, and the first successful match is thrown to
;; it from the result handler.

;; For `xml-query-all*', the result is pushed into an output list.

(defun xml-query--compile-tag (tag subexp subloop-p)
  `(when (and (consp v) (eq (car v) ',tag))
     ,(if subloop-p
          `(dolist (v (cddr v))
             ,subexp)
        subexp)))

(defun xml-query--compile-attrib (pair subexp subloop-p)
  `(let ((value (cdr (assq ',(aref pair 0) (cadr v)))))
     (when (equal value ,(aref pair 1))
       ,(if subloop-p
            `(dolist (v (cddr v))
               ,subexp)
          subexp))))

(defun xml-query--compile-keyword (keyword subexp)
  (let ((attrib (intern (substring (symbol-name keyword) 1))))
    `(let ((v (cdr (assq ',attrib (cadr v)))))
       (when v
         ,subexp))))

(defun xml-query--compile-star (subexp)
  `(when (and (stringp v) (string-match "[^ \t\r\n]" v))
     ,subexp))

(defun xml-query--compile-top (query input subexp)
  (let* ((rquery (reverse query))
         (prev nil))
    (while rquery
      (let ((matcher (pop rquery))
            ;; Should the next item loop over its children?
            (subloop-p (and (not (null prev))
                            (not (keywordp prev))
                            (symbolp prev))))
        (cond
         ((eq '* matcher)
          (setf subexp (xml-query--compile-star subexp)))
         ((keywordp matcher)
          (setf subexp (xml-query--compile-keyword matcher subexp)))
         ((symbolp matcher)
          (setf subexp (xml-query--compile-tag matcher subexp subloop-p)))
         ((vectorp matcher)
          (setf subexp (xml-query--compile-attrib matcher subexp subloop-p)))
         ((error "Bad query: %S" query)))
        (setf prev matcher)))
    `(dolist (v ,input)
       ,subexp)))

(defun xml-query--compile (query input)
  (let ((tag (make-symbol "done")))
    `(catch ',tag
       ,(xml-query--compile-top query input `(throw ',tag v)))))

(defmacro xml-query* (query sexp)
  "Like `xml-query' but generate code to execute QUERY on SEXP.

Unlike `xml-query', QUERY must be a static, compile-time
s-expression. See `xml-query-all*' for more information.

QUERY is *not* evaluated, so it should not be quoted."
  (xml-query--compile query sexp))

(defun xml-query-all--compile (query input)
  (let ((output (make-symbol "output")))
    `(let ((,output ()))
       ,(xml-query--compile-top query input `(push v ,output))
       (nreverse ,output))))

(defmacro xml-query-all* (query sexp)
  "Like `xml-query-all' but generate code to execute QUERY on SEXP.

Unlike `xml-query-all', QUERY must be a static, compile-time
s-expression. This macro compiles the query into actual code. The
result is faster since the query will be compiled into byte-code
rather than \"interpreted\" at run time.

Also unlike `xml-query-all', the parsed XML s-expression must
also have its namespace pre-stripped. This is accomplished by
setting the optional PARSE-NS argument of `xml-parse-region' to
symbol-qnames.

Sub-expression lists are not supported by this macro.

QUERY is *not* evaluated, so it should not be quoted."
  (xml-query-all--compile query sexp))

(provide 'xml-query)

;;; xml-query.el ends here
