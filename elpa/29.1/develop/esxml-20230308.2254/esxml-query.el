;;; esxml-query.el --- select esxml nodes jQuery-style

;; Copyright (C) 2017 Vasilij Schneidermann <mail@vasilij.de>

;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; Maintainer: Vasilij Schneidermann
;; Version: 0.1.1
;; Keywords: data, lisp
;; Package-Requires: ((cl-lib "0.1"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Traditionally people pick one of the following options when faced
;; with the task of extracting data from XML in Emacs Lisp:
;;
;; - Using regular expressions on the unparsed document
;; - Manual tree traversal with `assoc', `car' and `cdr'
;;
;; Browsers faced a similar problem until jQuery happened, shortly
;; afterwards they started providing the `node.querySelector' and
;; `node.querySelectorAll' API for retrieving one or all nodes
;; matching a given CSS selector.  This code implements the same API
;; with the `esxml-query' and `esxml-query-all' functions.  The
;; following table summarizes the currently supported modifiers and
;; combinators:
;;
;; | Name                               | Supported? | Syntax      |
;; |------------------------------------+------------+-------------|
;; | Namespaces                         | No         | foo|bar     |
;; | Commas                             | Yes        | foo, bar    |
;; | Descendant combinator              | Yes        | foo bar     |
;; | Child combinator                   | Yes        | foo>bar     |
;; | Adjacent sibling combinator        | No         | foo+bar     |
;; | General sibling combinator         | No         | foo~bar     |
;; | Universal selector                 | Yes        | *           |
;; | Type selector                      | Yes        | tag         |
;; | ID selector                        | Yes        | #foo        |
;; | Class selector                     | Yes        | .foo        |
;; | Attribute selector                 | Yes        | [foo]       |
;; | Exact match attribute selector     | Yes        | [foo=bar]   |
;; | Prefix match attribute selector    | Yes        | [foo^=bar]  |
;; | Suffix match attribute selector    | Yes        | [foo$=bar]  |
;; | Substring match attribute selector | Yes        | [foo*=bar]  |
;; | Include match attribute selector   | Yes        | [foo~=bar]  |
;; | Dash match attribute selector      | Yes        | [foo|=bar]  |
;; | Attribute selector modifiers       | No         | [foo=bar i] |
;; | Pseudo elements                    | No         | ::foo       |
;; | Pseudo classes                     | No         | :foo        |

;;; Code:

(require 'cl-lib)


;;; CSS selector parsing

;; https://www.w3.org/TR/selectors/#w3cselgrammar
;; https://www.w3.org/TR/selectors4/#grammar
;; https://www.w3.org/TR/2003/WD-css3-syntax-20030813/#detailed-grammar
;; https://www.w3.org/TR/2003/WD-css3-syntax-20030813/#tokenization

;; you might be wondering why I'm using both level 3 and 4 standards,
;; well, the level 3 one has a buggy lexer section whereas level 4
;; omits crucial parser definitions, so both have to be used...

;; TODO: support :not
(defvar esxml--css-selector-token-matchers
  (let* ((h "[0-9a-f]")
         (nl "\n\\|\r\n\\|\r\\|\f")
         (nonascii "[\200-\U0010ffff]")
         (unicode (format "\\\\%s\\{1,6\\}[ \t\r\n\f]?" h))
         (escape (format "\\(?:%s\\)\\|\\\\[ -~\200-\U0010ffff]" unicode))
         (nmstart (format "[a-z_]\\|%s\\|\\(?:%s\\)" nonascii escape))
         (nmchar (format "[a-z0-9_-]\\|%s\\|\\(?:%s\\)" nonascii escape))
         (num "[0-9]+\\|[0-9]*\\.[0-9]+")
         (string1 (format "\"\\(?:[\t !#$%%&(-~]\\|\\\\\\(?:%s\\)\\|'\\|%s\\|\\(?:%s\\)\\)*\"" nl nonascii escape))
         (string2 (format "'\\(?:[\t !#$%%&(-~]\\|\\\\\\(?:%s\\)\\|\"\\|%s\\|\\(?:%s\\)\\)*'" nl nonascii escape))
         (ident (format "[-]?\\(?:%s\\)\\(?:%s\\)*" nmstart nmchar))
         (unit (format "[-]?\\(?:%s\\)\\(?:%s\\)+" nmstart nmchar))
         (name (format "\\(?:%s\\)+" nmchar)))

    `((whitespace . "[ \t\r\n\f]+")
      (string . ,(format "\\(?:%s\\|%s\\)" string1 string2))
      (ident . ,ident)
      (hash . ,(format "#%s" name))
      (function . ,(format "%s(" ident))
      (number . ,num)
      (dimension . ,(format "\\(?:%s\\)%s" num unit))
      (prefix-match . "\\^=")
      (suffix-match . "\\$=")
      (substring-match . "\\*=")
      (include-match . "~=")
      (dash-match . "|=")
      (comma . ",")
      (gt . ">")
      (plus . "\\+")
      (minus . "-")
      (tilde . "~")
      (asterisk . "\\*")
      (period . "\\.")
      (equals . "=")
      (colon . ":")
      (lbracket . "\\[")
      (rbracket . "\\]")
      (rparen . ")"))))

(defun esxml--tokenize-css-selector (string)
  (let (result)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((max-length 0)
              longest)
          (dolist (matcher esxml--css-selector-token-matchers)
            (let ((id (car matcher))
                  (re (cdr matcher)))
              (when (looking-at re)
                (let* ((token (match-string 0))
                       (length (length token)))
                  (when (> length max-length)
                    (setq max-length length)
                    (setq longest (cons id token)))))))
          (when (not longest)
            (error "Invalid token detected: %s"
                   (buffer-substring (point) (point-max))))
          (push longest result)
          (goto-char (+ (point) max-length)))))
    (nreverse result)))

;; the alternative is creating a mutable object with peek/next methods
;; and passing it around, so I chose the one requiring less typing, a
;; dynamically bound variable :<

(defvar esxml--token-stream)

;; TODO: support :not
;; css-selector:
;;   css-selector-list;
;; css-selector-list:
;;   complex-css-selector [ comma whitespace* complex-css-selector ]*;
;; complex-css-selector:
;;   compound-css-selector [ css-combinator compound-css-selector ]* whitespace*;
;; css-combinator:
;;   whitespace+ | whitespace* [ '>' | '+' | '~' ] whitespace*;
;; compound-css-selector:
;;   css-type-selector css-modifier* | css-modifier+;
;; css-type-selector:
;;   IDENT | *;
;; css-modifier:
;;    css-id | css-class | css-attrib | css-pseudo;
;; css-id:
;;   HASH;
;; css-class:
;;   '.' IDENT;
;; css-attrib:
;;   '[' whitespace* css-attrib-name ']'
;;   | '[' whitespace* css-attrib-name css-attrib-match css-attrib-value whitespace* ']';
;; css-attrib-name:
;;   IDENT whitespace*;
;; css-attrib-match:
;;   [ '=' | PREFIX-MATCH | SUFFIX-MATCH | SUBSTRING-MATCH | INCLUDE-MATCH | DASH-MATCH ] whitespace*;
;; css-attrib-value:
;;   IDENT | STRING;
;; css-pseudo:
;;   ':' ':'? [ IDENT | css-functional-pseudo ];
;; css-functional-pseudo:
;;   FUNCTION whitespace* [ css-expression whitespace* ]+ ')';
;; css-expression:
;;   '+' | '-' | DIMENSION | NUMBER | STRING | IDENT

(defun esxml-query-css-escape (string)
  "Returns escaped version of STRING for use in selectors.
The logic used here corresponds to the CSS.escape API as
specified in https://drafts.csswg.org/cssom/#the-css.escape()-method."
  (let (chars)
    (dotimes (i (length string))
      (let* ((char (aref string i))
             (unprintablep (or (and (>= char ?\u0001) (<= char ?\u001f))
                               (= char ?\u007f)))
             (nonasciip (>= char ?\u0080))
             (digitp (and (>= char ?\u0030) (<= char ?\u0039)))
             (upperp (and (>= char ?\u0041) (<= char ?\u005a)))
             (lowerp (and (>= char ?\u0061) (<= char ?\u007a))))
        (cond
         ((= char ?\u0000)
          (push ?\ufffd chars))
         (unprintablep
          (dolist (char (string-to-list (format "\\%x " char)))
            (push char chars)))
         ((and (= i 0) digitp)
          (dolist (char (string-to-list (format "\\%x " char)))
            (push char chars)))
         ((and (= i 1) digitp (= (aref string 0) ?-))
          (dolist (char (string-to-list (format "\\%x " char)))
            (push char chars)))
         ((and (= i 0) (= char ?-) (= (length string) 1))
          (push ?\\ chars)
          (push char chars))
         ((or nonasciip (= char ?-) (= char ?_) digitp upperp lowerp)
          (push char chars))
         (t
          (push ?\\ chars)
          (push char chars)))))
    (concat (nreverse chars))))

(defun esxml--parse-css-identifier (string)
  ;; https://www.w3.org/TR/css-syntax-3/#consume-string-token
  (let* ((code-points (string-to-list string))
         chars
         token)
    (while code-points
      (let ((char (pop code-points)))
        (if (= char ?\\)
            (let ((char (pop code-points)))
              (cond
               ((not char))
               ((= char ?\n))
               ((or (and (>= char ?0) (<= char ?9))
                    (and (>= char ?a) (<= char ?f))
                    (and (>= char ?A) (<= char ?F)))
                (let ((i 0)
                      (hex-chars (list char)))
                  (while (and (< i 5) code-points)
                    (let ((char (car code-points)))
                      (if (or (and (>= char ?0) (<= char ?9))
                              (and (>= char ?a) (<= char ?f))
                              (and (>= char ?A) (<= char ?F)))
                          (push (pop code-points) hex-chars)
                        (setq i 5)))
                    (setq i (1+ i)))
                  (let ((char (car code-points)))
                    (when (and char (= char ?\s))
                      (pop code-points)))
                  (let* ((hex-token (concat (nreverse hex-chars)))
                         (code-point (string-to-number hex-token 16)))
                    (if (or (zerop code-point)
                            (and (>= code-point ?\ud800) (<= code-point ?\udfff))
                            (> code-point ?\U0010ffff))
                        (push ?\ufffd chars)
                      (push code-point chars)))))
               (t ; unspecified: non-hex digit
                (push char chars))))
          (push char chars))))
    (concat (nreverse chars))))

(defun esxml--parse-css-string-literal (string)
  (esxml--parse-css-identifier (substring string 1 -1)))

(defmacro esxml--with-parse-shorthands (&rest body)
  `(cl-macrolet ((peek () '(car esxml--token-stream))
                 (next () '(pop esxml--token-stream))
                 (accept (type) `(and (peek) (eq (car (peek)) ,type)
                                      (cdr (next))))
                 (eat-whitespace () '(while (accept 'whitespace))))
     ,@body))
(def-edebug-spec esxml--with-parse-shorthands (body))

(defun esxml-parse-css-selector (string)
  "Parse CSS selector STRING into a list of alists.
Each alist represents a complex CSS selector.  The result can be
passed to `esxml-query' and `esxml-query-all' as the selector
argument."
  (let* ((esxml--token-stream (esxml--tokenize-css-selector string))
         (result (esxml--parse-css-selector-list)))
    (when esxml--token-stream
      (error "Trailing garbage: %s"
             (mapconcat 'cdr esxml--token-stream "")))
    result))

(defun esxml--parse-css-selector-list ()
  (esxml--with-parse-shorthands
   (let ((first (esxml--parse-complex-css-selector))
         result)
     (when (not first)
       (error "Expected at least one selector"))
     (push first result)

     (while (accept 'comma)
       (eat-whitespace)
       (let ((selector (esxml--parse-complex-css-selector)))
         (when (not selector)
           (error "Expected selector after comma"))
         (push selector result)))
     (nreverse result))))

(defun esxml--parse-complex-css-selector ()
  (esxml--with-parse-shorthands
   (let ((first (esxml--parse-compound-css-selector))
         result done)
     (when first
       (push first result)

       (while (not done)
         (let ((combinator (esxml--parse-css-combinator)))
           (if combinator
               (let ((compound (esxml--parse-compound-css-selector)))
                 (cond
                  (compound
                   (setq result (append (list compound combinator) result)))
                  ;; allow whitespace before comma
                  ((not (eq (car (peek)) 'comma))
                   (error "Trailing combinator"))))
             (setq done t))))
       (nreverse result)))))

(defun esxml--parse-css-combinator ()
  (esxml--with-parse-shorthands
   ;; NOTE: whitespace-surrounded combinators are distinguished from
   ;; whitespace-only ones by checking whether there has been
   ;; whitespace followed by a non-blank combinator
   (let ((leading-whitespace-p (eq (car (peek)) 'whitespace))
         result)
     (eat-whitespace)
     (let ((type (car (peek))))
       (cond
        ((member type '(gt plus tilde))
         (next)
         (cond
          ((eq type 'gt)
           (setq result '((combinator . child))))
          ((eq type 'plus)
           (setq result '((combinator . direct-sibling))))
          ((eq type 'tilde)
           (setq result '((combinator . indirect-sibling)))))
         (eat-whitespace))
        (leading-whitespace-p
         (setq result '((combinator . descendant))))
        (t nil)))
     result)))

(defun esxml--parse-compound-css-selector ()
  (esxml--with-parse-shorthands
   (let ((type-selector (esxml--parse-css-type-selector))
         done
         result)
     ;; NOTE: css-type-selector css-modifier* | css-modifier+; is
     ;; equivalent to: [ css-type-selector | css-modifier ] css-modifier*;
     (if type-selector
         (push type-selector result)
       (let ((modifier (esxml--parse-css-modifier)))
         (if modifier
             (push modifier result)
           ;; NOTE: this allows the trailing combinator error to be thrown
           (setq done t))))

     (while (not done)
       (let ((modifier (esxml--parse-css-modifier)))
         (if modifier
             (push modifier result)
           (setq done t))))
     (when (> (cl-count 'id result :key 'car) 1)
       (error "Only one id selector allowed per compound"))
     (nreverse result))))

(defun esxml--parse-css-type-selector ()
  (esxml--with-parse-shorthands
   (let ((token (peek)))
     (cond
      ((eq (car token) 'ident)
       (next)
       (cons 'tag (intern (esxml--parse-css-identifier (cdr token)))))
      ((eq (car token) 'asterisk)
       (next)
       '(wildcard))
      (t nil)))))

(defun esxml--parse-css-modifier ()
  (or (esxml--parse-css-id)
      (esxml--parse-css-class)
      (esxml--parse-css-attrib)
      (esxml--parse-css-pseudo)))

(defun esxml--parse-css-id ()
  (esxml--with-parse-shorthands
   (let ((value (accept 'hash)))
     (when value
       (cons 'id (substring value 1))))))

(defun esxml--parse-css-class ()
  (esxml--with-parse-shorthands
   (when (accept 'period)
     (let ((value (accept 'ident)))
       (if value
           (cons 'class value)
         (error "Expected identifier after period"))))))

(defun esxml--parse-css-attrib ()
  (esxml--with-parse-shorthands
   (let (result)
     (when (accept 'lbracket)
       (eat-whitespace)
       (let ((name (esxml--parse-css-attrib-name)))
         (when (not name)
           (error "Expected attribute name"))
         (push (cons 'name (esxml--parse-css-identifier name)) result)
         (when (not (accept 'rbracket))
           (let ((match (esxml--parse-css-attrib-match)))
             (when (not match)
               (error "Expected attribute matcher"))
             (let ((value (esxml--parse-css-attrib-value)))
               (when (not value)
                 (error "Expected attribute value"))
               (eat-whitespace)
               (when (not (accept 'rbracket))
                 (error "Unterminated attribute"))
               (push (cons match value) result)))))
       (cons 'attribute (nreverse result))))))

(defun esxml--parse-css-attrib-name ()
  (esxml--with-parse-shorthands
   (let ((name (accept 'ident)))
     (when name
       (eat-whitespace)
       name))))

(defun esxml--parse-css-attrib-match ()
  (esxml--with-parse-shorthands
   (let (result)
     (cond
      ((accept 'equals)
       (setq result 'exact-match))
      ((accept 'prefix-match)
       (setq result 'prefix-match))
      ((accept 'suffix-match)
       (setq result 'suffix-match))
      ((accept 'substring-match)
       (setq result 'substring-match))
      ((accept 'include-match)
       (setq result 'include-match))
      ((accept 'dash-match)
       (setq result 'dash-match)))
     (eat-whitespace)
     result)))

(defun esxml--parse-css-attrib-value ()
  (esxml--with-parse-shorthands
   (let ((token (peek)))
     (cond
      ((eq (car token) 'ident)
       (next)
       (esxml--parse-css-identifier (cdr token)))
      ((eq (car token) 'string)
       (next)
       (esxml--parse-css-string-literal (cdr token)))
      (t nil)))))

(defun esxml--parse-css-pseudo ()
  (esxml--with-parse-shorthands
   (let (result type)
     (when (accept 'colon)
       (if (accept 'colon)
           (setq type 'pseudo-element)
         (setq type 'pseudo-class))
       (let ((functional (esxml--parse-css-functional-pseudo)))
         (if functional
             (if (eq type 'pseudo-class)
                 (let ((value (car functional))
                       (args (cdr functional)))
                   (push (cons 'name (esxml--parse-css-identifier value)) result)
                   (push (cons 'args args) result))
               (error "Pseudo-elements may not have arguments"))
           (let ((value (accept 'ident)))
             (if value
                 (push (cons 'name (esxml--parse-css-identifier value)) result)
               (error "Expected function or identifier")))))
       (cons type (nreverse result))))))

(defun esxml--parse-css-functional-pseudo ()
  (esxml--with-parse-shorthands
   (let ((function (accept 'function))
         result)
     (when function
       (push (substring function 0 -1) result)
       (eat-whitespace)
       (let ((expression (esxml--parse-css-expression))
             done)
         (eat-whitespace)
         (when (not expression)
           (error "Expected at least one expression for function"))
         (push expression result)
         (while (not done)
           (setq expression (esxml--parse-css-expression))
           (if expression
               (progn
                 (push expression result)
                 (eat-whitespace))
             (setq done t))))
       (when (not (accept 'rparen))
         (error "Unterminated function argument list"))
       (nreverse result)))))

(defun esxml--parse-css-expression ()
  (esxml--with-parse-shorthands
   (let ((token (peek)))
     (cond
      ((accept 'plus)
       '(operator . +))
      ((accept 'minus)
       '(operator . -))
      ((eq (car token) 'dimension)
       (next)
       (cons 'dimension (esxml--parse-css-identifier (cdr token))))
      ((eq (car token) 'number)
       (next)
       (cons 'number (string-to-number (cdr token))))
      ((eq (car token) 'string)
       (next)
       (cons 'string (esxml--parse-css-string-literal (cdr token))))
      ((eq (car token) 'ident)
       (next)
       (cons 'ident (esxml--parse-css-identifier (cdr token))))
      (t nil)))))


;;; tree traversal

;; TODO: these helpers should be part of esxml.el
(defun esxml-branch-p (node)
  "Non-nil if NODE refers to an esxml branch."
  (and (listp node)
       (>= (length node) 2)
       (symbolp (car node))
       (listp (cadr node))))

(defun esxml-node-tag (node)
  "Returns the tag of NODE if available."
  (and (esxml-branch-p node)
       (car node)))

(defun esxml-node-attributes (node)
  "Returns the attributes of NODE if available."
  (and (esxml-branch-p node)
       (cadr node)))

(defun esxml-node-attribute (attribute node)
  "Returns the attribute ATTRIBUTE of NODE if available."
  (and (esxml-branch-p node)
       (cdr (assq attribute (cadr node)))))

(defun esxml-node-children (node)
  "Returns the children of NODE if available."
  (and (esxml-branch-p node)
       (nthcdr 2 node)))

(defun esxml-find-node (pred root)
  "Locates a node satisfying PRED starting from ROOT.
Returns the node or nil if none found."
  (if (funcall pred root)
      root
    (cl-some (lambda (node) (esxml-find-node pred node))
             (esxml-node-children root))))

(defun esxml-visit-nodes (function root)
  "Visit nodes by calling FUNCTION on each starting from ROOT."
  (funcall function root)
  (mapc (lambda (node) (esxml-visit-nodes function node))
        (esxml-node-children root)))

(defun esxml-find-nodes (pred root)
  "Locates all nodes satisfying PRED starting from ROOT.
Returns a list of the nodes or nil if none found."
  (let ((acc '()))
    (esxml-visit-nodes
     (lambda (node)
       (when (funcall pred node)
         (push node acc)))
     root)
    (nreverse acc)))

(defun esxml-find-descendant (pred root)
  "Locates a node satisfying PRED starting from ROOT's children.
Returns the node or nil if none found."
  (cl-some (lambda (node) (esxml-find-node pred node))
           (esxml-node-children root)))

(defun esxml-find-descendants (pred root)
  "Locates all nodes satisfying PRED starting from ROOT's children.
Returns a list of the nodes or nil if none found."
  (cl-mapcan (lambda (node) (esxml-find-nodes pred node))
             (esxml-node-children root)))

(defun esxml-find-child (pred root)
  "Locates a node satisfying PRED among ROOT's children.
Returns the node or nil if none found."
  (cl-some (lambda (node) (when (funcall pred node) node))
           (esxml-node-children root)))

(defun esxml-find-children (pred root)
  "Locates all nodes satisfying PRED among ROOT's children.
Returns a list of the nodes or nil if none found."
  (mapcar (lambda (node) (when (funcall pred node) node))
          (esxml-node-children root)))

(defun esxml--node-with-children (node children)
  (let ((tag (esxml-node-tag node))
        (attributes (esxml-node-attributes node)))
    (append (list tag attributes) children)))

(defun esxml--node-with-attributes (node attributes)
  (let ((tag (esxml-node-tag node))
        (children (esxml-node-children node)))
    (append (list tag attributes) children)))

(defun esxml-tree-map (function root)
  "Returns a copy of ROOT with FUNCTION applied to each node."
  (if (esxml-branch-p root)
      (esxml--node-with-children
       (funcall function root)
       (mapcar (lambda (node) (esxml-tree-map function node))
               (esxml-node-children root)))
    (funcall function root)))

(defvar esxml--symbol (make-symbol "id"))

(defun esxml--decorate-tree (root)
  (let ((i 0))
    (esxml-tree-map
     (lambda (node)
       (let ((attribute (cons esxml--symbol i))
             (attributes (esxml-node-attributes node)))
         (setq attributes (append (list attribute) attributes))
         (setq i (1+ i))
         (if (esxml-branch-p node)
             (esxml--node-with-attributes node attributes)
           node)))
     root)))

(defun esxml--undecorate-node (node)
  (if (esxml-branch-p node)
      (let ((attributes (esxml-node-attributes node)))
        (esxml--node-with-attributes node (assq-delete-all esxml--symbol
                                                           attributes)))
    node))

(defun esxml--retrieve-decoration (node)
  (esxml-node-attribute esxml--symbol node))


;;; querying

;; NOTE: supporting structural pseudo functions, direct siblings and
;; indirect siblings requires breadth instead of depth traversal,
;; something that could be emulated without zippers if you had the
;; parent of the node (and the position of the child)...

(defun esxml--node-matches-attribute-p (node modifier)
  (let ((attributes (esxml-node-attributes node))
        haystack)
    (cl-every
     (lambda (item)
       (let ((type (car item))
             (value (cdr item)))
         (cond
          ((eq type 'name)
           (let ((match (assq (intern value) attributes)))
             (setq haystack (cdr match))
             match))
          ((eq type 'exact-match)
           (equal haystack value))
          ((eq type 'prefix-match)
           (string-prefix-p value haystack))
          ((eq type 'suffix-match)
           (string-suffix-p value haystack))
          ((eq type 'substring-match)
           (string-match-p (regexp-quote value) haystack))
          ((eq type 'include-match)
           (member value (split-string haystack " ")))
          ((eq type 'dash-match)
           (or (equal value haystack)
               (string-match-p (format "^%s-" (regexp-quote value)) haystack)))
          (t (error "Unknown attribute modifier")))))
     modifier)))

(defun esxml--node-matches-modifier-p (node type value)
  (cond
   ((eq type 'wildcard)
    t)
   ((eq type 'tag)
    (equal (esxml-node-tag node) value))
   ((eq type 'id)
    (equal (esxml-node-attribute 'id node) value))
   ((eq type 'class)
    (let ((class (esxml-node-attribute 'class node)))
      (and class (member value (split-string class " ")))))
   ((eq type 'attribute)
    (esxml--node-matches-attribute-p node value))
   ;; TODO: support structural pseudo functions
   ;; TODO: error out on invalid pseudo-class arguments
   (t (error "Unimplemented attribute type: %s" type))))

(defun esxml--find-node-for (attributes)
  (lambda (node)
    (cl-every
     (lambda (attribute)
       (let ((type (car attribute))
             (value (cdr attribute)))
         (esxml--node-matches-modifier-p node type value)))
     attributes)))

(defun esxml--find-nodes (root combinator attributes)
  (let* ((type (cdr (assq 'combinator combinator)))
         (walker (cond
                 ((not type)
                  'esxml-find-nodes)
                 ((eq type 'descendant)
                  'esxml-find-descendants)
                 ((eq type 'child)
                  'esxml-find-children)
                 ;; TODO: support direct sibling
                 ;; TODO: support indirect sibling
                 (t (error "Unimplemented combinator %s" combinator)))))
    (funcall walker (esxml--find-node-for attributes) root)))

(defun esxml--query (selector root)
  (let* ((attributes (pop selector))
         combinator
         (result (esxml--find-nodes root nil attributes)))
    (while (and result selector)
      (setq combinator (pop selector))
      (setq attributes (pop selector))
      (setq result (cl-mapcan
                    (lambda (node)
                      (esxml--find-nodes node combinator attributes))
                    result))
      (setq result (delq nil result)))
    result))

(defun esxml--delete-dups (items test)
  (let ((seen (make-hash-table :test test))
        result)
    (while items
      (let ((item (pop items)))
        (when (not (gethash item seen))
          (push item result)
          (puthash item t seen))))
    (nreverse result)))

(defun esxml-query-all (selector root)
  "Locates all nodes satisfying SELECTOR starting from ROOT.
SELECTOR must be a string containing a CSS selector or a parsed
CSS selector returned by `esxml-parse-css-selector'.  Returns a
list of the nodes or nil if none found."
  (when (stringp selector)
    (setq selector (esxml-parse-css-selector selector)))
  (if (= (length selector) 1)
      ;; no commas, we can only get the same nodes repeatedly
      (esxml--delete-dups (esxml--query (car selector) root) 'eq)
    ;; commas, nodes might be the same *and* in the wrong order
    (setq root (esxml--decorate-tree root))
    (let (result)
      (while selector
        (setq result (nconc result (esxml--query (pop selector) root))))
      (setq result (cl-sort result '< :key 'esxml--retrieve-decoration))
      (setq result (cl-delete-duplicates result :test '=
                                         :key 'esxml--retrieve-decoration))
      (mapcar (lambda (node) (esxml--undecorate-node node)) result))))

(defun esxml-query (selector root)
  "Locates a node satisfying SELECTOR starting from ROOT.
SELECTOR must be a string containing a CSS selector or a parsed
CSS selector returned by `esxml-parse-css-selector'.  Returns the
node or nil if none found."
  ;; NOTE: you can do a bit less work (the savings decrease the more
  ;; branches the query discards), but it's simpler and safer to just
  ;; have the same algorithm for both entry points
  (car (esxml-query-all selector root)))

(provide 'esxml-query)
;;; esxml-query.el ends here
