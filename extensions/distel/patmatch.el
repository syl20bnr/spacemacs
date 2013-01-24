;; -*- comment-column: 32 -*-

(eval-when-compile (require 'cl))

(put 'mcase 'lisp-indent-function 1)
(put 'pmatch 'lisp-indent-function 2)
(put 'mlet 'lisp-indent-function 2)

(defmacro mcase (object &rest clauses)
  "Pattern-matching case expression.
The syntax is like the normal `case':

  (mcase EXPR
    (PATTERN . BODY)
    ...)

The body of the first matching pattern is executed, with pattern
variables bound to their matching values. If no patterns match, an
error is signaled.

See `mlet' for a description of pattern syntax."
  `(mcase* ,object ,(mcase-parse-clauses clauses)))

(eval-and-compile
(defun mcase-parse-clauses (clauses)
  `(list ,@(mapcar #'(lambda (clause)
		       `(list ',(car clause)
			      (lambda () ,@(cdr clause))))
		   clauses))))

(defmacro pmatch (&rest args)
  "Deprecated; see `mlet'."
  `(mlet ,@args))

(defmacro mlet (pattern object &rest body)
  "Match PATTERN with OBJECT, and execute BODY with all bindings.
The pattern syntax is:

Trivial: t, nil, 42
  Testing with `equal'
Pattern variable: x, my-variable
  Variable that the pattern should bind. If the same variable
  appears several times in a pattern, then all of its bindings must
  match.
  Within the body of a successful pattern match, lisp variables are
  bound for all pattern variables.
Constant: 'symbol, '(1 2 3), ...
  Quoted constant, matched with `equal'.
Bound variable: ,var
  Pre-bound Lisp variable, matched by value.
Wild card: _ (underscore)
  Matches anything, with no binding.
Sequence: (pat1 ...), [pat1 ...]
  Matches the \"shape\" of the pattern, as well as each individual
  subpattern."
  (let ((var (make-symbol "var")))
    `(let ((,var ,object))	; so that we just eval `object' once
       (mcase ,var
	 (,pattern ,@body)
	 (_        (signal 'erl-exit-signal
			   (list (tuple 'badmatch ',pattern ,var))))))))

(defun mcase* (object clauses)
  (let ((clause (mcase-choose object clauses)))
    (if clause
	(funcall clause)
      (signal 'erl-exit-signal '(case-clause)))))

(defun mcase-choose (object clauses)
  (if (null clauses)
      nil
    (let* ((clause  (car clauses))
	   (pattern (car clause))
	   (action  (cadr clause))
	   (result  (patmatch pattern object)))
      (if (eq result 'fail)
	  (mcase-choose object (cdr clauses))
	`(lambda ()
	   (let ,(alist-to-letlist result)
	     (funcall ,action)))))))

(defun alist-to-letlist (alist)
  "Convert an alist into `let' binding syntax, eg: ((A . B)) => ((A 'B))"
  (mapcar (lambda (cell)
	    (list (car cell) (list 'quote (cdr cell))))
	  alist))

(defun pmatch-tail (seq)
  (if (consp seq)
      (cdr seq)
    (let ((new (make-vector (1- (length seq)) nil)))
      (dotimes (i (length new))
	(aset new i (aref seq (1+ i))))
      new)))

(defun patmatch (pattern object &optional bindings)
  "Match OBJECT with PATTERN, and return an alist of bindings."
  (if (eq bindings 'fail)
      'fail
    (cond ((pmatch-wildcard-p pattern)
	   bindings)
	  ((pmatch-constant-p pattern) ; '(x)
	   (pmatch-constant pattern object bindings))
	  ((pmatch-bound-var-p pattern)	; ,foo
	   (pmatch-match-var pattern object bindings))
	  ((pmatch-unbound-var-p pattern) ; foo
	   (pmatch-bind-var pattern object bindings))
	  ((pmatch-trivial-p pattern) ; nil, t, any-symbol
	   (if (equal pattern object) bindings 'fail))
          ((consp pattern)
           (if (consp object)
               (patmatch (cdr pattern) (cdr object)
                         (patmatch (car pattern) (car object) bindings))
             'fail))
          ((vectorp pattern)
           (if (and (vectorp object)
                    (= (length pattern) (length object)))
               (patmatch (coerce pattern 'list) (coerce object 'list) bindings)
             'fail))
	  (t
	   'fail))))

(defun pmatch-wildcard-p (pat)
  (eq pat '_))

(defun pmatch-trivial-p (pat)
  "Test for patterns which can always be matched literally with `equal'."
  (or (numberp pat)
      (equal pat [])
      (equal pat nil)
      (equal pat t)))

(defun pmatch-constant-p (pat)
  "Test for (quoted) constant patterns.
Example: (QUOTE QUOTE)"
  (and (consp pat)
       (= (length pat) 2)
       (eq (car pat) 'quote)))

(defun pmatch-constant-value (pat)
  "The value of a constant pattern.
(QUOTE X) => X"
  (cadr pat))

(defun pmatch-constant (pat object bindings)
  "Match OBJECT with the constant pattern PAT."
  (if (equal (pmatch-constant-value pat) object)
      bindings
    'fail))

(defun pmatch-unbound-var-p (obj)
  "Unbound variable is any symbol except nil or t."
  (and (symbolp obj)
       (not (eq obj nil))
       (not (eq obj t))))

(defun pmatch-unbound-var-symbol (sym)
  sym)

(defun pmatch-bind-var (pat object bindings)
  "Add a binding of pattern variable VAR to OBJECT in BINDINGS."
  (if (eq object erl-tag)
      ;; `erl-tag' cannot bind to a variable; this is to prevent pids
      ;; or ports from matching tuple patterns.
      'fail
    (let* ((var (pmatch-unbound-var-symbol pat))
	   (binding (assoc var bindings)))
      (cond ((null binding)
	     (acons var object bindings))
	    ((equal (cdr binding) object)
	     bindings)
	    (t
	     'fail)))))

(eval-when-compile (defvar pattern)) ; dynamic

(defun pmatch-match-var (var object bindings)
  "Match the value of the Lisp variable VAR with OBJECT."
  (if (equal (symbol-value (pmatch-bound-var-name pattern)) object)
      bindings
    'fail))

(defun pmatch-bound-var-p (obj)
  (and (symbolp obj)
       (eq (elt (symbol-name obj) 0) ?,)))

(defun pmatch-bound-var-name (sym)
  (intern (substring (symbol-name sym) 1)))

(defun pmatch-alist-keysort (alist)
  (sort alist (lambda (a b)
		(string< (symbol-name (car a))
			 (symbol-name (car b))))))

;;; Test suite

(defun pmatch-expect (pattern object expected)
  "Assert that matching PATTERN with OBJECT yields EXPECTED.
EXPECTED is either 'fail or a list of bindings (in any order)."
  (let ((actual (patmatch pattern object)))
    (if (or (and (eq actual 'fail)
		 (eq actual expected))
	    (and (listp expected)
		 (listp actual)
		 (equal (pmatch-alist-keysort actual)
			(pmatch-alist-keysort expected))))
	t
      (error "Patmatch: %S %S => %S, expected %S"
	     pattern object actual expected))))

(defun pmatch-test ()
  "Test the pattern matcher."
  (interactive)
  (pmatch-expect t t ())
  (pmatch-expect '(t nil 1) '(t nil 1) ())
  (let ((foo 'foo))
    (pmatch-expect '(FOO ,foo 'foo [FOO]) '(foo foo foo [foo])
		   '((FOO . foo))))
  (pmatch-expect 1 2 'fail)
  (pmatch-expect '(x x) '(1 2) 'fail)
  (pmatch-expect '_ '(1 2) 'nil)
  (assert (equal 'yes
		 (mcase '(call 42 lists length ((1 2 3)))
		   (t 'no)
		   (1 'no)
		   ((call Ref 'lists 'length (_))
		    'yes)
		   (_ 'no))))
  (message "Smooth sailing"))

(provide 'patmatch)

