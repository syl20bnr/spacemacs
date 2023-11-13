;;; slynk-flex-completion.lisp --- Common Lisp symbol completion routines
;;
;; Authors: João Távora, some parts derivative works of SLIME, by its
;; authors.
;;
(defpackage :slynk-completion
  (:use #:cl #:slynk-api)
  (:export
   #:flex-completions
   #:simple-completions
   #:flex-matches))

;; for testing package-local nicknames
#+sbcl
(defpackage :slynk-completion-local-nicknames-test
  (:use #:cl)
  (:local-nicknames (#:api #:slynk-api)))

(in-package :slynk-completion)


;;; Simple completion
;;;
(defslyfun simple-completions (prefix package)
  "Return a list of completions for the string PREFIX."
  (let ((strings (all-simple-completions prefix package)))
    (list strings (longest-common-prefix strings))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'simple-completions :slynk)
  (export 'simple-completions :slynk))

(defun all-simple-completions (prefix package)
  (multiple-value-bind (name pname intern) (tokenize-symbol prefix)
    (let* ((extern (and pname (not intern)))
	   (pkg (cond ((equal pname "") +keyword-package+)
                      ((not pname) (guess-buffer-package package))
                      (t (guess-package pname))))
	   (test (lambda (sym) (prefix-match-p name (symbol-name sym))))
	   (syms (and pkg (matching-symbols pkg extern test)))
           (strings (loop for sym in syms
                          for str = (unparse-symbol sym)
                          when (prefix-match-p name str) ; remove |Foo|
                          collect str)))
      (format-completion-set strings intern pname))))

(defun matching-symbols (package external test)
  (let ((test (if external
		  (lambda (s)
		    (and (symbol-external-p s package)
			 (funcall test s)))
		  test))
	(result '()))
    (do-symbols (s package)
      (when (funcall test s)
	(push s result)))
    (remove-duplicates result)))

(defun unparse-symbol (symbol)
  (let ((*print-case* (case (readtable-case *readtable*)
                        (:downcase :upcase)
                        (t :downcase))))
    (unparse-name (symbol-name symbol))))

(defun prefix-match-p (prefix string)
  "Return true if PREFIX is a prefix of STRING."
  (not (mismatch prefix string :end2 (min (length string) (length prefix))
                 :test #'char-equal)))

(defun longest-common-prefix (strings)
  "Return the longest string that is a common prefix of STRINGS."
  (if (null strings)
      ""
      (flet ((common-prefix (s1 s2)
               (let ((diff-pos (mismatch s1 s2)))
                 (if diff-pos (subseq s1 0 diff-pos) s1))))
        (reduce #'common-prefix strings))))

(defun format-completion-set (strings internal-p package-name)
  "Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."
  (mapcar (lambda (string) (untokenize-symbol package-name internal-p string))
          (sort strings #'string<)))




;;; Fancy "flex" completion
;;;
(defmacro collecting ((&rest collectors) &body body) ; lifted from uiop
  "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

(defun to-chunks (string indexes)
  "Return chunks of STRING in as specified by INDEXES."
  ;; (to-chunks "farfalhini" '(1 2 3 4))           => ((1 "arfa"))
  ;; (to-chunks "farfalhini" '(1 3 4))             => ((1 "a") (3 "fa"))
  ;; (to-chunks "farfalhini" '(1 2 3 4 5 7 8 9))   => ((1 "arfal") (7 "ini"))
  ;; (to-chunks "farfalhini" '(1 2 3 4 5 6 7 8 9)) => ((1 "arfalhini"))
  (reverse (reduce (lambda (chunk-list number)
                     (let ((latest-chunk (car chunk-list)))
                       (if (and latest-chunk
                                (= (+
                                    (length (second latest-chunk))
                                    (first latest-chunk))
                                   number))
                           (progn (setf (second latest-chunk)
                                        (format nil "~a~c" (second latest-chunk)
                                                (aref string number)))
                                  chunk-list)
                           (cons (list number (format nil "~c" (aref string number)))
                                 chunk-list))))
                   indexes
                   :initial-value nil)))

(defun readably-classify (sym)
  (let* ((translations '((:fboundp . "fn")
                         (:class . "cla")
                         (:typespec . "type")
                         (:generic-function . "generic-fn")
                         (:macro . "macro")
                         (:special-operator . "special-op")
                         (:package . "pak")
                         (:boundp . "var")
                         (:constant . "constant")))
         (classes (slynk::classify-symbol sym))
         (classes (if (some (lambda (m) (member m classes)) '(:generic-function :macro))
                      (delete :fboundp classes)
                      classes))
         (translated (mapcar (lambda (cla) (cdr (assoc cla translations)))
                             classes)))
    (format nil "~{~a~^,~}" translated)))

(defparameter *flex-score-falloff* 1.5
  "The larger the value, the more big index distances are penalized.")

(defparameter *more-qualified-matches* t
  "If non-nil, \"foo\" more likely completes to \"bar:foo\".
Specifically this assigns a \"foo\" on \"bar:foo\" a
higher-than-usual score, as if the package qualifier \"bar\" was
shorter.")

(defun flex-score (string indexes pattern)
  "Score the match of STRING as given by INDEXES.
INDEXES as calculated by FLEX-MATCHES."
  (let* ((first-pattern-colon (and pattern
                                   (position #\: pattern)))
         (index-of-first-pattern-colon (and first-pattern-colon
                                            (elt indexes first-pattern-colon)))
         (first-string-colon)
         (string-length (length string)))
    (cond ((and first-pattern-colon
                (plusp first-pattern-colon))
           ;; If the user included a colon (":") in the pattern, score
           ;; the pre-colon and post-colon parts separately and add
           ;; the resulting halves together. This tends to fare
           ;; slightly better when matching qualified symbols.
           (let ((package-designator-score
                   (flex-score-1 index-of-first-pattern-colon
                                 (subseq indexes 0 first-pattern-colon)))
                 (symbol-name-score
                   (flex-score-1 (- string-length
                                    index-of-first-pattern-colon)
                                 (mapcar (lambda (index)
                                           (- index index-of-first-pattern-colon))
                                         (subseq indexes (1+ first-pattern-colon))))))
             (+ (/ package-designator-score 2)
                (/ symbol-name-score 2))))
          ((and
            *more-qualified-matches*
            (setf first-string-colon (position #\: string))
            (< first-string-colon
               (car indexes)))
           ;; If the user did not include a colon, but the string
           ;; we're matching again does have that colon (we're
           ;; matching a qualified name), and the position of that
           ;; colon happens to be less than the first index, then act
           ;; as if the pre-colon part were actually half the size of
           ;; what it is. This also tends to promote qualified matches
           ;; meant on the symbol-name.
           (let ((adjust (truncate (/ first-string-colon 2))))
             (flex-score-1 (- string-length
                              adjust)
                           (mapcar (lambda (idx)
                                     (- idx adjust))
                                   indexes))))
          (t
           ;; the default: score the whole pattern on the whole
           ;; string.
           (flex-score-1 string-length indexes)))))

(defun flex-score-1 (string-length indexes)
  "Does the real work of FLEX-SCORE.
Given that INDEXES is a list of integer position of characters in a
string of length STRING-LENGTH, say how well these characters
represent that STRING. There is a non-linear falloff with the
distances between the indexes, according to *FLEX-SCORE-FALLOFF*. If
that value is 2, for example, the indices '(0 1 2) on a 3-long
string of is a perfect (100% match,) while '(0 2) on that same
string is a 33% match and just '(1) is a 11% match."
  (float
   (/ (length indexes)
      (* string-length
         (+ 1 (reduce #'+
                      (loop for i from 0
                            for (a b) on `(,-1
                                           ,@indexes
                                           ,string-length)
                            while b
                            collect (expt (- b a 1) *flex-score-falloff*))))))))

(defun flex-matches (pattern string char-test)
  "Return non-NIL if PATTERN flex-matches STRING.
In case of a match, return two values:

A list of non-negative integers which are the indexes of the
characters in PATTERN as found consecutively in STRING. This list
measures in length the number of characters in PATTERN.

A floating-point score. Higher scores for better matches."
  (declare (optimize (speed 3) (safety 0))
           (type simple-string string)
           (type simple-string pattern)
           (type function char-test))
  (let* ((strlen (length string))
         (indexes (loop for char across pattern
                        for from = 0 then (1+ pos)
                        for pos = (loop for i from from below strlen
                                        when (funcall char-test
                                                      (aref string i) char)
                                          return i)
                        unless pos
                          return nil
                        collect pos)))
    (values indexes
            (and indexes
                 (flex-score string indexes pattern)))))

(defun collect-if-matches (collector pattern string symbol)
  "Make and collect a match with COLLECTOR if PATTERN matches STRING.
A match is a list (STRING SYMBOL INDEXES SCORE).
Return non-nil if match was collected, nil otherwise."
  (multiple-value-bind (indexes score)
      (flex-matches pattern string #'char=)
    (when indexes
      (funcall collector
               (list string
                     symbol
                     indexes
                     score)))))

(defun sort-by-score (matches)
  "Sort MATCHES by SCORE, highest score first.

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (sort matches #'> :key #'fourth))

(defun keywords-matching (pattern)
  "Find keyword symbols flex-matching PATTERN.
Return an unsorted list of matches.

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (collecting (collect)
    (and (char= (aref pattern 0) #\:)
         (do-symbols (s +keyword-package+)
           (collect-if-matches #'collect pattern (concatenate 'simple-string ":"
                                                              (symbol-name s))
                               s)))))

(defun accessible-matching (pattern package)
  "Find symbols flex-matching PATTERN accessible without package-qualification.
Return an unsorted list of matches.

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (and (not (find #\: pattern))
       (collecting (collect)
         (let ((collected (make-hash-table)))
           (do-symbols (s package)
             ;; XXX: since DO-SYMBOLS may visit a symbol more than
             ;; once. Read similar note apropos DO-ALL-SYMBOLS in
             ;; QUALIFIED-MATCHING for how we do it.
             (collect-if-matches
              (lambda (thing)
                (unless (gethash s collected)
                  (setf (gethash s collected) t)
                  (funcall #'collect thing)))
              pattern (symbol-name s) s))))))

(defun qualified-matching (pattern home-package)
  "Find package-qualified symbols flex-matching PATTERN.
Return, as two values, a set of matches for external symbols,
package-qualified using one colon, and another one for internal
symbols, package-qualified using two colons.

The matches in the two sets are not guaranteed to be in their final
order, i.e. they are not sorted (except for the fact that
qualifications with shorter package nicknames are tried first).

Matches are produced by COLLECT-IF-MATCHES (which see)."
  (let* ((first-colon (position #\: pattern))
         (starts-with-colon (and first-colon (zerop first-colon)))
         (two-colons (and first-colon (< (1+ first-colon) (length pattern))
                          (eq #\: (aref pattern (1+ first-colon))))))
    (if (and starts-with-colon
             (not two-colons))
        (values nil nil)
        (let* ((package-local-nicknames
                 (slynk-backend:package-local-nicknames home-package))
               (package-local-nicknames-by-package
                 (let ((ret (make-hash-table)))
                   (loop for (short . full) in
                         package-local-nicknames
                         do (push short (gethash (find-package full)
                                                 ret)))
                   ret))
               (nicknames-by-package (make-hash-table)))
          (flet ((sorted-nicknames (package)
                   (or (gethash package nicknames-by-package)
                       (setf (gethash package nicknames-by-package)
                             (sort (append
                                    (gethash package package-local-nicknames-by-package)
                                    (package-nicknames package)
                                    (list (package-name package)))
                                   #'<
                                   :key #'length)))))
            (collecting (collect-external collect-internal)
              (cond
                (two-colons
                 (let ((collected (make-hash-table)))
                   (do-all-symbols (s)
                     (loop
                       with package = (symbol-package s)
                       for nickname in (and package ; gh#226
                                            (sorted-nicknames package))
                       do (collect-if-matches
                           (lambda (thing)
                             ;; XXX: since DO-ALL-SYMBOLS may visit
                             ;; a symbol more than once, we want to
                             ;; avoid double collections.  But
                             ;; instead of marking every traversed
                             ;; symbol in a hash table, we mark just
                             ;; those collected.  We do pay an added
                             ;; price of checking matching duplicate
                             ;; symbols, but the much smaller hash
                             ;; table pays off when benchmarked,
                             ;; because the number of collections is
                             ;; generally much smaller than the
                             ;; total number of symbols.
                             (unless (gethash s collected)
                               (setf (gethash s collected) t)
                               (funcall #'collect-internal thing)))
                           pattern
                           (concatenate 'simple-string
                                        nickname
                                        "::"
                                        (symbol-name s))
                           s)))))
                (t
                 (loop
                   with use-list = (package-use-list home-package)
                   for package in (remove +keyword-package+ (list-all-packages))
                   for sorted-nicknames
                     = (and (not (eq package home-package))
                            (sorted-nicknames package))
                   do (when sorted-nicknames
                        (do-external-symbols (s package)
                          ;;; XXX: This condition is slightly
                          ;;; opinionated.  It says, for example, that
                          ;;; you never want to complete "c:del" to
                          ;;; "cl:delete" or "common-lisp:delete" in
                          ;;; packages that use :CL (a very common
                          ;;; case).
                          (when (or first-colon
                                    (not (member (symbol-package s) use-list)))
                            (loop for nickname in sorted-nicknames
                                  do (collect-if-matches #'collect-external
                                                         pattern
                                                         (concatenate 'simple-string
                                                                      nickname
                                                                      ":"
                                                                      (symbol-name s))
                                                         s))))))))))))))

(defslyfun flex-completions (pattern package-name &key (limit 300))
  "Compute \"flex\" completions for PATTERN given current PACKAGE-NAME.
Returns a list of (COMPLETIONS NIL). COMPLETIONS is a list of
\(STRING SCORE CHUNKS CLASSIFICATION-STRING)."
  (when (plusp (length pattern))
    (list (loop
            with package = (guess-buffer-package package-name)
            with upcasepat = (string-upcase pattern)
            for (string symbol indexes score)
              in
              (loop with (external internal)
                      = (multiple-value-list (qualified-matching upcasepat package))
                    for e in (append (sort-by-score
                                      (keywords-matching upcasepat))
                                     (sort-by-score
                                      (append (accessible-matching upcasepat package)
                                              external))
                                     (sort-by-score
                                      internal))
                    for i upto limit
                    collect e)
            collect
            (list (if (every #'common-lisp:upper-case-p pattern)
                      (string-upcase string)
                      (string-downcase string))
                  score
                  (to-chunks string indexes)
                  (readably-classify symbol)))
          nil)))

(provide :slynk/completion)
