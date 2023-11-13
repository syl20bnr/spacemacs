;;; s.el --- The long lost Emacs string manipulation library. -*- lexical-binding: t -*-

;; Copyright (C) 2012-2022 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Maintainer: Jason Milkins <jasonm23@gmail.com>
;; Version: 1.13.1
;; Keywords: strings

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

;; The long lost Emacs string manipulation library.
;;
;; See documentation on https://github.com/magnars/s.el#functions

;;; Code:

;; Silence byte-compiler
(defvar ucs-normalize-combining-chars)  ; Defined in `ucs-normalize'
(autoload 'slot-value "eieio")

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (declare (pure t) (side-effect-free t))
  (s-trim-left (s-trim-right s)))

(defun s-collapse-whitespace (s)
  "Convert all adjacent whitespace characters to a single space."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string "[ \t\n\r]+" " " s))

(defun s-unindent (s &optional bol)
  "Unindent S which has BOL (beginning of line) indicators.
BOL will default to pipe. You can optionally supply your own."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil)
        (bol (or bol "|")))
   (s-replace-regexp (concat "^[[:space:]]*" (regexp-quote bol)) "" s)))

(defun s-split (separator s &optional omit-nulls)
  "Split S into substrings bounded by matches for regexp SEPARATOR.
If OMIT-NULLS is non-nil, zero-length substrings are omitted.

This is a simple wrapper around the built-in `split-string'."
  (declare (side-effect-free t))
  (save-match-data
    (split-string s separator omit-nulls)))

(defun s-split-up-to (separator s n &optional omit-nulls)
  "Split S up to N times into substrings bounded by matches for regexp SEPARATOR.

If OMIT-NULLS is non-nil, zero-length substrings are omitted.

See also `s-split'."
  (declare (side-effect-free t))
  (save-match-data
    (let ((op 0)
          (r nil))
      (with-temp-buffer
        (insert s)
        (setq op (goto-char (point-min)))
        (while (and (re-search-forward separator nil t)
                    (< 0 n))
          (let ((sub (buffer-substring op (match-beginning 0))))
            (unless (and omit-nulls
                         (equal sub ""))
              (push sub r)))
          (setq op (goto-char (match-end 0)))
          (setq n (1- n)))
        (let ((sub (buffer-substring op (point-max))))
          (unless (and omit-nulls
                       (equal sub ""))
            (push sub r))))
      (nreverse r))))

(defun s-lines (s)
  "Splits S into a list of strings on newline characters."
  (declare (pure t) (side-effect-free t))
  (s-split "\\(\r\n\\|[\n\r]\\)" s))

(defun s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (declare (pure t) (side-effect-free t))
  (mapconcat 'identity strings separator))

(defun s-concat (&rest strings)
  "Join all the string arguments into one string."
  (declare (pure t) (side-effect-free t))
  (apply 'concat strings))

(defun s-prepend (prefix s)
  "Concatenate PREFIX and S."
  (declare (pure t) (side-effect-free t))
  (concat prefix s))

(defun s-append (suffix s)
  "Concatenate S and SUFFIX."
  (declare (pure t) (side-effect-free t))
  (concat s suffix))

(defun s-splice (needle n s)
  "Splice NEEDLE into S at position N.
0 is the beginning of the string, -1 is the end."
  (if (< n 0)
      (let ((left (substring s 0 (+ 1 n (length s))))
            (right (s-right (- -1 n) s)))
        (concat left needle right))
    (let ((left (s-left n s))
          (right (substring s n (length s))))
        (concat left needle right))))


(defun s-repeat (num s)
  "Make a string of S repeated NUM times."
  (declare (pure t) (side-effect-free t))
  (let (ss)
    (while (> num 0)
      (setq ss (cons s ss))
      (setq num (1- num)))
    (apply 'concat ss)))

(defun s-chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (declare (pure t) (side-effect-free t))
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun s-chop-suffixes (suffixes s)
  "Remove SUFFIXES one by one in order, if they are at the end of S."
  (declare (pure t) (side-effect-free t))
  (while suffixes
    (setq s (s-chop-suffix (car suffixes) s))
    (setq suffixes (cdr suffixes)))
  s)

(defun s-chop-prefix (prefix s)
  "Remove PREFIX if it is at the start of S."
  (declare (pure t) (side-effect-free t))
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

(defun s-chop-prefixes (prefixes s)
  "Remove PREFIXES one by one in order, if they are at the start of S."
  (declare (pure t) (side-effect-free t))
  (while prefixes
    (setq s (s-chop-prefix (car prefixes) s))
    (setq prefixes (cdr prefixes)))
  s)

(defun s-shared-start (s1 s2)
  "Returns the longest prefix S1 and S2 have in common."
  (declare (pure t) (side-effect-free t))
  (let ((cmp (compare-strings s1 0 (length s1) s2 0 (length s2))))
    (if (eq cmp t) s1 (substring s1 0 (1- (abs cmp))))))

(defun s-shared-end (s1 s2)
  "Returns the longest suffix S1 and S2 have in common."
  (declare (pure t) (side-effect-free t))
  (let* ((l1 (length s1))
         (l2 (length s2))
         (search-length (min l1 l2))
         (i 0))
    (while (and (< i search-length)
                (= (aref s1 (- l1 i 1)) (aref s2 (- l2 i 1))))
      (setq i (1+ i)))
    ;; If I is 0, then it means that there's no common suffix between
    ;; S1 and S2.
    ;;
    ;; However, since (substring s (- 0)) will return the whole
    ;; string, `s-shared-end' should simply return the empty string
    ;; when I is 0.
    (if (zerop i)
        ""
      (substring s1 (- i)))))

(defun s-chomp (s)
  "Remove one trailing `\\n`, `\\r` or `\\r\\n` from S."
  (declare (pure t) (side-effect-free t))
  (s-chop-suffixes '("\n" "\r") s))

(defun s-truncate (len s &optional ellipsis)
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters
long.

When not specified, ELLIPSIS defaults to ‘...’."
  (declare (pure t) (side-effect-free t))
  (unless ellipsis
    (setq ellipsis "..."))
  (if (> (length s) len)
      (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
    s))

(defun s-word-wrap (len s)
  "If S is longer than LEN, wrap the words with newlines."
  (declare (side-effect-free t))
  (save-match-data
    (with-temp-buffer
      (insert s)
      (let ((fill-column len))
        (fill-region (point-min) (point-max)))
      (buffer-substring (point-min) (point-max)))))

(defun s-center (len s)
  "If S is shorter than LEN, pad it with spaces so it is centered."
  (declare (pure t) (side-effect-free t))
  (let ((extra (max 0 (- len (length s)))))
    (concat
     (make-string (ceiling extra 2) ?\s)
     s
     (make-string (floor extra 2) ?\s))))

(defun s-pad-left (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the left."
  (declare (pure t) (side-effect-free t))
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra (string-to-char padding))
            s)))

(defun s-pad-right (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the right."
  (declare (pure t) (side-effect-free t))
  (let ((extra (max 0 (- len (length s)))))
    (concat s
            (make-string extra (string-to-char padding)))))

(defun s-left (len s)
  "Returns up to the LEN first chars of S."
  (declare (pure t) (side-effect-free t))
  (if (> (length s) len)
      (substring s 0 len)
    s))

(defun s-right (len s)
  "Returns up to the LEN last chars of S."
  (declare (pure t) (side-effect-free t))
  (let ((l (length s)))
    (if (> l len)
        (substring s (- l len) l)
      s)))

(defun s-chop-left (len s)
  "Remove the first LEN chars from S."
  (let ((l (length s)))
    (if (> l len)
        (substring s len l)
      "")))

(defun s-chop-right (len s)
  "Remove the last LEN chars from S."
  (let ((l (length s)))
    (if (> l len)
        (substring s 0 (- l len))
      "")))

(defun s-ends-with? (suffix s &optional ignore-case)
  "Does S end with SUFFIX?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-suffix?'"
  (declare (pure t) (side-effect-free t))
  (let ((start-pos (- (length s) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                s start-pos nil ignore-case)))))

(defun s-starts-with? (prefix s &optional ignore-case)
  "Does S start with PREFIX?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.

Alias: `s-prefix?'. This is a simple wrapper around the built-in
`string-prefix-p'."
  (declare (pure t) (side-effect-free t))
  (string-prefix-p prefix s ignore-case))

(defun s--truthy? (val)
  (declare (pure t) (side-effect-free t))
  (not (null val)))

(defun s-contains? (needle s &optional ignore-case)
  "Does S contain NEEDLE?

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search ignore-case))
    (s--truthy? (string-match-p (regexp-quote needle) s))))

(defun s-equals? (s1 s2)
  "Is S1 equal to S2?

This is a simple wrapper around the built-in `string-equal'."
  (declare (pure t) (side-effect-free t))
  (string-equal s1 s2))

(defun s-less? (s1 s2)
  "Is S1 less than S2?

This is a simple wrapper around the built-in `string-lessp'."
  (declare (pure t) (side-effect-free t))
  (string-lessp s1 s2))

(defun s-matches? (regexp s &optional start)
  "Does REGEXP match S?
If START is non-nil the search starts at that index.

This is a simple wrapper around the built-in `string-match-p'."
  (declare (side-effect-free t))
  (s--truthy? (string-match-p regexp s start)))

(defun s-blank? (s)
  "Is S nil or the empty string?"
  (declare (pure t) (side-effect-free t))
  (or (null s) (string= "" s)))

(defun s-blank-str? (s)
  "Is S nil or the empty string or string only contains whitespace?"
  (declare (pure t) (side-effect-free t))
  (or (s-blank? s) (s-blank? (s-trim s))))

(defun s-present? (s)
  "Is S anything but nil or the empty string?"
  (declare (pure t) (side-effect-free t))
  (not (s-blank? s)))

(defun s-presence (s)
  "Return S if it's `s-present?', otherwise return nil."
  (declare (pure t) (side-effect-free t))
  (and (s-present? s) s))

(defun s-lowercase? (s)
  "Are all the letters in S in lower case?"
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (not (string-match-p "[[:upper:]]" s))))

(defun s-uppercase? (s)
  "Are all the letters in S in upper case?"
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (not (string-match-p "[[:lower:]]" s))))

(defun s-mixedcase? (s)
  "Are there both lower case and upper case letters in S?"
  (let ((case-fold-search nil))
    (s--truthy?
     (and (string-match-p "[[:lower:]]" s)
          (string-match-p "[[:upper:]]" s)))))

(defun s-capitalized? (s)
  "In S, is the first letter upper case, and all other letters lower case?"
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (s--truthy?
     (string-match-p "^[[:upper:]][^[:upper:]]*$" s))))

(defun s-numeric? (s)
  "Is S a number?"
  (declare (pure t) (side-effect-free t))
  (s--truthy?
   (string-match-p "^[0-9]+$" s)))

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defalias 's-replace-regexp 'replace-regexp-in-string)

(defun s--aget (alist key)
  "Get the value of KEY in ALIST."
  (declare (pure t) (side-effect-free t))
  (cdr (assoc-string key alist)))

(defun s-replace-all (replacements s)
  "REPLACEMENTS is a list of cons-cells. Each `car` is replaced with `cdr` in S."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil))
   (replace-regexp-in-string (regexp-opt (mapcar 'car replacements))
                             (lambda (it) (s--aget replacements it))
                             s t t)))

(defun s-downcase (s)
  "Convert S to lower case.

This is a simple wrapper around the built-in `downcase'."
  (declare (side-effect-free t))
  (downcase s))

(defun s-upcase (s)
  "Convert S to upper case.

This is a simple wrapper around the built-in `upcase'."
  (declare (side-effect-free t))
  (upcase s))

(defun s-capitalize (s)
  "Convert S first word's first character to upper and the rest to lower case."
  (declare (side-effect-free t))
  (concat (upcase (substring s 0 1)) (downcase (substring s 1))))

(defun s-titleize (s)
  "Convert in S each word's first character to upper and the rest to lower case.

This is a simple wrapper around the built-in `capitalize'."
  (declare (side-effect-free t))
  (capitalize s))

(defmacro s-with (s form &rest more)
  "Threads S through the forms. Inserts S as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
last item in second form, etc."
  (declare (debug (form &rest [&or (function &rest form) fboundp])))
  (if (null more)
      (if (listp form)
          `(,(car form) ,@(cdr form) ,s)
        (list form s))
    `(s-with (s-with ,s ,form) ,@more)))

(put 's-with 'lisp-indent-function 1)

(defun s-index-of (needle s &optional ignore-case)
  "Returns first index of NEEDLE in S, or nil.

If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search ignore-case))
    (string-match-p (regexp-quote needle) s)))

(defun s-reverse (s)
  "Return the reverse of S."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (multibyte-string-p s)
        (let ((input (string-to-list s))
              output)
          (require 'ucs-normalize)
          (while input
            ;; Handle entire grapheme cluster as a single unit
            (let ((grapheme (list (pop input))))
              (while (memql (car input) ucs-normalize-combining-chars)
                (push (pop input) grapheme))
              (setq output (nconc (nreverse grapheme) output))))
          (concat output))
      (concat (nreverse (string-to-list s))))))

(defun s-match-strings-all (regex string)
  "Return a list of matches for REGEX in STRING.

Each element itself is a list of matches, as per
`match-string'. Multiple matches at the same position will be
ignored after the first."
  (declare (side-effect-free t))
  (save-match-data
    (let ((all-strings ())
          (i 0))
      (while (and (< i (length string))
                  (string-match regex string i))
        (setq i (1+ (match-beginning 0)))
        (let (strings
              (num-matches (/ (length (match-data)) 2))
              (match 0))
          (while (/= match num-matches)
            (push (match-string match string) strings)
            (setq match (1+ match)))
          (push (nreverse strings) all-strings)))
      (nreverse all-strings))))

(defun s-matched-positions-all (regexp string &optional subexp-depth)
  "Return a list of matched positions for REGEXP in STRING.
SUBEXP-DEPTH is 0 by default."
  (declare (side-effect-free t))
  (if (null subexp-depth)
      (setq subexp-depth 0))
  (save-match-data
    (let ((pos 0) result)
      (while (and (string-match regexp string pos)
                  (< pos (length string)))
        (push (cons (match-beginning subexp-depth) (match-end subexp-depth)) result)
        (setq pos (match-end 0)))
      (nreverse result))))

(defun s-match (regexp s &optional start)
  "When the given expression matches the string, this function returns a list
of the whole matching string and a string for each matched subexpressions.
Subexpressions that didn't match are represented by nil elements
in the list, except that non-matching subexpressions at the end
of REGEXP might not appear at all in the list.  That is, the
returned list can be shorter than the number of subexpressions in
REGEXP plus one.  If REGEXP did not match the returned value is
an empty list (nil).

When START is non-nil the search will start at that index."
  (declare (side-effect-free t))
  (save-match-data
    (if (string-match regexp s start)
        (let ((match-data-list (match-data))
              result)
          (while match-data-list
            (let* ((beg (car match-data-list))
                   (end (cadr match-data-list))
                   (subs (if (and beg end) (substring s beg end) nil)))
              (setq result (cons subs result))
              (setq match-data-list
                    (cddr match-data-list))))
          (nreverse result)))))

(defun s-slice-at (regexp s)
  "Slices S up at every index matching REGEXP."
  (declare (side-effect-free t))
  (if (s-blank? s)
      (list s)
    (let (ss)
      (while (not (s-blank? s))
        (save-match-data
          (let ((i (string-match regexp s 1)))
            (if i
                (setq ss (cons (substring s 0 i) ss)
                      s (substring s i))
              (setq ss (cons s ss)
                    s "")))))
      (nreverse ss))))

(defun s-split-words (s)
  "Split S into list of words."
  (declare (side-effect-free t))
  (s-split
   "[^[:word:]0-9]+"
   (let ((case-fold-search nil))
     (replace-regexp-in-string
      "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
      (replace-regexp-in-string "\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)" "\\1 \\2" s)))
   t))

(defun s--mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun s-lower-camel-case (s)
  "Convert S to lowerCamelCase."
  (declare (side-effect-free t))
  (s-join "" (s--mapcar-head 'downcase 'capitalize (s-split-words s))))

(defun s-upper-camel-case (s)
  "Convert S to UpperCamelCase."
  (declare (side-effect-free t))
  (s-join "" (mapcar 'capitalize (s-split-words s))))

(defun s-snake-case (s)
  "Convert S to snake_case."
  (declare (side-effect-free t))
  (s-join "_" (mapcar 'downcase (s-split-words s))))

(defun s-dashed-words (s)
  "Convert S to dashed-words."
  (declare (side-effect-free t))
  (s-join "-" (mapcar 'downcase (s-split-words s))))

(defun s-spaced-words (s)
  "Convert S to spaced words."
  (declare (side-effect-free t))
  (s-join " " (s-split-words s)))

(defun s-capitalized-words (s)
  "Convert S to Capitalized words."
  (declare (side-effect-free t))
  (let ((words (s-split-words s)))
    (s-join " " (cons (capitalize (car words)) (mapcar 'downcase (cdr words))))))

(defun s-titleized-words (s)
  "Convert S to Titleized Words."
  (declare (side-effect-free t))
  (s-join " " (mapcar 's-titleize (s-split-words s))))

(defun s-word-initials (s)
  "Convert S to its initials."
  (declare (side-effect-free t))
  (s-join "" (mapcar (lambda (ss) (substring ss 0 1))
                     (s-split-words s))))

;; Errors for s-format
(progn
  (put 's-format-resolve
       'error-conditions
       '(error s-format s-format-resolve))
  (put 's-format-resolve
       'error-message
       "Cannot resolve a template to values"))

(defun s-format (template replacer &optional extra)
  "Format TEMPLATE with the function REPLACER.

REPLACER takes an argument of the format variable and optionally
an extra argument which is the EXTRA value from the call to
`s-format'.

Several standard `s-format' helper functions are recognized and
adapted for this:

    (s-format \"${name}\" \\='gethash hash-table)
    (s-format \"${name}\" \\='aget alist)
    (s-format \"$0\" \\='elt sequence)

The REPLACER function may be used to do any other kind of
transformation."
  (let ((saved-match-data (match-data)))
    (unwind-protect
        (replace-regexp-in-string
         "\\$\\({\\([^}]+\\)}\\|[0-9]+\\)"
         (lambda (md)
           (let ((var
                  (let ((m (match-string 2 md)))
                    (if m m
                      (string-to-number (match-string 1 md)))))
                 (replacer-match-data (match-data)))
             (unwind-protect
                 (let ((v
                        (cond
                         ((eq replacer 'gethash)
                          (funcall replacer var extra))
                         ((eq replacer 'aget)
                          (funcall 's--aget extra var))
                         ((eq replacer 'elt)
                          (funcall replacer extra var))
                         ((eq replacer 'oref)
                          (funcall #'slot-value extra (intern var)))
                         (t
                          (set-match-data saved-match-data)
                          (if extra
                              (funcall replacer var extra)
                            (funcall replacer var))))))
                   (if v (format "%s" v) (signal 's-format-resolve md)))
               (set-match-data replacer-match-data))))
         template
         ;; Need literal to make sure it works
         t t)
      (set-match-data saved-match-data))))

(defvar s-lex-value-as-lisp nil
  "If `t' interpolate lisp values as lisp.

`s-lex-format' inserts values with (format \"%S\").")

(defun s-lex-fmt|expand (fmt)
  "Expand FMT into lisp."
  (declare (side-effect-free t))
  (list 's-format fmt (quote 'aget)
        (append '(list)
                (mapcar
                 (lambda (matches)
                   (list
                    'cons
                    (cadr matches)
                    `(format
                      (if s-lex-value-as-lisp "%S" "%s")
                      ,(intern (cadr matches)))))
                 (s-match-strings-all "${\\([^}]+\\)}" fmt)))))

(defmacro s-lex-format (format-str)
  "`s-format` with the current environment.

FORMAT-STR may use the `s-format' variable reference to refer to
any variable:

 (let ((x 1))
   (s-lex-format \"x is: ${x}\"))

The values of the variables are interpolated with \"%s\" unless
the variable `s-lex-value-as-lisp' is `t' and then they are
interpolated with \"%S\"."
  (declare (debug (form)))
  (s-lex-fmt|expand format-str))

(defun s-count-matches (regexp s &optional start end)
  "Count occurrences of `regexp' in `s'.

`start', inclusive, and `end', exclusive, delimit the part of `s' to
match.  `start' and `end' are both indexed starting at 1; the initial
character in `s' is index 1.

This function starts looking for the next match from the end of the
previous match.  Hence, it ignores matches that overlap a previously
found match.  To count overlapping matches, use
`s-count-matches-all'."
  (declare (side-effect-free t))
  (save-match-data
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (count-matches regexp (or start 1) (or end (point-max))))))

(defun s-count-matches-all (regexp s &optional start end)
  "Count occurrences of `regexp' in `s'.

`start', inclusive, and `end', exclusive, delimit the part of `s' to
match.  `start' and `end' are both indexed starting at 1; the initial
character in `s' is index 1.

This function starts looking for the next match from the second
character of the previous match.  Hence, it counts matches that
overlap a previously found match.  To ignore matches that overlap a
previously found match, use `s-count-matches'."
  (declare (side-effect-free t))
  (let* ((anchored-regexp (format "^%s" regexp))
         (match-count 0)
         (i 0)
         (narrowed-s (substring s (if start (1- start) 0)
                                  (when end (1- end)))))
    (save-match-data
      (while (< i (length narrowed-s))
        (when (s-matches? anchored-regexp (substring narrowed-s i))
          (setq match-count (1+ match-count)))
        (setq i (1+ i))))
    match-count))

(defun s-wrap (s prefix &optional suffix)
  "Wrap string S with PREFIX and optionally SUFFIX.

Return string S with PREFIX prepended.  If SUFFIX is present, it
is appended, otherwise PREFIX is used as both prefix and
suffix."
  (declare (pure t) (side-effect-free t))
  (concat prefix s (or suffix prefix)))


;;; Aliases

(defalias 's-blank-p 's-blank?)
(defalias 's-blank-str-p 's-blank-str?)
(defalias 's-capitalized-p 's-capitalized?)
(defalias 's-contains-p 's-contains?)
(defalias 's-ends-with-p 's-ends-with?)
(defalias 's-equals-p 's-equals?)
(defalias 's-less-p 's-less?)
(defalias 's-lowercase-p 's-lowercase?)
(defalias 's-matches-p 's-matches?)
(defalias 's-mixedcase-p 's-mixedcase?)
(defalias 's-numeric-p 's-numeric?)
(defalias 's-prefix-p 's-starts-with?)
(defalias 's-prefix? 's-starts-with?)
(defalias 's-present-p 's-present?)
(defalias 's-starts-with-p 's-starts-with?)
(defalias 's-suffix-p 's-ends-with?)
(defalias 's-suffix? 's-ends-with?)
(defalias 's-uppercase-p 's-uppercase?)


(provide 's)
;;; s.el ends here
