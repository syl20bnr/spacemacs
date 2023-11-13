;;; parsebib.el --- A library for parsing bib files  -*- lexical-binding: t -*-

;; Copyright (c) 2014-2022 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2014
;; Version: 4.3
;; Keywords: text bibtex
;; URL: https://github.com/joostkremers/parsebib
;; Package-Requires: ((emacs "25.1"))

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;

;;; Code:

(require 'bibtex)
(require 'cl-lib)
(eval-when-compile (require 'subr-x)) ; for `string-join'.
(eval-and-compile (unless (fboundp 'json-parse-buffer)
                    (require 'json)
                    (defvar json-object-type)))

(declare-function json-read "json.el")

(define-error 'parsebib-entry-type-error "[Parsebib] Illegal entry type at point" 'error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BibTeX / biblatex parser ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar parsebib-hashid-fields nil
  "List of fields used to create a hash id for each entry.
Hash ids can only be created for BibTeX/biblatex files.  The hash
id is stored in the entry in the special field `=hashid='.")

(defvar parsebib--biblatex-inheritances '(;; Source                        Target
                                          ("all"                           "all"
                                           (("ids"                         . none)
                                            ("crossref"                    . none)
                                            ("xref"                        . none)
                                            ("entryset"                    . none)
                                            ("entrysubtype"                . none)
                                            ("execute"                     . none)
                                            ("label"                       . none)
                                            ("options"                     . none)
                                            ("presort"                     . none)
                                            ("related"                     . none)
                                            ("relatedoptions"              . none)
                                            ("relatedstring"               . none)
                                            ("relatedtype"                 . none)
                                            ("shorthand"                   . none)
                                            ("shorthandintro"              . none)
                                            ("sortkey"                     . none)))

                                          ;; Source                        Target
                                          ("mvbook, book"                  "inbook, bookinbook, suppbook"
                                           (("author"                      . "author")
                                            ("author"                      . "bookauthor")))

                                          ;; Source                        Target
                                          ("mvbook"                        "book, inbook, bookinbook, suppbook"
                                           (("title"                       . "maintitle")
                                            ("subtitle"                    . "mainsubtitle")
                                            ("titleaddon"                  . "maintitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("mvcollection, mvreference"     "collection, reference, incollection, inreference, suppcollection"
                                           (("title"                       . "maintitle")
                                            ("subtitle"                    . "mainsubtitle")
                                            ("titleaddon"                  . "maintitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("mvproceedings"                 "proceedings, inproceedings"
                                           (("title"                       . "maintitle")
                                            ("subtitle"                    . "mainsubtitle")
                                            ("titleaddon"                  . "maintitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("book"                          "inbook, bookinbook, suppbook"
                                           (("title"                       . "booktitle")
                                            ("subtitle"                    . "booksubtitle")
                                            ("titleaddon"                  . "booktitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("collection, reference"         "incollection, inreference, suppcollection"
                                           (("title"                       . "booktitle")
                                            ("subtitle"                    . "booksubtitle")
                                            ("titleaddon"                  . "booktitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("proceedings"                   "inproceedings"
                                           (("title"                       . "booktitle")
                                            ("subtitle"                    . "booksubtitle")
                                            ("titleaddon"                  . "booktitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("periodical"                    "article, suppperiodical"
                                           (("title"                       . "journaltitle")
                                            ("subtitle"                    . "journalsubtitle")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none))))

  "Inheritance scheme for BibLaTeX cross-referencing.
Inheritances are specified for pairs of source and target entry
type, where the target is the cross-referencing entry and the
source the cross-referenced entry.  Each pair specifies the
fields in the source and the fields in the target that they
correspond with.

Inheritances valid for all entry types are defined by specifying
the entry type as \"all\".  The entry type may also be a
comma-separated list of entry types.

If no inheritance rule is set up for a given entry type+field
combination, the field inherits from the same-name field in the
cross-referenced entry.  If no inheritance should take place, the
target field is set to the symbol `none'.")

;; Regexes describing BibTeX identifiers and keys.  Note that while $ ^ & are
;; valid in BibTeX keys, they may nonetheless be problematic, because they are
;; special for TeX.  The difference between `parsebib--bibtex-identifier' and
;; `parsebib--key-regexp' are the parentheses (), which are valid in keys.  It may in
;; fact not be necessary (or desirable) to distinguish the two, but until
;; someone complains, I'll keep it this way.
(defconst parsebib--bibtex-identifier "[^\"@\\#%',={}() \t\n\f]+" "Regexp describing a licit BibTeX identifier.")
(defconst parsebib--key-regexp "[^\"@\\#%',={} \t\n\f]+" "Regexp describing a licit key.")
(defconst parsebib--entry-start "^[ \t]*@" "Regexp describing the start of an entry.")

(defun parsebib--convert-tex-italics (str)
  "Return STR with face property `italic'."
  (propertize str 'face 'italic))

(defun parsebib--convert-tex-bold (str)
  "Return STR with face property `bold'."
  (propertize str 'face 'bold))

(defun parsebib--convert-tex-small-caps (str)
  "Return STR capitalised."
  (upcase str))

(defvar parsebib-TeX-command-replacement-alist
  '(("ddag" . "\N{DOUBLE DAGGER}")
    ("textdaggerdbl" . "\N{DOUBLE DAGGER}")
    ("dag" . "\N{DAGGER}")
    ("textdagger" . "\N{DAGGER}")
    ("textpertenthousand" . "\N{PER TEN THOUSAND SIGN}")
    ("textperthousand" . "\N{PER MILLE SIGN}")
    ("textquestiondown" . "\N{INVERTED QUESTION MARK}")
    ("P" . "\N{PILCROW SIGN}")
    ("textdollar" . "$")
    ("S" . "\N{SECTION SIGN}")
    ("ldots" . "\N{HORIZONTAL ELLIPSIS}")
    ("dots" . "\N{HORIZONTAL ELLIPSIS}")
    ("textellipsis" . "\N{HORIZONTAL ELLIPSIS}")
    ("textemdash" . "\N{EM DASH}")
    ("textendash" . "\N{EN DASH}")

    ;; Non-ASCII Letters (Excluding Accented Letters)
    ("AA" . "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}")
    ("AE" . "\N{LATIN CAPITAL LETTER AE}")
    ("DH" . "\N{LATIN CAPITAL LETTER ETH}")
    ("DJ" . "\N{LATIN CAPITAL LETTER ETH}")
    ("L"  . "\N{LATIN CAPITAL LETTER L WITH STROKE}")
    ("SS" . "\N{LATIN CAPITAL LETTER SHARP S}")
    ("NG" . "\N{LATIN CAPITAL LETTER ENG}")
    ("OE" . "\N{LATIN CAPITAL LIGATURE OE}")
    ("O"  . "\N{LATIN CAPITAL LETTER O WITH STROKE}")
    ("TH" . "\N{LATIN CAPITAL LETTER THORN}")

    ("aa" . "\N{LATIN SMALL LETTER A WITH RING ABOVE}")
    ("ae" . "\N{LATIN SMALL LETTER AE}")
    ("dh" . "\N{LATIN SMALL LETTER ETH}")
    ("dj" . "\N{LATIN SMALL LETTER ETH}")
    ("l"  . "\N{LATIN SMALL LETTER L WITH STROKE}")
    ("ss" . "\N{LATIN SMALL LETTER SHARP S}")
    ("ng" . "\N{LATIN SMALL LETTER ENG}")
    ("oe" . "\N{LATIN SMALL LIGATURE OE}")
    ("o"  . "\N{LATIN SMALL LETTER O WITH STROKE}")
    ("th" . "\N{LATIN SMALL LETTER THORN}")

    ("ij" . "ij")
    ("i" . "\N{LATIN SMALL LETTER DOTLESS I}")
    ("j" . "\N{LATIN SMALL LETTER DOTLESS J}")
    ;; Formatting Commands
    ("textit" . parsebib--convert-tex-italics)
    ("emph"   . parsebib--convert-tex-italics)
    ("textbf" . parsebib--convert-tex-bold)
    ("textsc" . parsebib--convert-tex-small-caps))
  "An alist of <command>-<replacement> pairs for LaTeX commands.
<command> is the name of a TeX or LaTeX command (without
backslash), <replacement> is the string with which it is
replaced.

<replacement> can also be a function of one argument.  In this
case, <command> must take at least one obligatory argument, which
is passed as the first argument of the replacement function.  The
return value of this function is used as the replacement string
for <command>.

See `parsebib-TeX-markup-replacement-alist' and the function
`parsebib-clean-TeX-markup' to see how this variable is used.")

(defvar parsebib-TeX-accent-replacement-alist
  '(("\"" . "\N{COMBINING DIAERESIS}")
    ("'" . "\N{COMBINING ACUTE ACCENT}")
    ("." . "\N{COMBINING DOT ABOVE}")
    ("=" . "\N{COMBINING MACRON}")
    ("^" . "\N{COMBINING CIRCUMFLEX ACCENT}")
    ("`" . "\N{COMBINING GRAVE ACCENT}")
    ("b" . "\N{COMBINING MACRON BELOW}")
    ("c" . "\N{COMBINING CEDILLA}")
    ("d" . "\N{COMBINING DOT BELOW}")
    ("H" . "\N{COMBINING DOUBLE ACUTE ACCENT}")
    ("k" . "\N{COMBINING OGONEK}")
    ("U" . "\N{COMBINING DOUBLE VERTICAL LINE ABOVE}")
    ("u" . "\N{COMBINING BREVE}")
    ("v" . "\N{COMBINING CARON}")
    ("~" . "\N{COMBINING TILDE}")
    ("|" . "\N{COMBINING COMMA ABOVE}")
    ("f" . "\N{COMBINING INVERTED BREVE}")
    ("G" . "\N{COMBINING DOUBLE GRAVE ACCENT}")
    ("h" . "\N{COMBINING HOOK ABOVE}")
    ("C" . "\N{COMBINING DOUBLE GRAVE ACCENT}")
    ("r" . "\N{COMBINING RING ABOVE}") )
"Alist of <command>-<accent> pairs for LaTeX diacritics.
<command> is the name of a TeX or LaTeX command (without
backslash), <accent> is the Unicode combining character for the
diacritic that <command> generates.  Both <command> and <accent>
must be strings.

The replacement string for <command> is composed of its
obligatory argument (usually a single character) and the
combining diacritic.

See `parsebib-TeX-markup-replacement-alist' and the function
`parsebib-clean-TeX-markup' to see how this variable is used.")

(defvar parsebib-TeX-literal-replacement-alist
  ;; LaTeX2 Escapable "Special" Characters
  `(("\\%" . "%") ("\\&" . "&") ("\\#" . "#") ("\\$" . "$")
    ;; Quotes
    ("``" . "\N{LEFT DOUBLE QUOTATION MARK}")
    ("`"  . "\N{LEFT SINGLE QUOTATION MARK}")
    ("''" . "\N{RIGHT DOUBLE QUOTATION MARK}")
    ("'"  . "\N{RIGHT SINGLE QUOTATION MARK}")
    ;; Dashes
    ("---" . "\N{EM DASH}")
    ("--" . "\N{EN DASH}")
    ;; Remove all remaining {braces}
    ("{" . "") ("}" . ""))
  "Alist of <literal>-<replacement> pairs.  Both are strings.
This variable contains characters that are special in LaTeX and
single-character, non-ASCII LaTeX commands.

Note that adding pairs to this variable has no effect unless
`parsebib-TeX-markup-replacement-alist' is adjusted accordingly.
For example, after adding a <literal>-<replacement> pair, the
following code will ensure that <literal> gets replaced with
<replacement>.

  (cl-callf (lambda (regex) (rx (or <literal> (regexp regex))))
     (alist-get (quote parsebib--replace-literal)
                parsebib-TeX-markup-replacement-alist))

See `parsebib-TeX-markup-replacement-alist' and the function
`parsebib-clean-TeX-markup' to see how this variable is used.")

(defvar parsebib-TeX-markup-replacement-alist
  `((parsebib--replace-command-or-accent
     ;; This regexp matches any latex command i.e. anything that
     ;; starts with a backslash. The name of the command which
     ;; is either a string of alphabetic characters or a single
     ;; non-alphabetic character is captured by group 1. The command
     ;; can have a mandatory argument enclosed by braces which is
     ;; captured by group 2. If the command has no arguments in
     ;; brackets or braces, the first non-white space letter after
     ;; the command is captured in group 3. This is to be able to deal
     ;; with accents.
     ;; Note that the capturing of arguments in braces is imperfect,
     ;; because doing it properly requires sexp parsing. It will fail
     ;; for cases like \command{\anothercommand{an arg}some text}.
     . ,(rx "\\" (group-n 1 (or (1+ letter) nonl))
          (: (* blank) (opt (or (: (* (: "[" (* (not (any "]"))) "]"))
                                 "{" (group-n 2 (0+ (not (any "}")))) (opt "}"))
                                (group-n 3 letter))))))
    (parsebib--replace-literal
     . ,(rx-to-string `(or ,@(mapcar #'car parsebib-TeX-literal-replacement-alist)
                           (1+ blank)))))
  "Alist of replacements and strings for TeX markup.
This is used in `parsebib-clean-TeX-markup' to make TeX markup more
suitable for display.  Each item in the list consists of a replacement
and a regexp.  The replacement can be a string (which will
simply replace the match) or a function (the match will be
replaced by the result of calling the function on the match
string).  Earlier elements are evaluated before later ones, so if
one string is a subpattern of another, the second must appear
later (e.g. \"''\" is before \"'\").

For the common cases of replacing a LaTeX command or a literal
it is faster to use `parsebib-TeX-command-replacement-alist'
and `parsebib-TeX-literal-replacement-alist' respectively.")

(defvar parsebib-clean-TeX-markup-excluded-fields '("file"
                                                    "url"
                                                    "doi")
  "List of fields that should not be passed to `parsebib-clean-TeX-markup'.")

(defun parsebib--replace-command-or-accent (string)
  "Return the replacement text for the command or accent matched by STRING."
  (let* ((cmd (match-string 1 string))
         ;; bar is the argument in braces.
         (bar (match-string 2 string))
         ;; If there is no argument in braces, consider the letter after
         ;; the command as the argument. Clean this argument.
         (arg (parsebib-clean-TeX-markup (or (if bar bar (match-string 3 string)) "")))
         ;; Check if the cmd is an accent that needs to be replaced
         ;; and get its replacement.
         (acc (alist-get cmd parsebib-TeX-accent-replacement-alist nil nil #'equal))
         ;; If it is not an accent, check if it is a command that needs to be replaced
         ;; and get the replacement.
         (rep (or acc (alist-get cmd parsebib-TeX-command-replacement-alist nil nil #'equal))))
    (cond
     ;; If replacement is a function call it with the argument.
     ((functionp rep) (funcall rep arg))
     ;; Otherwise combine the replacement with the argument. The order of combination
     ;; depends on whether the command is an accent or not.
     (rep (if acc (concat arg rep) (concat rep arg)))
     ;; Now we handle the fallback cases. If there is a braced argument but no
     ;; replacement for the command was found, consider the replacement to be
     ;; empty.
     ((and bar (not (equal "" bar))) bar)
     ;; Otherwise clean any optional arguments by discarding them.
     (t (replace-regexp-in-string (rx "[" (* (not (any "]"))) "]") "" string t t)))))

(defun parsebib--replace-literal (string)
  "Look up the replacement text for literal STRING."
  (or (alist-get string parsebib-TeX-literal-replacement-alist nil nil #'equal)
      " "))

(defun parsebib-clean-TeX-markup (string)
  "Return STRING without TeX markup.
Any substring matching the car of a cell in
`parsebib-TeX-markup-replace-alist' is replaced with the
corresponding cdr (if the cdr is a string), or with the result of
calling the cdr on the match (if it is a function).  This is done
with `replace-regexp-in-string', which see for details."
  (let ((case-fold-search nil))
    (cl-loop for (replacement . pattern) in parsebib-TeX-markup-replacement-alist
             do (setq string (replace-regexp-in-string
                              pattern replacement string
                              t t))
             finally return string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching and parsing stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parsebib--looking-at-goto-end (str &optional match)
  "Like `looking-at' but move point to the end of the matching string STR.
MATCH acts just like the argument to MATCH-END, and defaults to
0. Comparison is done case-insensitively."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at str)
        (goto-char (match-end match)))))

(defun parsebib--match-paren-forward ()
  "Move forward to the closing paren matching the opening paren at point.
This function handles parentheses () and braces {}.  Return t if
a matching parenthesis was found.  This function puts point
immediately after the matching parenthesis."
  (cond
   ((eq (char-after) ?\{)
    (parsebib--match-brace-forward))
   ((eq (char-after) ?\()
    (bibtex-end-of-entry))))

(defun parsebib--match-delim-forward ()
  "Move forward to the closing delimiter matching the delimiter at point.
This function handles braces {} and double quotes \"\". Return t
if a matching delimiter was found."
  (let ((result (cond
                 ((eq (char-after) ?\{)
                  (parsebib--match-brace-forward))
                 ((eq (char-after) ?\")
                  (parsebib--match-quote-forward)))))
    result))

(defun parsebib--match-brace-forward ()
  "Move forward to the closing brace matching the opening brace at point."
  (with-syntax-table bibtex-braced-string-syntax-table
    (forward-sexp 1)
    ;; If forward-sexp does not result in an error, we want to return t.
    t))

(defun parsebib--match-quote-forward ()
  "Move to the closing double quote matching the quote at point."
  (with-syntax-table bibtex-quoted-string-syntax-table
    (forward-sexp 1)
    ;; If forward-sexp does not result in an error, we want to return t.
    t))

(defun parsebib--parse-bib-value (limit &optional strings replace-TeX)
  "Parse value at point.
A value is either a field value or a @String expansion.  Return
the value as a string.  No parsing is done beyond LIMIT, but note
that parsing may stop well before LIMIT.

STRINGS, if non-nil, is a hash table of @String definitions.
@String abbrevs in the value to be parsed are then replaced with
their expansions.  Additionally, newlines in field values are
removed, white space is reduced to a single space and braces or
double quotes around field values are removed.

REPLACE-TEX indicates whether TeX markup should be replaced with
ASCII/Unicode characters.  See the variable
`parsebib-TeX-markup-replace-alist' for details."
  (let (res)
    (while (and (< (point) limit)
                (not (looking-at-p ",")))
      (cond
       ((looking-at-p "[{\"]")
        (let ((beg (point)))
          (parsebib--match-delim-forward)
          (push (buffer-substring-no-properties beg (point)) res)))
       ((looking-at parsebib--bibtex-identifier)
        (push (buffer-substring-no-properties (point) (match-end 0)) res)
        (goto-char (match-end 0)))
       ((looking-at "[[:space:]]*#[[:space:]]*")
        (goto-char (match-end 0)))
       (t (forward-char 1)))) ; So as not to get stuck in an infinite loop.
    (setq res (if strings
                  (string-join (parsebib--expand-strings (nreverse res) strings))
                (string-join (nreverse res) " # ")))
    (if replace-TeX
        (parsebib-clean-TeX-markup res)
      res)))

;;;;;;;;;;;;;;;;;;;;;
;; Expanding stuff ;;
;;;;;;;;;;;;;;;;;;;;;

(defun parsebib--expand-strings (strings abbrevs)
  "Expand strings in STRINGS using expansions in ABBREVS.
STRINGS is a list of strings.  If a string in STRINGS has an
expansion in hash table ABBREVS, replace it with its expansion.
Otherwise, if the string is enclosed in braces {} or double
quotes \"\", remove the delimiters.  In addition, newlines and
multiple spaces in the string are replaced with a single space."
  (mapcar (lambda (str)
            (setq str (replace-regexp-in-string "[ \t\n\f[:space:]]+" " " str))
            (cond
             ((gethash str abbrevs))
             ((string-match "\\`[\"{]\\(.*?\\)[\"}]\\'" str)
              (match-string 1 str))
             (t str)))
          strings))

(defun parsebib-expand-xrefs (entries inheritance)
  "Expand cross-referencing items in ENTRIES.
BibTeX entries in ENTRIES that have a `crossref' field are
expanded with the fields in the cross-referenced entry.  ENTRIES
is a hash table with entries.  This hash table is updated with
the new fields.  The return value of this function is always nil.

INHERITANCE indicates the inheritance schema.  It can be a symbol
`BibTeX' or `biblatex', or it can be an explicit inheritance
schema.  See the variable `parsebib--biblatex-inheritances' for
details on the structure of such an inheritance schema."
  (maphash (lambda (key fields)
             (let ((xref (cdr (assoc-string "crossref" fields))))
               (when xref
                 (if (string-match-p (concat "\\b[\"{]" parsebib--key-regexp "[\"}]\\b") xref)
                     (setq xref (substring xref 1 -1)))
                 (let* ((source (gethash xref entries))
                        (updated-entry (parsebib--get-xref-fields fields source inheritance)))
                   (when updated-entry
                     (puthash key updated-entry entries))))))
           entries))

(defun parsebib--get-xref-fields (target-entry source-entry inheritance)
  "Return TARGET-ENTRY supplemented with fields inherited from SOURCE-ENTRY.
TARGET-ENTRY and SOURCE-ENTRY are entry alists.  Fields in
SOURCE-ENTRY for which TARGET-ENTRY has no value are added to
TARGET-ENTRY.  Return value is the modified TARGET-ENTRY.

INHERITANCE is an inheritance schema.  It can either be one of
the symbols `BibTeX' or `biblatex', or it can be an explicit
inheritance schema.  See the variable
`parsebib--biblatex-inheritances' for details on the structure of
such an inheritance schema."
  (when (and target-entry source-entry)
    (when (eq inheritance 'biblatex)
      (setq inheritance parsebib--biblatex-inheritances))
    (let* ((inheritable-fields
            (unless (eq inheritance 'BibTeX)
              (append
               (apply #'append (mapcar #'cl-third
                                       (cl-remove-if-not
                                        (lambda (elem)
                                          (and (string-match-p (concat "\\b" (cdr (assoc-string "=type=" source-entry)) "\\b")
                                                               (cl-first elem))
                                               (string-match-p (concat "\\b" (cdr (assoc-string "=type=" target-entry)) "\\b")
                                                               (cl-second elem))))
                                        inheritance)))
               (cl-third (assoc-string "all" inheritance)))))
           (new-fields (delq nil (mapcar (lambda (field)
                                           (let ((target-field (parsebib--get-target-field (car field) inheritable-fields)))
                                             (if (and target-field
                                                      (not (assoc-string target-field target-entry 'case-fold)))
                                                 (cons target-field (cdr field)))))
                                         source-entry))))
      (append target-entry new-fields))))

(defun parsebib--get-target-field (source-field inheritances)
  "Return the target field for inheritance from SOURCE-FIELD.
Inheritance is determined by INHERITANCES, which is an alist of
source/target pairs.  If no inheritance should take place for
SOURCE-FIELD, the target in the relevant item in INHERITANCES is
the symbol `none'.  If there is no item for SOURCE-FIELD in
INHERITANCES, SOURCE-FIELD is returned.  Note that it is valid
for INHERITANCES to be nil."
  ;; Note: the argument INHERITANCES differs from the INHERITANCE argument in
  ;; the previous two functions.  It is a simple alist of (source-field
  ;; . target-field) pairs.
  (let ((target-field (cdr (assoc-string source-field inheritances 'case-fold))))
    (cond
     ((null target-field)
      source-field)
     ((eq target-field 'none)
      nil)
     (t target-field))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level BibTeX/biblatex API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parsebib-find-next-item (&optional pos)
  "Find the first (potential) BibTeX item following POS.
This function simply searches for an @ at the start of a line,
possibly preceded by spaces or tabs, followed by a string of
characters as defined by `parsebib--bibtex-identifier'.  When
successful, point is placed right after the item's type, i.e.,
generally on the opening brace or parenthesis following the entry
type, \"@Comment\", \"@Preamble\" or \"@String\".

The return value is the name of the item as a string, either
\"Comment\", \"Preamble\" or \"String\", or the entry
type (without the @). If an item name is found that includes an
illegal character, an error of type `parsebib-entry-type-error'
is raised. If no item is found, nil is returned and point is left
at the end of the buffer.

POS can be a number or a marker and defaults to point."
  (when pos (goto-char pos))
  (when (re-search-forward parsebib--entry-start nil 0)
    (if (parsebib--looking-at-goto-end (concat "\\(" parsebib--bibtex-identifier "\\)" "[[:space:]]*[\(\{]?") 1)
        (match-string-no-properties 1)
      (signal 'parsebib-entry-type-error (list (point))))))

(defun parsebib-read-comment (&optional pos)
  "Read the @Comment beginning at the line POS is on.
Return value is the text of the @Comment including the braces.
For comments that last until the end of the line (i.e., comments
that are not delimited by braces), the return value includes the
whitespace between `@comment' and the actual comment text.

If no comment could be found, return nil.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @Comment entry must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point."
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (parsebib--looking-at-goto-end (concat parsebib--entry-start "\\(comment\\)[[:space:]]*[\(\{]?") 1)
    (let ((beg (point)))
      (if (looking-at-p "[[:space:]]*[\(\{]")
          (progn (skip-chars-forward "[:space:]")
                 (parsebib--match-paren-forward))
        (goto-char (line-end-position)))
      (buffer-substring-no-properties beg (point)))))

(defun parsebib-read-string (&optional pos strings)
  "Read the @String definition beginning at the line POS is on.
If a proper abbreviation and expansion are found, they are
returned as a cons cell (<abbrev> . <expansion>).  Otherwise, nil
is returned.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @String entry must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point.

If STRINGS is provided it should be a hash table with string
abbreviations, which are used to expand abbrevs in the string's
expansion."
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (parsebib--looking-at-goto-end (concat parsebib--entry-start "\\(string[[:space:]]*\\)[\(\{]") 1)
    (let ((limit (save-excursion
                   (parsebib--match-paren-forward)
                   (point))))
      (parsebib--looking-at-goto-end (concat "[({]\\(" parsebib--bibtex-identifier "\\)[[:space:]]*=[[:space:]]*"))
      (let ((abbr (match-string-no-properties 1)))
        (when (and abbr (> (length abbr) 0))            ; If we found an abbrev.
          (let ((expansion (parsebib--parse-bib-value limit strings)))
            (goto-char limit)
            (cons abbr expansion)))))))

(defun parsebib-read-preamble (&optional pos)
  "Read the @Preamble definition at the line POS is on.
Return the preamble as a string (including the braces surrounding
the preamble text), or nil if no preamble was found.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @Preamble must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point."
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (parsebib--looking-at-goto-end (concat parsebib--entry-start "\\(preamble[[:space:]]*\\)[\(\{]") 1)
    (let ((beg (point)))
      (when (parsebib--match-paren-forward)
        (buffer-substring-no-properties beg (point))))))

(defun parsebib--get-hashid-string (fields)
  "Create a string from the contents of FIELDS to compute a hash id."
  (cl-loop
   for field in parsebib-hashid-fields
   collect (or
            ;; Remove braces {}.
            (replace-regexp-in-string "^{\\|}\\'" "" (cdr (assoc-string field fields 'case-fold)))
            "")
   into hashid-fields
   finally return (mapconcat #'identity hashid-fields "")))

(defun parsebib-read-entry (type &optional pos strings fields replace-TeX)
  "Read a BibTeX entry of type TYPE at the line POS is on.
TYPE should be a string and should not contain the @
sign.  The return value is the entry as an alist of (<field> .
<contents>) cons pairs, or nil if no entry was found.  In this
alist, the entry key is provided in the field \"=key=\" and the
entry type in the field \"=type=\".

If `parsebib-hashid-fields' is non-nil, a hash ID is added in the
field \"=hashid=\".  The hash is computed on the basis of the
contents of the fields listed in `parsebib-hashid-fields' using
the function `secure-hash' and the `sha256' algorithm.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the entry must start at the beginning of
the line POS is on.  If POS is nil, it defaults to point.

ENTRY should not be \"Comment\", \"Preamble\" or \"String\", but
is otherwise not limited to any set of possible entry types.

If STRINGS is provided, it should be a hash table with string
abbreviations, which are used to expand abbrevs in the entry's
fields.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored,
except \"=key=\" and \"=type=\", which are always included.  Case
is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned.

REPLACE-TEX indicates whether TeX markup should be replaced with
ASCII/Unicode characters.  See the variable
`parsebib-TeX-markup-replace-alist' for details."
  (unless (member-ignore-case type '("comment" "preamble" "string"))
    (when pos (goto-char pos))
    (beginning-of-line)
    (when (parsebib--looking-at-goto-end (concat parsebib--entry-start type "[[:space:]]*[\(\{]"))
      ;; Find the end of the entry and the beginning of the entry key.
      (let* ((limit (save-excursion
                      (backward-char)
                      (parsebib--match-paren-forward)
                      (point)))
             (beg (progn
                    (skip-chars-forward " \n\t\f") ; Note the space!
                    (point)))
             (key (when (parsebib--looking-at-goto-end (concat "\\(" parsebib--key-regexp "\\)[ \t\n\f]*,") 1)
                    (buffer-substring-no-properties beg (point)))))
        (or key (setq key "")) ; If no key was found, we pretend it's empty and try to read the entry anyway.
        (skip-chars-forward "^," limit) ; Move to the comma after the entry key.
        (let ((fields (cl-loop for field = (parsebib--parse-bibtex-field limit strings fields replace-TeX)
                               while field
                               if (consp field) collect field)))
          (push (cons "=type=" type) fields)
          (push (cons "=key=" key) fields)
          (if parsebib-hashid-fields
              (push (cons "=hashid=" (secure-hash 'sha256 (parsebib--get-hashid-string fields))) fields))
          (nreverse fields))))))

(defun parsebib--parse-bibtex-field (limit &optional strings fields replace-TeX)
  "Parse the field starting at point.
Do not search beyond LIMIT (a buffer position).  Return a
cons (FIELD . VALUE), or nil if no field was found.

STRINGS is a hash table with string abbreviations, which are used
to expand abbrevs in the field's value.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored,
except \"=key=\" and \"=type=\", which are always included.  Case
is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned.

REPLACE-TEX indicates whether TeX markup should be replaced with
ASCII/Unicode characters.  See the variable
`parsebib-TeX-markup-replace-alist' for details."
  (skip-chars-forward "\"#%'(),={} \n\t\f" limit) ; Move to the first char of the field name.
  (unless (>= (point) limit)                      ; If we haven't reached the end of the entry.
    (let ((beg (point)))
      (if (parsebib--looking-at-goto-end (concat "\\(" parsebib--bibtex-identifier "\\)[[:space:]]*=[[:space:]]*") 1)
          (let* ((field (buffer-substring-no-properties beg (point)))
                 (replace-TeX (and replace-TeX
                                   (not (member-ignore-case field parsebib-clean-TeX-markup-excluded-fields)))))
            (if (or (not fields)
                    (member-ignore-case field fields))
                (cons field (parsebib--parse-bib-value limit strings replace-TeX))
              (parsebib--parse-bib-value limit) ; Skip over the field value.
              :ignore)))))) ; Ignore this field but keep the `cl-loop' in `parsebib-read-entry' going.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level BibTeX/biblatex API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parsebib-collect-preambles ()
  "Collect all @Preamble definitions in the current buffer.
Return a list of strings, each string a separate @Preamble."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (when (cl-equalp item "preamble")
                 (push (parsebib-read-preamble) res)))
      (nreverse res))))

(defun parsebib-collect-comments ()
  "Collect all @Comment definitions in the current buffer.
Return a list of strings, each string a separate @Comment."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (when (cl-equalp item "comment")
                 (push (parsebib-read-comment) res)))
      (nreverse (delq nil res)))))

(cl-defun parsebib-collect-strings (&key strings expand-strings)
  "Collect all @String definitions in the current buffer.
Return value is a hash with the abbreviations as keys and the
expansions as values.  If STRINGS is a hash table with test
function `equal', it is used to store the @String definitions.
If EXPAND-STRINGS is non-nil, @String expansions are expanded
themselves using the @String definitions already stored in
STRINGS."
  (or (and (hash-table-p strings)
           (eq 'equal (hash-table-test strings)))
      (setq strings (make-hash-table :test #'equal)))
  (save-excursion
    (goto-char (point-min))
    (cl-loop with string = nil
             for item = (parsebib-find-next-item)
             while item do
             (when (cl-equalp item "string")
               (setq string (parsebib-read-string nil (if expand-strings strings)))
               (puthash (car string) (cdr string) strings)))
    strings))

(cl-defun parsebib-collect-bib-entries (&key entries strings inheritance fields)
  "Collect all BibTeX / biblatex entries in the current buffer.
Return value is a hash table containing the entries.  If ENTRIES
is a hash table with test function `equal', it is used to store
the entries collected in the buffer.  Note that ENTRIES does not
have to be empty.  It may contain entries from a previous parse.

If STRINGS is non-nil, it should be a hash table of string
definitions, which are used to expand abbreviations used in the
entries.

If INHERITANCE is non-nil, cross-references in the entries are
resolved: if the crossref field of an entry points to an entry
already in ENTRIES (which includes the entries that appear
earlier in the buffer), the fields of the latter that do not occur
in the entry are added to it.  INHERITANCE indicates the
inheritance schema used for determining which fields inherit from
which fields.  It can be a symbol `BibTeX' or `biblatex', or it
can be an explicit inheritance schema.  (See the variable
`parsebib--biblatex-inheritances' for details on the structure of
such an inheritance schema.)  It can also be the symbol t, in
which case the local variable block is checked for a
dialect (using the variable `bibtex-dialect'), or, if no such
local variable is found, the value of the variable
`bibtex-dialect'.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored,
except \"=key=\" and \"=type=\", which are always included.  Case
is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned."
  (or (and (hash-table-p entries)
           (eq 'equal (hash-table-test entries)))
      (setq entries (make-hash-table :test #'equal)))
  (if (eq inheritance t)
      (setq inheritance (or (parsebib-find-bibtex-dialect)
                            bibtex-dialect
                            'BibTeX)))
  (save-excursion
    (goto-char (point-min))
    (cl-loop with entry = nil
             for entry-type = (parsebib-find-next-item)
             while entry-type do
             (unless (member-ignore-case entry-type '("preamble" "string" "comment"))
               (setq entry (parsebib-read-entry entry-type nil strings fields))
               (if entry
                   (puthash (cdr (assoc-string "=key=" entry)) entry entries))))
    (when inheritance
      (parsebib-expand-xrefs entries inheritance))
    entries))

(defun parsebib-find-bibtex-dialect ()
  "Find the BibTeX dialect of a file if one is set.
This function looks for a local value of the variable
`bibtex-dialect' in the local variable block at the end of the
file.  Return nil if no dialect is found."
  (save-excursion
    (goto-char (point-max))
    (let ((case-fold-search t))
      (when (re-search-backward (concat parsebib--entry-start "comment") (- (point-max) 3000) t)
        (let ((comment (parsebib-read-comment)))
          (when (and comment
                     (string-match-p "\\`{[ \n\t\r]*Local Variables:" comment)
                     (string-match-p "End:[ \n\t\r]*}\\'" comment)
                     (string-match (concat "bibtex-dialect: " (regexp-opt (mapcar #'symbol-name bibtex-dialect-list) t)) comment))
            (intern (match-string 1 comment))))))))

(cl-defun parsebib-parse-bib-buffer (&key entries strings expand-strings inheritance fields replace-TeX)
  "Parse the current buffer and return all BibTeX data.
Return a list of five elements: a hash table with the entries, a
hash table with the @String definitions, a list of @Preamble
definitions, a list of @Comments and the BibTeX dialect, if
present in the file.

If ENTRIES is a hash table with test function `equal', it is used
to store the entries.  Any existing entries with identical keys
are overwritten.  Similarly, if STRINGS is a hash table with test
function `equal', the @String definitions are stored in it.

If EXPAND-STRINGS is non-nil, abbreviations in the entries and
@String definitions are expanded using the @String definitions
already in STRINGS.

If INHERITANCE is non-nil, cross-references in the entries are
resolved: if the crossref field of an entry points to an entry
already in ENTRIES, the fields of the latter that do not occur in
the entry are added to it.  INHERITANCE indicates the inheritance
schema used for determining which fields inherit from which
fields.  It can be a symbol `BibTeX' or `biblatex', which means
to use the default inheritance schema for either dialect, or it
can be an explicit inheritance schema.  (See the variable
`parsebib--biblatex-inheritances' for details on the structure of
such an inheritance schema.)  It can also be the symbol t, in
which case the local variable block is checked for a
dialect (using the variable `bibtex-dialect'), or, if no such
local variable is found, the value of the variable
`bibtex-dialect'.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored,
except \"=key=\" and \"=type=\", which are always included.  Case
is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned.

REPLACE-TEX indicates whether TeX markup should be replaced with
ASCII/Unicode characters.  See the variable
`parsebib-TeX-markup-replace-alist' for details."
  (save-excursion
    (goto-char (point-min))
    (or (and (hash-table-p entries)
             (eq (hash-table-test entries) 'equal))
        (setq entries (make-hash-table :test #'equal)))
    (or (and (hash-table-p strings)
             (eq (hash-table-test strings) 'equal))
        (setq strings (make-hash-table :test #'equal)))
    (let ((dialect (or (parsebib-find-bibtex-dialect)
                       bibtex-dialect
                       'BibTeX))
          preambles comments)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (cond
                ((cl-equalp item "string") ; `cl-equalp' compares strings case-insensitively.
                 (let ((string (parsebib-read-string nil (if expand-strings strings))))
                   (if string
                       (puthash (car string) (cdr string) strings))))
                ((cl-equalp item "preamble")
                 (push (parsebib-read-preamble) preambles))
                ((cl-equalp item "comment")
                 (push (parsebib-read-comment) comments))
                ((stringp item)
                 (let ((entry (parsebib-read-entry item nil (if expand-strings strings) fields replace-TeX)))
                   (when entry
                     (puthash (cdr (assoc-string "=key=" entry)) entry entries))))))
      (when inheritance (parsebib-expand-xrefs entries (if (eq inheritance t) dialect inheritance)))
      (list entries strings (nreverse preambles) (nreverse comments) dialect))))

;;;;;;;;;;;;;;;;;;
;; CSL-JSON API ;;
;;;;;;;;;;;;;;;;;;

(cl-defun parsebib-parse-json-buffer (&key entries stringify year-only fields)
  "Parse the current buffer and return all CSL-JSON data.
The return value is a hash table containing all the elements.
The hash table's keys are the \"id\" values of the entries, the
hash table's values are alists as returned by `json-parse-buffer'
or `json-read'

If ENTRIES is a hash table with test function `equal', it is used
to store the entries.  Any existing entries with identical keys
are overwritten.

If STRINGIFY is non-nil, JSON values that are not
strings (notably name and date fields) are converted to strings.
If additionally YEAR-ONLY is non-nil, dates are shortened to just
the year part.

FIELDS is a list of field names (as symbols) to be read and
included in the result.  Fields not in the list are ignored,
except `id' and `type', which are always included.  If FIELDS is
nil, all fields are returned.

If a JSON object is encountered that does not have an \"id\"
field, a `parsebib-entry-type-error' is raised."
  (or (and (hash-table-p entries)
           (eq (hash-table-test entries) 'equal))
      (setq entries (make-hash-table :test #'equal)))
  (when fields
    (setq fields (append '(id type) fields)))
  (let ((parse (if (and (fboundp 'json-serialize)
                        (json-serialize '((test . 1)))) ; Returns nil if native json support isn't working for some reason.
                   (lambda ()
                     (json-parse-buffer :object-type 'alist))
                 (lambda ()
                   (let ((json-object-type 'alist))
                     (json-read))))))
    ;; We do not read the entire file in one go, but instead parse each entry
    ;; separately.  Large bibliographies would otherwise be returned as one
    ;; gigantic vector, which then needs to be converted to a hash table.  If we
    ;; need to convert some of the data because `stringify' is t, the data is
    ;; held in memory twice.
    (save-excursion
      (goto-char (point-min))
      ;; JSON is pretty strict, not even comments are allowed.  CSL-JSON
      ;; requires that the file is essentially one big array, so we know that
      ;; the first non-whitespace character in the file must be an opening
      ;; bracket;
      (if (not (looking-at-p "[\n\t ]*\\["))
          (error "[Parsebib] Not a valid CSL-JSON file"))
      (let ((continue t))
        (while continue
          ;; We also know that the first non-whitespace character after that
          ;; must be an opening brace:
          (skip-chars-forward "^{")
          (if-let ((entry (funcall parse))
                   (id (alist-get 'id entry)))
              (progn
                (when fields
                  (setq entry (seq-filter (lambda (elt)
                                            (memq (car elt) fields))
                                          entry)))
                (puthash id (if stringify
                                (parsebib-stringify-json entry year-only)
                              entry)
                         entries))
            (signal 'parsebib-entry-type-error (list (point))))
          ;; Parsing an entry moves point to the end of the entry.  The next
          ;; character must be a comma if there is another entry.  If we're not
          ;; seeing a comma, we've reached the end of the file:
          (if (not (looking-at-p "[\n-t ]*,"))
              (setq continue nil))))))
  entries)

(defun parsebib-stringify-json (entry &optional year-only)
  "Return ENTRY with all non-string values converted to strings.
ENTRY is a CSL-JSON entry in the form of an alist.  ENTRY is
modified in place.  Return value is ENTRY.  If YEAR-ONLY is
non-nil, date fields are shortened to just the year."
  (mapc (lambda (field)
          (unless (stringp (alist-get field entry))
            (setf (alist-get field entry)
                  (parsebib-stringify-json-field (assq field entry) year-only))))
        (mapcar #'car entry))
  entry)

(defvar parsebib--json-name-fields  '(author
                                      collection-editor
                                      composer
                                      container-author
                                      director
                                      editor
                                      editorial-director
                                      illustrator
                                      interviewer
                                      original-author
                                      recipient
                                      reviewed-author
                                      translator))

(defvar parsebib--json-date-fields '(accessed
                                     container
                                     event-date
                                     issued
                                     original-date
                                     submitted))

(defvar parsebib--json-number-fields '(chapter-number
                                       collection-number
                                       edition
                                       issue
                                       number
                                       number-of-pages
                                       number-of-volumes
                                       volume))

(defvar parsebib-json-name-field-template "{non-dropping-particle }{family, }{given}{ dropping-particle}{, suffix}{literal}"
  "Template used to display name fields.")

(defvar parsebib-json-name-field-separator " and "
  "Separator used to concatenate names in a name field.")

(defvar parsebib-json-field-separator ", "
  "Separator used to concatenate items of array fields.")

(defun parsebib--process-template (template items)
  "Process TEMPLATE and return a formatted string.
ITEMS is an alist, the keys of which may occur in TEMPLATE.
Braced occurrences of the keys in ITEMS are replaced with the
corresponding values.  Note that the keys in ITEMS should be
symbols."
  (cl-flet ((create-replacements (match)
                                 (save-match-data
                                   (string-match "{\\([^A-Za-z]*\\)\\([A-Za-z][A-za-z-]+\\)\\([^A-Za-z]*\\)}" match)
                                   (let* ((pre (match-string 1 match))
                                          (key (match-string 2 match))
                                          (post (match-string 3 match))
                                          (value (alist-get (intern key) items)))
                                     (if value
                                         (format "%s%s%s" pre value post)
                                       "")))))
    (replace-regexp-in-string "{.*?}" #'create-replacements template nil t)))

(defun parsebib-stringify-json-field (field &optional short)
  "Return the value of FIELD as a string.
FIELD is a cons cell that constitutes a CSL-JSON field-value
pair.  The car is the key, the cdr the value.  If the value is a
string, return it with sequences of white space reduced to a
single space.  Otherwise, convert it into a string.  SHORT is
only relevant for date fields: if it is non-nil, return just a
year, or the string \"XXXX\" if no year part is present."
  (let ((key (car field))
        (value (cdr field)))
    (cond
     ((stringp value)
      (replace-regexp-in-string "[ \t\n\f[:space:]]+" " " value))

     ((numberp value)
      (format "%s" value))

     ((memq key parsebib--json-name-fields)
      (parsebib--json-stringify-name-field value))

     ((memq key parsebib--json-date-fields)
      (parsebib--json-stringify-date-field value short))

     ;; In CSL-JSON v1.0, the only array field besides name and date fields
     ;; is "categories".  It has an array of strings as value, so the `format'
     ;; isn't strictly necessary.  We do it this way just to be on the safe
     ;; side.
     ((arrayp value)
      (mapconcat (lambda (e) (format "%s" e)) value parsebib-json-field-separator))

     ;; This clause should never be reached.
     (t (replace-regexp-in-string "\n" " " (format "%s" value))))))

(defun parsebib--json-stringify-name-field (names)
  "Convert NAMES to a string.
NAMES is the value of a CSL-JSON name field, a vector of alists.
Conversion is done on the basis of
`parsebib-json-name-field-template': each field in this template
is replaced with the value of the field in NAME.  Fields that
have no value in NAME are ignored."
  (mapconcat (lambda (name)
               (parsebib--process-template parsebib-json-name-field-template name))
             names
             parsebib-json-name-field-separator))

(defun parsebib--json-stringify-date-field (date &optional short)
  "Convert DATE to a string.
DATE is the value of a CSL-JSON date field.  If SHORT is non-nil,
try to return only a year (in a date range, just the year of the
first date).  If no year part is present, SHORT returns
\"XXXX\"."
  (if short
      (if-let ((date-parts (alist-get 'date-parts date))
               (first-date (aref date-parts 0))
               (year (aref first-date 0)))
          (format "%s" year)
        "XXXX")

    ;; Work with a copy of the original alist.
    (setq date (copy-sequence date))

    ;; Set start-date and end-date.
    (when-let ((date-parts (alist-get 'date-parts date)))
      (let* ((start-date (aref date-parts 0))
             (end-date (if (= (length date-parts) 2)
                           (aref date-parts 1))))
        (setf (alist-get 'date-parts date nil :remove) nil)
        (setf (alist-get 'start-date date)
              (parsebib--json-stringify-date-part start-date))
        (if end-date (setf (alist-get 'end-date date)
                           (parsebib--json-stringify-date-part end-date)))))

    ;; Set season.
    (when-let ((season (alist-get 'season date)))
      (if (numberp season)
          (setf (alist-get 'season date)
                (aref ["Spring" "Summer" "Autumn" "Winter"] (1- season)))))

    ;; Set circa.
    (when-let ((circa (alist-get 'circa date)))
      (setf (alist-get 'circa date) "ca."))

    ;; Now convert the date.
    (parsebib--process-template "{circa }{season }{start-date}{/end-date}{literal}{raw}"
                                date)))

(defun parsebib--json-stringify-date-part (date-parts)
  "Convert DATE-PARTS into a string.
DATE-PARTS is a sequence with up to three numeric elements: a
year, a month and a day."
  (parsebib--process-template "{year}{-month}{-day}"
                              (seq-mapn #'cons '(year month day) date-parts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format-independent API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun parsebib-parse (files &key entries strings (display t) fields)
  "Parse one or more bibliography files.
FILES is the list of files to parse.  All bibliographic entries
in FILES are collected and returned in a single hash table.
FILES can be a list of `.bib' or `.json' files, or a combination
of these.  FILES can also be a string, which should be the path
to a single bibliography file.

ENTRIES, if provided, should be a hash table with test function
`equal', it is used to store the entries.  Any existing entries
with identical keys are overwritten.  If provided, ENTRIES is
also the return value.  If ENTRIES is nil, a new hash table is
created and returned.

STRINGS, similarly a hash table with test function `equal', is
used to store the @String definitions.  Although STRINGS is not
returned, it is modified in place and can therefore be used to
collect the @String definitions in the files being parsed.

If DISPLAY is non-nil, field values are returned in a way that is
suitable for display: in `.bib' files, @String abbreviations are
expanded, in `.json' files, values that are not strings are
converted to strings.  Furthermore, sequences of white space
characters (including newlines) are reduced to a single space.

Specifically, setting DISPLAY means setting the arguments
EXPAND-STRINGS and INHERITANCES in the function
`parsebib-parse-bib-buffer' and setting STRINGIFY and YEAR-ONLY
in the function `parsebib-parse-json-buffer'.  DISPLAY is simply
passed on to these arguments, which means that it can be set to
anything that INHERITANCES in `parsebib-parse-bib-buffer'
accepts.  (The other arguments only distinguish between nil and
non-nil.) Note that DISPLAY defaults to t.

FIELDS is a list of the field names to be read and included in
the result.  Fields not in the list are ignored.  Note that field
names should be strings; when parsing a `.json' file, they are
converted to symbols.  See the doc strings of
`parsebib-parse-bib-buffer' and `parsebib-parse-json-buffer' for
details.  If FIELDS is nil, all fields are returned."
  (or (and (hash-table-p entries)
           (eq (hash-table-test entries) 'equal))
      (setq entries (make-hash-table :test #'equal)))
  (or (and (hash-table-p strings)
           (eq (hash-table-test strings) 'equal))
      (setq strings (make-hash-table :test #'equal)))
  (when (stringp files)
    (setq files (list files)))
  (mapc (lambda (file)
          (with-temp-buffer
            (insert-file-contents file)
            (cond
             ((string= (file-name-extension file t) ".bib")
              (parsebib-parse-bib-buffer :entries entries
                                         :strings strings
                                         :expand-strings display
                                         :inheritance display
                                         :fields fields
                                         :replace-TeX display))
             ((string= (file-name-extension file t) ".json")
              (parsebib-parse-json-buffer :entries entries
                                          :stringify display
                                          :year-only display
                                          :fields (mapcar #'intern fields)))
             (t (error "[Parsebib] Not a bibliography file: %s" file)))))
        files)
  entries)

(provide 'parsebib)

;;; parsebib.el ends here
