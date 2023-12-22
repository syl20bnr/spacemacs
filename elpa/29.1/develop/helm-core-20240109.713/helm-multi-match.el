;;; helm-multi-match.el --- Multiple regexp matching methods for helm -*- lexical-binding: t -*-

;; Original Author: rubikitch

;; Copyright (C) 2008 ~ 2011 rubikitch
;; Copyright (C) 2011 ~ 2020 Thierry Volpiatto 

;; Author: Thierry Volpiatto 
;; URL: http://github.com/emacs-helm/helm

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

;;; Code:

(require 'cl-lib)
(require 'helm-lib)


(defgroup helm-multi-match nil
  "Helm multi match."
  :group 'helm)

(defcustom helm-mm-matching-method 'multi3
  "Matching method for helm match plugin.
You can set here different methods to match candidates in helm.
Here are the possible value of this symbol and their meaning:
- multi1: Respect order, prefix of pattern must match.
- multi2: Same but with partial match.
- multi3: The best, multiple regexp match, allow negation.
- multi3p: Same but prefix must match.

Default is multi3, you should keep this for a better experience.

Note that multi1 and multi3p are incompatible with fuzzy matching
in file completion and by the way fuzzy matching will be disabled there
when these options are used."
  :type  '(radio :tag "Matching methods for helm"
           (const :tag "Multiple regexp 1 ordered with prefix match"         multi1)
           (const :tag "Multiple regexp 2 ordered with partial match"        multi2)
           (const :tag "Multiple regexp 3 matching no order, partial, best." multi3)
           (const :tag "Multiple regexp 3p matching with prefix match"       multi3p))
  :group 'helm-multi-match)


;; Internal
(defvar helm-mm-default-match-functions
  '(helm-mm-exact-match helm-mm-match))
(defvar helm-mm-default-search-functions
  '(helm-mm-exact-search helm-mm-search))


;;; Build regexps
;;
;;
(defconst helm-mm-space-regexp "\\s\\\\s-"
  "Regexp to represent space itself in multiple regexp match.")

(defun helm-mm-split-pattern (pattern &optional grep-space)
  "Split PATTERN if it contains spaces and return resulting list.
If spaces in PATTERN are escaped, don't split at this place.
i.e \"foo bar baz\"=> (\"foo\" \"bar\" \"baz\")
but \"foo\\ bar baz\"=> (\"foo\\s-bar\" \"baz\").
If GREP-SPACE is used translate escaped space to \"\\s\" instead of \"\\s-\"."
  (split-string
   ;; Match spaces litteraly because candidate buffer syntax-table
   ;; doesn't understand "\s-" properly.
   (replace-regexp-in-string
    helm-mm-space-regexp
    (if grep-space "\\s" "\\s-") pattern nil t)))

(defun helm-mm-1-make-regexp (pattern)
  "Replace spaces in PATTERN with \".*\"."
  (mapconcat 'identity (helm-mm-split-pattern pattern) ".*"))


;;; Exact match.
;;
;;
;; Internal.
(defvar helm-mm--exact-pattern-str nil)
(defvar helm-mm--exact-pattern-real nil)

(defun helm-mm-exact-get-pattern (pattern)
  (unless (equal pattern helm-mm--exact-pattern-str)
    (setq helm-mm--exact-pattern-str pattern
          helm-mm--exact-pattern-real (concat "^" (regexp-quote pattern) "$")))
  helm-mm--exact-pattern-real)


(cl-defun helm-mm-exact-match (candidate &optional (pattern helm-pattern))
  (if case-fold-search
      (string= (downcase candidate) (downcase pattern))
    (string= candidate pattern)))

(defun helm-mm-exact-search (pattern &rest _ignore)
  (re-search-forward (helm-mm-exact-get-pattern pattern) nil t))


;;; Prefix match
;;
;;
;; Internal
(defvar helm-mm--prefix-pattern-str nil)
(defvar helm-mm--prefix-pattern-real nil)

(defun helm-mm-prefix-get-pattern (pattern)
  (unless (equal pattern helm-mm--prefix-pattern-str)
    (setq helm-mm--prefix-pattern-str pattern
          helm-mm--prefix-pattern-real (concat "\n" pattern)))
  helm-mm--prefix-pattern-real)

(defun helm-mm-prefix-match (candidate &optional pattern)
  ;; In filename completion basename and basedir may be
  ;; quoted, unquote them for string comparison (Bug#1283).
  (setq pattern (replace-regexp-in-string
                 "\\\\" "" (or pattern helm-pattern)))
  (let ((len (length pattern)))
    (and (<= len (length candidate))
         (string= (substring candidate 0 len) pattern ))))

(defun helm-mm-prefix-search (pattern &rest _ignore)
  (search-forward (helm-mm-prefix-get-pattern pattern) nil t))


;;; Multiple regexp patterns 1 (order is preserved / prefix).
;;
;;
;; Internal
(defvar helm-mm--1-pattern-str nil)
(defvar helm-mm--1-pattern-real nil)

(defun helm-mm-1-get-pattern (pattern)
  (unless (equal pattern helm-mm--1-pattern-str)
    (setq helm-mm--1-pattern-str pattern
          helm-mm--1-pattern-real
          (concat "^" (helm-mm-1-make-regexp pattern))))
  helm-mm--1-pattern-real)

(cl-defun helm-mm-1-match (candidate &optional (pattern helm-pattern))
  (string-match (helm-mm-1-get-pattern pattern) candidate))

(defun helm-mm-1-search (pattern &rest _ignore)
  (re-search-forward (helm-mm-1-get-pattern pattern) nil t))


;;; Multiple regexp patterns 2 (order is preserved / partial).
;;
;;
;; Internal
(defvar helm-mm--2-pattern-str nil)
(defvar helm-mm--2-pattern-real nil)

(defun helm-mm-2-get-pattern (pattern)
  (unless (equal pattern helm-mm--2-pattern-str)
    (setq helm-mm--2-pattern-str pattern
          helm-mm--2-pattern-real
          (concat "^.*" (helm-mm-1-make-regexp pattern))))
  helm-mm--2-pattern-real)

(cl-defun helm-mm-2-match (candidate &optional (pattern helm-pattern))
  (string-match (helm-mm-2-get-pattern pattern) candidate))

(defun helm-mm-2-search (pattern &rest _ignore)
  (re-search-forward (helm-mm-2-get-pattern pattern) nil t))


;;; Multiple regexp patterns 3 (permutation).
;;
;;
;; Internal
(defvar helm-mm--3-pattern-str nil)
(defvar helm-mm--3-pattern-list nil)

(defun helm-mm-3-get-patterns (pattern)
  "Return a list of predicate/regexp cons cells.
E.g., ((identity . \"foo\") (not . \"bar\")).
If PATTERN is unchanged, don't recompute PATTERN and return the
previous value stored in `helm-mm--3-pattern-list'."
  (unless (equal pattern helm-mm--3-pattern-str)
    (setq helm-mm--3-pattern-str pattern
          helm-mm--3-pattern-list
          (helm-mm-3-get-patterns-internal pattern)))
  helm-mm--3-pattern-list)

(defun helm-mm-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
E.g., ((identity . \"foo\") (not . \"bar\"))."
  (unless (string= pattern "")
    (cl-loop for pat in (helm-mm-split-pattern pattern)
          collect (if (char-equal ?! (aref pat 0))
                      (cons 'not (substring pat 1))
                    (cons 'identity pat)))))

(defun helm-mm-regexp-p (string)
  (string-match-p "[][*+^$.?]" string))

(defvar helm-mm--match-on-diacritics nil)

(cl-defun helm-mm-3-match (candidate &optional (pattern helm-pattern))
  "Check if PATTERN match CANDIDATE.
When PATTERN contains a space, it is splitted and matching is
done with the several resulting regexps against CANDIDATE.
E.g., \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of cons cell
with `helm-mm-3-get-patterns' if it contains a space.
E.g., \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with the regexp of
the same cons cell against CANDIDATE.
I.e. (identity (string-match \"foo\" \"foo bar\")) => t."
  (let ((pat (helm-mm-3-get-patterns pattern)))
    (cl-loop for (predicate . regexp) in pat
             for re = (if (and helm-mm--match-on-diacritics
                               (not (helm-mm-regexp-p regexp)))
                          (char-fold-to-regexp regexp)
                        regexp)
             always (funcall predicate
                             (condition-case _err
                                 ;; FIXME: Probably do nothing when
                                 ;; using fuzzy leaving the job
                                 ;; to the fuzzy fn.
                                 (string-match re candidate)
                               (invalid-regexp nil))))))

(defun helm-mm-3-search-base (pattern searchfn1 searchfn2)
  "Try to find PATTERN in `helm-buffer' with SEARCHFN1 and SEARCHFN2.
This is the search function for `candidates-in-buffer' enabled sources.
Use the same method as `helm-mm-3-match' except it search in buffer
instead of matching on a string.
i.e (identity (re-search-forward \"foo\" (pos-eol) t)) => t."
  (cl-loop with pat = (if (stringp pattern)
                          (helm-mm-3-get-patterns pattern)
                          pattern)
           with regex = (cdar pat)
           with regex1 = (if (and regex
                                  helm-mm--match-on-diacritics
                                  (not (helm-mm-regexp-p regex)))
                             (char-fold-to-regexp regex)
                           regex)
           when (eq (caar pat) 'not) return
           ;; Pass the job to `helm-search-match-part'.
           (prog1 (list (pos-bol) (pos-eol))
             (forward-line 1))
           while (condition-case _err
                     (funcall searchfn1 (or regex1 "") nil t)
                   (invalid-regexp nil))
           for bol = (pos-bol)
           for eol = (pos-eol)
           if (cl-loop for (pred . str) in (cdr pat)
                       for regexp = (if (and helm-mm--match-on-diacritics
                                             (not (helm-mm-regexp-p str)))
                                        (char-fold-to-regexp str)
                                      str)
                       always
                       (progn (goto-char bol)
                              (funcall pred (condition-case _err
                                                (funcall searchfn2 regexp eol t)
                                              (invalid-regexp nil)))))
           do (helm-mm-3--search-move-forward bol eol) and return t
           else do (helm-mm-3--search-move-forward bol eol)
           finally return nil))

(defun helm-mm-3--search-move-forward (bol eol)
  "Move point forward for next search.
Forward line on empty lines, otherwise goto eol."
  (if (eql bol eol) (forward-line 1) (goto-char eol)))

(defun helm-mm-3-search (pattern &rest _ignore)
  (helm-mm-3-search-base
   pattern 're-search-forward 're-search-forward))

(defun helm-mm-3-search-on-diacritics (pattern &rest _ignore)
  (let ((helm-mm--match-on-diacritics t))
    (helm-mm-3-search pattern)))

;;; mp-3 with migemo
;;  Needs https://github.com/emacs-jp/migemo
;;
(defvar helm-mm--previous-migemo-info nil
  "[Internal] Cache previous migemo query.")
(make-local-variable 'helm-mm--previous-migemo-info)

(declare-function migemo-get-pattern "ext:migemo.el")
(declare-function migemo-search-pattern-get "ext:migemo.el")

(define-minor-mode helm-migemo-mode
    "Enable migemo in helm.
It will be available in the sources handling it,
i.e. the sources which have the slot :migemo with non--nil value."
  :lighter " Hmio"
  :group 'helm
  :global t
  (cl-assert (featurep 'migemo)
             nil "No feature called migemo found, install migemo.el."))

(defun helm-mm-migemo-get-pattern (pattern)
  (let ((regex (migemo-get-pattern pattern)))
    (if (ignore-errors (string-match regex "") t)
        (concat regex "\\|" pattern) pattern)))

(defun helm-mm-migemo-search-pattern-get (pattern)
  (let ((regex (migemo-search-pattern-get pattern)))
    (if (ignore-errors (string-match regex "") t)
        (concat regex "\\|" pattern) pattern)))

(defun helm-mm-migemo-string-match (pattern str)
  "Migemo version of `string-match'."
  (unless (assoc pattern helm-mm--previous-migemo-info)
    (with-helm-buffer
      (setq helm-mm--previous-migemo-info
            (push (cons pattern (helm-mm-migemo-get-pattern pattern))
                  helm-mm--previous-migemo-info))))
  (string-match (assoc-default pattern helm-mm--previous-migemo-info) str))

(defun helm-mm-diacritics-string-match (pattern str)
  "Check if PATTERN match STR ignoring diacritics.

If PATTERN is a regexp (i.e. `helm-mm-regexp-p') use PATTERN
unmodified, otherwise transform PATTERN with `char-fold-to-regexp'.

This function is used to search match-part of candidate in in-buffer
sources."
  (string-match (if (helm-mm-regexp-p pattern)
                    pattern
                  (char-fold-to-regexp pattern))
                str))

(cl-defun helm-mm-3-migemo-match (candidate &optional (pattern helm-pattern))
  (and helm-migemo-mode
       (cl-loop for (pred . re) in (helm-mm-3-get-patterns pattern)
                always (funcall pred (helm-mm-migemo-string-match re candidate)))))

(defun helm-mm-migemo-forward (word &optional bound noerror count)
  (with-helm-buffer
    (unless (assoc word helm-mm--previous-migemo-info)
      (setq helm-mm--previous-migemo-info
            (push (cons word (if (delq 'ascii (find-charset-string word))
                                 word
                               (helm-mm-migemo-search-pattern-get word)))
                  helm-mm--previous-migemo-info))))
  (re-search-forward
   (assoc-default word helm-mm--previous-migemo-info) bound noerror count))

(defun helm-mm-3-migemo-search (pattern &rest _ignore)
  (and helm-migemo-mode
       (helm-mm-3-search-base
        pattern 'helm-mm-migemo-forward 'helm-mm-migemo-forward)))


;;; mp-3p- (multiple regexp pattern 3 with prefix search)
;;
;;
(defun helm-mm-3p-match (candidate &optional pattern)
  "Check if PATTERN match CANDIDATE.
Same as `helm-mm-3-match' but only for the cdr of patterns, the car of
patterns must always match CANDIDATE prefix.
E.g. \"bar foo baz\" will match \"barfoobaz\" or \"barbazfoo\" but not
\"foobarbaz\" whereas `helm-mm-3-match' would match all."
  (let* ((pat (helm-mm-3-get-patterns (or pattern helm-pattern)))
         (first (car pat)))
    (and (funcall (car first) (helm-mm-prefix-match candidate (cdr first)))
         (cl-loop for (predicate . regexp) in (cdr pat)
               always (funcall predicate (string-match regexp candidate))))))

(defun helm-mm-3p-search (pattern &rest _ignore)
  (helm-mm-3-search-base
   pattern 'helm-mm-prefix-search 're-search-forward))


;;; Generic multi-match/search functions
;;
;;
(cl-defun helm-mm-match (candidate &optional (pattern helm-pattern))
  "Call `helm-mm-matching-method' function against CANDIDATE."
  (let ((fun (cl-ecase helm-mm-matching-method
               (multi1 #'helm-mm-1-match)
               (multi2 #'helm-mm-2-match)
               (multi3 #'helm-mm-3-match)
               (multi3p #'helm-mm-3p-match))))
    (funcall fun candidate pattern)))

(cl-defun helm-mm-3-match-on-diacritics (candidate &optional (pattern helm-pattern))
  "Same as `helm-mm-3-match' but match on diacritics if possible."
  (let ((helm-mm--match-on-diacritics t))
    (helm-mm-match candidate pattern)))

(defun helm-mm-search (pattern &rest _ignore)
  "Search for PATTERN with `helm-mm-matching-method' function."
  (let ((fun (cl-ecase helm-mm-matching-method
               (multi1 #'helm-mm-1-search)
               (multi2 #'helm-mm-2-search)
               (multi3 #'helm-mm-3-search)
               (multi3p #'helm-mm-3p-search))))
    (funcall fun pattern)))


(provide 'helm-multi-match)


;;; helm-multi-match.el ends here
