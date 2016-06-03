;;; rst-lists.el --- Build Sphinx projects.

;; Copyright (C) 2012 Wei-Wei Guo.

;; Author: Wei-Wei Guo <wwguocn at gmail dot com>
;; Version: 0.1
;;
;; This file is published under the GNU Gerneral Public License,
;; see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file provides funcationalities for Emacs rst-mode.

(defgroup rst-list nil
  "Functions supporting of directives with \\[rst-lists]."
  :group 'rst
  :version "24.5")

(defconst rst-list-bullets
  ;; Sorted so they can form a character class when concatenated.
  '(?- ?* ?+ ?\u2022 ?\u2023 ?\u2043)
  "List of all possible bullet characters for bulleted lists.")

(defconst rst-list-roman-number-list
  '("I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X" "XI" "XII" "XIII" "XIV" "XV"
    "XVI" "XVII" "XVIII" "XIX" "XX" "XXI" "XXII" "XXIII" "XXIV" "XXV" "XXVI" "XXVII"
    "XXVIII" "XXIX" "XXX" "XXXI" "XXXII" "XXXIII" "XXXIV" "XXXV" "XXXVI" "XXXVII"
    "XXXVIII" "XXXIX" "XL" "XLI" "XLII" "XLIII" "XLIV" "XLV" "XLVI" "XLVII" "XLVIII"
    "XLIX" "L")
  "List of Roman numerals.")

(defconst rst-list-letter-list
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
  "List of Latin letter.")

(defvar rst-list-re-bullets
  (format "\\([%s][ \t]\\)[^ \t]" (regexp-quote (concat rst-list-bullets)))
  "Regexp for finding bullets.")

(defvar rst-list-initial-enums
  '("#." "1." "a." "A." "I." "i." "(1)" "(a)" "(A)" "(I)" "(i)" "1)" "a)" "A)" "I)" "i)")
  "List of initial enumerates.")

(defvar rst-list-initial-items
  (append (mapcar 'char-to-string rst-list-bullets) rst-list-initial-enums)
  "List of initial items. It's collection of bullets and enumerations")

(defvar rst-list-re-enumerates
  (format "^[ \t]*\\(%s\\|%s\\)[ \t]"
          "\\([0-9]+\\|[a-zA-Z]\\|[IVXLCDMivxlcdm]+\\)\\."
          "(?\\([0-9]+\\|[a-zA-Z]\\|[IVXLCDMivxlcdm]+\\))")
  "Regexp for finding enumerates (# is not included).")

(defvar rst-list-re-items
  (format "^[ \t]*\\(%s\\|%s\\|%s\\)[ \t]"
          (format "[%s]" (regexp-quote (concat rst-list-bullets)))
          "\\(#\\|[a-z]\\|[0-9]+\\|[A-Z]\\|[IVXLCDM]+\\|[ivxlcdm]+\\)\\."
          "(?\\([a-z]\\|[0-9]+\\|[A-Z]\\|[IVXLCDM]+\\|[ivxlcdm]+\\))")
  "Regexp for finding bullets and enumerates.")

(defun rst-list-insert-list-pos (newitem)
  "Arrage relative position of a newly inserted list item.

Adding a new list might consider three situations:

 (a) Current line is a blank line.
 (b) Previous line is a blank line.
 (c) Following line is a blank line.

When (a) and (b), just add the new list at current line.

when (a) and not (b), add a blank line before adding the new list.

When not (a), add a blank line and a new line at current point.

Other situations are just ignored and left to users themselves."
  (if (save-excursion
        (beginning-of-line)
        (looking-at "^[ \t]*$"))
      (if (save-excursion
            (forward-line -1)
            (looking-at "^[ \t]*$"))
          (insert (concat newitem " "))
        (insert (concat "\n" newitem " ")))
    (progn
      (insert (concat "\n\n" newitem " ")))))

(defun rst-list-insert-list-new-bullet ()
  "Insert a new list bullet.
"
  (interactive)
  (let (itemstyle)
    (setq itemstyle "-")
    (rst-list-insert-list-pos itemstyle)))

(defun rst-list-insert-list-new-item (itemno)
  "Insert a new list item.

User is asked to select the item style first, for example (a), i), +. Use TAB
for completition and choices.

If user selects bullets or #, it's just added with position arranged by
`rst-list-insert-list-new-pos'.

If user selects enumerates, a further prompt is given. User need to input a
starting item, for example 'e' for 'A)' style. The position is also arranged by
`rst-list-insert-list-new-pos'.
"
  (interactive "P")
  (if (not itemno)
      (setq itemno 1))
  (setq itemno (1- itemno))
  (let (itemstyle itemfirst)
    (setq itemstyle (completing-read "Providing perfered item (default '#.'): "
                                     rst-list-initial-items nil t nil nil "#."))
    (when (string-match "[aA1Ii]" itemstyle)
      (setq itemfirst (match-string 0 itemstyle))
      (cond ((equal itemfirst "A")
             (setq itemstyle (replace-match (nth itemno rst-list-letter-list)
                                            nil nil itemstyle)))
            ((equal itemfirst "a")
             (setq itemstyle (replace-match (downcase (nth itemno rst-list-letter-list))
                                            nil nil itemstyle)))
            ((equal itemfirst "I")
             (setq itemstyle (replace-match (nth itemno rst-list-roman-number-list)
                                            nil nil itemstyle)))
            ((equal itemfirst "i")
             (setq itemstyle (replace-match (downcase (nth itemno rst-list-roman-number-list))
                                            nil nil itemstyle)))
            ((equal itemfirst "1")
             (setq itemstyle (replace-match (number-to-string (1+ itemno))
                                            nil nil itemstyle)))
            ))
    (rst-list-insert-list-pos itemstyle)))

(defun rst-list-match-string (reg)
  "Match a regex in a line and return the matched string by match-string.

If nothing matched, a empty string is returned."
  (let (matched)
    (save-excursion
      (end-of-line)
      (if (re-search-backward reg (line-beginning-position) t)
          (setq matched (match-string 0))
        (setq matched "")))
    matched))

(defun rst-list-insert-list-continue ()
  "Insert a list item with current list style and indentation level.

The function works for all style of bullet lists and enumeration lists. Only one
thing need to be noticed:

List style alphabetical list, such as 'a.', and roman numerical list, such as 'i.',
have some overlapping items, for example 'v.' The function can deal with the
problem elegantly in most situations. But when those overlapped list proceeded
by a blank line, it is hard to determine which type to use automatically. The
function uses roman numerical list defaultly. If you want alphabetical list, just
use a prefix (\\[universal-argument]).
"
  (interactive)
  (let (curitem newitem itemno previtem tmpitem)
    (setq curitem (rst-list-match-string rst-list-re-items))
    (cond ((string-match (format "#.\\|[%s]"
                                 (regexp-quote (concat rst-list-bullets))) curitem)
           (setq newitem curitem))
          ((string-match "[0-9]+" curitem)
           (progn
             (setq itemno (1+
                           (string-to-number
                            (match-string 0 curitem))))
             (setq newitem (replace-match
                            (number-to-string itemno)
                            nil nil curitem))))
          ((and (string-match "[IVXLCDMivxlcdm]+" curitem)
                (progn
                  (setq tmpitem (match-string 0 curitem))
                  (or (> (length tmpitem) 1)
                      (and (= (length tmpitem) 1)
                           (progn
                             (save-excursion
                               (forward-line -1)
                               (setq previtem (rst-list-match-string rst-list-re-enumerates))
                               (when (string-match "[a-zA-Z]+" previtem)
                                 (setq previtem (match-string 0 previtem))))
                             (or (> (length previtem) 1)
                                 (= (length previtem) 0)))))))
           (progn
             (string-match "[IVXLCDMivxlcdm]+" curitem)
             (if (isearch-no-upper-case-p tmpitem nil)
                 (progn
                   (setq itemno (car (cdr (member
                                           (match-string 0 (upcase curitem))
                                           rst-list-roman-number-list))))
                   (setq newitem (replace-match (downcase itemno) nil nil curitem)))
               (progn
                 (setq itemno (car (cdr (member
                                         (match-string 0 curitem)
                                        rst-list-roman-number-list))))
                 (setq newitem (replace-match itemno nil nil curitem))))))
          ((string-match "[a-yA-Y]" curitem)
           (progn
             (setq itemno (1+
                           (string-to-char
                            (match-string 0 curitem))))
             (setq newitem (replace-match
                            (char-to-string itemno)
                            nil nil curitem)))))
    (insert (concat "\n" newitem))))

(defun rst-list-insert (itemno)
  "Insert a list item at the current point.

The command can insert a new list or a continuing list. When it is called at a
non-list line, it will promote to insert new list. When it is called at a list
line, it will insert a list with the same list style.

1. When inserting a new list:

User is asked to select the item style first, for example (a), i), +. Use TAB
for completition and choices.

 (a) If user selects bullets or #, it's just added.
 (b) If user selects enumerates, a further prompt is given. User need to input a
starting item, for example 'e' for 'A)' style.

The position of the new list is arranged according whether or not the current line
and the previous line are blank lines.

2. When continuing a list, one thing need to be noticed:

List style alphabetical list, such as 'a.', and roman numerical list, such as 'i.',
have some overlapping items, for example 'v.' The function can deal with the
problem elegantly in most situations. But when those overlapped list proceeded
by a blank line, it is hard to determine which type to use automatically. The
function uses roman numerical list defaultly. If you want alphabetical list, just
use a prefix (\\[universal-argument]).
"
  (interactive "P")
  (if (equal (rst-list-match-string rst-list-re-items) "")
      (if (null current-prefix-arg)
          (rst-list-insert-list-new-bullet)
        (rst-list-insert-list-new-item itemno))
    (rst-list-insert-list-continue)))


(provide 'rst-lists)

