;;; ascii-art-to-unicode.el --- a small artist adjunct -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Maintainer: Thien-Thi Nguyen <ttn@gnu.org>
;; Version: 1.13
;; Keywords: ascii, unicode, box-drawing
;; URL: http://www.gnuvola.org/software/aa2u/

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

;; The command ‘aa2u’ converts simple ASCII art line drawings in
;; the {active,accessible} region of the current buffer to Unicode.
;; Command ‘aa2u-rectangle’ is like ‘aa2u’, but works on rectangles.
;;
;; Example use case:
;; - M-x artist-mode RET
;; - C-c C-a r               ; artist-select-op-rectangle
;; - (draw two rectangles)
;;
;;   +---------------+
;;   |               |
;;   |       +-------+--+
;;   |       |       |  |
;;   |       |       |  |
;;   |       |       |  |
;;   +-------+-------+  |
;;           |          |
;;           |          |
;;           |          |
;;           +----------+
;;
;; - C-c C-c                 ; artist-mode-off (optional)
;; - C-x n n                 ; narrow-to-region
;; - M-x aa2u RET
;;
;;   ┌───────────────┐
;;   │               │
;;   │       ┌───────┼──┐
;;   │       │       │  │
;;   │       │       │  │
;;   │       │       │  │
;;   └───────┼───────┘  │
;;           │          │
;;           │          │
;;           │          │
;;           └──────────┘
;;
;; Much easier on the eyes now!
;;
;; Normally, lines are drawn with the ‘LIGHT’ weight.  If you set var
;; ‘aa2u-uniform-weight’ to symbol ‘HEAVY’, you will see, instead:
;;
;;   ┏━━━━━━━━━━━━━━━┓
;;   ┃               ┃
;;   ┃       ┏━━━━━━━╋━━┓
;;   ┃       ┃       ┃  ┃
;;   ┃       ┃       ┃  ┃
;;   ┃       ┃       ┃  ┃
;;   ┗━━━━━━━╋━━━━━━━┛  ┃
;;           ┃          ┃
;;           ┃          ┃
;;           ┃          ┃
;;           ┗━━━━━━━━━━┛
;;
;; To protect particular ‘|’, ‘-’ or ‘+’ characters from conversion,
;; you can set the property ‘aa2u-text’ on that text with command
;; ‘aa2u-mark-as-text’.  A prefix arg clears the property, instead.
;; (You can use ‘describe-text-properties’ to check.)  For example:
;;
;;      ┌───────────────────┐
;;      │                   │
;;      │ |\/|              │
;;      │ ‘Oo’   --Oop Ack! │
;;      │  ^&-MM.           │
;;      │                   │
;;      └─────────┬─────────┘
;;                │
;;            """""""""
;;
;; Command ‘aa2u-mark-rectangle-as-text’ is similar, for rectangles.
;;
;; Tip: For best results, you should make sure all the tab characaters
;; are converted to spaces.  See: ‘untabify’, ‘indent-tabs-mode’.

;;; Code:

(require 'cl-lib)
(require 'pcase)

(autoload 'apply-on-rectangle "rect")

(defvar aa2u-uniform-weight 'LIGHT
  "A symbol, one of: ‘LIGHT’, ‘HEAVY’, ‘DOUBLE’.
This specifies the weight of all the lines.")

;;;---------------------------------------------------------------------------
;;; support

(defalias 'aa2u--lookup-char
  ;; Keep some slack: don't ‘eval-when-compile’ here.
  (if (hash-table-p (ucs-names))
      ;; Emacs 26 and later
      #'gethash
    ;; prior to Emacs 26
    (lambda (string alist)
      (cdr (assoc-string string alist)))))

(defsubst aa2u--text-p (pos)
  (get-text-property pos 'aa2u-text))

(defun aa2u-ucs-bd-uniform-name (&rest components)
  "Return the name of the UCS box-drawing char w/ COMPONENTS.
The string begins with \"BOX DRAWINGS\"; followed by the weight
as per variable ‘aa2u-uniform-weight’, followed by COMPONENTS,
a list of one or two symbols from the set:

  VERTICAL
  HORIZONTAL
  DOWN
  UP
  RIGHT
  LEFT

If of length two, the first element in COMPONENTS should be
the \"Y-axis\" (VERTICAL, DOWN, UP).  In that case, the returned
string includes \"AND\" between the elements of COMPONENTS.

Lastly, all words are separated by space (U+20)."
  (format "BOX DRAWINGS %s %s"
          aa2u-uniform-weight
          (mapconcat 'symbol-name components
                     " AND ")))

(defun aa2u-1c (stringifier &rest components)
  "Apply STRINGIFIER to COMPONENTS; return the UCS char w/ this name.
The char is a string (of length one), with two properties:

  aa2u-stringifier
  aa2u-components

Their values are STRINGIFIER and COMPONENTS, respectively."
  (let* ((store (ucs-names))
         (key (apply stringifier components))
         (s (string (if (hash-table-p store)
                        ;; modern: hash table
                        (gethash key store)
                      ;; classic: alist
                      (cdr (assoc-string key store))))))
    (propertize s
                'aa2u-stringifier stringifier
                'aa2u-components components)))

(defun aa2u-phase-1 ()
  (cl-flet
      ((gsr (was name)
            (goto-char (point-min))
            (let ((now (aa2u-1c 'aa2u-ucs-bd-uniform-name name)))
              (while (search-forward was nil t)
                (unless (aa2u--text-p (match-beginning 0))
                  (replace-match now t t))))))
    (gsr "|" 'VERTICAL)
    (gsr "-" 'HORIZONTAL)))

(defun aa2u-replacement (pos)
  (let ((cc (- pos (line-beginning-position))))
    (cl-flet*
        ((ok (name pos)
             (when (or
                    ;; Infer LIGHTness between "snug" ‘?+’es.
                    ;;              |
                    ;;  +-----------++--+   +
                    ;;  | somewhere ++--+---+-+----+
                    ;;  +-+---------+ nowhere |+--+
                    ;;    +         +---------++
                    ;;              |      +---|
                    (eq ?+ (char-after pos))
                    ;; Require properly directional neighborliness.
                    (memq (cl-case name
                            ((UP DOWN)    'VERTICAL)
                            ((LEFT RIGHT) 'HORIZONTAL))
                          (get-text-property pos 'aa2u-components)))
               name))
         (v (name dir) (let ((bol (line-beginning-position dir))
                             (eol (line-end-position dir)))
                         (when (< cc (- eol bol))
                           (ok name (+ bol cc)))))
         (h (name dir) (let ((bol (line-beginning-position))
                             (eol (line-end-position))
                             (pos (+ pos dir)))
                         (unless (or (> bol pos)
                                     (<= eol pos))
                           (ok name pos))))
         (two-p (ls) (= 2 (length ls)))
         (just (&rest args) (delq nil args)))
      (apply 'aa2u-1c
             'aa2u-ucs-bd-uniform-name
             (just (pcase (just (v 'UP   0)
                                (v 'DOWN 2))
                     ((pred two-p) 'VERTICAL)
                     (`(,vc)        vc)
                     (_             nil))
                   (pcase (just (h 'LEFT  -1)
                                (h 'RIGHT  1))
                     ((pred two-p) 'HORIZONTAL)
                     (`(,hc)        hc)
                     (_             nil)))))))

(defun aa2u-phase-2 ()
  (goto-char (point-min))
  (let (changes)
    ;; (phase 2.1 -- what WOULD change)
    ;; This is for the benefit of ‘aa2u-replacement ok’, which
    ;; otherwise (monolithic phase 2) would need to convert the
    ;; "properly directional neighborliness" impl from a simple
    ;; ‘memq’ to an ‘intersction’.
    (while (search-forward "+" nil t)
      (let ((p (point)))
        (unless (aa2u--text-p (1- p))
          (push (cons p (or (aa2u-replacement (1- p))
                            "?"))
                changes))))
    ;; (phase 2.2 -- apply changes)
    (dolist (ch changes)
      (goto-char (car ch))
      (delete-char -1)
      (insert (cdr ch)))))

(defun aa2u-phase-3 ()
  (remove-text-properties (point-min) (point-max)
                          (list 'aa2u-stringifier nil
                                'aa2u-components nil)))

;;;---------------------------------------------------------------------------
;;; commands

;;;###autoload
(defun aa2u (beg end &optional interactive)
  "Convert simple ASCII art line drawings to Unicode.
Specifically, perform the following replacements:

  - (hyphen)          BOX DRAWINGS LIGHT HORIZONTAL
  | (vertical bar)    BOX DRAWINGS LIGHT VERTICAL
  + (plus)            (one of)
                      BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
                      BOX DRAWINGS LIGHT DOWN AND RIGHT
                      BOX DRAWINGS LIGHT DOWN AND LEFT
                      BOX DRAWINGS LIGHT UP AND RIGHT
                      BOX DRAWINGS LIGHT UP AND LEFT
                      BOX DRAWINGS LIGHT VERTICAL AND RIGHT
                      BOX DRAWINGS LIGHT VERTICAL AND LEFT
                      BOX DRAWINGS LIGHT UP AND HORIZONTAL
                      BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
                      BOX DRAWINGS LIGHT UP
                      BOX DRAWINGS LIGHT DOWN
                      BOX DRAWINGS LIGHT LEFT
                      BOX DRAWINGS LIGHT RIGHT
                      QUESTION MARK

More precisely, hyphen and vertical bar are substituted unconditionally,
first, and plus is substituted with a character depending on its north,
south, east and west neighbors.

NB: Actually, ‘aa2u’ can also use \"HEAVY\" instead of \"LIGHT\",
depending on the value of variable ‘aa2u-uniform-weight’.

This command operates on either the active region,
or the accessible portion otherwise."
  (interactive "r\np")
  ;; This weirdness, along w/ the undocumented "p" in the ‘interactive’
  ;; form, is to allow ‘M-x aa2u’ (interactive invocation) w/ no region
  ;; selected to default to the accessible portion (as documented), which
  ;; was the norm in ascii-art-to-unicode.el prior to 1.5.  A bugfix,
  ;; essentially.  This is ugly, unfortunately -- is there a better way?!
  (when (and interactive (not (region-active-p)))
    (setq beg (point-min)
          end (point-max)))
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region beg end)
      (aa2u-phase-1)
      (aa2u-phase-2)
      (aa2u-phase-3))))

;;;###autoload
(defun aa2u-rectangle (start end)
  "Like ‘aa2u’ on the region-rectangle.
When called from a program the rectangle's corners
are START (top left) and END (bottom right)."
  (interactive "r")
  (let* ((was (delete-extract-rectangle start end))
         (now (with-temp-buffer
                (insert-rectangle was)
                (aa2u (point) (mark))
                (extract-rectangle (point-min) (point-max)))))
    (goto-char (min start end))
    (insert-rectangle now)))

;;;###autoload
(defun aa2u-mark-as-text (start end &optional unmark)
  "Set property ‘aa2u-text’ of the text from START to END.
This prevents ‘aa2u’ from misinterpreting \"|\", \"-\" and \"+\"
in that region as lines and intersections to be replaced.
Prefix arg means to remove property ‘aa2u-text’, instead."
  (interactive "r\nP")
  (funcall (if unmark
               'remove-text-properties
             'add-text-properties)
           start end
           '(aa2u-text t)))

;;;###autoload
(defun aa2u-mark-rectangle-as-text (start end &optional unmark)
  "Like ‘aa2u-mark-as-text’ on the region-rectangle.
When called from a program the rectangle's corners
are START (top left) and END (bottom right)."
  (interactive "r\nP")
  (apply-on-rectangle
   (lambda (scol ecol unmark)
     (let ((p (point)))
       (aa2u-mark-as-text (+ p scol) (+ p ecol) unmark)))
   start end
   unmark))

;;;---------------------------------------------------------------------------
;;; that's it

;;;; ChangeLog:

;; 2020-11-24  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.13
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	[Version]: Bump to "1.13".
;; 
;; 2020-11-24  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Update years in copyright notice; nfc.
;; 
;; 2020-11-24  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Handle modern ‘ucs-names’ being a hash table.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el (aa2u-1c): If
;; 	the ‘ucs-names’ returns a hash table, use ‘gethash’; otherwise, fall
;; 	back to classic ‘assoc-string’.
;; 
;; 2018-04-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.12
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	[Version]: Bump to "1.12".
;; 
;; 2018-04-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Mention ‘DOUBLE’ in ‘aa2u-uniform-weight’ docstring.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-uniform-weight): ...here.
;; 
;; 2018-04-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Fix docstring for ‘aa2u-ucs-bd-uniform-name’.
;; 
;; 	Omission from 2014-05-09, "Make weight dynamically customizable".
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-ucs-bd-uniform-name): Don't mention WEIGHT in docstring.
;; 
;; 2018-04-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Update years in copyright notice; nfc.
;; 
;; 2018-04-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Use U+2018, U+2019 instead of U+60, U+27.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	[Commentary]: ...here.
;; 	(aa2u-uniform-weight, aa2u-ucs-bd-uniform-name)
;; 	(aa2u, aa2u-rectangle, aa2u-mark-as-text)
;; 	(aa2u-mark-rectangle-as-text): Likewise, in docstring.
;; 
;; 2017-10-04  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Add Kaushal Modi to THANKS; nfc.
;; 
;; 2017-10-03  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.11
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.11".
;; 
;; 2017-10-03  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u slog] Fix botched bifurcation.
;; 
;; 	Bug introduced 2017-10-03, "Handle ‘ucs-names’ that returns a hash
;; 	table".	 Culprit: No testing (sigh).
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u--lookup): Delete alias.
;; 	(aa2u--lookup-char): New alias.
;; 	(aa2u-1c): Use ‘aa2u--lookup-char’.
;; 
;; 2017-10-03  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.10
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.10".
;; 
;; 2017-10-03  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u slog] Handle ‘ucs-names’ that returns a hash table.
;; 
;; 	Reported by Kaushal Modi (bug#28688): 
;; 	http://lists.gnu.org/archive/html/bug-gnu-emacs/2017-10/threads.html
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u--lookup): New alias.
;; 	(aa2u-1c): Use ‘aa2u--lookup’.
;; 
;; 2017-02-04  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u int] Update years in copyright notice; nfc.
;; 
;; 2017-02-04  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	Specify copyright update policy in some HACKING files; nfc.
;; 
;; 2017-02-04  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	Add some THANKS files; nfc.
;; 
;; 2014-05-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.9
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.9".
;; 
;; 2014-05-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Mention TAB infelicity.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Commentary]:
;; 	...here.
;; 
;; 2014-05-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Update homepage; drop other links.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [URL]: New
;; 	header.
;; 	[Commentary]: Remove the HACKING and Tip Jar links.
;; 
;; 2014-05-29  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] New command: aa2u-mark-rectangle-as-text
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el: Arrange to
;; 	autoload "rect" for ‘apply-on-rectangle’.
;; 	(aa2u-mark-rectangle-as-text): New command, w/ autoload cookie.
;; 
;; 2014-05-24  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Mention TAB infelicity in HACKING; nfc.
;; 
;; 2014-05-21  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.8
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.8".
;; 
;; 2014-05-21  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] New command: aa2u-mark-as-text
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u--text-p): New defsubst.
;; 	(aa2u-phase-1, aa2u-phase-2): If the character in question is
;; 	‘aa2u--text-p’, just ignore it.
;; 	(aa2u-mark-as-text): New command, w/ autoload cookie.
;; 
;; 2014-05-21  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u int] Add abstraction: gsr
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-phase-1 gsr): New internal func.
;; 
;; 2014-05-21  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Declare package keywords.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Keywords]: New
;; 	header.
;; 
;; 2014-05-21  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Add ‘Maintainer’ header per top-level README; nfc.
;; 
;; 2014-05-11  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.7
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.7".
;; 
;; 2014-05-11  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] New command: aa2u-rectangle
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-rectangle): New command.
;; 
;; 2014-05-11  Andreas Schwab  <schwab@linux-m68k.org>
;; 
;; 	ascii-art-to-unicode.el (aa2u-replacement): Use cl-case instead of case.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	fixup! [aa2u] Make weight dynamically customizable.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Update HACKING; nfc.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Make weight dynamically customizable.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-uniform-weight): New defvar.
;; 	(aa2u-ucs-bd-uniform-name): Don't take arg WEIGHT; instead, consult
;; 	‘aa2u-uniform-weight’.
;; 	(aa2u-phase-1, aa2u-replacement): Update calls to
;; 	‘aa2u-ucs-bd-uniform-name’.
;; 	(aa2u): Mention new var in docstring.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u int] Compute vertical/horizontal components separately.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-replacement ok): Recognize ‘UP’, ‘DOWN’, ‘LEFT’, ‘RIGHT’ instead
;; 	of ‘n’, ‘s’, ‘w’, ‘e’.
;; 	(aa2u-replacement two-p): New internal func.
;; 	(aa2u-replacement just): Likewise.
;; 	(aa2u-replacement): Don't glom everything for one ‘pcase’; instead,
;; 	construct args to ‘aa2u-ucs-bd-uniform-name’ by computing vertical and
;; 	horizontal components separately.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u int] Don't use ‘cl-labels’ when ‘cl-flet*’ will do.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el
;; 	(aa2u-replacement): ...here.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u int] Add "Tip Jar" URL in Commentary; nfc.
;; 
;; 2014-05-09  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u maint] Extract NEWS and HACKING to separate files; nfc.
;; 
;; 2014-05-08  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.6
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.6".
;; 
;; 2014-05-08  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Fix bug: Make ‘M-x aa2u’ operate on accessible portion.
;; 
;; 	Regression introduced 2014-04-03, "Make ‘aa2u’ region-aware".
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el (aa2u): Take
;; 	optional arg INTERACTIVE; add "p" to ‘interactive’ form; when
;; 	INTERACTIVE and region is not active, set BEG, END.
;; 
;; 2014-04-03  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Release: 1.5
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el [Version]: Bump
;; 	to "1.5".
;; 
;; 2014-04-03  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	[aa2u] Make ‘aa2u’ region-aware.
;; 
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el (aa2u): Take
;; 	args BEG and END; use "r" in ‘interactive’ spec; don't bother w/
;; 	internal func ‘do-it!’.
;; 
;; 2014-01-14  Thien-Thi Nguyen  <ttn@gnu.org>
;; 
;; 	New package: ascii-art-to-unicode
;; 
;; 	* packages/ascii-art-to-unicode/: New dir.
;; 	* packages/ascii-art-to-unicode/ascii-art-to-unicode.el: New file.
;; 


(provide 'ascii-art-to-unicode)

;;; ascii-art-to-unicode.el ends here
