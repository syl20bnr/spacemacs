;;; avy.el --- Jump to arbitrary positions in visible text and select text quickly. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/avy
;; Version: 0.5.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; Keywords: point, location

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With Avy, you can move point to any position in Emacs – even in a
;; different window – using very few keystrokes. For this, you look at
;; the position where you want point to be, invoke Avy, and then enter
;; the sequence of characters displayed at that position.
;;
;; If the position you want to jump to can be determined after only
;; issuing a single keystroke, point is moved to the desired position
;; immediately after that keystroke. In case this isn't possible, the
;; sequence of keystrokes you need to enter is comprised of more than
;; one character. Avy uses a decision tree where each candidate position
;; is a leaf and each edge is described by a character which is distinct
;; per level of the tree. By entering those characters, you navigate the
;; tree, quickly arriving at the desired candidate position, such that
;; Avy can move point to it.
;;
;; Note that this only makes sense for positions you are able to see
;; when invoking Avy. These kinds of positions are supported:
;;
;; * character positions
;; * word or subword start positions
;; * line beginning positions
;; * link positions
;; * window positions
;;
;; If you're familiar with the popular `ace-jump-mode' package, this
;; package does all that and more, without the implementation
;; headache.

;;; Code:
(require 'cl-lib)
(require 'ring)

;;* Customization
(defgroup avy nil
  "Jump to things tree-style."
  :group 'convenience
  :prefix "avy-")

(defcustom avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Default keys for jumping.
Any key is either a character representing a self-inserting
key (letters, digits, punctuation, etc.) or a symbol denoting a
non-printing key like an arrow key (left, right, up, down).  For
non-printing keys, a corresponding entry in
`avy-key-to-char-alist' must exist in order to visualize the key
in the avy overlays.

If `avy-style' is set to words, make sure there are at least three
keys different than the following: a, e, i, o, u, y"
  :type '(repeat :tag "Keys" (choice
                              (character :tag "char")
                              (symbol :tag "non-printing key"))))

(defconst avy--key-type
  '(choice :tag "Command"
    (const avy-goto-char)
    (const avy-goto-char-2)
    (const avy-isearch)
    (const avy-goto-line)
    (const avy-goto-subword-0)
    (const avy-goto-subword-1)
    (const avy-goto-word-0)
    (const avy-goto-word-1)
    (const avy-copy-line)
    (const avy-copy-region)
    (const avy-move-line)
    (const avy-move-region)
    (const avy-kill-whole-line)
    (const avy-kill-region)
    (const avy-kill-ring-save-whole-line)
    (const avy-kill-ring-save-region)
    (function :tag "Other command")))

(defcustom avy-keys-alist nil
  "Alist of avy-jump commands to `avy-keys' overriding the default `avy-keys'."
  :type `(alist
          :key-type ,avy--key-type
          :value-type (repeat :tag "Keys" character)))

(defcustom avy-orders-alist '((avy-goto-char . avy-order-closest))
  "Alist of candidate ordering functions.
Usually, candidates appear in their point position order."
  :type `(alist
          :key-type ,avy--key-type
          :value-type function))

(defcustom avy-words
  '("am" "by" "if" "is" "it" "my" "ox" "up"
    "ace" "act" "add" "age" "ago" "aim" "air" "ale" "all" "and" "ant" "any"
    "ape" "apt" "arc" "are" "arm" "art" "ash" "ate" "awe" "axe" "bad" "bag"
    "ban" "bar" "bat" "bay" "bed" "bee" "beg" "bet" "bid" "big" "bit" "bob"
    "bot" "bow" "box" "boy" "but" "cab" "can" "cap" "car" "cat" "cog" "cop"
    "cow" "cry" "cup" "cut" "day" "dew" "did" "die" "dig" "dim" "dip" "dog"
    "dot" "dry" "dub" "dug" "dye" "ear" "eat" "eel" "egg" "ego" "elf" "eve"
    "eye" "fan" "far" "fat" "fax" "fee" "few" "fin" "fit" "fix" "flu" "fly"
    "foe" "fog" "for" "fox" "fry" "fun" "fur" "gag" "gap" "gas" "gel" "gem"
    "get" "gig" "gin" "gnu" "god" "got" "gum" "gun" "gut" "guy" "gym" "had"
    "hag" "ham" "has" "hat" "her" "hid" "him" "hip" "his" "hit" "hop" "hot"
    "how" "hub" "hue" "hug" "hut" "ice" "icy" "imp" "ink" "inn" "ion" "ire"
    "ivy" "jab" "jam" "jar" "jaw" "jet" "job" "jog" "joy" "key" "kid" "kit"
    "lag" "lap" "lay" "let" "lid" "lie" "lip" "lit" "lob" "log" "lot" "low"
    "mad" "man" "map" "mat" "may" "men" "met" "mix" "mob" "mop" "mud" "mug"
    "nag" "nap" "new" "nil" "nod" "nor" "not" "now" "nun" "oak" "odd" "off"
    "oil" "old" "one" "orb" "ore" "ork" "our" "out" "owl" "own" "pad" "pan"
    "par" "pat" "paw" "pay" "pea" "pen" "pet" "pig" "pin" "pit" "pod" "pot"
    "pry" "pub" "pun" "put" "rag" "ram" "ran" "rat" "raw" "ray" "red" "rib"
    "rim" "rip" "rob" "rod" "rot" "row" "rub" "rug" "rum" "run" "sad" "sat"
    "saw" "say" "sea" "see" "sew" "she" "shy" "sin" "sip" "sit" "six" "ski"
    "sky" "sly" "sob" "son" "soy" "spy" "sum" "sun" "tab" "tad" "tag" "tan"
    "tap" "tar" "tax" "tea" "the" "tie" "tin" "tip" "toe" "ton" "too" "top"
    "toy" "try" "tub" "two" "urn" "use" "van" "war" "was" "wax" "way" "web"
    "wed" "wet" "who" "why" "wig" "win" "wit" "woe" "won" "wry" "you" "zap"
    "zip" "zoo")
  "Words to use in case `avy-style' is set to `words'.
Every word should contain at least one vowel i.e. one of the following
characters: a, e, i, o, u, y
They do not have to be sorted but no word should be a prefix of another one."
  :type '(repeat string))

(defcustom avy-style 'at-full
  "The default method of displaying the overlays.
Use `avy-styles-alist' to customize this per-command."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)
          (const :tag "De Bruijn" de-bruijn)
          (const :tag "Words" words)))

(defcustom avy-styles-alist nil
  "Alist of avy-jump commands to the style for each command.
If the commands isn't on the list, `avy-style' is used."
  :type '(alist
          :key-type (choice :tag "Command"
                     (const avy-goto-char)
                     (const avy-goto-char-2)
                     (const avy-isearch)
                     (const avy-goto-line)
                     (const avy-goto-subword-0)
                     (const avy-goto-subword-1)
                     (const avy-goto-word-0)
                     (const avy-goto-word-1)
                     (const avy-copy-line)
                     (const avy-copy-region)
                     (const avy-move-line)
                     (const avy-move-region)
                     (const avy-kill-whole-line)
                     (const avy-kill-region)
                     (const avy-kill-ring-save-whole-line)
                     (const avy-kill-ring-save-region)
                     (function :tag "Other command"))
          :value-type (choice
                       (const :tag "Pre" pre)
                       (const :tag "At" at)
                       (const :tag "At Full" at-full)
                       (const :tag "Post" post)
                       (const :tag "De Bruijn" de-bruijn)
                       (const :tag "Words" words))))

(defcustom avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?t . avy-action-teleport)
    (?m . avy-action-mark)
    (?n . avy-action-copy)
    (?y . avy-action-yank)
    (?Y . avy-action-yank-line)
    (?i . avy-action-ispell)
    (?z . avy-action-zap-to-char))
  "List of actions for `avy-handler-default'.

Each item is (KEY . ACTION).  When KEY not on `avy-keys' is
pressed during the dispatch, ACTION is set to replace the default
`avy-action-goto' once a candidate is finally selected."
  :type
  '(alist
    :key-type (choice (character :tag "Char"))
    :value-type (choice
                 (const :tag "Mark" avy-action-mark)
                 (const :tag "Copy" avy-action-copy)
                 (const :tag "Kill and move point" avy-action-kill-move)
                 (const :tag "Kill" avy-action-kill-stay))))

(defcustom avy-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)

(defcustom avy-all-windows t
  "Determine the list of windows to consider in search of candidates."
  :type
  '(choice
    (const :tag "All Frames" all-frames)
    (const :tag "This Frame" t)
    (const :tag "This Window" nil)))

(defcustom avy-case-fold-search t
  "Non-nil if searches should ignore case."
  :type 'boolean)

(defcustom avy-word-punc-regexp "[!-/:-@[-`{-~]"
  "Regexp of punctuation chars that count as word starts for `avy-goto-word-1.
When nil, punctuation chars will not be matched.

\"[!-/:-@[-`{-~]\" will match all printable punctuation chars."
  :type 'regexp)

(defcustom avy-goto-word-0-regexp "\\b\\sw"
  "Regexp that determines positions for `avy-goto-word-0'."
  :type '(choice
          (const :tag "Default" "\\b\\sw")
          (const :tag "Symbol" "\\_<\\(\\sw\\|\\s_\\)")
          (const :tag "Not whitespace" "[^ \r\n\t]+")
          (regexp :tag "Regex")))

(defcustom avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode)
  "List of modes to ignore when searching for candidates.
Typically, these modes don't use the text representation."
  :type 'list)

(defcustom avy-single-candidate-jump t
  "In case there is only one candidate jumps directly to it."
  :type 'boolean)

(defcustom avy-del-last-char-by '(?\b ?\d)
  "List of event types, i.e. key presses, that delete the last
character read.  The default represents `C-h' and `DEL'.  See
`event-convert-list'."
  :type 'list)

(defcustom avy-escape-chars '(?\e ?\C-g)
  "List of characters that quit avy during `read-char'."
  :type 'list)

(defvar avy-ring (make-ring 20)
  "Hold the window and point history.")

(defvar avy-translate-char-function #'identity
  "Function to translate user input key into another key.
For example, to make SPC do the same as ?a, use
\(lambda (c) (if (= c 32) ?a c)).")

(defface avy-lead-face-0
  '((t (:foreground "white" :background "#4f57f9")))
  "Face used for first non-terminating leading chars.")

(defface avy-lead-face-1
  '((t (:foreground "white" :background "gray")))
  "Face used for matched leading chars.")

(defface avy-lead-face-2
  '((t (:foreground "white" :background "#f86bf3")))
  "Face used for leading chars.")

(defface avy-lead-face
  '((t (:foreground "white" :background "#e52b50")))
  "Face used for the leading chars.")

(defface avy-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defface avy-goto-char-timer-face
  '((t (:inherit highlight)))
  "Face for matches during reading chars using `avy-goto-char-timer'.")

(defconst avy-lead-faces '(avy-lead-face
                           avy-lead-face-0
                           avy-lead-face-2
                           avy-lead-face
                           avy-lead-face-0
                           avy-lead-face-2)
  "Face sequence for `avy--overlay-at-full'.")

(defvar avy-key-to-char-alist '((left . ?◀)
                                (right . ?▶)
                                (up . ?▲)
                                (down . ?▼)
                                (prior . ?△)
                                (next . ?▽))
  "An alist from non-character keys to printable chars used in avy overlays.
This alist must contain all keys used in `avy-keys' which are not
self-inserting keys and thus aren't read as characters.")

;;* Internals
;;** Tree
(defmacro avy-multipop (lst n)
  "Remove LST's first N elements and return them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

(defun avy--de-bruijn (keys n)
  "De Bruijn sequence for alphabet KEYS and subsequences of length N."
  (let* ((k (length keys))
         (a (make-list (* n k) 0))
         sequence)
    (cl-labels ((db (T p)
                  (if (> T n)
                      (if (eq (% n p) 0)
                          (setq sequence
                                (append sequence
                                        (cl-subseq a 1 (1+ p)))))
                    (setf (nth T a) (nth (- T p) a))
                    (db (1+ T) p)
                    (cl-loop for j from (1+ (nth (- T p) a)) to (1- k) do
                         (setf (nth T a) j)
                         (db (1+ T) T)))))
      (db 1 1)
      (mapcar (lambda (n)
                (nth n keys))
              sequence))))

(defun avy--path-alist-1 (lst seq-len keys)
  "Build a De Bruin sequence from LST.
SEQ-LEN is how many elements of KEYS it takes to identify a match."
  (let ((db-seq (avy--de-bruijn keys seq-len))
        prev-pos prev-seq prev-win path-alist)
    ;; The De Bruijn seq is cyclic, so append the seq-len - 1 first chars to
    ;; the end.
    (setq db-seq (nconc db-seq (cl-subseq db-seq 0 (1- seq-len))))
    (cl-labels ((subseq-and-pop ()
                  (when (nth (1- seq-len) db-seq)
                    (prog1 (cl-subseq db-seq 0 seq-len)
                      (pop db-seq)))))
      (while lst
        (let* ((cur (car lst))
               (pos (cond
                      ;; ace-window has matches of the form (pos . wnd)
                      ((integerp (car cur)) (car cur))
                      ;; avy-jump have form ((start . end) . wnd)
                      ((consp (car cur)) (caar cur))
                      (t (error "Unexpected match representation: %s" cur))))
               (win (cdr cur))
               (path (if prev-pos
                         (let ((diff (if (eq win prev-win)
                                         (- pos prev-pos)
                                       0)))
                           (when (and (> diff 0) (< diff seq-len))
                             (while (and (nth (1- seq-len) db-seq)
                                         (not
                                          (eq 0
                                              (cl-search
                                               (cl-subseq prev-seq diff)
                                               (cl-subseq db-seq 0 seq-len)))))
                               (pop db-seq)))
                           (subseq-and-pop))
                       (subseq-and-pop))))
          (if (not path)
              (setq lst nil
                    path-alist nil)
            (push (cons path (car lst)) path-alist)
            (setq prev-pos pos
                  prev-seq path
                  prev-win win
                  lst (cdr lst))))))
    (nreverse path-alist)))

(defun avy-order-closest (x)
  (abs (- (if (numberp (car x))
              (car x)
            (caar x))
          (point))))

(defvar avy-command nil
  "Store the current command symbol.
E.g. `avy-goto-line' or `avy-goto-char'.")

(defun avy-tree (lst keys)
  "Coerce LST into a balanced tree.
The degree of the tree is the length of KEYS.
KEYS are placed appropriately on internal nodes."
  (let* ((len (length keys))
         (order-fn (cdr (assq avy-command avy-orders-alist)))
         (lst (if order-fn
                  (cl-sort lst #'< :key order-fn)
                lst)))
    (cl-labels
        ((rd (ls)
           (let ((ln (length ls)))
             (if (< ln len)
                 (cl-pairlis keys
                             (mapcar (lambda (x) (cons 'leaf x)) ls))
               (let ((ks (copy-sequence keys))
                     res)
                 (dolist (s (avy-subdiv ln len))
                   (push (cons (pop ks)
                               (if (eq s 1)
                                   (cons 'leaf (pop ls))
                                 (rd (avy-multipop ls s))))
                         res))
                 (nreverse res))))))
      (rd lst))))

(defun avy-subdiv (n b)
  "Distribute N in B terms in a balanced way."
  (let* ((p (1- (floor (+ (log n b) 1e-6))))
         (x1 (expt b p))
         (x2 (* b x1))
         (delta (- n x2))
         (n2 (/ delta (- x2 x1)))
         (n1 (- b n2 1)))
    (append
     (make-list n1 x1)
     (list
      (- n (* n1 x1) (* n2 x2)))
     (make-list n2 x2))))

(defun avy-traverse (tree walker &optional recur-key)
  "Traverse TREE generated by `avy-tree'.
WALKER is a function that takes KEYS and LEAF.

RECUR-KEY is used in recursion.

LEAF is a member of LST argument of `avy-tree'.

KEYS is the path from the root of `avy-tree' to LEAF."
  (dolist (br tree)
    (let ((key (cons (car br) recur-key)))
      (if (eq (cadr br) 'leaf)
          (funcall walker key (cddr br))
        (avy-traverse (cdr br) walker key)))))

(defvar avy-action nil
  "Function to call at the end of select.")

(defvar avy-action-oneshot nil
  "Function to call once at the end of select.")

(defun avy-handler-default (char)
  "The default handler for a bad CHAR."
  (let (dispatch)
    (cond ((setq dispatch (assoc char avy-dispatch-alist))
           (unless (eq avy-style 'words)
             (setq avy-action (cdr dispatch)))
           (throw 'done 'restart))
          ((memq char avy-escape-chars)
           ;; exit silently
           (throw 'done 'abort))
          ((eq char ??)
           (avy-show-dispatch-help)
           (throw 'done 'restart))
          ((mouse-event-p char)
           (signal 'user-error (list "Mouse event not handled" char)))
          (t
           (message "No such candidate: %s, hit `C-g' to quit."
                    (if (characterp char) (string char) char))))))

(defun avy-show-dispatch-help ()
  "Display action shortucts in echo area."
  (let ((len (length "avy-action-")))
    (message "%s" (mapconcat
                   (lambda (x)
                     (format "%s: %s"
                             (propertize
                              (char-to-string (car x))
                              'face 'aw-key-face)
                             (substring (symbol-name (cdr x)) len)))
                   avy-dispatch-alist
                   " "))))

(defvar avy-handler-function 'avy-handler-default
  "A function to call for a bad `read-key' in `avy-read'.")

(defvar avy-current-path ""
  "Store the current incomplete path during `avy-read'.")

(defun avy-mouse-event-window (char)
  "Return the window of mouse event CHAR if any or the selected window.
Return nil if CHAR is not a mouse event."
  (when (mouse-event-p char)
    (cond ((windowp (posn-window (event-start char)))
           (posn-window (event-start char)))
          ((framep (posn-window (event-start char)))
           (frame-selected-window (posn-window (event-start char))))
          (t (selected-window)))))

(defun avy-read (tree display-fn cleanup-fn)
  "Select a leaf from TREE using consecutive `read-key'.

DISPLAY-FN should take CHAR and LEAF and signify that LEAFs
associated with CHAR will be selected if CHAR is pressed.  This is
commonly done by adding a CHAR overlay at LEAF position.

CLEANUP-FN should take no arguments and remove the effects of
multiple DISPLAY-FN invocations."
  (catch 'done
    (setq avy-current-path "")
    (while tree
      (let ((avy--leafs nil))
        (avy-traverse tree
                      (lambda (path leaf)
                        (push (cons path leaf) avy--leafs)))
        (dolist (x avy--leafs)
          (funcall display-fn (car x) (cdr x))))
      (let ((char (funcall avy-translate-char-function (read-key)))
            window
            branch)
        (funcall cleanup-fn)
        (if (setq window (avy-mouse-event-window char))
            (throw 'done (cons char window))
          (if (setq branch (assoc char tree))
              (progn
                ;; Ensure avy-current-path stores the full path prior to
                ;; exit so other packages can utilize its value.
                (setq avy-current-path
                      (concat avy-current-path (string (avy--key-to-char char))))
                (if (eq (car (setq tree (cdr branch))) 'leaf)
                    (throw 'done (cdr tree))))
            (funcall avy-handler-function char)))))))

(defun avy-read-de-bruijn (lst keys)
  "Select from LST dispatching on KEYS."
  ;; In theory, the De Bruijn sequence B(k,n) has k^n subsequences of length n
  ;; (the path length) usable as paths, thus that's the lower bound.  Due to
  ;; partially overlapping matches, not all subsequences may be usable, so it's
  ;; possible that the path-len must be incremented, e.g., if we're matching
  ;; for x and a buffer contains xaxbxcx only every second subsequence is
  ;; usable for the four matches.
  (catch 'done
    (let* ((path-len (ceiling (log (length lst) (length keys))))
           (alist (avy--path-alist-1 lst path-len keys)))
      (while (not alist)
        (cl-incf path-len)
        (setq alist (avy--path-alist-1 lst path-len keys)))
      (let* ((len (length (caar alist)))
             (i 0))
        (setq avy-current-path "")
        (while (< i len)
          (dolist (x (reverse alist))
            (avy--overlay-at-full (reverse (car x)) (cdr x)))
          (let ((char (funcall avy-translate-char-function (read-key))))
            (avy--remove-leading-chars)
            (setq alist
                  (delq nil
                        (mapcar (lambda (x)
                                  (when (eq (caar x) char)
                                    (cons (cdr (car x)) (cdr x))))
                                alist)))
            (setq avy-current-path
                  (concat avy-current-path (string (avy--key-to-char char))))
            (cl-incf i)
            (unless alist
              (funcall avy-handler-function char))))
        (cdar alist)))))

(defun avy-read-words (lst words)
  "Select from LST using WORDS."
  (catch 'done
    (let ((num-words (length words))
          (num-entries (length lst))
          alist)
      ;; If there are not enough words to cover all the candidates,
      ;; we use a De Bruijn sequence to generate the remaining ones.
      (when (< num-words num-entries)
        (let ((keys avy-keys)
              (bad-keys '(?a ?e ?i ?o ?u ?y))
              (path-len 1)
              (num-remaining (- num-entries num-words))
              tmp-alist)
          ;; Delete all keys which could lead to duplicates.
          ;; We want at least three keys left to work with.
          (dolist (x bad-keys)
            (when (memq x keys)
              (setq keys (delq ?a keys))))
          (when (< (length keys) 3)
            (signal 'user-error
                    '("Please add more keys to the variable `avy-keys'.")))
          ;; Generate the sequence and add the keys to the existing words.
          (while (not tmp-alist)
            (cl-incf path-len)
            (setq tmp-alist (avy--path-alist-1 lst path-len keys)))
          (while (>= (cl-decf num-remaining) 0)
            (push (mapconcat 'string (caar tmp-alist) nil) (cdr (last words)))
            (setq tmp-alist (cdr tmp-alist)))))
      (dolist (x lst)
        (push (cons (string-to-list (pop words)) x) alist))
      (setq avy-current-path "")
      (while (or (> (length alist) 1)
                 (caar alist))
        (dolist (x (reverse alist))
          (avy--overlay-at-full (reverse (car x)) (cdr x)))
        (let ((char (funcall avy-translate-char-function (read-key))))
          (avy--remove-leading-chars)
          (setq alist
                (delq nil
                      (mapcar (lambda (x)
                                (when (eq (caar x) char)
                                  (cons (cdr (car x)) (cdr x))))
                              alist)))
          (setq avy-current-path
                (concat avy-current-path (string (avy--key-to-char char))))
          (unless alist
            (funcall avy-handler-function char))))
      (cdar alist))))

;;** Rest
(defun avy-window-list ()
  "Return a list of windows depending on `avy-all-windows'."
  (cond ((eq avy-all-windows 'all-frames)
         (cl-mapcan #'window-list (frame-list)))

        ((eq avy-all-windows t)
         (window-list))

        ((null avy-all-windows)
         (list (selected-window)))

        (t
         (error "Unrecognized option: %S" avy-all-windows))))

(defcustom avy-all-windows-alt nil
  "The alternative `avy-all-windows' for use with \\[universal-argument]."
  :type '(choice
          (const :tag "Current window" nil)
          (const :tag "All windows on the current frame" t)
          (const :tag "All windows on all frames" all-frames)))

(defmacro avy-dowindows (flip &rest body)
  "Depending on FLIP and `avy-all-windows' run BODY in each or selected window."
  (declare (indent 1)
           (debug (form body)))
  `(let ((avy-all-windows (if ,flip
                              avy-all-windows-alt
                            avy-all-windows)))
     (dolist (wnd (avy-window-list))
       (with-selected-window wnd
         (unless (memq major-mode avy-ignored-modes)
           ,@body)))))

(defun avy-resume ()
  "Stub to hold last avy command.
Commands using `avy-with' macro can be resumed."
  (interactive))

(defmacro avy-with (command &rest body)
  "Set `avy-keys' according to COMMAND and execute BODY.
Set `avy-style' according to COMMAND as well."
  (declare (indent 1)
           (debug (form body)))
  `(let ((avy-keys (or (cdr (assq ',command avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq ',command avy-styles-alist))
                        avy-style))
         (avy-command ',command))
     (setq avy-action nil)
     (setf (symbol-function 'avy-resume)
           (lambda ()
             (interactive)
             ,@(if (eq command 'avy-goto-char-timer)
                   (cdr body)
                 body)))
     ,@body))

(defun avy-action-goto (pt)
  "Goto PT."
  (let ((frame (window-frame (selected-window))))
    (unless (equal frame (selected-frame))
      (select-frame-set-input-focus frame)
      (raise-frame frame))
    (goto-char pt)))

(defun avy-forward-item ()
  (if (eq avy-command 'avy-goto-line)
      (end-of-line)
    (forward-sexp))
  (point))

(defun avy-action-mark (pt)
  "Mark sexp at PT."
  (goto-char pt)
  (set-mark (point))
  (avy-forward-item))

(defun avy-action-copy (pt)
  "Copy sexp starting on PT."
  (save-excursion
    (let (str)
      (goto-char pt)
      (avy-forward-item)
      (setq str (buffer-substring pt (point)))
      (kill-new str)
      (message "Copied: %s" str)))
  (let ((dat (ring-ref avy-ring 0)))
    (select-frame-set-input-focus
     (window-frame (cdr dat)))
    (select-window (cdr dat))
    (goto-char (car dat))))

(defun avy-action-yank (pt)
  "Yank sexp starting at PT at the current point."
  (avy-action-copy pt)
  (yank)
  t)

(defun avy-action-yank-line (pt)
  "Yank sexp starting at PT at the current point."
  (let ((avy-command 'avy-goto-line))
    (avy-action-yank pt)))

(defun avy-action-kill-move (pt)
  "Kill sexp at PT and move there."
  (goto-char pt)
  (avy-forward-item)
  (kill-region pt (point))
  (message "Killed: %s" (current-kill 0))
  (point))

(defun avy-action-kill-stay (pt)
  "Kill sexp at PT."
  (save-excursion
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (just-one-space))
  (message "Killed: %s" (current-kill 0))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-zap-to-char (pt)
  "Kill from point up to PT."
  (if (> pt (point))
      (kill-region (point) pt)
    (kill-region pt (point))))

(defun avy-action-teleport (pt)
  "Kill sexp starting on PT and yank into the current location."
  (avy-action-kill-stay pt)
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  (save-excursion
    (yank))
  t)

(declare-function flyspell-correct-word-before-point "flyspell")

(defcustom avy-flyspell-correct-function #'flyspell-correct-word-before-point
  "Function called to correct word by `avy-action-ispell' when
`flyspell-mode' is enabled."
  :type 'function)

(defun avy-action-ispell (pt)
  "Auto correct word at PT."
  (save-excursion
    (goto-char pt)
    (cond
      ((eq avy-command 'avy-goto-line)
       (ispell-region
        (line-beginning-position)
        (line-end-position)))
      ((bound-and-true-p flyspell-mode)
       (funcall avy-flyspell-correct-function))
      ((looking-at-p "\\b")
       (ispell-word))
      (t
       (progn
         (backward-word)
         (when (looking-at-p "\\b")
           (ispell-word)))))))

(defvar avy-pre-action #'avy-pre-action-default
  "Function to call before `avy-action' is called.")

(defun avy-pre-action-default (res)
  (avy-push-mark)
  (when (and (consp res)
             (windowp (cdr res)))
    (let* ((window (cdr res))
           (frame (window-frame window)))
      (unless (equal frame (selected-frame))
        (select-frame-set-input-focus frame))
      (select-window window))))

(defun avy--process-1 (candidates overlay-fn &optional cleanup-fn)
  (let ((len (length candidates)))
    (cond ((= len 0)
           nil)
          ((and (= len 1) avy-single-candidate-jump)
           (car candidates))
          (t
           (unwind-protect
                (progn
                  (avy--make-backgrounds
                   (avy-window-list))
                  (cond ((eq avy-style 'de-bruijn)
                         (avy-read-de-bruijn
                          candidates avy-keys))
                        ((eq avy-style 'words)
                         (avy-read-words
                          candidates avy-words))
                        (t
                         (avy-read (avy-tree candidates avy-keys)
                                   overlay-fn
                                   (or cleanup-fn #'avy--remove-leading-chars)))))
             (avy--done))))))

(defvar avy-last-candidates nil
  "Store the last candidate list.")

(defun avy--last-candidates-cycle (advancer)
  (let* ((avy-last-candidates
          (cl-remove-if-not
           (lambda (x) (equal (cdr x) (selected-window)))
           avy-last-candidates))
         (min-dist
          (apply #'min
                 (mapcar (lambda (x) (abs (- (if (listp (car x)) (caar x) (car x)) (point)))) avy-last-candidates)))
         (pos
          (cl-position-if
           (lambda (x)
             (= (- (if (listp (car x)) (caar x) (car x)) (point)) min-dist))
           avy-last-candidates)))
    (funcall advancer pos avy-last-candidates)))

(defun avy-prev ()
  "Go to the previous candidate of the last `avy-read'."
  (interactive)
  (avy--last-candidates-cycle
   (lambda (pos lst)
     (when (> pos 0)
       (let ((candidate (nth (1- pos) lst)))
         (goto-char (if (listp (car candidate)) (caar candidate) (car candidate))))))))

(defun avy-next ()
  "Go to the next candidate of the last `avy-read'."
  (interactive)
  (avy--last-candidates-cycle
   (lambda (pos lst)
     (when (< pos (1- (length lst)))
       (let ((candidate (nth (1+ pos) lst)))
         (goto-char (if (listp (car candidate)) (caar candidate) (car candidate))))))))

;;;###autoload
(defun avy-process (candidates &optional overlay-fn cleanup-fn)
  "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations."
  (setq overlay-fn (or overlay-fn (avy--style-fn avy-style)))
  (setq cleanup-fn (or cleanup-fn #'avy--remove-leading-chars))
  (unless (and (consp (car candidates))
               (windowp (cdar candidates)))
    (setq candidates
          (mapcar (lambda (x) (cons x (selected-window)))
                  candidates)))
  (setq avy-last-candidates (copy-sequence candidates))
  (let ((original-cands (copy-sequence candidates))
        (res (avy--process-1 candidates overlay-fn cleanup-fn)))
    (cond
      ((null res)
       (if (and (eq avy-style 'words) candidates)
           (avy-process original-cands overlay-fn cleanup-fn)
         (message "zero candidates")
         t))
      ((eq res 'restart)
       (avy-process original-cands overlay-fn cleanup-fn))
      ;; ignore exit from `avy-handler-function'
      ((eq res 'exit))
      ((eq res 'abort)
       nil)
      (t
       (funcall avy-pre-action res)
       (setq res (car res))
       (let ((action (or avy-action avy-action-oneshot 'avy-action-goto)))
         (funcall action
                  (if (consp res)
                      (car res)
                    res)))
       res))))

(define-obsolete-function-alias 'avy--process 'avy-process
  "0.4.0")

(defvar avy--overlays-back nil
  "Hold overlays for when `avy-background' is t.")

(defun avy--make-backgrounds (wnd-list)
  "Create a dim background overlay for each window on WND-LIST."
  (when avy-background
    (setq avy--overlays-back
          (mapcar (lambda (w)
                    (let ((ol (make-overlay
                               (window-start w)
                               (window-end w)
                               (window-buffer w))))
                      (overlay-put ol 'face 'avy-background-face)
                      (overlay-put ol 'window w)
                      ol))
                  wnd-list))))

(defun avy--done ()
  "Clean up overlays."
  (mapc #'delete-overlay avy--overlays-back)
  (setq avy--overlays-back nil)
  (avy--remove-leading-chars))

(defun avy--visible-p (s)
  (let ((invisible (get-char-property s 'invisible)))
    (or (null invisible)
        (eq t buffer-invisibility-spec)
        (null (assoc invisible buffer-invisibility-spec)))))

(defun avy--next-visible-point ()
  "Return the next closest point without `invisible' property."
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-char-property-change s))))
                (not (avy--visible-p s))))
    s))

(defun avy--next-invisible-point ()
  "Return the next closest point with `invisible' property."
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-char-property-change s))))
                (avy--visible-p s)))
    s))

(defun avy--find-visible-regions (rbeg rend)
  "Return a list of all visible regions between RBEG and REND."
  (setq rbeg (max rbeg (point-min)))
  (setq rend (min rend (point-max)))
  (when (< rbeg rend)
    (let (visibles beg)
      (save-excursion
        (save-restriction
          (narrow-to-region rbeg rend)
          (setq beg (goto-char (point-min)))
          (while (not (= (point) (point-max)))
            (goto-char (avy--next-invisible-point))
            (push (cons beg (point)) visibles)
            (setq beg (goto-char (avy--next-visible-point))))
          (nreverse visibles))))))

(defun avy--regex-candidates (regex &optional beg end pred group)
  "Return all elements that match REGEX.
Each element of the list is ((BEG . END) . WND)
When PRED is non-nil, it's a filter for matching point positions.
When GROUP is non-nil, (BEG . END) should delimit that regex group."
  (setq group (or group 0))
  (let ((case-fold-search (or avy-case-fold-search
                              (string= regex (downcase regex))))
        candidates)
    (avy-dowindows current-prefix-arg
      (dolist (pair (avy--find-visible-regions
                     (or beg (window-start))
                     (or end (window-end (selected-window) t))))
        (save-excursion
          (goto-char (car pair))
          (while (re-search-forward regex (cdr pair) t)
            (when (avy--visible-p (1- (point)))
              (when (or (null pred)
                        (funcall pred))
                (push (cons
                       (if (numberp group)
                           (cons (match-beginning group)
                                 (match-end group))
                         (funcall group))
                       wnd) candidates)))))))
    (nreverse candidates)))

(defvar avy--overlay-offset 0
  "The offset to apply in `avy--overlay'.")

(defvar avy--overlays-lead nil
  "Hold overlays for leading chars.")

(defun avy--remove-leading-chars ()
  "Remove leading char overlays."
  (mapc #'delete-overlay avy--overlays-lead)
  (setq avy--overlays-lead nil))

(defun avy--old-str (pt wnd)
  "Return a one-char string at PT in WND."
  (let ((old-str (with-selected-window wnd
                   (buffer-substring pt (1+ pt)))))
    (if avy-background
        (propertize old-str 'face 'avy-background-face)
      old-str)))

(defun avy--overlay (str beg end wnd &optional compose-fn)
  "Create an overlay with STR from BEG to END in WND.
COMPOSE-FN is a lambda that concatenates the old string at BEG with STR."
  (let ((eob (with-selected-window wnd (point-max))))
    (when (<= beg eob)
      (let* ((beg (+ beg avy--overlay-offset))
             (ol (make-overlay beg (or end (1+ beg)) (window-buffer wnd)))
             (old-str (if (eq beg eob) "" (avy--old-str beg wnd)))
             (os-line-prefix (get-text-property 0 'line-prefix old-str))
             (os-wrap-prefix (get-text-property 0 'wrap-prefix old-str))
             other-ol)
        (unless (= (length str) 0)
          (when os-line-prefix
            (add-text-properties 0 1 `(line-prefix ,os-line-prefix) str))
          (when os-wrap-prefix
            (add-text-properties 0 1 `(wrap-prefix ,os-wrap-prefix) str)))
        (when (setq other-ol (cl-find-if
                              (lambda (o) (overlay-get o 'goto-address))
                              (overlays-at beg)))
          (add-text-properties
           0 (length old-str)
           `(face ,(overlay-get other-ol 'face)) old-str))
        (overlay-put ol 'window wnd)
        (overlay-put ol 'category 'avy)
        (overlay-put ol (if (eq beg eob)
                            'after-string
                          'display)
                     (funcall
                      (or compose-fn #'concat)
                      str old-str))
        (push ol avy--overlays-lead)))))

(defcustom avy-highlight-first nil
  "When non-nil highlight the first decision char with `avy-lead-face-0'.
Do this even when the char is terminating."
  :type 'boolean)

(defun avy--key-to-char (c)
  "If C is no character, translate it using `avy-key-to-char-alist'."
  (cond ((characterp c) c)
        ((cdr (assoc c avy-key-to-char-alist)))
        ((mouse-event-p c) c)
        (t
         (error "Unknown key %s" c))))

(defun avy-candidate-beg (leaf)
  "Return the start position for LEAF."
  (cond ((numberp leaf)
         leaf)
        ((consp (car leaf))
         (caar leaf))
        (t
         (car leaf))))

(defun avy-candidate-end (leaf)
  "Return the end position for LEAF."
  (cond ((numberp leaf)
         leaf)
        ((consp (car leaf))
         (cdar leaf))
        (t
         (car leaf))))

(defun avy-candidate-wnd (leaf)
  "Return the window for LEAF."
  (if (consp leaf)
      (cdr leaf)
    (selected-window)))

(defun avy--overlay-pre (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (if (with-selected-window (cdr leaf)
        (bound-and-true-p visual-line-mode))
      (avy--overlay-at-full path leaf)
    (let* ((path (mapcar #'avy--key-to-char path))
           (str (propertize (apply #'string (reverse path))
                            'face 'avy-lead-face)))
      (when (or avy-highlight-first (> (length str) 1))
        (set-text-properties 0 1 '(face avy-lead-face-0) str))
      (setq str (concat
                 (propertize avy-current-path
                             'face 'avy-lead-face-1)
                 str))
      (avy--overlay
       str
       (avy-candidate-beg leaf) nil
       (avy-candidate-wnd leaf)))))

(defun avy--overlay-at (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (string (car (last path)))
               'face 'avy-lead-face)))
    (avy--overlay
     str
     (avy-candidate-beg leaf) nil
     (avy-candidate-wnd leaf)
     (lambda (str old-str)
       (cond ((string= old-str "\n")
              (concat str "\n"))
             ;; add padding for wide-width character
             ((eq (string-width old-str) 2)
              (concat str " "))
             (t
              str))))))

(defun avy--overlay-at-full (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize
               (apply #'string (reverse path))
               'face 'avy-lead-face))
         (len (length path))
         (beg (avy-candidate-beg leaf))
         (wnd (cdr leaf))
         end)
    (dotimes (i len)
      (set-text-properties i (1+ i)
                           `(face ,(nth i avy-lead-faces))
                           str))
    (when (eq avy-style 'de-bruijn)
      (setq str (concat
                 (propertize avy-current-path
                             'face 'avy-lead-face-1)
                 str))
      (setq len (length str)))
    (with-selected-window wnd
      (save-excursion
        (goto-char beg)
        (let* ((lep (if (bound-and-true-p visual-line-mode)
                        (save-excursion
                          (end-of-visual-line)
                          (point))
                      (line-end-position)))
               ;; `end-of-visual-line' is bugged sometimes
               (lep (if (< lep beg)
                        (line-end-position)
                      lep))
               (len-and-str (avy--update-offset-and-str len str lep)))
          (setq len (car len-and-str))
          (setq str (cdr len-and-str))
          (setq end (if (= beg lep)
                        (1+ beg)
                      (min (+ beg
                              (if (eq (char-after) ?\t)
                                  1
                                len))
                           lep)))
          (when (and (bound-and-true-p visual-line-mode)
                     (> len (- end beg))
                     (not (eq lep beg)))
            (setq len (- end beg))
            (let ((old-str (apply #'string (reverse path))))
              (setq str
                    (substring
                     (propertize
                      old-str
                      'face
                      (if (= (length old-str) 1)
                          'avy-lead-face
                        'avy-lead-face-0))
                     0 len)))))))
    (avy--overlay
     str beg end wnd
     (lambda (str old-str)
       (cond ((string= old-str "\n")
              (concat str "\n"))
             ((string= old-str "\t")
              (concat str (make-string (max (- tab-width len) 0) ?\ )))
             (t
              ;; add padding for wide-width character
              (if (eq (string-width old-str) 2)
                  (concat str " ")
                str)))))))

(defun avy--overlay-post (path leaf)
  "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (propertize (apply #'string (reverse path))
                          'face 'avy-lead-face)))
    (when (or avy-highlight-first (> (length str) 1))
      (set-text-properties 0 1 '(face avy-lead-face-0) str))
    (setq str (concat
               (propertize avy-current-path
                           'face 'avy-lead-face-1)
               str))
    (avy--overlay
     str
     (avy-candidate-end leaf) nil
     (avy-candidate-wnd leaf))))

(defun avy--update-offset-and-str (offset str lep)
  "Recalculate the length of the new overlay at point.

OFFSET is the previous overlay length.
STR is the overlay string that we wish to add.
LEP is the line end position.

We want to add an overlay between point and END=point+OFFSET.
When other overlays already exist between point and END, set
OFFSET to be the difference between the start of the first
overlay and point.  This is equivalent to truncating our new
overlay, so that it doesn't intersect with overlays that already
exist."
  (let* ((wnd (selected-window))
         (beg (point))
         (oov (delq nil
                    (mapcar
                     (lambda (o)
                       (and (eq (overlay-get o 'category) 'avy)
                            (eq (overlay-get o 'window) wnd)
                            (overlay-start o)))
                     (overlays-in beg (min (+ beg offset) lep))))))
    (when oov
      (setq offset (- (apply #'min oov) beg))
      (setq str (substring str 0 offset)))
    (let ((other-ov (cl-find-if
                     (lambda (o)
                       (and (eq (overlay-get o 'category) 'avy)
                            (eq (overlay-start o) beg)
                            (not (eq (overlay-get o 'window) wnd))))
                     (overlays-in (point) (min (+ (point) offset) lep)))))
      (when (and other-ov
                 (> (overlay-end other-ov)
                    (+ beg offset)))
        (setq str (concat str (buffer-substring
                               (+ beg offset)
                               (overlay-end other-ov))))
        (setq offset (- (overlay-end other-ov)
                        beg))))
    (cons offset str)))

(defun avy--style-fn (style)
  "Transform STYLE symbol to a style function."
  (cl-case style
    (pre #'avy--overlay-pre)
    (at #'avy--overlay-at)
    (at-full 'avy--overlay-at-full)
    (post #'avy--overlay-post)
    (de-bruijn #'avy--overlay-at-full)
    (words #'avy--overlay-at-full)
    (ignore #'ignore)
    (t (error "Unexpected style %S" style))))

(cl-defun avy-jump (regex &key window-flip beg end action pred group)
  "Jump to REGEX.
The window scope is determined by `avy-all-windows'.
When WINDOW-FLIP is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
ACTION is a function that takes point position as an argument.
When PRED is non-nil, it's a filter for matching point positions.
When GROUP is non-nil, it's either a match group in REGEX, or a function
that returns a cons of match beginning and end."
  (setq avy-action (or action avy-action))
  (let ((avy-all-windows
         (if window-flip
             (not avy-all-windows)
           avy-all-windows)))
    (avy-process
     (avy--regex-candidates regex beg end pred group))))

(defun avy--generic-jump (regex window-flip &optional beg end)
  "Jump to REGEX.
The window scope is determined by `avy-all-windows'.
When WINDOW-FLIP is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (declare (obsolete avy-jump "0.4.0"))
  (let ((avy-all-windows
         (if window-flip
             (not avy-all-windows)
           avy-all-windows)))
    (avy-process
     (avy--regex-candidates regex beg end))))

;;* Commands
;;;###autoload
(defun avy-goto-char (char &optional arg)
  "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char
    (avy-jump
     (if (= 13 char)
         "\n"
       (regexp-quote (string char)))
     :window-flip arg)))

;;;###autoload
(defun avy-goto-char-in-line (char)
  "Jump to the currently visible CHAR in the current line."
  (interactive (list (read-char "char: " t)))
  (avy-with avy-goto-char
    (avy-jump
     (regexp-quote (string char))
     :beg (line-beginning-position)
     :end (line-end-position))))

;;;###autoload
(defun avy-goto-char-2 (char1 char2 &optional arg beg end)
  "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive (list (let ((c1 (read-char "char 1: " t)))
                       (if (memq c1 '(? ?\b))
                           (keyboard-quit)
                         c1))
                     (let ((c2 (read-char "char 2: " t)))
                       (cond ((eq c2 ?)
                              (keyboard-quit))
                             ((memq c2 avy-del-last-char-by)
                              (keyboard-escape-quit)
                              (call-interactively 'avy-goto-char-2))
                             (t
                              c2)))
                     current-prefix-arg
                     nil nil))
  (when (eq char1 ?)
    (setq char1 ?\n))
  (when (eq char2 ?)
    (setq char2 ?\n))
  (avy-with avy-goto-char-2
    (avy-jump
     (regexp-quote (string char1 char2))
     :window-flip arg
     :beg beg
     :end end)))

;;;###autoload
(defun avy-goto-char-2-above (char1 char2 &optional arg)
  "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char-2-above
    (avy-goto-char-2
     char1 char2 arg
     (window-start) (point))))

;;;###autoload
(defun avy-goto-char-2-below (char1 char2 &optional arg)
  "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (avy-with avy-goto-char-2-below
    (avy-goto-char-2
     char1 char2 arg
     (point) (window-end (selected-window) t))))

;;;###autoload
(defun avy-isearch ()
  "Jump to one of the current isearch candidates."
  (interactive)
  (avy-with avy-isearch
    (let ((avy-background nil)
          (avy-case-fold-search case-fold-search))
      (prog1
          (avy-process
           (avy--regex-candidates (if isearch-regexp
                                      isearch-string
                                    (regexp-quote isearch-string))))
        (isearch-done)))))

;;;###autoload
(defun avy-goto-word-0 (arg &optional beg end)
  "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy-jump avy-goto-word-0-regexp
              :window-flip arg
              :beg beg
              :end end)))

;;;###autoload
(defun avy-goto-whitespace-end (arg &optional beg end)
  "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (avy-with avy-goto-whitespace-end
    (avy-jump "[ \t]+\\|\n[ \t]*"
              :window-flip arg
              :beg beg
              :end end
              :group (lambda () (cons (point) (1+ (point)))))))

(defun avy-goto-word-0-above (arg)
  "Jump to a word start between window start and point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy-goto-word-0 arg (window-start) (point))))

(defun avy-goto-word-0-below (arg)
  "Jump to a word start between point and window end.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive "P")
  (avy-with avy-goto-word-0
    (avy-goto-word-0 arg (point) (window-end (selected-window) t))))

(defun avy-goto-whitespace-end-above (arg)
  "Jump to the end of a whitespace sequence between point and window end.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive "P")
  (avy-with avy-goto-whitespace-end
    (avy-goto-whitespace-end arg (window-start) (point))))

(defun avy-goto-whitespace-end-below (arg)
  "Jump to the end of a whitespace sequence between window start and point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive "P")
  (avy-with avy-goto-whitespace-end
    (avy-goto-whitespace-end arg (point) (window-end (selected-window) t))))

;;;###autoload
(defun avy-goto-word-1 (char &optional arg beg end symbol)
  "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (let* ((str (string char))
           (regex (cond ((string= str ".")
                         "\\.")
                        ((and avy-word-punc-regexp
                              (string-match avy-word-punc-regexp str))
                         (regexp-quote str))
                        ((<= char 26)
                         str)
                        (t
                         (concat
                          (if symbol "\\_<" "\\b")
                          str)))))
      (avy-jump regex
                :window-flip arg
                :beg beg
                :end end))))

;;;###autoload
(defun avy-goto-word-1-above (char &optional arg)
  "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (avy-goto-word-1 char arg (window-start) (point))))

;;;###autoload
(defun avy-goto-word-1-below (char &optional arg)
  "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-word-1
    (avy-goto-word-1 char arg (point) (window-end (selected-window) t))))

;;;###autoload
(defun avy-goto-symbol-1 (char &optional arg)
  "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-symbol-1
    (avy-goto-word-1 char arg nil nil t)))

;;;###autoload
(defun avy-goto-symbol-1-above (char &optional arg)
  "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-symbol-1-above
    (avy-goto-word-1 char arg (window-start) (point) t)))

;;;###autoload
(defun avy-goto-symbol-1-below (char &optional arg)
  "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-symbol-1-below
    (avy-goto-word-1 char arg (point) (window-end (selected-window) t) t)))

(declare-function subword-backward "subword")
(defvar subword-backward-regexp)

(defcustom avy-subword-extra-word-chars '(?{ ?= ?} ?* ?: ?> ?<)
  "A list of characters that should temporarily match \"\\w\".
This variable is used by `avy-goto-subword-0' and `avy-goto-subword-1'."
  :type '(repeat character))

;;;###autoload
(defun avy-goto-subword-0 (&optional arg predicate beg end)
  "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched."
  (interactive "P")
  (require 'subword)
  (avy-with avy-goto-subword-0
    (let ((case-fold-search nil)
          (subword-backward-regexp
           "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([!-/:@`~[:upper:]]+\\W*\\)\\|\\W\\w+\\)")
          candidates)
      (avy-dowindows arg
        (let ((syn-tbl (copy-syntax-table)))
          (dolist (char avy-subword-extra-word-chars)
            (modify-syntax-entry char "w" syn-tbl))
          (with-syntax-table syn-tbl
            (let ((ws (or beg (window-start)))
                  window-cands)
              (save-excursion
                (goto-char (or end (window-end (selected-window) t)))
                (subword-backward)
                (while (> (point) ws)
                  (when (or (null predicate)
                            (and predicate (funcall predicate)))
                    (unless (not (avy--visible-p (point)))
                      (push (cons (cons (point) (1+ (point)))
                                  (selected-window)) window-cands)))
                  (subword-backward))
                (and (= (point) ws)
                     (or (null predicate)
                         (and predicate (funcall predicate)))
                     (not (get-char-property (point) 'invisible))
                     (push (cons (cons (point) (1+ (point)))
                                 (selected-window)) window-cands)))
              (setq candidates (nconc candidates window-cands))))))
      (avy-process candidates))))

;;;###autoload
(defun avy-goto-subword-1 (char &optional arg)
  "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-with avy-goto-subword-1
    (let ((char (downcase char)))
      (avy-goto-subword-0
       arg (lambda ()
             (and (char-after)
                  (eq (downcase (char-after)) char)))))))

;;;###autoload
(defun avy-goto-word-or-subword-1 ()
  "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (call-interactively #'avy-goto-subword-1)
    (call-interactively #'avy-goto-word-1)))

(defvar visual-line-mode)

(defcustom avy-indent-line-overlay nil
  "When non-nil, display line overlay next to the first non-whitespace character.
This affects `avy-goto-line'."
  :type 'boolean)

(defun avy--line-cands (&optional arg beg end bottom-up)
  "Get candidates for selecting a line.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When BOTTOM-UP is non-nil, display avy candidates from top to bottom"
  (let (candidates)
    (avy-dowindows arg
      (let ((ws (or beg (window-start))))
        (save-excursion
          (save-restriction
            (narrow-to-region ws (or end (window-end (selected-window) t)))
            (goto-char (point-min))
            (while (< (point) (point-max))
              (when (member (get-char-property
                             (max (1- (point)) ws) 'invisible) '(nil org-link))
                (push (cons
                       (if (eq avy-style 'post)
                           (line-end-position)
                         (save-excursion
                           (when avy-indent-line-overlay
                             (skip-chars-forward " \t"))
                           (point)))
                       (selected-window)) candidates))
              (if visual-line-mode
                  (line-move-visual 1 t)
                (forward-line 1)))))))
    (if bottom-up
        candidates
      (nreverse candidates))))

(defun avy--linum-strings ()
  "Get strings for `avy-linum-mode'."
  (let* ((lines (mapcar #'car (avy--line-cands)))
         (line-tree (avy-tree lines avy-keys))
         (line-list nil))
    (avy-traverse
     line-tree
     (lambda (path _leaf)
       (let ((str (propertize (apply #'string (reverse path))
                              'face 'avy-lead-face)))
         (when (> (length str) 1)
           (set-text-properties 0 1 '(face avy-lead-face-0) str))
         (push str line-list))))
    (nreverse line-list)))

(defvar linum-available)
(defvar linum-overlays)
(defvar linum-format)
(declare-function linum--face-width "linum")
(declare-function linum-mode "linum")

(define-minor-mode avy-linum-mode
  "Minor mode that uses avy hints for `linum-mode'."
  :group 'avy
  (if avy-linum-mode
      (progn
        (require 'linum)
        (advice-add 'linum-update-window :around 'avy--linum-update-window)
        (linum-mode 1))
    (advice-remove 'linum-update-window 'avy--linum-update-window)
    (linum-mode -1)))

(defun avy--linum-update-window (_ win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let ((line (line-number-at-pos))
        (limit (window-end win t))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat "%" (number-to-string w) "d")))))
        (width 0)
        (avy-strs (when avy-linum-mode
                    (avy--linum-strings))))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (< (point) limit))
      (let* ((str
              (cond (avy-linum-mode
                     (pop avy-strs))
                    (fmt
                     (propertize (format fmt line) 'face 'linum))
                    (t
                     (funcall linum-format line))))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
                                 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (when (display-graphic-p)
      (setq width (ceiling
                   (/ (* width 1.0 (linum--face-width 'linum))
                      (frame-char-width)))))
    (set-window-margins win width (cdr (window-margins win)))))

(defun avy--line (&optional arg beg end bottom-up)
  "Select a line.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When BOTTOM-UP is non-nil, display avy candidates from top to bottom"
  (setq avy-action (or avy-action #'identity))
  (let ((avy-style (if avy-linum-mode
                       (progn
                         (message "Goto line:")
                         'ignore)
                     avy-style)))
    (avy-process
     (avy--line-cands arg beg end bottom-up))))

;;;###autoload
(defun avy-goto-line (&optional arg)
  "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG."
  (interactive "p")
  (setq arg (or arg 1))
  (if (not (memq arg '(1 4)))
      (progn
        (goto-char (point-min))
        (forward-line (1- arg)))
    (avy-with avy-goto-line
      (let* ((avy-handler-old avy-handler-function)
             (avy-handler-function
              (lambda (char)
                (if (or (< char ?0)
                        (> char ?9))
                    (funcall avy-handler-old char)
                  (let ((line (read-from-minibuffer
                               "Goto line: " (string char))))
                    (when line
                      (avy-push-mark)
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (forward-line (1- (string-to-number line))))
                      (throw 'done 'exit))))))
             (r (avy--line (eq arg 4))))
        (when (and (not (memq r '(t nil))) (eq avy-action #'identity))
          (avy-action-goto r))))))

;;;###autoload
(defun avy-goto-line-above (&optional offset bottom-up)
  "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom"
  (interactive)
  (if offset
    (setq offset (+ 2 (- offset))))
  (let* ((avy-all-windows nil)
         (r (avy--line nil (window-start)
                       (line-beginning-position (or offset 1))
		       bottom-up)))
    (unless (eq r t)
      (avy-action-goto r))))

;;;###autoload
(defun avy-goto-line-below (&optional offset bottom-up)
  "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom"
  (interactive)
  (if offset
    (setq offset (+ offset 1)))
  (let* ((avy-all-windows nil)
         (r (avy--line
             nil (line-beginning-position (or offset 2))
             (window-end (selected-window) t)
	     bottom-up)))
    (unless (eq r t)
      (avy-action-goto r))))

(defcustom avy-line-insert-style 'above
  "How to insert the newly copied/cut line."
  :type '(choice
          (const :tag "Above" above)
          (const :tag "Below" below)))

;;;###autoload
(defun avy-goto-end-of-line (&optional arg)
  "Call `avy-goto-line' and move to the end of the line."
  (interactive "p")
  (avy-goto-line arg)
  (end-of-line))

;;;###autoload
(defun avy-copy-line (arg)
  "Copy a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (let ((initial-window (selected-window)))
    (avy-with avy-copy-line
      (let* ((start (avy--line))
             (str (buffer-substring-no-properties
                   start
                   (save-excursion
                     (goto-char start)
                     (move-end-of-line arg)
                     (point)))))
        (select-window initial-window)
        (cond ((eq avy-line-insert-style 'above)
               (beginning-of-line)
               (save-excursion
                 (insert str "\n")))
              ((eq avy-line-insert-style 'below)
               (end-of-line)
               (insert "\n" str)
               (beginning-of-line))
              (t
               (user-error "Unexpected `avy-line-insert-style'")))))))

;;;###autoload
(defun avy-move-line (arg)
  "Move a selected line above the current line.
ARG lines can be used."
  (interactive "p")
  (let ((initial-window (selected-window)))
    (avy-with avy-move-line
      (let ((start (avy--line)))
        (save-excursion
          (goto-char start)
          (kill-whole-line arg))
        (select-window initial-window)
        (cond ((eq avy-line-insert-style 'above)
               (beginning-of-line)
               (save-excursion
                 (insert
                  (current-kill 0))))
              ((eq avy-line-insert-style 'below)
               (end-of-line)
               (newline)
               (save-excursion
                 (insert (substring (current-kill 0) 0 -1))))
              (t
               (user-error "Unexpected `avy-line-insert-style'")))))))

;;;###autoload
(defun avy-copy-region (arg)
  "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-copy-region
      (let* ((beg (save-selected-window
                    (avy--line arg)))
             (end (avy--line arg))
             (str (buffer-substring-no-properties
                   beg
                   (save-excursion
                     (goto-char end)
                     (line-end-position)))))
        (select-window initial-window)
        (cond ((eq avy-line-insert-style 'above)
               (beginning-of-line)
               (save-excursion
                 (insert str "\n")))
              ((eq avy-line-insert-style 'below)
               (end-of-line)
               (newline)
               (save-excursion
                 (insert str)))
              (t
               (user-error "Unexpected `avy-line-insert-style'")))))))

;;;###autoload
(defun avy-move-region ()
  "Select two lines and move the text between them above the current line."
  (interactive)
  (avy-with avy-move-region
    (let* ((initial-window (selected-window))
           (beg (avy--line))
           (end (avy--line))
           text)
      (when (> beg end)
        (cl-rotatef beg end))
      (setq end (save-excursion
                  (goto-char end)
                  (1+ (line-end-position))))
      (setq text (buffer-substring beg end))
      (move-beginning-of-line nil)
      (delete-region beg end)
      (select-window initial-window)
      (insert text))))

;;;###autoload
(defun avy-kill-region (arg)
  "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-kill-region
      (let* ((beg (save-selected-window
                    (list (avy--line arg) (selected-window))))
             (end (list (avy--line arg) (selected-window))))
        (cond
          ((not (numberp (car beg)))
           (user-error "Fail to select the beginning of region"))
          ((not (numberp (car end)))
           (user-error "Fail to select the end of region"))
          ;; Restrict operation to same window. It's better if it can be
          ;; different windows but same buffer; however, then the cloned
          ;; buffers with different narrowed regions might cause problem.
          ((not (equal (cdr beg) (cdr end)))
           (user-error "Selected points are not in the same window"))
          ((< (car beg) (car end))
           (save-excursion
             (kill-region
              (car beg)
              (progn (goto-char (car end)) (forward-visible-line 1) (point)))))
          (t
           (save-excursion
             (kill-region
              (progn (goto-char (car beg)) (forward-visible-line 1) (point))
              (car end)))))))
    (select-window initial-window)))

;;;###autoload
(defun avy-kill-ring-save-region (arg)
  "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-kill-ring-save-region
      (let* ((beg (save-selected-window
                    (list (avy--line arg) (selected-window))))
             (end (list (avy--line arg) (selected-window))))
        (cond
          ((not (numberp (car beg)))
           (user-error "Fail to select the beginning of region"))
          ((not (numberp (car end)))
           (user-error "Fail to select the end of region"))
          ((not (equal (cdr beg) (cdr end)))
           (user-error "Selected points are not in the same window"))
          ((< (car beg) (car end))
           (save-excursion
             (kill-ring-save
              (car beg)
              (progn (goto-char (car end)) (forward-visible-line 1) (point)))))
          (t
           (save-excursion
             (kill-ring-save
              (progn (goto-char (car beg)) (forward-visible-line 1) (point))
              (car end)))))))
    (select-window initial-window)))

;;;###autoload
(defun avy-kill-whole-line (arg)
  "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-kill-whole-line
      (let* ((start (avy--line)))
        (if (not (numberp start))
            (user-error "Fail to select the line to kill")
          (save-excursion (goto-char start)
                          (kill-whole-line arg)))))
    (select-window initial-window)))

;;;###autoload
(defun avy-kill-ring-save-whole-line (arg)
  "Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (avy-with avy-kill-ring-save-whole-line
      (let* ((start (avy--line)))
        (if (not (numberp start))
            (user-error "Fail to select the line to kill")
          (save-excursion
            (let ((kill-read-only-ok t)
                  (buffer-read-only t))
              (goto-char start)
              (kill-whole-line arg))))))
    (select-window initial-window)))

;;;###autoload
(defun avy-setup-default ()
  "Setup the default shortcuts."
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)))

(defcustom avy-timeout-seconds 0.5
  "How many seconds to wait for the second char."
  :type 'float)

(defcustom avy-enter-times-out t
  "Whether enter exits avy-goto-char-timer early. If nil it matches newline"
  :type 'boolean)

(defvar avy-text ""
  "Store the input read by `avy--read-candidates'.")

(defun avy--read-candidates (&optional re-builder)
  "Read as many chars as possible and return their occurrences.
At least one char must be read, and then repeatedly one next char
may be read if it is entered before `avy-timeout-seconds'.  DEL
deletes the last char entered, and RET exits with the currently
read string immediately instead of waiting for another char for
`avy-timeout-seconds'.
The format of the result is the same as that of `avy--regex-candidates'.
This function obeys `avy-all-windows' setting.
RE-BUILDER is a function that takes a string and returns a regex.
When nil, `regexp-quote' is used.
If a group is captured, the first group is highlighted.
Otherwise, the whole regex is highlighted."
  (setq avy-text "")
  (let ((re-builder (or re-builder #'regexp-quote))
        char break overlays regex)
    (unwind-protect
         (progn
           (avy--make-backgrounds
            (avy-window-list))
           (while (and (not break)
                       (setq char
                             (read-char (format "%d  char%s: "
                                                (length overlays)
                                                (if (string= avy-text "")
                                                    avy-text
                                                  (format " (%s)" avy-text)))
                                        t
                                        (and (not (string= avy-text ""))
                                             avy-timeout-seconds))))
             ;; Unhighlight
             (dolist (ov overlays)
               (delete-overlay ov))
             (setq overlays nil)
             (cond
               ;; Handle RET
               ((= char 13)
                (if avy-enter-times-out
                    (setq break t)
                  (setq avy-text (concat avy-text (list ?\n)))))
               ;; Handle C-h, DEL
               ((memq char avy-del-last-char-by)
                (let ((l (length avy-text)))
                  (when (>= l 1)
                    (setq avy-text (substring avy-text 0 (1- l))))))
               ;; Handle ESC
               ((= char 27)
                (keyboard-quit))
               (t
                (setq avy-text (concat avy-text (list char)))))
             ;; Highlight
             (when (>= (length avy-text) 1)
               (let ((case-fold-search
                      (or avy-case-fold-search (string= avy-text (downcase avy-text))))
                     found)
                 (avy-dowindows current-prefix-arg
                   (dolist (pair (avy--find-visible-regions
                                  (window-start)
                                  (window-end (selected-window) t)))
                     (save-excursion
                       (goto-char (car pair))
                       (setq regex (funcall re-builder avy-text))
                       (while (re-search-forward regex (cdr pair) t)
                         (unless (not (avy--visible-p (1- (point))))
                           (let* ((idx (if (= (length (match-data)) 4) 1 0))
                                  (ov (make-overlay
                                       (match-beginning idx) (match-end idx))))
                             (setq found t)
                             (push ov overlays)
                             (overlay-put
                              ov 'window (selected-window))
                             (overlay-put
                              ov 'face 'avy-goto-char-timer-face)))))))
                 ;; No matches at all, so there's surely a typo in the input.
                 (unless found (beep)))))
           (nreverse (mapcar (lambda (ov)
                               (cons (cons (overlay-start ov)
                                           (overlay-end ov))
                                     (overlay-get ov 'window)))
                             overlays)))
      (dolist (ov overlays)
        (delete-overlay ov))
      (avy--done))))

(defvar avy--old-cands nil)

;;;###autoload
(defun avy-goto-char-timer (&optional arg)
  "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (setq avy--old-cands (avy--read-candidates))
      (avy-process avy--old-cands))))

(defun avy-push-mark ()
  "Store the current point and window."
  (let ((inhibit-message t))
    (ring-insert avy-ring
                 (cons (point) (selected-window)))
    (unless (region-active-p)
      (push-mark))))

(defun avy-pop-mark ()
  "Jump back to the last location of `avy-push-mark'."
  (interactive)
  (let (res)
    (condition-case nil
        (progn
          (while (not (window-live-p
                       (cdr (setq res (ring-remove avy-ring 0))))))
          (let* ((window (cdr res))
                 (frame (window-frame window)))
            (when (and (frame-live-p frame)
                       (not (eq frame (selected-frame))))
              (select-frame-set-input-focus frame))
            (select-window window)
            (goto-char (car res))))
      (error
       (set-mark-command 4)))))

;;;###autoload
(defun avy-transpose-lines-in-region ()
  "Transpose lines in the active region."
  (interactive)
  (when (and (use-region-p) (> (count-lines (region-beginning) (region-end)) 1))
    (let ((avy-all-windows nil)
          (fst-line-point (avy--line nil (region-beginning) (region-end))))
      (when fst-line-point
        (let ((snd-line-point (avy--line nil (region-beginning) (region-end))))
          (when snd-line-point
            (save-mark-and-excursion
              (push-mark fst-line-point)
              (goto-char snd-line-point)
              (transpose-lines 0))
            (avy-transpose-lines-in-region)))))))

;; ** Org-mode
(defvar org-reverse-note-order)
(declare-function org-refile "org")
(declare-function org-back-to-heading "org")
(declare-function org-reveal "org")

(defvar org-after-refile-insert-hook)

(defun avy-org-refile-as-child ()
  "Refile current heading as first child of heading selected with `avy.'"
  ;; Inspired by `org-teleport': http://kitchingroup.cheme.cmu.edu/blog/2016/03/18/Org-teleport-headlines/
  (interactive)
  (let* ((org-reverse-note-order t)
         (marker (save-excursion
                   (avy-with avy-goto-line
                     (unless (eq 't (avy-jump (rx bol (1+ "*") (1+ space))))
                       ;; `avy-jump' returns t when aborted with C-g.
                       (point-marker)))))
         (filename (buffer-file-name (or (buffer-base-buffer (marker-buffer marker))
                                         (marker-buffer marker))))
         (rfloc (list nil filename nil marker))
         ;; Ensure the refiled heading is visible.
         (org-after-refile-insert-hook (if (member 'org-reveal org-after-refile-insert-hook)
                                           org-after-refile-insert-hook
                                         (cons #'org-reveal org-after-refile-insert-hook))))
    (when marker
      ;; Only attempt refile if avy session was not aborted.
      (org-refile nil nil rfloc))))

(defun avy-org-goto-heading-timer (&optional arg)
  "Read one or many characters and jump to matching Org headings.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (avy-process
       (avy--read-candidates
        (lambda (input)
          (format "^\\*+ .*\\(%s\\)" input))))
      (org-back-to-heading))))

(provide 'avy)

;;; avy.el ends here
