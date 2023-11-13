;;; evil-common.el --- Common functions and utilities -*- lexical-binding: t -*-
;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'evil-vars)
(require 'rect)
(require 'thingatpt)
(require 'cl-lib)

(declare-function evil-visual-state-p "evil-states")
(declare-function evil-visual-restore "evil-states")
(declare-function evil-motion-state "evil-states")
(declare-function evil-replace-state-p "evil-states")
(declare-function evil-set-jump "evil-jumps")

;;; Compatibility with different Emacs versions

;; x-set-selection and x-get-selection have been deprecated since 25.1
;; by gui-set-selection and gui-get-selection
(defalias 'evil-get-selection
  (if (fboundp 'gui-get-selection) 'gui-get-selection 'x-get-selection))
(defalias 'evil-set-selection
  (if (fboundp 'gui-set-selection) 'gui-set-selection 'x-set-selection))

(defmacro evil-with-delay (condition hook &rest body)
  "Execute BODY when CONDITION becomes true, checking with HOOK.
HOOK can be a simple symbol or of the form (HOOK APPEND LOCAL NAME)
where:
NAME specifies the name of the entry added to HOOK.
If APPEND is non-nil, the entry is appended to the hook.
If LOCAL is non-nil, the buffer-local value of HOOK is modified."
  (declare (debug (form sexp body)) (indent 2))
  (cl-destructuring-bind (hook-sym &optional append local name)
      (mapcar #'macroexp-quote (if (consp hook) hook (list hook)))
    (macroexp-let2* nil
        ((fun-name `(make-symbol
                     ,(or name (format "evil-delay-in-%s" hook-sym))))
         (fun `(apply-partially
                (lambda (name &rest _)
                  (when ,(or condition t)
                    (remove-hook ,hook-sym name ,local)
                    ,@body
                    t))
                ,fun-name)))
      `(unless ,(and condition `(funcall ,fun))
         (progn (fset ,fun-name ,fun)
                ,@(when local `((put ,fun-name 'permanent-local-hook t)))
                (add-hook ,hook-sym ,fun-name ,append ,local))))))

(defun evil-delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK.  If APPEND is
non-nil, the entry is appended to the hook.  If LOCAL is non-nil,
the buffer-local value of HOOK is modified."
  (declare (obsolete evil-with-delay "1.15.0") (indent 2))
  (eval `(evil-with-delay ,condition (,hook ,append ,local ,name) ,form) t))

;;; List functions

(defmacro evil--add-to-alist (alist &rest elements)
  "Add the association of KEY and VAL to the value of ALIST.
If the list already contains an entry for KEY, update that entry;
otherwise prepend it to the list.

\(fn ALIST [KEY VAL]...)"
  `(progn
     ,@(cl-loop
        for (key val) on elements by #'cddr collect
        (if (< emacs-major-version 26)
            (macroexp-let2* nil ((k key) (p `(assoc ,k ,alist)))
              `(if ,p (setcdr ,p ,val)
                 (push (cons ,k ,val) ,alist)))
          `(setf (alist-get ,key ,alist nil nil #'equal) ,val)))
     ,alist))

;; custom version of `delete-if'
(defun evil-filter-list (predicate list &optional pointer)
  "Delete by side-effect all items satisfying PREDICATE in LIST.
Stop when reaching POINTER.  If the first item satisfies PREDICATE,
there is no way to remove it by side-effect; therefore, write
\(setq foo (evil-filter-list #\\='predicate foo)) to be sure of
changing the value of `foo'."
  (let ((tail list) elt head)
    (while (and tail (not (eq tail pointer)))
      (setq elt (car tail))
      (cond
       ((funcall predicate elt)
        (setq tail (cdr tail))
        (if head
            (setcdr head tail)
          (setq list tail)))
       (t
        (setq head tail
              tail (cdr tail)))))
    list))

(defun evil-member-if (predicate list &optional pointer)
  "Find the first item satisfying PREDICATE in LIST.
Stop when reaching POINTER, which should point at a link
in the list."
  (let (elt)
    (catch 'done
      (while (and (consp list) (not (eq list pointer)))
        (setq elt (car list))
        (if (funcall predicate elt)
            (throw 'done elt)
          (setq list (cdr list)))))))

(defun evil-member-recursive-if (predicate tree)
  "Find the first item satisfying PREDICATE in TREE."
  (declare (obsolete nil "1.15.0"))
  (cond
   ((funcall predicate tree)
    tree)
   ((listp tree)
    (catch 'done
      (dolist (elt tree)
        (when (setq elt (evil-member-recursive-if predicate elt))
          (throw 'done elt)))))))

(defun evil-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
Elements are compared with `eq'."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (push elt result)))
    (nreverse (cl-remove-duplicates result :test #'eq))))

(defun evil-concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates.
An alist is a list of cons cells (KEY . VALUE) where each key
may occur only once. Later values overwrite earlier values."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (setq result (assq-delete-all (car-safe elt) result))
        (push elt result)))
    (nreverse result)))

(defun evil-concat-plists (&rest sequences)
  "Concatenate property lists, removing duplicates.
A property list is a list (:KEYWORD1 VALUE1 :KEYWORD2 VALUE2...)
where each keyword may occur only once. Later values overwrite
earlier values."
  (let (result)
    (dolist (sequence sequences result)
      (while sequence
        (setq result
              (plist-put result (pop sequence) (pop sequence)))))))

(defun evil-concat-keymap-alists (&rest sequences)
  "Concatenate keymap association lists, removing duplicates.
A keymap alist is a list of cons cells (VAR . MAP) where each keymap
may occur only once, but where the variables may be repeated
\(e.g., (VAR . MAP1) (VAR . MAP2) is allowed). The order matters,
with the highest priority keymaps being listed first."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (unless (rassq (cdr-safe elt) result)
          (push elt result))))
    (nreverse result)))

(defun evil-plist-delete (prop plist)
  "Delete by side effect the property PROP from PLIST.
If PROP is the first property in PLIST, there is no way
to remove it by side-effect; therefore, write
\(setq foo (evil-plist-delete :prop foo)) to be sure of
changing the value of `foo'."
  (let ((tail plist) elt head)
    (while tail
      (setq elt (car tail))
      (cond
       ((eq elt prop)
        (setq tail (cdr (cdr tail)))
        (if head
            (setcdr (cdr head) tail)
          (setq plist tail)))
       (t
        (setq head tail
              tail (cdr (cdr tail))))))
    plist))

(defun evil-get-property (alist key &optional prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list.
If PROP is nil, return all properties for KEY.
If KEY is t, return an association list of keys
and their PROP values."
  (declare (obsolete nil "1.15.0"))
  (cond
   ((null prop)
    (cdr (assq key alist)))
   ((eq key t)
    (let (result val)
      (dolist (entry alist result)
        (setq key (car entry)
              val (cdr entry))
        (when (plist-member val prop)
          (setq val (plist-get val prop))
          (push (cons key val) result)))))
   (t
    (plist-get (cdr (assq key alist)) prop))))

(defun evil-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (declare (obsolete nil "1.15.0"))
  (set alist-var
       (let* ((alist (symbol-value alist-var))
              (plist (cdr (assq key alist))))
         (setq plist (plist-put plist prop val))
         (when properties
           (setq plist (evil-concat-plists plist properties)
                 val (car (last properties))))
         (setq alist (assq-delete-all key alist))
         (push (cons key plist) alist)))
  val)

(defun evil-state-property (state prop &optional value)
  "Return the value of property PROP for STATE.
PROP is a keyword as used by `evil-define-state'. STATE is the state's
symbolic name. If VALUE is non-nil and the value is a variable, return
the value of that variable.

If STATE is t, return an association list of states and their PROP
values instead."
  (if (eq state t)
      (cl-loop for (key . plist) in evil-state-properties with result do
               (let ((p (plist-member plist prop)))
                 (when p (push (cons key (cadr p)) result)))
               finally return result)
    (let ((val (plist-get (cdr (assq state evil-state-properties)) prop)))
      (if (and value (symbolp val) (boundp val))
          (symbol-value val)
        val))))

(eval-and-compile (defalias 'evil-swap #'cl-rotatef))

(defmacro evil-sort (&rest vars)
  "Sort the symbol values of VARS.
Place the smallest value in the first argument and the largest in the
last, sorting in between."
  (if (= (length vars) 2)
      `(when (> ,@vars) (evil-swap ,@vars))
    (let ((sorted (make-symbol "sortvar")))
      `(let ((,sorted (sort (list ,@vars) #'<)))
         (setq ,@(apply #'nconc
                        (mapcar (lambda (var) (list var `(pop ,sorted)))
                                vars)))))))

(defun evil-vector-to-string (vector)
  "Turn VECTOR into a string, changing <escape> to \"\\e\"."
  (mapconcat (lambda (c) (if (eq c 'escape) "\e" (list c)))
             vector
             ""))

;;; Command properties

(defmacro evil-define-command (command &rest body)
  "Define a command COMMAND.

\(fn COMMAND (ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((interactive '(interactive))
        args doc doc-form keys)
    ;; collect arguments
    (when (listp (car body))
      (setq args (pop body)))
    ;; collect docstring
    (cond ((stringp (car body)) (setq doc (pop body)))
          ((eq (car-safe (car body)) #'format) (setq doc-form (pop body))))
    ;; collect keywords
    (while (keywordp (car body))
      (let* ((key (pop body))
             (arg (pop body)))
        (setq keys (plist-put keys key arg)))) ; TODO: add keyword check
    ;; collect `interactive' form
    (when (eq (caar body) 'interactive)
      (cl-destructuring-bind (form . attrs)
          (apply #'evil-interactive-form (cdr (pop body)))
        (setq interactive `(interactive ,form)
              keys (evil-concat-plists keys attrs))))
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,interactive
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       ;; set command properties for symbol or lambda function
       (let ((func ,(if (and (null command) body)
                        `(lambda ,args
                           ,interactive
                           ,@body)
                      `#',command)))
         (apply #'evil-set-command-properties func ',keys)
         func))))

(define-obsolete-variable-alias
  'evil-command-properties 'evil--command-properties "1.15.0")
(defvar evil--command-properties nil
  "Specifications made by `evil-define-command'.")

(defun evil-command-properties (command)
  "Return the Evil command property list for COMMAND.
See also `evil-get-command-property'."
  (if (symbolp command) (get command 'evil--command-plist)
    (cdr (assq command evil--command-properties))))
(define-obsolete-function-alias
  'evil-get-command-properties #'evil-command-properties "1.15.0")

(gv-define-setter evil-command-properties (val command)
  `(if (symbolp ,command) (put ,command 'evil--command-plist ,val)
     ,(macroexp-let2 nil p `(assq ,command evil--command-properties)
        `(if ,p (setcdr ,p ,val)
           (push (cons ,command ,val) evil--command-properties)))))

(defalias 'evil-has-command-properties-p #'evil-command-properties
  "Whether Evil properties are defined for COMMAND.
See also `evil-has-command-property-p'.")

(defun evil-has-command-property-p (command property)
  "Whether COMMAND has Evil PROPERTY.
See also `evil-has-command-properties-p'."
  (plist-member (evil-command-properties command) property))

(defun evil-get-command-property (command property &optional default)
  "Return the value of Evil PROPERTY of COMMAND.
If the command does not have the property, return DEFAULT.
See also `evil-get-command-properties'."
  (let ((p (plist-member (evil-command-properties command) property)))
    (if p (cadr p) default)))

(defun evil-add-command-properties (command &rest properties)
  "Set each Evil command property KEY to its VAL for COMMAND.
To replace existing properties, use `evil-set-command-properties'.

\(fn COMMAND [KEY VAL]...)"
  (let ((props (evil-command-properties command)))
    (while properties
      (setq props (plist-put props (pop properties) (pop properties))))
    (setf (evil-command-properties command) props)))

(defalias 'evil-set-command-property #'evil-add-command-properties
  "Set PROPERTY to VALUE for COMMAND.
To set multiple properties at once, see
`evil-set-command-properties' and `evil-add-command-properties'.

\(fn COMMAND PROPERTY VALUE)")
(defalias 'evil-put-command-property #'evil-set-command-property)

(defun evil-set-command-properties (command &rest properties)
  "Replace all of COMMAND's properties with PROPERTIES.
PROPERTIES should be a property list.
This erases all previous properties; to only add properties,
use `evil-set-command-property'."
  (setf (evil-command-properties command) properties))

(defun evil-remove-command-properties (command &rest properties)
  "Remove PROPERTIES from COMMAND.
PROPERTIES should be a list of properties (:PROP1 :PROP2 ...).
If PROPERTIES is the empty list, all properties are removed."
  (let (plist)
    (when properties
      (setq plist (evil-command-properties command))
      (dolist (property properties)
        (setq plist (evil-plist-delete property plist))))
    (apply #'evil-set-command-properties command plist)))

(defun evil-yank-handler (&optional motion)
  "Return the yank handler for MOTION.
MOTION defaults to the current motion."
  (evil-get-command-property (or motion evil-this-motion) :yank-handler))

(defun evil-declare-motion (command)
  "Declare COMMAND to be a movement function.
This ensures that it behaves correctly in visual state."
  (evil-add-command-properties command :keep-visual t :repeat 'motion))

(defun evil-declare-repeat (command)
  "Declare COMMAND to be repeatable."
  (evil-add-command-properties command :repeat t))

(defun evil-declare-not-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat nil))
(defalias 'evil-declare-ignore-repeat #'evil-declare-not-repeat)

(defun evil-declare-change-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes rather than
keystrokes."
  (evil-add-command-properties command :repeat 'change))

(defun evil-declare-insert-at-point-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes."
  (evil-add-command-properties command :repeat 'insert-at-point))

(defun evil-declare-abort-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat 'abort))

(defun evil-delimited-arguments (string &optional count)
  "Parse STRING as a sequence of delimited arguments.
Return a list of COUNT strings, or as many arguments as the string
contains.  The first non-blank character is taken to be the delimiter.
If some arguments are missing from STRING, the resulting list is
padded with nil values.  Two delimiters following directly after each
other gives an empty string."
  (unless count (setq count -1))
  (let ((idx 0) delim regexp result)
    (when (string-match "^[[:space:]]*\\([^[:space:]]\\)" string)
      (setq delim (match-string 1 string)
            regexp (format "%s\\(\\(?:\\\\.\\|[^%s]\\)*\\)"
                           (regexp-quote delim) delim))
      (while (and (/= count 0) (string-match regexp string idx)
                  (/= (match-beginning 1) (length string)))
        (setq idx (match-end 0)
              count (1- count))
        (push
         (if (when (= count 0)
               (not (string-match-p
                     (concat (regexp-quote delim) "[[:space:]]*$")
                     string idx)))
             (substring string (match-beginning 1))
           (match-string 1 string))
         result)))
    (dotimes (_ count) (push nil result))
    (nreverse result)))

(defun evil-concat-charsets (&rest sets)
  "Concatenate character sets.
A character set is the part between [ and ] in a regular expression.
If any character set is complemented, the result is also complemented."
  (let ((bracket "") (complement "") (hyphen "") result)
    (dolist (set sets)
      (when (string-match-p "^\\^" set)
        (setq set (substring set 1)
              complement "^"))
      (when (string-match-p "^]" set)
        (setq set (substring set 1)
              bracket "]"))
      (when (string-match-p "^-" set)
        (setq set (substring set 1)
              hyphen "-"))
      (setq result (concat result set)))
    (format "%s%s%s%s" complement bracket hyphen result)))

;;; Key sequences

(defun evil-keypress-parser (&optional input)
  "Read from keyboard or INPUT and build a command description.
Return (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (when input (setq unread-command-events (append input unread-command-events)))
  (let (count negative)
    (catch 'done
      (while t
        (let ((seq (read-key-sequence "")))
          (when seq
            (let ((cmd (key-binding seq)))
              (cond
               ((null cmd) (throw 'done (list nil nil)))
               ((arrayp cmd) ; keyboard macro, recursive call
                (let ((cmd (evil-keypress-parser cmd)))
                  (throw 'done
                         (list (car cmd)
                               (if (or count (cadr cmd))
                                   (list (car cmd) (* (or count 1)
                                                      (or (cadr cmd) 1))))))))
               ((or (eq cmd #'digit-argument)
                    (and (equal seq "0") count))
                (let* ((event (aref seq (1- (length seq))))
                       (char (or (when (characterp event) event)
                                 (when (symbolp event)
                                   (get event 'ascii-character))))
                       (digit (when (integerp char) (- (logand char ?\177) ?0))))
                  (setq count (+ (* 10 (or count 0)) digit))))
               ((eq cmd #'negative-argument)
                (setq negative (not negative)))
               (t
                (throw 'done (list cmd
                                   (and count
                                        (* count
                                           (if negative -1 1))))))))))))))

(defun evil-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map nil)
        (overriding-local-map evil-read-key-map)
        seq char cmd)
    (unwind-protect
        (condition-case nil
            (progn
              (define-key new-global-map [menu-bar]
                (lookup-key global-map [menu-bar]))
              (define-key new-global-map [tab-bar]
                (lookup-key global-map [tab-bar]))
              (define-key new-global-map [tool-bar]
                (lookup-key global-map [tool-bar]))
              (setq new-global-map
                    (append new-global-map
                            (list (make-char-table 'display-table
                                                   'self-insert-command))))
              (use-global-map new-global-map)
              (setq seq (read-key-sequence prompt nil t)
                    char (aref seq 0)
                    cmd (key-binding seq))
              (while (arrayp cmd)
                (setq char (aref cmd 0)
                      cmd (key-binding cmd)))
              (cond
               ((eq cmd 'self-insert-command) char)
               (cmd (call-interactively cmd))
               (t (user-error "No replacement character typed"))))
          (quit
           (when (fboundp 'evil-repeat-abort)
             (evil-repeat-abort))
           (signal 'quit nil)))
      (use-global-map old-global-map))))

(defun evil-read-quoted-char ()
  "Command that calls `read-quoted-char'.
This command can be used wherever `read-quoted-char' is required
as a command. Its main use is in the `evil-read-key-map'."
  (interactive)
  (read-quoted-char))

(defvar evil-digraph-read-key-keymap
  (let ((map (make-sparse-keymap))
        (n ?0))
    (while (<= n ?9)
      (define-key map (vector (intern (concat "kp-" (string n)))) nil)
      (cl-incf n))
    map)
   "By default, used to exclude otherwise disabled fallbacks.")

(declare-function evil-digraph "evil-digraphs")
(defun evil-read-digraph-char-with-overlay (overlay)
  "Read two chars, displaying the first in OVERLAY, replacing \"?\".
Return the digraph from `evil-digraph', else return second char."
  (interactive)
  (unwind-protect
      (let ((read-key-empty-map
             (let ((map (copy-keymap evil-digraph-read-key-keymap)))
               (set-keymap-parent map read-key-empty-map)
               ;; Disable read-key-sequence unbound fallbacks, e.g. downcasing
               (define-key map [t] 'dummy)
               map))
            char1 char2)
        ;; create overlay prompt
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'after-string
                     #("?" 0 1 (face minibuffer-prompt cursor 1)))
        (setq char1 (read-key))
        (overlay-put overlay 'after-string
                     (propertize (char-to-string char1)
                                 'face 'minibuffer-prompt
                                 'cursor 1))
        (setq char2 (read-key))

        (or (evil-digraph (list char1 char2))
            ;; use the last character if undefined
            char2))
    (delete-overlay overlay)))

(defun evil-read-digraph-char (&optional hide-chars)
  "Read two keys from keyboard forming a digraph.
This function creates an overlay at (point), hiding the next
HIDE-CHARS characters.  HIDE-CHARS defaults to 1."
  (interactive)
  (let ((overlay (make-overlay
                  (point) (min (+ (point) (or hide-chars 1))
                               (point-max)))))
    (evil-read-digraph-char-with-overlay overlay)))

(defun evil-read-motion (&optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `evil-define-visual-selection'.
Return a list (MOTION COUNT [TYPE])."
  (let (command prefix)
    (setq evil-this-type-modified nil)
    (unless motion
      (while (progn
               (setq command (evil-keypress-parser)
                     motion (pop command)
                     prefix (pop command))
               (when prefix
                 (setq count
                       (if count
                           (string-to-number
                            (concat (number-to-string count)
                                    (number-to-string prefix)))
                         prefix)))
               ;; if the command is a type modifier, read more
               (when (rassq motion evil-visual-alist)
                 (setq modifier
                       (or modifier
                           (car (rassq motion evil-visual-alist))))))))
    (when modifier
      (setq type (or type (evil-type motion 'exclusive)))
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (setq type (if (eq type 'exclusive) 'inclusive 'exclusive)))
       (t (setq type modifier)))
      (setq evil-this-type-modified type))
    (list motion count type)))

(defun evil-mouse-events-p (keys)
  "Return non-nil iff KEYS contains a mouse event."
  (cl-loop for key across keys thereis
           (or (mouse-event-p key)
               (mouse-movement-p key))))

(defun evil-extract-count (keys)
  "Split the key-sequence KEYS into prefix-argument and the rest.
Return the list (PREFIX CMD SEQ REST), where PREFIX is the
prefix count, CMD the command to be executed, SEQ the subsequence
calling CMD, and REST is all remaining events in the
key-sequence. PREFIX and REST may be nil if they do not exist.
If a command is bound to some keyboard macro, it is expanded
recursively."
  (catch 'done
    (let ((len (length keys))
          (beg 0)
          (end 1)
          found-prefix)
      (while (<= end len)
        (let* ((seq (substring keys beg end))
               (cmd (key-binding seq)))
          (cond
           ((memq cmd '(undefined nil))
            (user-error "No command bound to `%s'" seq))
           ((arrayp cmd) ; keyboard macro, replace command with macro
            (setq keys (vconcat (substring keys 0 beg)
                                cmd
                                (substring keys end))
                  end (1+ beg)
                  len (length keys)))
           ((functionp cmd)
            (if (or (memq cmd '(digit-argument negative-argument))
                    (and found-prefix
                         (equal (vconcat seq) (vector ?0))))
                ;; skip those commands
                (setq found-prefix t ; found at least one prefix argument
                      beg end
                      end (1+ end))
              ;; a real command, finish
              (throw 'done
                     (list (unless (zerop beg)
                             (string-to-number
                              (concat (substring keys 0 beg))))
                           cmd
                           seq
                           (when (< end len)
                             (substring keys end))))))
           (t ; append a further event
            (setq end (1+ end))))))
      (user-error "Key sequence contains no complete binding"))))

(defun evil-extract-append (file-or-append)
  "Return an (APPEND . FILENAME) pair based on FILE-OR-APPEND.
FILE-OR-APPEND should either be a filename or a \">> FILE\"
directive.  APPEND will be t if FILE-OR-APPEND is an append
directive and nil otherwise.  FILENAME will be the extracted
filename."
  (if (and (stringp file-or-append)
           (string-match "\\(>> *\\)" file-or-append))
      (cons t (substring file-or-append (match-end 1)))
    (cons nil file-or-append)))

(defun evil-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

(defun evil-lookup-key (map key)
  "Return non-nil value if KEY is bound in MAP."
  (let ((definition (lookup-key map key)))
    (unless (numberp definition) ; in-band error
      definition)))

;;; Display

(defun evil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above."
  (unless (and (not (functionp specs))
               (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (ignore-errors (funcall spec)))
     ((stringp spec)
      (evil-set-cursor-color spec))
     (t
      (setq cursor-type spec)))))

(defun evil-set-cursor-color (color)
  "Set the cursor color to COLOR."
  (unless (equal (frame-parameter nil 'cursor-color) color)
    ;; `set-cursor-color' forces a redisplay, so only
    ;; call it when the color actually changes
    (set-cursor-color color)))

(defun evil-refresh-cursor (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
BUFFER defaults to the current buffer.  If STATE is nil the
cursor type is either `evil-force-cursor' or the current state."
  (defvar evil-local-mode)
  (when evil-local-mode
    (let* ((state (or state evil-force-cursor evil-state 'normal))
           (default (or evil-default-cursor t))
           (cursor (evil-state-property state :cursor t))
           (color (or (and (stringp cursor) cursor)
                      (and (listp cursor) (evil-member-if #'stringp cursor))
                      (frame-parameter nil 'cursor-color))))
      (with-current-buffer (or buffer (current-buffer))
        ;; if both STATE and `evil-default-cursor'
        ;; specify a color, don't set it twice
        (evil-set-cursor (if (and color (listp default))
                             (cl-remove-if #'stringp default)
                           default))
        (evil-set-cursor cursor)))))

(defmacro evil-save-cursor (&rest body)
  "Save the current cursor; execute BODY; restore the cursor."
  (declare (indent defun) (debug t) (obsolete nil "1.15.0"))
  `(let ((cursor cursor-type)
         (color (frame-parameter (selected-frame) 'cursor-color))
         (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (setq cursor-type cursor)
       (evil-set-cursor-color color))))

(defun evil-echo (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (unless evil-no-display
    (let (message-log-max)
      (apply #'message string args))))

(defun evil-echo-area-save ()
  "Save the current echo area in `evil-echo-area-message'."
  (setq evil-echo-area-message (current-message)))

(defun evil-echo-area-restore ()
  "Restore the echo area from `evil-echo-area-message'.
Does not restore if `evil-write-echo-area' is non-nil."
  (unless evil-write-echo-area
    (if evil-echo-area-message
        (message "%s" evil-echo-area-message)
      (message nil)))
  (setq evil-echo-area-message nil
        evil-write-echo-area nil))

;; toggleable version of `with-temp-message'
(defmacro evil-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun) (debug t))
  `(let ((inhibit-quit t)
         evil-echo-area-message evil-write-echo-area)
     (evil-echo-area-save)
     (unwind-protect
         (progn ,@body)
       (evil-echo-area-restore))))

(defmacro evil-without-display (&rest body)
  "Execute BODY without Evil displays.
Inhibits echo area messages, mode line updates and cursor changes."
  (declare (indent defun)
           (debug t))
  `(let ((evil-no-display t))
     ,@body))

(defun evil-count-lines (beg end)
  "Return absolute line-number-difference betweeen BEG and END.
This should give the same results no matter where on the line BEG
and END are."
  (if (= beg end)
      0
    (let ((end-at-bol (save-excursion (goto-char (max beg end))
                                      (bolp))))
      (if end-at-bol
          (count-lines beg end)
        (1- (count-lines beg end))))))

;;; Movement

(defun evil-normalize-position (pos)
  "Return POS if it does not exceed the buffer boundaries.
If POS is less than `point-min', return `point-min'.
Is POS is more than `point-max', return `point-max'.
If POS is a marker, return its position."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t
    pos)))

(defmacro evil-save-goal-column (&rest body)
  "Restore the goal column after execution of BODY.
See also `evil-save-column'."
  (declare (indent defun)
           (debug t))
  `(let ((goal-column goal-column)
         (temporary-goal-column temporary-goal-column))
     ,@body))

(defmacro evil-save-column (&rest body)
  "Restore the column after execution of BODY.
See also `evil-save-goal-column'."
  (declare (indent defun)
           (debug t))
  `(let ((col (current-column)))
     (evil-save-goal-column
       ,@body
       (move-to-column col))))

(defun evil--stick-to-eol-p ()
  "Called by vertical movement commands to help determine cursor position."
  (and evil-track-eol
       (eq (or goal-column temporary-goal-column) most-positive-fixnum)
       (memq last-command '(next-line previous-line))))

(defun evil-eolp ()
  "Like `eolp' but accounts for `evil-move-beyond-eol' being nil."
  (ignore-errors
    (save-excursion
      (unless (or evil-move-beyond-eol (memq evil-state '(insert replace)))
        (forward-char))
      (eolp))))

(defmacro evil-ensure-column (&rest body)
  "Execute BODY so that column after execution is correct.
If `evil-start-of-line' is nil, treat BODY as if it were a `next-line' command.
This mostly copies the approach of Emacs' `line-move-1', but is modified
so it is more compatible with Evil's notion of EOL tracking."
  (declare (indent defun) (debug t))
  `(progn
     (unless evil-start-of-line
       (setq this-command 'next-line
             temporary-goal-column
             (cond ((memq last-command '(next-line previous-line))
                    temporary-goal-column)
                   ((and track-eol (eolp) (not (bolp))) most-positive-fixnum)
                   (t (current-column)))))
     ,@body
     (if evil-start-of-line
         (evil-first-non-blank)
       (line-move-to-column
        (or goal-column
            (if (consp temporary-goal-column)
                ;; Guard against a negative value as `temporary-goal-column'
                ;; may have a negative component when both `whitespace-mode'
                ;; and `display-line-numbers-mode' are enabled (#1297).
                (max 0 (+ (truncate (car temporary-goal-column))
                          (cdr temporary-goal-column)))
              temporary-goal-column))))))

(defun evil-narrow (beg end)
  "Restrict the buffer to BEG and END.
BEG or END may be nil, specifying a one-sided restriction including
`point-min' or `point-max'. See also `evil-with-restriction.'"
  (narrow-to-region
   (if beg (max beg (point-min)) (point-min))
   (if end (min end (point-max)) (point-max))))

(defmacro evil-with-restriction (beg end &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.
BEG or END may be nil as passed to `evil-narrow'; this creates
a one-sided restriction."
  (declare (indent 2)
           (debug t))
  `(save-restriction
     (let ((evil-restriction-stack
            (cons (cons (point-min) (point-max)) evil-restriction-stack)))
       (evil-narrow ,beg ,end)
       ,@body)))

(defmacro evil-without-restriction (&rest body)
  "Execute BODY with the top-most narrowing removed.
This works only if the previous narrowing has been generated by
`evil-with-restriction'."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (widen)
     (narrow-to-region (car (car evil-restriction-stack))
                       (cdr (car evil-restriction-stack)))
     (let ((evil-restriction-stack (cdr evil-restriction-stack)))
       ,@body)))

(defmacro evil-narrow-to-field (&rest body)
  "Narrow to the current field."
  (declare (indent defun)
           (debug t))
  `(evil-with-restriction (field-beginning) (field-end)
     ,@body))

(defun evil-move-beginning-of-line (&optional arg)
  "Move to the beginning of the line as displayed.
Like `move-beginning-of-line', but retains the goal column."
  (evil-save-goal-column
    (move-beginning-of-line arg)
    (beginning-of-line)))

(defun evil-move-end-of-line (&optional arg)
  "Move to the end of the line as displayed.
Like `move-end-of-line', but retains the goal column."
  (evil-save-goal-column
    (move-end-of-line arg)
    (end-of-line)))

(defun evil-adjust-cursor (&optional _)
  "Move point one character back if at the end of a non-empty line.
This behavior is controlled by `evil-move-beyond-eol'."
  (and (not evil-move-beyond-eol)
       (eolp)
       (= (point) (save-excursion (evil-move-end-of-line) (point)))
       (evil-move-cursor-back t)))

(defun evil-move-cursor-back (&optional force)
  "Move point one character back within the current line.
Contingent on the variable `evil-move-cursor-back' or the FORCE
argument.  Movement is constrained to the current field."
  (or (not (or evil-move-cursor-back force))
      (bolp)
      (when (bound-and-true-p visual-line-mode)
        (= (point) (save-excursion (vertical-motion 0) (point))))
      (goto-char (constrain-to-field (1- (point)) (point)))))

(defun evil-line-position (line &optional column)
  "Return the position of LINE.
If COLUMN is specified, return its position on the line.
A negative number means the end of the line."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (when (numberp column)
      (if (< column 0)
          (forward-line)
        (move-to-column column)))
    (point)))

(defun evil-column (&optional pos)
  "Return the horizontal position of POS.
POS defaults to point."
  (save-excursion
    (when pos
      (goto-char pos))
    (current-column)))

(defun evil-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right if DIR
is non-nil) and returns point."
  (interactive "p")
  (move-to-column column force)
  (unless force
    (when (or (not dir) (and (numberp dir) (< dir 1)))
      (when (> (current-column) column)
        (evil-move-cursor-back))))
  (point))

(defmacro evil-loop (spec &rest body)
  "Loop with countdown variable.
Evaluate BODY with VAR counting down from COUNT to 0.
COUNT can be negative, in which case VAR counts up instead.
The return value is the value of VAR when the loop
terminates, which is 0 if the loop completes successfully.
RESULT specifies a variable for storing this value.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun) (debug dolist) (obsolete nil "1.15.0"))
  (let* ((i (make-symbol "loopvar"))
         (var (pop spec))
         (count (pop spec))
         (result (pop spec)))
    (setq var (or (unless (eq var result) var) i)
          result (or result var))
    `(let ((,var ,count))
       (setq ,result ,var)
       (while (/= ,var 0)
         ,@body
         (if (> ,var 0)
             (setq ,var (1- ,var))
           (setq ,var (1+ ,var)))
         (setq ,result ,var))
       ,var)))

;;; Motions

(defmacro evil-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. Set RESULT, if specified, to the
number of unsuccessful iterations, which is 0 if the loop completes
successfully. This is also the return value.

Each iteration must move point; if point does not change, the loop
immediately quits.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (count (or (pop spec) 0))
         (result (or (pop spec) var))
         (i (make-symbol "loopvar")))
    `(let* ((,i ,count)
            (,var (if (< ,i 0) -1 1)))
       (while (and (/= ,i 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,i (if (< ,i 0) (1+ ,i) (1- ,i))))
       (setq ,result ,i))))

(defun evil-signal-at-bob-or-eob (&optional count)
  "Signal error if `point' is at boundaries.
If `point' is at bob and COUNT is negative this function signals
`beginning-of-buffer'.  If `point' is at eob and COUNT is positive
this function signals `end-of-buffer'.  This function should be used
in motions. COUNT defaults to 1."
  (setq count (or count 1))
  (cond
   ((< count 0) (evil-signal-at-bob))
   ((> count 0) (evil-signal-at-eob))))

(defun evil-signal-at-bob ()
  "Signal `beginning-of-buffer' if `point' is at bob.
This function should be used in backward motions. If `point' is at
bob so that no further backward motion is possible the error
`beginning-of-buffer' is raised."
  (when (bobp) (signal 'beginning-of-buffer nil)))

(defun evil-signal-at-eob ()
  "Signal `end-of-buffer' if `point' is at eob.
This function should be used in forward motions. If `point' is close
to eob so that no further forward motion is possible the error
`end-of-buffer' is raised. This is the case if `point' is at
`point-max' or if is one position before `point-max',
`evil-move-beyond-eol' is nil and `point' is not at the end
of a line. The latter is necessary because `point' cannot be
moved to `point-max' if `evil-move-beyond-eol' is nil and
the last line in the buffer is not empty."
  (when (or (eobp)
            (and (not (eolp))
                 (not evil-move-beyond-eol)
                 (save-excursion (forward-char) (eobp))))
    (signal 'end-of-buffer nil)))

(defmacro evil-with-hproject-point-on-window (&rest body)
  "Project point after BODY to current window.
If point is on a position left or right of the current window
then it is moved to the left and right boundary of the window,
respectively. If `auto-hscroll-mode' is non-nil then the left and
right positions are increased or decreased, respectively, by
`horizontal-margin' so that no automatic scrolling occurs."
  (declare (indent defun)
           (debug t))
  (let ((diff (make-symbol "diff"))
        (left (make-symbol "left"))
        (right (make-symbol "right")))
    `(let ((,diff (if auto-hscroll-mode (1+ hscroll-margin) 0))
           auto-hscroll-mode)
       ,@body
       (let* ((,left (+ (window-hscroll) ,diff))
              (,right (+ (window-hscroll) (window-width) (- ,diff) -1)))
         (move-to-column (min (max (current-column) ,left) ,right))))))

(defun evil-forward-not-thing (thing &optional count)
  "Move point to the end or beginning of the complement of THING."
  (evil-motion-loop (dir (or count 1))
    (let (bnd)
      (cond
       ((> dir 0)
        (while (and (setq bnd (bounds-of-thing-at-point thing))
                    (< (point) (cdr bnd)))
          (goto-char (cdr bnd)))
        ;; no thing at (point)
        (if (zerop (forward-thing thing))
            ;; now at the end of the next thing
            (let ((bnd (bounds-of-thing-at-point thing)))
              (if (or (< (car bnd) (point))    ; end of a thing
                      (= (car bnd) (cdr bnd))) ; zero width thing
                  (goto-char (car bnd))
                ;; beginning of yet another thing, go back
                (forward-thing thing -1)))
          (goto-char (point-max))))
       (t
        (while (and (not (bobp))
                    (setq bnd (progn (backward-char) (bounds-of-thing-at-point thing)))
                    (< (point) (cdr bnd)))
          (goto-char (car bnd)))
        ;; either bob or no thing at point
        (goto-char
         (if (and (not (bobp))
                  (zerop (forward-thing thing -1))
                  (setq bnd (bounds-of-thing-at-point thing)))
             (cdr bnd)
           (point-min))))))))

(defun evil-bounds-of-not-thing-at-point (thing &optional which)
  "Return the bounds of a complement of THING at point.
If there is a THING at point nil is returned.  Otherwise if WHICH
is nil or 0 a cons cell (BEG . END) is returned. If WHICH is
negative the beginning is returned. If WHICH is positive the END
is returned."
  (let ((pnt (point)))
    (let ((beg (save-excursion
                 (and (zerop (forward-thing thing -1))
                      (forward-thing thing))
                 (if (> (point) pnt) (point-min) (point))))
          (end (save-excursion
                 (and (zerop (forward-thing thing))
                      (forward-thing thing -1))
                 (if (< (point) pnt) (point-max) (point)))))
      (when (and (<= beg (point) end) (< beg end))
        (cond
         ((or (not which) (zerop which)) (cons beg end))
         ((< which 0) beg)
         ((> which 0) end))))))

(defun evil-forward-nearest (count &rest forwards)
  "Move point forward to the first of several motions.
FORWARDS is a list of forward motion functions (i.e. each moves
point forward to the next end of a text object (if passed a +1)
or backward to the preceeding beginning of a text object (if
passed a -1)). This function calls each of these functions once
and moves point to the nearest of the resulting positions. If
COUNT is positive point is moved forward COUNT times, if negative
point is moved backward -COUNT times."
  (evil-motion-loop (dir (or count 1))
    (let ((pnt (point))
          (nxt (if (< dir 0) (point-min) (point-max))))
      (dolist (fwd forwards)
        (goto-char pnt)
        (ignore-errors
          (evil-with-restriction
              (when (< dir 0)
                (save-excursion
                  (goto-char nxt)
                  (line-beginning-position 0)))
              (when (> dir 0)
                (save-excursion
                  (goto-char nxt)
                  (line-end-position 2)))
            (and (zerop (funcall fwd dir))
                 (/= (point) pnt)
                 (if (< dir 0) (> (point) nxt) (< (point) nxt))
                 (setq nxt (point))))))
      (goto-char nxt))))

(defun bounds-of-evil-string-at-point (&optional state)
  "Return the bounds of a string at point.
If STATE is given it used a parsing state at point."
  (save-excursion
    (let ((state (or state (syntax-ppss))))
      (when (nth 3 state)
        (cons (nth 8 state)
              (when (parse-partial-sexp
                     (point) (point-max) nil nil state 'syntax-table)
                (point)))))))
(put 'evil-string 'bounds-of-thing-at-point #'bounds-of-evil-string-at-point)

(defun bounds-of-evil-comment-at-point ()
  "Return the bounds of a string at point."
  (save-excursion
    (let ((state (syntax-ppss)))
      (when (nth 4 state)
        (cons (nth 8 state)
              (when (parse-partial-sexp
                     (point) (point-max) nil nil state 'syntax-table)
                (point)))))))
(put 'evil-comment 'bounds-of-thing-at-point #'bounds-of-evil-comment-at-point)

;; The purpose of this function is to provide line motions which
;; preserve the column. This is how `previous-line' and `next-line'
;; work, but unfortunately the behaviour is hard-coded: if and only if
;; the last command was `previous-line' or `next-line', the column is
;; preserved. Furthermore, in contrast to Vim, when we cannot go
;; further, those motions move point to the beginning resp. the end of
;; the line (we never want point to leave its column). The code here
;; comes from simple.el, and I hope it will work in future.
(defun evil-line-move (count &optional noerror)
  "Like `line-move' but conserves the column.
Signals an error at buffer boundaries unless NOERROR is non-nil."
  (setq this-command (if (< count 0) 'previous-line 'next-line))
  (let ((last-command
         ;; Reset tmp goal column between visual/logical movement
         (when (or (eq line-move-visual (consp temporary-goal-column))
                   (eq temporary-goal-column most-positive-fixnum))
           last-command))
        (opoint (point)))
    (condition-case err
        (line-move count)
      ((beginning-of-buffer end-of-buffer)
       (let ((col (or goal-column
                      (car-safe temporary-goal-column)
                      temporary-goal-column)))
         (line-move-finish col opoint (< count 0)))
       (or noerror (/= (point) opoint) (signal (car err) (cdr err)))))))

(defun evil-forward-syntax (syntax &optional count)
  "Move point to the end or beginning of a sequence of characters in SYNTAX.
Stop on reaching a character not in SYNTAX."
  (let ((notsyntax (if (= (aref syntax 0) ?^)
                       (substring syntax 1)
                     (concat "^" syntax))))
    (evil-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-syntax-backward notsyntax)
        (skip-syntax-backward syntax))
       (t
        (skip-syntax-forward notsyntax)
        (skip-syntax-forward syntax))))))

(defun evil-forward-chars (chars &optional count)
  "Move point to the end or beginning of a sequence of CHARS.
CHARS is a character set as inside [...] in a regular expression."
  (let ((notchars (if (= (aref chars 0) ?^)
                      (substring chars 1)
                    (concat "^" chars))))
    (evil-motion-loop (dir (or count 1))
      (cond
       ((< dir 0)
        (skip-chars-backward notchars)
        (skip-chars-backward chars))
       (t
        (skip-chars-forward notchars)
        (skip-chars-forward chars))))))

(defun evil-up-block (beg end &optional count)
  "Move point to the end or beginning of text enclosed by BEG and END.
BEG and END should be regular expressions matching the opening
and closing delimiters, respectively. If COUNT is greater than
zero point is moved forward otherwise it is moved
backwards. Whenever an opening delimiter is found the COUNT is
increased by one, if a closing delimiter is found the COUNT is
decreased by one. The motion stops when COUNT reaches zero. The
match-data reflects the last successful match (that caused COUNT
to reach zero). The behaviour of this functions is similar to
`up-list'."
  (let* ((count (or count 1))
         (forwardp (> count 0))
         (dir (if forwardp +1 -1)))
    (catch 'done
      (while (not (zerop count))
        (let* ((pnt (point))
               (cl (save-excursion
                     (and (re-search-forward (if forwardp end beg) nil t dir)
                          (or (/= pnt (point))
                              (progn
                                ;; zero size match, repeat search from
                                ;; the next position
                                (forward-char dir)
                                (re-search-forward (if forwardp end beg) nil t dir)))
                          (point))))
               (match (match-data t))
               (op (save-excursion
                     (and (not (equal beg end))
                          (re-search-forward (if forwardp beg end) cl t dir)
                          (or (/= pnt (point))
                              (progn
                                ;; zero size match, repeat search from
                                ;; the next position
                                (forward-char dir)
                                (re-search-forward (if forwardp beg end) cl t dir)))
                          (point)))))
          (cond
           ((not cl)
            (goto-char (if forwardp (point-max) (point-min)))
            (set-match-data nil)
            (throw 'done count))
           (t
            (if op
                (progn
                  (setq count (if forwardp (1+ count) (1- count)))
                  (goto-char op))
              (setq count (if forwardp (1- count) (1+ count)))
              (if (zerop count) (set-match-data match))
              (goto-char cl))))))
      0)))

(defun evil-up-paren (open close &optional count)
  "Move point to the end or beginning of balanced parentheses.
OPEN and CLOSE should be characters identifying the opening and
closing parenthesis, respectively. If COUNT is greater than zero
point is moved forward otherwise it is moved backwards. Whenever
an opening delimiter is found the COUNT is increased by one, if a
closing delimiter is found the COUNT is decreased by one. The
motion stops when COUNT reaches zero. The match-data reflects the
last successful match (that caused COUNT to reach zero)."
  ;; Always use the default `forward-sexp-function'. This is important
  ;; for modes that use a custom one like `python-mode'.
  ;; (addresses #364)
  (let (forward-sexp-function)
    (with-syntax-table (copy-syntax-table (syntax-table))
      (modify-syntax-entry open (format "(%c" close))
      (modify-syntax-entry close (format ")%c" open))
      (let ((rest (evil-motion-loop (dir count)
                    (let ((pnt (point)))
                      (condition-case nil
                          (cond
                           ((> dir 0)
                            (while (progn
                                     (up-list dir)
                                     (/= (char-before) close))))
                           (t
                            (while (progn
                                     (up-list dir)
                                     (/= (char-after) open)))))
                        (error (goto-char pnt)))))))
        (cond
         ((= rest count) (set-match-data nil))
         ((> count 0) (set-match-data (list (1- (point)) (point))))
         (t (set-match-data (list (point) (1+ (point))))))
        rest))))

(defun evil-up-xml-tag (&optional count)
  "Move point to the end or beginning of balanced xml tags.
If COUNT is greater than zero point is moved forward otherwise it is moved
backwards.  Whenever an opening delimiter is found the COUNT is increased by
one, if a closing delimiter is found the COUNT is decreased by one.  The motion
stops when COUNT reaches zero.  The match data reflects the last successful
match (that caused COUNT to reach zero)."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (op (if (> dir 0) 1 2))
         (cl (if (> dir 0) 2 1))
         (orig (point))
         pnt tags match)
    (catch 'done
      (while (> count 0)
        ;; find the previous opening tag
        (while
            (and (setq match
                       (re-search-forward
                        "<\\([^/ >\n]+\\)\\(?:=>?\\|[^\"/>]\\|\"[^\"]*\"\\)*?>\\|</\\([^>]+?\\)>"
                        nil t dir))
                 (cond
                  ((match-beginning op)
                   (push (match-string op) tags))
                  ((null tags) nil) ; free closing tag
                  ((and (< dir 0)
                        (string= (car tags) (match-string cl)))
                   ;; in backward direction we only accept matching
                   ;; tags. If the current tag is a free opener
                   ;; without matching closing tag, the subsequent
                   ;; test will make us ignore this tag
                   (pop tags))
                  ((and (> dir 0))
                   ;; non matching openers are considered free openers
                   (while (and tags
                               (not (string= (car tags)
                                             (match-string cl))))
                     (pop tags))
                   (pop tags)))))
        (unless (setq match (and match (match-data t)))
          (setq match nil)
          (throw 'done count))
        ;; found closing tag, look for corresponding opening tag
        (cond
         ((> dir 0)
          (setq pnt (match-end 0))
          (goto-char (match-beginning 0)))
         (t
          (setq pnt (match-beginning 0))
          (goto-char (match-end 0))))
        (let* ((tag (match-string cl))
               (refwd (concat "<\\(/\\)?"
                              (regexp-quote tag)
                              "\\(?:>\\|[ \n]\\(?:[^\"/>]\\|\"[^\"]*\"\\)*?>\\)"))
               (cnt 1))
          (while (and (> cnt 0) (re-search-backward refwd nil t dir))
            (setq cnt (+ cnt (if (match-beginning 1) dir (- dir)))))
          (if (zerop cnt) (setq count (1- count) tags nil))
          (goto-char pnt)))
      (if (> count 0)
          (set-match-data nil)
        (set-match-data match)
        (goto-char (if (> dir 0) (match-end 0) (match-beginning 0)))))
    ;; if not found, set to point-max/point-min
    (unless (zerop count)
      (set-match-data nil)
      (goto-char (if (> dir 0) (point-max) (point-min)))
      (if (/= (point) orig) (setq count (1- count))))
    (* dir count)))

(defun evil-forward-quote (quote &optional count)
  "Move point to the end or beginning of a string.
QUOTE is the character delimiting the string. If COUNT is greater
than zero point is moved forward otherwise it is moved
backwards."
  (let (reset-parser)
    (with-syntax-table (copy-syntax-table (syntax-table))
      (unless (= (char-syntax quote) ?\")
        (modify-syntax-entry quote "\"")
        (setq reset-parser t))
      ;; global parser state is out of state, use local one
      (let* ((pnt (point))
             (state (save-excursion
                      (beginning-of-defun)
                      (parse-partial-sexp (point) pnt nil nil (syntax-ppss))))
             (bnd (bounds-of-evil-string-at-point state)))
        (when (and bnd (< (point) (cdr bnd)))
          ;; currently within a string
          (if (> count 0)
              (progn
                (goto-char (cdr bnd))
                (setq count (1- count)))
            (goto-char (car bnd))
            (setq count (1+ count))))
        ;; forward motions work with local parser state
        (cond
         ((> count 0)
          ;; no need to reset global parser state because we only use
          ;; the local one
          (setq reset-parser nil)
          (catch 'done
            (while (and (> count 0) (not (eobp)))
              (setq state (parse-partial-sexp
                           (point) (point-max) nil nil state 'syntax-table))
              (cond
               ((nth 3 state)
                (setq bnd (bounds-of-thing-at-point 'evil-string))
                (goto-char (cdr bnd))
                (setq count (1- count)))
               ((eobp) (goto-char pnt) (throw 'done nil))))))
         ((< count 0)
          ;; need to update global cache because of backward motion
          (setq reset-parser (and reset-parser (point)))
          (save-excursion
            (beginning-of-defun)
            (syntax-ppss-flush-cache (point)))
          (catch 'done
            (while (and (< count 0) (not (bobp)))
              (setq pnt (point))
              (while (and (not (bobp))
                          (or (eobp) (/= (char-after) quote)))
                (backward-char))
              (cond
               ((setq bnd (bounds-of-thing-at-point 'evil-string))
                (goto-char (car bnd))
                (setq count (1+ count)))
               ((bobp) (goto-char pnt) (throw 'done nil))
               (t (backward-char))))))
         (t (setq reset-parser nil)))))
    (when reset-parser
      ;; reset global cache
      (save-excursion
        (goto-char reset-parser)
        (beginning-of-defun)
        (syntax-ppss-flush-cache (point))))
    count))

;;; Thing-at-point motion functions for Evil text objects and motions
(defun forward-evil-empty-line (&optional count)
  "Move forward COUNT empty lines."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0) (not (eobp)))
      (when (and (bolp) (eolp))
        (setq count (1- count)))
      (forward-line 1)))
   (t
    (while (and (< count 0) (not (bobp))
                (zerop (forward-line -1)))
      (when (and (bolp) (eolp))
        (setq count (1+ count))))))
  count)

(defun forward-evil-space (&optional count)
  "Move forward COUNT whitespace sequences [[:space:]]+."
  (evil-forward-chars "[:space:]" count))

(defun forward-evil-word (&optional count)
  "Move forward COUNT words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative.  Point is placed after the end of the word (if
forward) or at the first character of the word (if backward).  A
word is a sequence of word characters matching
\[[:word:]] (recognized by `forward-word'), a sequence of
non-whitespace non-word characters '[^[:word:]\\n\\r\\t\\f ]', or
an empty line matching ^$."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (let ((word-separating-categories evil-cjk-word-separating-categories)
             (word-combining-categories evil-cjk-word-combining-categories)
             (pnt (point)))
         (forward-word cnt)
         (if (= pnt (point)) cnt 0)))
   #'(lambda (&optional cnt)
       (evil-forward-chars "^[:word:]\n\r\t\f " cnt))
   #'forward-evil-empty-line))

(defun forward-evil-WORD (&optional count)
  "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
  (evil-forward-nearest count
                        #'(lambda (&optional cnt)
                            (evil-forward-chars "^\n\r\t\f " cnt))
                        #'forward-evil-empty-line))

(defun forward-evil-symbol (&optional count)
  "Move forward COUNT symbols.
Moves point COUNT symbols forward or (- COUNT) symbols backward
if COUNT is negative. Point is placed after the end of the
symbol (if forward) or at the first character of the symbol (if
backward). A symbol is either determined by `forward-symbol', or
is a sequence of characters not in the word, symbol or whitespace
syntax classes."
  (evil-forward-nearest
   count
   #'(lambda (&optional cnt)
       (evil-forward-syntax "^w_->" cnt))
   #'(lambda (&optional cnt)
       (let ((pnt (point)))
         (forward-symbol cnt)
         (if (= pnt (point)) cnt 0)))
   #'forward-evil-empty-line))

(defun forward-evil-defun (&optional count)
  "Move forward COUNT defuns.
Moves point COUNT defuns forward or (- COUNT) defuns backward
if COUNT is negative.  A defun is defined by
`beginning-of-defun' and `end-of-defun' functions."
  (evil-motion-loop (dir (or count 1))
    (if (> dir 0) (end-of-defun) (beginning-of-defun))))

(defun forward-evil-sentence (&optional count)
  "Move forward COUNT sentences.
Moves point COUNT sentences forward or (- COUNT) sentences
backward if COUNT is negative.  This function is the same as
`forward-sentence' but returns the number of sentences that could
NOT be moved over."
  (evil-motion-loop (dir (or count 1))
    (ignore-errors (forward-sentence dir))))

(defun forward-evil-paragraph (&optional count)
  "Move forward COUNT paragraphs.
Moves point COUNT paragraphs forward or (- COUNT) paragraphs backward
if COUNT is negative.  A paragraph is defined by
`start-of-paragraph-text' and `forward-paragraph' functions."
  (evil-motion-loop (dir (or count 1))
    (cond
     ((> dir 0) (forward-paragraph))
     ((not (bobp)) (start-of-paragraph-text) (beginning-of-line)))))

(defvar evil-forward-quote-char ?\"
  "The character to be used by `forward-evil-quote'.")

(defun forward-evil-quote (&optional count)
  "Move forward COUNT strings.
The quotation character is specified by the global variable
`evil-forward-quote-char'. This character is passed to
`evil-forward-quote'."
  (evil-forward-quote evil-forward-quote-char count))

(defun forward-evil-quote-simple (&optional count)
  "Move forward COUNT strings.
The quotation character is specified by the global variable
`evil-forward-quote-char'. This functions uses Vim's rules
parsing from the beginning of the current line for quotation
characters. It should only be used when looking for strings
within comments and buffer *must* be narrowed to the comment."
  (let ((dir (if (> (or count 1) 0) 1 -1))
        (ch evil-forward-quote-char)
        (pnt (point))
        (cnt 0))
    (beginning-of-line)
    ;; count number of quotes before pnt
    (while (< (point) pnt)
      (when (= (char-after) ch)
        (setq cnt (1+ cnt)))
      (forward-char))
    (setq cnt (- (* 2 (abs count)) (mod cnt 2)))
    (cond
     ((> dir 0)
      (while (and (not (eolp)) (not (zerop cnt)))
        (when (= (char-after) ch) (setq cnt (1- cnt)))
        (forward-char))
      (when (not (zerop cnt)) (goto-char (point-max))))
     (t
      (while (and (not (bolp)) (not (zerop cnt)))
        (when (= (char-before) ch) (setq cnt (1- cnt)))
        (forward-char -1))
      (when (not (zerop cnt)) (goto-char (point-min)))))
    (/ cnt 2)))

;;; Motion functions
(defun evil-forward-beginning (thing &optional count)
  "Move forward to beginning of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (if (< count 0)
      (forward-thing thing count)
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when (and bnd (< (point) (cdr bnd)))
        (goto-char (cdr bnd)))
      (ignore-errors
        (when (zerop (setq rest (forward-thing thing count)))
          (when (and (bounds-of-thing-at-point thing)
                     (not (bobp))
                     ;; handle final empty line
                     (not (and (bolp) (eobp))))
            (backward-char))
          (beginning-of-thing thing)))
      rest)))

(defun evil-backward-beginning (thing &optional count)
  "Move backward to beginning of THING.
The motion is repeated COUNT times. This is the same as calling
`evil-backward-beginning' with -COUNT."
  (evil-forward-beginning thing (- (or count 1))))

(defun evil-forward-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (if (> count 0)
      (progn (unless (eobp) (forward-char))
             (prog1 (forward-thing thing count)
               (unless (bobp) (backward-char))))
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when (and bnd (< (point) (cdr bnd) ))
        (goto-char (car bnd)))
      (ignore-errors
        (when (zerop (setq rest (forward-thing thing count)))
          (end-of-thing thing)
          (backward-char)))
      rest)))

(defun evil-backward-end (thing &optional count)
  "Move backward to end of THING.
The motion is repeated COUNT times. This is the same as calling
`evil-backward-end' with -COUNT."
  (evil-forward-end thing (- (or count 1))))

(defun evil-forward-word (&optional count)
  "Move by words.
Moves point COUNT words forward or (- COUNT) words backward if
COUNT is negative. This function is the same as `forward-word'
but returns the number of words by which point could *not* be
moved."
  (setq count (or count 1))
  (let* ((dir (if (>= count 0) +1 -1))
         (count (abs count)))
    (while (and (> count 0)
                (forward-word dir))
      (setq count (1- count)))
    count))

(defun evil-in-comment-p (&optional pos)
  "Check if POS is within a comment according to current syntax.
If POS is nil, (point) is used. The return value is the beginning
position of the comment."
  (setq pos (or pos (point)))
  (let ((chkpos
         (cond
          ((eobp) pos)
          ((= (char-syntax (char-after)) ?<) (1+ pos))
          ((and (not (zerop (logand (car (syntax-after (point)))
                                    (ash 1 16))))
                (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                                    (ash 1 17)))))
           (+ pos 2))
          ((and (not (zerop (logand (car (syntax-after (point)))
                                    (ash 1 17))))
                (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                                    (ash 1 16)))))
           (1+ pos))
          (t pos))))
    (let ((syn (save-excursion (syntax-ppss chkpos))))
      (and (nth 4 syn) (nth 8 syn)))))

(defun evil-looking-at-start-comment (&optional move)
  "Return t if point is at the start of a comment.
Point must be on one of the opening characters of a block comment
according to the current syntax table. Futhermore, these
characters must been parsed as opening characters, i.e. they
won't be considered as comment starters inside a string or
possibly another comment. Point is moved to the first character
of the comment opener if MOVE is non-nil."
  (cond
   ((= (point) (point-max)) nil)
   ;; one character opener
   ((= (char-syntax (char-after)) ?<)
    (equal (point) (evil-in-comment-p (1+ (point)))))
   ;; two character opener on first char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (ash 1 16))))
         (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                             (ash 1 17)))))
    (equal (point) (evil-in-comment-p (+ 2 (point)))))
   ;; two character opener on second char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (ash 1 17))))
         (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                             (ash 1 16)))))
    (and (equal (1- (point)) (evil-in-comment-p (1+ (point))))
         (prog1 t (when move (backward-char)))))))

(defun evil-looking-at-end-comment (&optional move)
  "Return t if point is at the end of a comment.
Point must be on one of the opening characters of a block comment
according to the current syntax table. Futhermore, these
characters must been parsed as opening characters, i.e. they
won't be considered as comment starters inside a string or
possibly another comment. Point is moved right after the comment
closer if MOVE is non-nil."
  (cond
   ;; one char closer
   ((= (char-syntax (char-after)) ?>)
    (and (evil-in-comment-p) ; in comment
         (not (evil-in-comment-p (1+ (point))))
         (prog1 t (when move (forward-char)))))
   ;; two char closer on first char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (ash 1 18))))
         (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                             (ash 1 19)))))
    (and (evil-in-comment-p)
         (not (evil-in-comment-p (+ (point) 2)))
         (prog1 t (when move (forward-char 2)))))
   ;; two char closer on second char
   ((and (not (zerop (logand (car (syntax-after (point)))
                             (ash 1 19))))
         (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                             (ash 1 18)))))
    (and (evil-in-comment-p)
         (not (evil-in-comment-p (1+ (point))))
         (prog1 t (when move (forward-char)))))))

(defun evil-insert-newline-above ()
  "Insert a new line above point and place point in that line
with regard to indentation."
  (evil-narrow-to-field
    (evil-move-beginning-of-line)
    (insert (if use-hard-newlines hard-newline "\n"))
    (forward-line -1)
    (back-to-indentation)))

(defun evil-insert-newline-below ()
  "Insert a new line below point and place point in that line
with regard to indentation."
  (evil-narrow-to-field
    (evil-move-end-of-line)
    (insert (if use-hard-newlines hard-newline "\n"))
    (back-to-indentation)))

;;; Markers

(defun evil-global-marker-p (char)
  "Whether CHAR denotes a global marker."
  (or (and (>= char ?A) (<= char ?Z))
      (assq char (default-value 'evil-markers-alist))))

(defun evil-set-marker (char &optional pos advance)
  "Set the marker denoted by CHAR to position POS.
POS defaults to the current position of point.
If ADVANCE is t, the marker advances when inserting text at it;
otherwise, it stays behind."
  (interactive (list (read-char)))
  (catch 'done
    (let ((marker (evil-get-marker char t)) alist)
      (unless (markerp marker)
        (cond
         ((eq marker 'evil-visual-beginning)
          (setq marker evil-visual-mark))
         ((eq marker 'evil-visual-goto-end)
          (setq marker evil-visual-point))
         ((and marker (symbolp marker) (boundp marker))
          (set marker (or (symbol-value marker) (make-marker)))
          (setq marker (symbol-value marker)))
         ((eq marker 'evil-jump-backward-swap)
          (evil-set-jump)
          (throw 'done nil))
         ((functionp marker)
          (user-error "Cannot set special marker `%c'" char))
         ((evil-global-marker-p char)
          (setq alist (default-value 'evil-markers-alist)
                marker (make-marker))
          (evil--add-to-alist alist char marker)
          (setq-default evil-markers-alist alist))
         (t
          (setq marker (make-marker))
          (evil--add-to-alist evil-markers-alist char marker))))
      (add-hook 'kill-buffer-hook #'evil-swap-out-markers nil t)
      (set-marker-insertion-type marker advance)
      (set-marker marker (or pos (point))))))
(evil-set-command-properties #'evil-set-marker :suppress-operator t)

(defun evil-get-marker (char &optional raw)
  "Return the marker denoted by CHAR.
This is either a marker object as returned by `make-marker',
a number, a cons cell (FILE . POS) with FILE being a string
and POS a number, or nil. If RAW is non-nil, then the
return value may also be a variable, a movement function,
or a marker object pointing nowhere."
  (let ((marker (cdr (assq char (if (evil-global-marker-p char)
                                    (default-value 'evil-markers-alist)
                                  evil-markers-alist)))))
    (if raw
        marker
      (when (and (symbolp marker) (boundp marker))
        (setq marker (symbol-value marker)))
      (when (functionp marker)
        (save-excursion
          (save-window-excursion
            (funcall marker)
            (setq marker (move-marker (make-marker) (point))))))
      (if (markerp marker)
          (if (eq (marker-buffer marker) (current-buffer))
              (marker-position marker)
            (when (marker-buffer marker) marker))
        marker))))

(defun evil-swap-out-markers ()
  "Turn markers into file references when the buffer is killed."
  (when buffer-file-name
    (dolist (entry evil-markers-alist)
      (and (markerp (cdr entry))
           (eq (marker-buffer (cdr entry)) (current-buffer))
           (setcdr entry (cons buffer-file-name
                               (marker-position (cdr entry))))))))
(put 'evil-swap-out-markers 'permanent-local-hook t)

(defvar calc-multiplication-has-precedence)
(defun evil--eval-expr (input)
  "Eval INPUT and return stringified result, if of a suitable type.
If INPUT starts with a number, +, -, or . use `calc-eval' instead."
  (let* ((calcable-p (string-match-p "\\`[[:space:]]*[0-9+.-]" input))
         (result (if calcable-p
                     (let ((calc-multiplication-has-precedence nil))
                       (calc-eval input))
                   (eval (car (read-from-string input)) t))))
    (cond
     ((stringp result) result)
     ((or (numberp result) (symbolp result))
      (format "%s" result))
     ((sequencep result)
      (mapconcat (lambda (x) (format "%s" x)) result "\n"))
     (t (user-error "Using `%s' as a string" (type-of result))))))

(defvar evil-paste-clear-minibuffer-first nil
  "`evil-paste-before' cannot have `delete-minibuffer-contents' called before
it fetches certain registers becuase this would trigger various ex-updates,
sometimes moving point, so `C-a' `C-w' etc. would miss their intended target.")

(defun evil-ex-remove-default ()
  "Remove the default text shown in the ex minibuffer.
When Ex starts, the previous command is shown enclosed in
parenthesis. This function removes this text when the first key
is pressed."
  (when (and (not (eq this-command 'exit-minibuffer))
             (/= (minibuffer-prompt-end) (point-max)))
    (when (eq this-command 'evil-ex-delete-backward-char)
      (setq this-command 'ignore))
    (if (eq this-original-command 'evil-paste-from-register)
        (setq evil-paste-clear-minibuffer-first t)
      (delete-minibuffer-contents)))
  (remove-hook 'pre-command-hook #'evil-ex-remove-default t))

(defun evil-get-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil.

The following special registers are supported.
  \"  the unnamed register
  *  the clipboard contents
  +  the clipboard contents
  <C-w> the word at point (ex mode only)
  <C-a> the WORD at point (ex mode only)
  <C-o> the symbol at point (ex mode only)
  <C-f> the current file at point (ex mode only)
  %  the current file name (read only)
  #  the alternate file name (read only)
  /  the last search pattern (read only)
  :  the last command line (read only)
  .  the last inserted text (read only)
  -  the last small (less than a line) delete
  _  the black hole register
  =  the expression register (read only)"
  (unless (characterp register) (error "Invalid register `%S'" register))
  (condition-case err
      (or (cond
           ((eq register ?\")
            (current-kill 0))
           ((<= ?1 register ?9)
            (let ((reg (- register ?1)))
              (and (< reg (length kill-ring))
                   (current-kill reg t))))
           ((memq register '(?* ?+))
            (let ((what (if (eq register ?*) 'PRIMARY 'CLIPBOARD)))
              (if (eval-when-compile (>= emacs-major-version 29))
                  (gui--selection-value-internal what)
                ;; The following code is based on `x-selection-value-internal'
                ;; (now `gui--selection-value-internal') circa Emacs 24. We're
                ;; unsure why exactly it's duplicated here, and it's possible
                ;; it needn't be for newer versions of Emacs.
                (let ((request-type (or (bound-and-true-p x-select-request-type)
                                        '(UTF8_STRING COMPOUND_TEXT STRING)))
                      text)
                  (unless (consp request-type)
                    (setq request-type (list request-type)))
                  (while (and request-type (not text))
                    (setq text (ignore-errors
                                 (evil-get-selection what (pop request-type)))))
                  (when text
                    (remove-text-properties 0 (length text) '(foreign-selection nil) text))
                  text))))
           ((eq register ?\C-W)
            (with-current-buffer
                (or evil-ex-original-buffer
                    (let ((w (minibuffer-selected-window))) (and w (window-buffer w)))
                    (user-error "Register <C-w> is only available in Ex state"))
              (thing-at-point 'evil-word)))
           ((eq register ?\C-A)
            (with-current-buffer
                (or evil-ex-original-buffer
                    (let ((w (minibuffer-selected-window))) (and w (window-buffer w)))
                    (user-error "Register <C-a> is only available in Ex state"))
              (thing-at-point 'evil-WORD)))
           ((eq register ?\C-O)
            (with-current-buffer
                (or evil-ex-original-buffer
                    (let ((w (minibuffer-selected-window))) (and w (window-buffer w)))
                    (user-error "Register <C-o> is only available in Ex state"))
              (thing-at-point 'evil-symbol)))
           ((eq register ?\C-F)
            (with-current-buffer
                (or evil-ex-original-buffer
                    (let ((w (minibuffer-selected-window))) (and w (window-buffer w)))
                    (user-error "Register <C-f> is only available in Ex state"))
              (thing-at-point 'filename)))
           ((eq register ?\C-L)
            (with-current-buffer
                (or evil-ex-original-buffer
                    (let ((w (minibuffer-selected-window))) (and w (window-buffer w)))
                    (user-error "Register <C-l> is only available in Ex state"))
              (replace-regexp-in-string "\n\\'" "" (thing-at-point 'line))))
           ((eq register ?%)
            (or (buffer-file-name
                 (or evil-ex-original-buffer
                     (let ((w (minibuffer-selected-window))) (and w (window-buffer w)))))
                (user-error "No file name")))
           ((= register ?#)
            (or (with-current-buffer (other-buffer) (buffer-file-name))
                (user-error "No file name")))
           ((eq register ?/)
            (defvar evil-search-module)
            (or (car (cond
                      ((eq evil-search-module 'evil-search) evil-ex-search-history)
                      (isearch-regexp regexp-search-ring)
                      (t search-ring)))
                (user-error "No previous regular expression")))
           ((eq register ?:)
            (or (car evil-ex-history) (user-error "No previous command line")))
           ((eq register ?.) evil-last-insertion)
           ((eq register ?-) evil-last-small-deletion)
           ((eq register ?=)
            (let ((enable-recursive-minibuffers t))
              (setq evil-last-=-register-input
                    (minibuffer-with-setup-hook
                        (lambda ()
                          (when evil-last-=-register-input
                            (add-hook 'pre-command-hook #'evil-ex-remove-default nil t)))
                      (read-from-minibuffer
                       "="
                       (and evil-last-=-register-input
                            (propertize evil-last-=-register-input 'face 'shadow))
                       evil-eval-map
                       nil
                       'evil-eval-history
                       evil-last-=-register-input
                       t)))
              (evil--eval-expr evil-last-=-register-input)))
           ((eq register ?_) "") ; the black hole register
           (t (setq register (downcase register))
              (get-register register)))
          (user-error "Register `%c' is empty" register))
    (error (unless noerror (signal (car err) (cdr err))))))

(defun evil-append-register (register text)
  "Append TEXT to the contents of REGISTER."
  (let ((content (get-register register)))
    (set-register
     register
     (cond
      ((not content) text)
      ;; if the register does not contain a string treat it as a vector
      ((not (stringp content)) (vconcat content text))
      ;; some non-trivial yank-handler -> always switch to line handler
      ((or (text-property-not-all
            0 (length content) 'yank-handler nil content)
           (text-property-not-all
            0 (length text) 'yank-handler nil text))
       ;; ensure complete lines
       (setq text
             (concat
              content
              (and (> (length content) 0)
                   (/= (aref content (1- (length content))) ?\n)
                   "\n")
              text
              (and (> (length text) 0)
                   (/= (aref text (1- (length text))) ?\n)
                   "\n")))
       (put-text-property 0 (length text) 'yank-handler '(evil-yank-line-handler)
                          text)
       text)
      (t (concat content text))))))

(defun evil-set-register (register text)
  "Set the contents of REGISTER to TEXT.
If REGISTER is an upper case character then TEXT is appended to that
register instead of replacing its content."
  (cond
   ((not (characterp register)) (user-error "Invalid register"))
   ;; don't allow modification of read-only registers
   ((member register '(?: ?. ?%))
    (user-error "Can't modify read-only register"))
   ((eq register ?\") (kill-new text))
   ((<= ?1 register ?9)
    (if (null kill-ring)
        (kill-new text)
      (let ((kill-ring-yank-pointer kill-ring-yank-pointer)
            interprogram-paste-function
            interprogram-cut-function)
        (current-kill (- register ?1))
        (setcar kill-ring-yank-pointer text))))
   ((eq register ?*) (evil-set-selection 'PRIMARY text))
   ((eq register ?+) (evil-set-selection 'CLIPBOARD text))
   ((eq register ?-) (setq evil-last-small-deletion text))
   ((eq register ?_) nil) ; the black hole register
   ((and (<= ?A register) (<= register ?Z))
    (evil-append-register (downcase register) text))
   (t (set-register register text))))

(defun evil-register-list ()
  "Return an alist of all registers, but only those named
with number or character. Registers with symbol or string in names are ignored
to keep Vim compatibility with register jumps."
  (sort (append (mapcar (lambda (reg) (cons reg (evil-get-register reg t)))
                        '(?\" ?* ?+ ?% ?# ?/ ?: ?. ?-
                              ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
                (list (cons ?= evil-last-=-register-input))
                (cl-remove-if-not (lambda (reg) (number-or-marker-p (car reg))) register-alist)
                nil)
        #'car-less-than-car))

(defsubst evil-kbd-macro-suppress-motion-error ()
  "Return non-nil if a motion error should be suppressed.
Whether the motion error should be suppressed depends on the
variable `evil-kbd-macro-suppress-motion-error'."
  (or (and defining-kbd-macro
           (memq evil-kbd-macro-suppress-motion-error '(t record)))
      (and executing-kbd-macro
           (memq evil-kbd-macro-suppress-motion-error '(t replay)))))

;;; Region

;; `set-mark' does too much at once
(defun evil-move-mark (pos)
  "Set buffer's mark to POS.
If POS is nil, delete the mark."
  (set-marker (mark-marker) pos))

(defun evil-save-transient-mark-mode ()
  "Save Transient Mark mode and make it buffer-local.
Any changes to Transient Mark mode are now local to the current
buffer, until `evil-restore-transient-mark-mode' is called.

Variables pertaining to Transient Mark mode are listed in
`evil-transient-vars', and their values are stored in
`evil-transient-vals'."
  (dolist (var evil-transient-vars)
    (when (and (boundp var)
               (not (assq var evil-transient-vals)))
      (push (list var (symbol-value var)
                  (local-variable-p var))
            evil-transient-vals)
      (make-variable-buffer-local var)
      (put var 'permanent-local t))))

(defun evil-restore-transient-mark-mode ()
  "Restore Transient Mark mode.
This presupposes that `evil-save-transient-mark-mode' has been
called earlier. If Transient Mark mode was disabled before but
enabled in the meantime, this function disables it; if it was
enabled before but disabled in the meantime, this function
enables it.

The earlier settings of Transient Mark mode are stored in
`evil-transient-vals'."
  (let (entry local var val)
    (while (setq entry (pop evil-transient-vals))
      (setq var (pop entry)
            val (pop entry)
            local (pop entry))
      (unless local
        (kill-local-variable var))
      (unless (equal (symbol-value var) val)
        (if (fboundp var)
            (funcall var (if val 1 -1))
          (setq var val))))))

;; In theory, an active region implies Transient Mark mode, and
;; disabling Transient Mark mode implies deactivating the region.
;; In practice, Emacs never clears `mark-active' except in Transient
;; Mark mode, so we define our own toggle functions to make things
;; more predictable.
(defun evil-transient-mark (&optional arg)
  "Toggle Transient Mark mode.
Ensure that the region is properly deactivated.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if transient-mark-mode -1 1)))
  (cond
   ((< arg 1)
    (evil-active-region -1)
    ;; Transient Mark mode cannot be disabled
    ;; while CUA mode is enabled
    (when (fboundp 'cua-mode)
      (cua-mode -1))
    (when transient-mark-mode
      (transient-mark-mode -1)))
   (t
    (unless transient-mark-mode
      (evil-active-region -1)
      (transient-mark-mode 1)))))

(defun evil-active-region (&optional arg)
  "Toggle active region.
Ensure that Transient Mark mode is properly enabled.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if (region-active-p) -1 1)))
  (cond
   ((and (< arg 1))
    (when (or transient-mark-mode mark-active)
      (setq mark-active nil
            deactivate-mark nil)
      (when (boundp 'cua--explicit-region-start)
        (setq cua--explicit-region-start nil))
      (run-hooks 'deactivate-mark-hook)))
   (t
    (evil-transient-mark 1)
    (when deactivate-mark
      (setq deactivate-mark nil))
    (unless (mark t)
      (evil-move-mark (point)))
    (unless (region-active-p)
      (set-mark (mark t)))
    (when (boundp 'cua--explicit-region-start)
      (setq cua--explicit-region-start t)))))

(defmacro evil-with-transient-mark-mode (&rest body)
  "Execute BODY with Transient Mark mode.
Then restore Transient Mark mode to its previous setting."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         evil-transient-vals)
     (unwind-protect
         (progn
           (evil-save-transient-mark-mode)
           (evil-transient-mark 1)
           ,@body)
       (evil-restore-transient-mark-mode))))

(defmacro evil-with-active-region (beg end &rest body)
  "Execute BODY with an active region from BEG to END."
  (declare (indent 2)
           (debug t))
  `(let ((beg ,beg) (end ,end)
         evil-transient-vals)
     (evil-with-transient-mark-mode
       (save-excursion
         (evil-active-region 1)
         (evil-move-mark beg)
         (goto-char end)
         ,@body))))

(defun evil-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun evil-apply-on-block (func beg end pass-columns &rest args)
  "Call FUNC for each line of a block selection.
The selection is specified by the region BEG and END.  FUNC must
take at least two arguments, the beginning and end of each
line.  If PASS-COLUMNS is non-nil, these values are the columns,
otherwise they are buffer positions.  Extra arguments to FUNC may
be passed via ARGS."
  (let (startcol startpt endcol endpt)
    (save-excursion
      (goto-char beg)
      (setq startcol (current-column)
            startpt (line-beginning-position))
      (goto-char end)
      (setq endcol (current-column))
      (forward-line 1)
      (setq endpt (point-marker))
      ;; ensure the start column is the left one.
      (evil-sort startcol endcol)
      ;; maybe extend up to EOL
      (when (and (memq last-command '(next-line previous-line))
                 (eq temporary-goal-column most-positive-fixnum))
        (goto-char startpt)
        (while (< (point) endpt)
          (setq endcol (max endcol (evil-column (line-end-position))))
          (forward-line 1)))
      ;; start looping over lines
      (goto-char startpt)
      (while (< (point) endpt)
        (if pass-columns
            (apply func startcol endcol args)
          (apply func
                 (save-excursion (evil-move-to-column startcol))
                 (save-excursion (evil-move-to-column endcol t))
                 args))
        (forward-line 1)))))

(defun evil-apply-on-rectangle (function start end &rest args)
  "Like `apply-on-rectangle' but maybe extends to eol.
If `temporary-goal-column' is set to a big number, then the
region of each line is extended to the end of each line. The end
column is set to the maximal column in all covered lines."
  (apply #'evil-apply-on-block function start end t args))

;;; Insertion

(defun evil-track-last-insertion (beg end old-len)
  "Track the last insertion range and its text.
BEG, END and OLD-LEN are as for `after-change-functions'.
The insertion range is stored as a pair of buffer positions in
`evil-current-insertion'."
  ;; For deletions left of, or insertions right of the end of the last
  ;; insertion (with any changes in between), extend or contract the
  ;; insertion range as appropriate. Otherwise, start a new range.
  (if (when evil-current-insertion
        (let ((ins-beg (car evil-current-insertion))
              (ins-end (cdr evil-current-insertion)))
          (and (<= ins-beg beg)
               (or (= (+ beg old-len) ins-end)
                   (when (evil-replace-state-p) (= beg ins-end))))))
      ;; Ignore insertion of previous char from Backspace in Replace state
      (unless (and (eq this-command 'evil-replace-backspace) (= old-len 0))
        (setcdr evil-current-insertion end))
    (setq evil-current-insertion (cons beg end))))
(put 'evil-track-last-insertion 'permanent-local-hook t)

(defun evil-start-track-last-insertion ()
  "Start tracking the last insertion."
  (setq evil-current-insertion nil)
  (add-hook 'after-change-functions #'evil-track-last-insertion nil t))

(defun evil-stop-track-last-insertion ()
  "Stop tracking the last insertion.
The tracked insertion is set to `evil-last-insertion'."
  (setq evil-last-insertion
        (and evil-current-insertion
             ;; Check whether the insertion range is a valid buffer
             ;; range.  If a buffer modification is done from within
             ;; another change hook or modification-hook (yasnippet
             ;; does this using overlay modification-hooks), then the
             ;; insertion information may be invalid. There is no way
             ;; to detect this situation, but at least we should
             ;; ensure that no error occurs (see bug #272).
             (>= (car evil-current-insertion) (point-min))
             (<= (cdr evil-current-insertion) (point-max))
             (buffer-substring-no-properties (car evil-current-insertion)
                                             (cdr evil-current-insertion))))
  (remove-hook 'after-change-functions #'evil-track-last-insertion t))

;;; Paste

(defun evil-yank-characters (beg end &optional register yank-handler)
  "Save the characters defined by the region BEG and END in the kill-ring."
  (let ((text (filter-buffer-substring beg end)))
    (when yank-handler
      (put-text-property 0 (length text) 'yank-handler (list yank-handler) text))
    (when register
      (evil-set-register register text))
    (when evil-was-yanked-without-register
      (evil-set-register ?0 text)) ; "0 register contains last yanked text
    (unless (eq register ?_)
      (kill-new text))))

(defun evil-yank-lines (beg end &optional register yank-handler)
  "Save the lines in the region BEG and END into the kill-ring."
  (let ((text (filter-buffer-substring beg end))
        (yank-handler (list (or yank-handler #'evil-yank-line-handler)
                            nil
                            t)))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (put-text-property 0 (length text) 'yank-handler yank-handler text)
    (when register
      (evil-set-register register text))
    (when evil-was-yanked-without-register
      (evil-set-register ?0 text)) ; "0 register contains last yanked text
    (unless (eq register ?_)
      (kill-new text))))

(defun evil-yank-rectangle (beg end &optional register yank-handler)
  "Save the rectangle defined by region BEG and END into the kill-ring."
  (let ((lines (list nil)))
    (evil-apply-on-rectangle #'extract-rectangle-line beg end lines)
    ;; We remove spaces from the beginning and the end of the next.
    ;; Spaces are inserted explicitly in the yank-handler in order to
    ;; NOT insert lines full of spaces.
    (setq lines (nreverse (cdr lines)))
    ;; `text' is used as default insert text when pasting this rectangle
    ;; in another program, e.g., using the X clipboard.
    (let ((yank-handler (list (or yank-handler #'evil-yank-block-handler)
                              lines
                              t
                              #'evil-delete-yanked-rectangle))
          (text (mapconcat #'identity lines "\n")))
      (put-text-property 0 (length text) 'yank-handler yank-handler text)
      (when register
        (evil-set-register register text))
      (when evil-was-yanked-without-register
        (evil-set-register ?0 text)) ; "0 register contains last yanked text
      (unless (eq register ?_)
        (kill-new text))
      text)))

(defun evil-remove-yank-excluded-properties (text)
  "Remove `yank-excluded-properties' from TEXT."
  (if (eq yank-excluded-properties t)
      (set-text-properties 0 (length text) nil text)
    (remove-list-of-text-properties 0 (length text)
                                    yank-excluded-properties text)))

(defun evil-yank-line-handler (text)
  "Insert the current text linewise."
  (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
        (opoint (point)))
    (evil-remove-yank-excluded-properties text)
    (cond
     ((eq this-command 'evil-paste-before)
      (evil-move-beginning-of-line)
      (let ((beg (point)))
        (insert text)
        (setq evil-last-paste
              (list 'evil-paste-before evil-paste-count opoint beg (point)))
        (evil-set-marker ?\[ beg)
        (evil-set-marker ?\] (1- (point)))
        (goto-char beg))
      (back-to-indentation))
     ((eq this-command 'evil-paste-after)
      (evil-move-end-of-line)
      (let ((beg (point)))
        (insert "\n")
        (insert text)
        (delete-char -1) ; delete the last newline
        (setq evil-last-paste
              (list 'evil-paste-after evil-paste-count opoint beg (point)))
        (evil-set-marker ?\[ (1+ beg))
        (evil-set-marker ?\] (point))
        (unless evil--cursor-after
          (goto-char (1+ beg))))
      (back-to-indentation))
     (t (insert text)))))

(defun evil-yank-block-handler (lines)
  "Insert the current text as block."
  (let ((count (or evil-paste-count 1))
        (col (if (eq this-command 'evil-paste-after)
                 (1+ (current-column))
               (current-column)))
        (opoint (point))
        (first t))
    (dolist (line lines)
      ;; maybe we have to insert a new line at eob
      (if first
          (setq first nil)
        (when (or (> (forward-line 1) 0)
                  (and (eobp) (not (bolp))))
          (insert "\n")))
      ;; concat multiple copies according to count
      (setq line (apply #'concat (make-list count line)))
      ;; trim whitespace at beginning and end
      (string-match "\\` *\\(.*?\\) *\\'" line)
      (let ((text (match-string 1 line))
            (begextra (match-beginning 1))
            (endextra (- (match-end 0) (match-end 1))))
        ;; insert text unless we insert an empty line behind eol
        (unless (and (< (evil-column (line-end-position)) col)
                     (string= text ""))
          ;; if we paste behind eol, it may be sufficient to insert tabs
          (if (< (evil-column (line-end-position)) col)
              (move-to-column (+ col begextra) t)
            (move-to-column col t)
            (insert (make-string begextra ?\s)))
          (evil-remove-yank-excluded-properties text)
          (insert text)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string endextra ?\s))))))
    (setq evil-last-paste
          (list this-command
                evil-paste-count
                opoint
                (length lines)                   ; number of rows
                (* count (length (car lines))))) ; number of columns
    (evil-set-marker ?\[ opoint)
    (evil-set-marker ?\] (1- (point)))
    (if evil--cursor-after
        (backward-char)
      (goto-char opoint)
      (when (and (eq this-command 'evil-paste-after)
                 (not (eolp)))
        (forward-char)))))

(defun evil-delete-yanked-rectangle (nrows ncols)
  "Special function to delete the block yanked by a previous paste command.
Supplied as the `undo' element of a yank handler."
  (let ((opoint (point))
        (col (if (eq last-command 'evil-paste-after)
                 (1+ (current-column))
               (current-column))))
    (dotimes (_ nrows)
      (delete-region (save-excursion
                       (move-to-column col)
                       (point))
                     (save-excursion
                       (move-to-column (+ col ncols))
                       (point)))
      (unless (eobp) (forward-line)))
    (goto-char opoint)))

;; TODO: if undoing is disabled in the current buffer, paste-pop won't
;; work. Although this is probably not a big problem, because usually
;; buffers where `evil-paste-pop' may be useful have undoing enabled.
;; A solution would be to temporarily enable undo when pasting and
;; store the undo information in a special variable that does not
;; interfere with `buffer-undo-list'.
(defun evil-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
This command is allowed only immediatly after a `yank',
`evil-paste-before', `evil-paste-after' or `evil-paste-pop'.
This command uses the same paste command as before, i.e., when
used after `evil-paste-after' the new text is also yanked using
`evil-paste-after', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(evil-paste-after
                  evil-paste-before
                  evil-visual-paste))
    (user-error "Previous command was not an evil-paste: %s" last-command))
  (unless evil-last-paste
    (user-error "Previous paste command used a register"))
  (evil-undo-pop)
  (goto-char (nth 2 evil-last-paste))
  (setq this-command (nth 0 evil-last-paste))
  ;; use temporary kill-ring, so the paste cannot modify it
  (let ((kill-ring (list (current-kill
                          (if (and (> count 0) (nth 5 evil-last-paste))
                              ;; if was visual paste then skip the
                              ;; text that has been replaced
                              (1+ count)
                            count))))
        (kill-ring-yank-pointer kill-ring))
    (when (eq last-command 'evil-visual-paste)
      (let ((evil-no-display t))
        (evil-visual-restore)))
    (funcall (nth 0 evil-last-paste) (nth 1 evil-last-paste))
    ;; if this was a visual paste, then mark the last paste as NOT
    ;; being the first visual paste
    (when (eq last-command 'evil-visual-paste)
      (setcdr (nthcdr 4 evil-last-paste) nil))))

(defun evil-paste-pop-next (count)
  "Same as `evil-paste-pop' but with negative argument."
  (interactive "p")
  (evil-paste-pop (- count)))

;;; Interactive forms

(defun evil-match-interactive-code (interactive &optional pos)
  "Match an interactive code at position POS in string INTERACTIVE.
Return the first matching entry in `evil-interactive-alist', or nil."
  (unless pos (setq pos 0))
  (cl-loop
   with len = (length interactive) for entry in evil-interactive-alist thereis
   (let* ((string (car entry))
          (end (+ pos (length string))))
     (and (<= end len)
          (eq (compare-strings string nil nil interactive pos end) t)
          entry))))

(defun evil-concatenate-interactive-forms (&rest forms)
  "Concatenate interactive list expressions FORMS.
Return a single expression where successive expressions
are joined, if possible."
  (let (result)
    (when forms
      (while (cdr forms)
        (cond
         ((null (car forms))
          (pop forms))
         ((and (eq (car (car forms)) 'list)
               (eq (car (cadr forms)) 'list))
          (setq forms (cons (append (car forms)
                                    (cdr (cadr forms)))
                            (cdr (cdr forms)))))
         (t (push (pop forms) result))))
      (when (car forms)
        (push (pop forms) result))
      (setq result (nreverse result))
      (cond
       ((null result))
       ((null (cdr result)) (car result))
       (t `(append ,@result))))))

(defun evil-interactive-string (string)
  "Evaluate the interactive string STRING.
The string may contain extended interactive syntax.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `evil-define-command'."
  (let ((len (length string))
        (pos 0)
        forms properties)
    (while (< pos len)
      (if (eq (aref string pos) ?\n)
          (setq pos (1+ pos))
        (cl-destructuring-bind (code expr . plist)
            (or (evil-match-interactive-code string pos)
                (user-error "Unknown interactive code: `%s'"
                            (substring string pos)))
          (setq pos (+ pos (length code)))
          (when (functionp expr)
            (let ((prompt (substring
                           string pos
                           (or (string-match-p "\n" string pos) len))))
              (setq pos (+ pos (length prompt))
                    expr `(funcall ,expr ,prompt))))
          (setq forms (append forms (list expr))
                properties (append properties plist)))))
    (cons `(append ,@forms) properties)))

(defun evil-interactive-form (&rest args)
  "Evaluate interactive forms ARGS.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `evil-define-command'."
  (let (forms properties)
    (dolist (arg args)
      (if (not (stringp arg))
          (setq forms (append forms (list arg)))
        (setq arg (evil-interactive-string arg)
              forms (append forms (cdr (car arg)))
              properties (append properties (cdr arg)))))
    (cons (apply #'evil-concatenate-interactive-forms forms)
          properties)))

;;; Types

(defun evil-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (let (type)
    (cond
     ((overlayp object)
      (setq type (overlay-get object :type)))
     ((evil-range-p object)
      (setq type (nth 2 object)))
     ((listp object)
      (setq type (plist-get object :type)))
     ((commandp object)
      (setq type (evil-get-command-property object :type)))
     ((symbolp object)
      (setq type (get object 'type))))
    (setq type (or type default))
    (and (evil-type-p type) type)))

(defun evil-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (evil-set-type \\='next-line \\='line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((evil-range-p object)
    (evil-set-range-type object type))
   ((listp object)
    (plist-put object :type type))
   ((commandp object)
    (evil-set-command-property object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun evil-type-property (type prop)
  "Return property PROP for TYPE."
  (plist-get (cdr (assq type evil-type-properties)) prop))

(defun evil-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym evil-type-properties))

(defun evil-expand (beg end type &rest properties)
  "Expand BEG and END as TYPE with PROPERTIES.
Return a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'evil-transform
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) :expand)
         beg end type properties))

(defun evil-contract (beg end type &rest properties)
  "Contract BEG and END as TYPE with PROPERTIES.
Return a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'evil-transform :contract beg end type properties))

(defun evil-normalize (beg end type &rest properties)
  "Normalize BEG and END as TYPE with PROPERTIES.
Return a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply #'evil-transform :normalize beg end type properties))

(defun evil-transform (transform beg end type &rest properties)
  "Apply TRANSFORM on BEG and END with PROPERTIES.
Return a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged."
  (let* ((type (or type (evil-type properties)))
         (transform (when (and type transform)
                      (evil-type-property type transform))))
    (if transform
        (apply transform beg end properties)
      (apply #'evil-range beg end type properties))))

(defun evil-describe (beg end type &rest properties)
  "Return description of BEG and END with PROPERTIES.
If no description is available, return the empty string."
  (let* ((type (or type (evil-type properties)))
         (properties (plist-put properties :type type))
         (describe (evil-type-property type :string)))
    (or (when describe
          (apply describe beg end properties))
        "")))

;;; Ranges

(defun evil-range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
BEG and END are buffer positions (numbers or markers),
TYPE is a type as per `evil-type-p', and PROPERTIES is
a property list."
  (let ((beg (evil-normalize-position beg))
        (end (evil-normalize-position end)))
    (when (and (numberp beg) (numberp end))
      (evil-sort beg end)
      (nconc (list beg end)
             (when (evil-type-p type) (list type))
             properties))))

(defun evil-range-p (object)
  "Whether OBJECT is a range."
  (and (listp object)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun evil-range-beginning (range)
  "Return beginning of RANGE."
  (when (evil-range-p range)
    (let ((beg (evil-normalize-position (nth 0 range)))
          (end (evil-normalize-position (nth 1 range))))
      (min beg end))))

(defun evil-range-end (range)
  "Return end of RANGE."
  (when (evil-range-p range)
    (let ((beg (evil-normalize-position (nth 0 range)))
          (end (evil-normalize-position (nth 1 range))))
      (max beg end))))

(defun evil-range-properties (range)
  "Return properties of RANGE."
  (when (evil-range-p range)
    (nthcdr (if (evil-type range) 3 2) range)))

(defalias 'evil-copy-range #'copy-sequence
  "Return a copy of RANGE.")

(defun evil-set-range (range &optional beg end type &rest properties)
  "Set RANGE to have beginning BEG and end END.
The TYPE and additional PROPERTIES may also be specified.
If an argument is nil, it's not used; the previous value is retained.
See also `evil-set-range-beginning', `evil-set-range-end',
`evil-set-range-type' and `evil-set-range-properties'."
  (when (evil-range-p range)
    (let ((beg (or (evil-normalize-position beg)
                   (evil-range-beginning range)))
          (end (or (evil-normalize-position end)
                   (evil-range-end range)))
          (type (or type (evil-type range)))
          (plist (evil-range-properties range)))
      (evil-sort beg end)
      (setq plist (evil-concat-plists plist properties))
      (evil-set-range-beginning range beg)
      (evil-set-range-end range end)
      (evil-set-range-type range type)
      (evil-set-range-properties range plist)
      range)))

(defun evil-set-range-beginning (range beg &optional copy)
  "Set RANGE's beginning to BEG.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcar range beg)
  range)

(defun evil-set-range-end (range end &optional copy)
  "Set RANGE's end to END.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcar (cdr range) end)
  range)

(defun evil-set-range-type (range type &optional copy)
  "Set RANGE's type to TYPE.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (if type
      (setcdr (cdr range)
              (cons type (evil-range-properties range)))
    (setcdr (cdr range) (evil-range-properties range)))
  range)

(defun evil-set-range-properties (range properties &optional copy)
  "Set RANGE's properties to PROPERTIES.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcdr (if (evil-type range) (cddr range) (cdr range)) properties)
  range)

(defun evil-range-union (range1 range2 &optional type)
  "Return the union of the ranges RANGE1 and RANGE2.
If the ranges have conflicting types, use RANGE1's type.
This can be overridden with TYPE."
  (when (and (evil-range-p range1)
             (evil-range-p range2))
    (evil-range (min (evil-range-beginning range1)
                     (evil-range-beginning range2))
                (max (evil-range-end range1)
                     (evil-range-end range2))
                (or type
                    (evil-type range1)
                    (evil-type range2)))))

(defun evil-subrange-p (range1 range2)
  "Whether RANGE1 is contained within RANGE2."
  (and (evil-range-p range1)
       (evil-range-p range2)
       (<= (evil-range-beginning range2)
           (evil-range-beginning range1))
       (>= (evil-range-end range2)
           (evil-range-end range1))))

(defun evil-select-inner-object (thing beg end type &optional count line)
  "Return an inner text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by `thing-at-point'.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (let* ((count (or count 1))
         (bnd (or (let ((b (bounds-of-thing-at-point thing)))
                    (and b (< (point) (cdr b)) b))
                  (evil-bounds-of-not-thing-at-point thing)
                  (cons (point-min) (point-max)))))
    ;; check if current object is selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd))
              (and (eq type 'inclusive)
                   (= (1+ beg) end))) ; empty region does not count
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (setq count (if (> count 0) (1- count) (1+ count))))
    (goto-char (if (< count 0) beg end))
    (evil-forward-nearest count
                          #'(lambda (cnt) (forward-thing thing cnt))
                          #'(lambda (cnt) (evil-forward-not-thing thing cnt)))
    (evil-range (if (>= count 0) beg (point))
                (if (< count 0) end (point))
                (if line 'line type)
                :expanded t)))

(defun evil-select-inner-restricted-object (thing beg end type &optional count line)
  "Return an inner text object range of COUNT objects.
Selection is restricted to the current line, unless it is empty.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by `thing-at-point'.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (save-restriction
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (unless (= start end)
        (narrow-to-region start end)))
    (evil-select-inner-object thing beg end type count line)))

(defun evil-select-an-object (thing beg end type count &optional line)
  "Return an outer text object range of COUNT objects.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (let* ((dir (if (> (or count 1) 0) +1 -1))
         (count (abs (or count 1)))
         (objbnd (let ((b (bounds-of-thing-at-point thing)))
                   (and b (< (point) (cdr b)) b)))
         (bnd (or objbnd
                  (evil-bounds-of-not-thing-at-point thing)
                  (cons (point-min) (point-max))))
         addcurrent other)
    ;; check if current object is not selected
    (when (or (not beg) (not end)
              (> beg (car bnd))
              (< end (cdr bnd))
              (and (eq type 'inclusive)
                   (= (1+ beg) end))) ; empty region does not count
      ;; if not, enlarge selection
      (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
      (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
      (if objbnd (setq addcurrent t)))
    ;; make other and (point) reflect the selection
    (cond
     ((> dir 0) (goto-char end) (setq other beg))
     (t (goto-char beg) (setq other end)))
    (cond
     ;; do nothing more than only current is selected
     ((not (and (= beg (car bnd)) (= end (cdr bnd)))))
     ;; current match is thing, add whitespace
     (objbnd
      (let ((wsend (evil-with-restriction
                       ;; restrict to current line if we do non-line selection
                       (and (not line) (line-beginning-position))
                       (and (not line) (line-end-position))
                     (evil-bounds-of-not-thing-at-point thing dir))))
        (cond
         (wsend
          ;; add whitespace at end
          (goto-char wsend)
          (setq addcurrent t))
         (t
          ;; no whitespace at end, try beginning
          (save-excursion
            (goto-char other)
            (setq wsend
                  (evil-with-restriction
                      ;; restrict to current line if we do non-line selection
                      (and (not line)
                           (if (member thing '(evil-word evil-WORD))
                               (save-excursion (back-to-indentation) (point))
                             (line-beginning-position)))
                      (and (not line) (line-end-position))
                    (evil-bounds-of-not-thing-at-point thing (- dir))))
            (when wsend (setq other wsend addcurrent t)))))))
     ;; current match is whitespace, add thing
     (t
      (forward-thing thing dir)
      (setq addcurrent t)))
    ;; possibly count current object as selection
    (if addcurrent (setq count (1- count)))
    ;; move
    (dotimes (_ count)
      (let ((wsend (evil-bounds-of-not-thing-at-point thing dir)))
        (if (and wsend (/= wsend (point)))
            ;; start with whitespace
            (forward-thing thing dir)
          ;; start with thing
          (forward-thing thing dir)
          (setq wsend (evil-bounds-of-not-thing-at-point thing dir))
          (when wsend (goto-char wsend)))))
    ;; return range
    (evil-range (if (> dir 0) other (point))
                (if (< dir 0) other (point))
                (if line 'line type)
                :expanded t)))

(defun evil-select-a-restricted-object (thing beg end type &optional count line)
  "Return an outer text object range of COUNT objects.
Selection is restricted to the current line, unless it is empty.
If COUNT is positive, return objects following point; if COUNT is
negative, return objects preceding point.  If one is unspecified,
the other is used with a negative argument.  THING is a symbol
understood by thing-at-point.  BEG, END and TYPE specify the
current selection.  If LINE is non-nil, the text object should be
linewise, otherwise it is character wise."
  (save-restriction
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (when (/= start end)
        (narrow-to-region start end)))
    (evil-select-an-object thing beg end type count line)))

(defun evil--get-block-range (op cl selection-type)
  "Return the exclusive range of a visual selection.
OP and CL are pairs of buffer positions for the opening and
closing delimiter of a range. SELECTION-TYPE is the desired type
of selection.  It is a symbol that determines which parts of the
block are selected.  If it is `inclusive' or t the returned range
is \(cons (car OP) (cdr CL)). If it is `exclusive' or nil the
returned range is (cons (cdr OP) (car CL)).  If it is
`exclusive-line' the returned range will skip whitespace at the
end of the line of OP and at the beginning of the line of CL."
  (cond
   ((memq selection-type '(inclusive t)) (cons (car op) (cdr cl)))
   ((memq selection-type '(exclusive nil)) (cons (cdr op) (car cl)))
   ((eq selection-type 'exclusive-line)
    (let ((beg (cdr op))
          (end (car cl)))
      (save-excursion
        (goto-char beg)
        (when (and (eolp) (not (eobp)))
          (setq beg (line-beginning-position 2)))
        (goto-char end)
        (skip-chars-backward " \t")
        (when (bolp)
          (setq end (point))
          (goto-char beg)
          (when (and (not (bolp)) (< beg end))
            (setq end (1- end)))))
      (cons beg end)))
   (t (user-error "Unknown selection-type `%s'" selection-type))))

(defun evil-select-block (thing beg end type count
                                &optional
                                selection-type
                                countcurrent
                                fixedscan)
  "Return a range (BEG END) of COUNT delimited text objects.
BEG END TYPE are the currently selected (visual) range.  The
delimited object must be given by THING-up function (see
`evil-up-block').

SELECTION-TYPE is symbol that determines which parts of the block
are selected.  If it is `inclusive' or t OPEN and CLOSE are
included in the range. If it is `exclusive' or nil the delimiters
are not contained. If it is `exclusive-line' the delimiters are
not included as well as adjacent whitespace until the beginning
of the next line or the end of the previous line. If the
resulting selection consists of complete lines only and visual
state is not active, the returned selection is linewise.

If COUNTCURRENT is non-nil an objected is counted if the current
selection matches that object exactly.

Usually scanning for the surrounding block starts at (1+ beg)
and (1- end). If this might fail due to the behavior of THING
then FIXEDSCAN can be set to t. In this case the scan starts at
BEG and END. One example where this might fail is if BEG and END
are the delimiters of a string or comment."
  (save-excursion
    (save-match-data
      (let* ((orig-beg beg)
             (orig-end end)
             (beg (or beg (point)))
             (end (or end (point)))
             (count (abs (or count 1)))
             op cl op-end cl-end)
        ;; We always assume at least one selected character.
        (if (= beg end) (setq end (1+ end)))
        ;; We scan twice: starting at (1+ beg) forward and at (1- end)
        ;; backward. The resulting selection is the smaller one.
        (goto-char (if fixedscan beg (1+ beg)))
        (when (and (zerop (funcall thing +1)) (match-beginning 0))
          (setq cl (cons (match-beginning 0) (match-end 0)))
          (goto-char (car cl))
          (when (and (zerop (funcall thing -1)) (match-beginning 0))
            (setq op (cons (match-beginning 0) (match-end 0)))))
        ;; start scanning from end
        (goto-char (if fixedscan end (1- end)))
        (when (and (zerop (funcall thing -1)) (match-beginning 0))
          (setq op-end (cons (match-beginning 0) (match-end 0)))
          (goto-char (cdr op-end))
          (when (and (zerop (funcall thing +1)) (match-beginning 0))
            (setq cl-end (cons (match-beginning 0) (match-end 0)))))
        ;; Bug #607: use the tightest selection that contains the
        ;; original selection. If non selection contains the original,
        ;; use the larger one.
        (cond
         ((and (not op) (not cl-end))
          (error "No surrounding delimiters found"))
         ((or (not op) ; first not found
              (and cl-end ; second found
                   (>= (car op-end) (car op)) ; second smaller
                   (<= (cdr cl-end) (cdr cl))
                   (<= (car op-end) beg)      ; second contains orig
                   (>= (cdr cl-end) end)))
          (setq op op-end cl cl-end)))
        (setq op-end op cl-end cl) ; store copy
        ;; if the current selection contains the surrounding
        ;; delimiters, they do not count as new selection
        (let ((cnt (if (and orig-beg orig-end (not countcurrent))
                       (let ((sel (evil--get-block-range op cl selection-type)))
                         (if (and (<= orig-beg (car sel))
                                  (>= orig-end (cdr sel)))
                             count
                           (1- count)))
                     (1- count))))
          ;; starting from the innermost surrounding delimiters
          ;; increase selection
          (when (> cnt 0)
            (setq op (progn
                       (goto-char (car op-end))
                       (funcall thing (- cnt))
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         op))
                  cl (progn
                       (goto-char (cdr cl-end))
                       (funcall thing cnt)
                       (if (match-beginning 0)
                           (cons (match-beginning 0) (match-end 0))
                         cl)))))
        (let ((sel (evil--get-block-range op cl selection-type)))
          (setq op (car sel)
                cl (cdr sel)))
        (cond
         ((and (equal op orig-beg) (equal cl orig-end)
               (or (not countcurrent) (/= count 1)))
          (error "No surrounding delimiters found"))
         ((save-excursion
            (and (not (evil-visual-state-p))
                 (eq type 'inclusive)
                 (progn (goto-char op) (bolp))
                 (progn (goto-char cl) (bolp))))
          (evil-range op cl 'line :expanded t))
         (t (evil-range op cl type :expanded t)))))))

(defun evil-select-paren (open close beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN and CLOSE specify the opening and closing delimiter,
respectively. BEG END TYPE are the currently selected (visual)
range.  If INCLUSIVE is non-nil, OPEN and CLOSE are included in
the range; otherwise they are excluded.

If you aren't inside a pair of the opening and closing delimiters,
it jumps you inside the next one. If there isn't one, it errors.

The types of OPEN and CLOSE specify which kind of THING is used
for parsing with `evil-select-block'. If OPEN and CLOSE are
characters `evil-up-paren' is used. Otherwise OPEN and CLOSE
must be regular expressions and `evil-up-block' is used.

If the selection is exclusive, whitespace at the end or at the
beginning of the selection until the end-of-line or beginning-of-line
is ignored."
  (condition-case nil
      (progn
        ;; we need special linewise exclusive selection
        (unless inclusive (setq inclusive 'exclusive-line))
        (cond
         ((and (characterp open) (characterp close))
          (let ((thing #'(lambda (&optional cnt)
                           (evil-up-paren open close cnt)))
                (bnd (or (bounds-of-thing-at-point 'evil-string)
                         (bounds-of-thing-at-point 'evil-comment)
                         ;; If point is at the opening quote of a string,
                         ;; this must be handled as if point is within the
                         ;; string, i.e. the selection must be extended
                         ;; around the string. Otherwise
                         ;; `evil-select-block' might do the wrong thing
                         ;; because it accidentally moves point inside the
                         ;; string (for inclusive selection) when looking
                         ;; for the current surrounding block. (re #364)
                         (and (= (point) (or beg (point)))
                              (save-excursion
                                (goto-char (1+ (or beg (point))))
                                (or (bounds-of-thing-at-point 'evil-string)
                                    (bounds-of-thing-at-point 'evil-comment)))))))
            (if (not bnd)
                (evil-select-block thing beg end type count inclusive)
              (or (evil-with-restriction (car bnd) (cdr bnd)
                    (ignore-errors
                      (evil-select-block thing beg end type count inclusive)))
                  (save-excursion
                    (setq beg (or beg (point))
                          end (or end (point)))
                    (goto-char (car bnd))
                    (let ((extbeg (min beg (car bnd)))
                          (extend (max end (cdr bnd))))
                      (evil-select-block thing
                                         extbeg extend
                                         type
                                         count
                                         inclusive
                                         (or (< extbeg beg) (> extend end))
                                         t)))))))
         (t
          (evil-select-block #'(lambda (&optional cnt)
                                 (evil-up-block open close cnt))
                             beg end type count inclusive))))
    (error ; we aren't in the parens, so find next instance
     (save-match-data
       (goto-char (or (if (and count (> 0 count)) end beg)
                      (point)))
       (let ((re (if (characterp open) (regexp-quote (string open)) open)))
         (if (and (not (looking-at-p re))
                  (re-search-forward re nil t count))
             (progn
               (goto-char (match-beginning 0))
               (let* ((mbeg (match-beginning 0))
                      (res (evil-select-paren open close mbeg mbeg
                                              type nil inclusive)))
                 (if (< (car res) mbeg)
                     ;; this will error if the beginning of the found parens is before the target paren
                     ;; this prevents things such as on the line `prova ( verder "((testo)")`,
                     ;; the inputs `g2ci(` from putting your cursor inside the deleted `()` after `prova`
                     ;; without this, it would go to the second paren (the unbalanced first paren inside the quotes)
                     ;; and then do a change there, changing inside the whole paren after `prova`
                     (error "No surrounding delimiters found")
                   res)))
           (error "No surrounding delimiters found")))))))

(defun evil-select-quote-thing (thing beg end _type count &optional inclusive)
  "Selection THING as if it described a quoted object.
THING is typically either `evil-quote' or `evil-chars'. This
function is called from `evil-select-quote'."
  (save-excursion
    (let* ((count (or count 1))
           (dir (if (> count 0) 1 -1))
           (bnd (let ((b (bounds-of-thing-at-point thing)))
                  (and b (< (point) (cdr b)) b)))
           addcurrent
           wsboth)
      (if inclusive (setq inclusive t)
        (when (= (abs count) 2)
          (setq count dir)
          (setq inclusive 'quote-only))
        ;; never extend with exclusive selection
        (setq beg nil end nil))
      ;; check if the previously selected range does not contain a
      ;; string
      (unless (and beg end
                   (save-excursion
                     (goto-char (if (> dir 0) beg end))
                     (forward-thing thing dir)
                     (and (<= beg (point)) (< (point) end))))
        ;; if so forget the range
        (setq beg nil end nil))
      ;; check if there is a current object, if not fetch one
      (when (not bnd)
        (unless (and (zerop (forward-thing thing dir))
                     (setq bnd (bounds-of-thing-at-point thing)))
          (error "No quoted string found"))
        (if (> dir 0)
            (setq end (point))
          (setq beg (point)))
        (setq addcurrent t))
      ;; check if current object is not selected
      (when (or (not beg) (not end) (> beg (car bnd)) (< end (cdr bnd)))
        ;; if not, enlarge selection
        (when (or (not beg) (< (car bnd) beg)) (setq beg (car bnd)))
        (when (or (not end) (> (cdr bnd) end)) (setq end (cdr bnd)))
        (setq addcurrent t wsboth t))
      ;; maybe count current element
      (when addcurrent
        (setq count (if (> dir 0) (1- count) (1+ count))))
      ;; enlarge selection
      (goto-char (if (> dir 0) end beg))
      (when (and (not addcurrent)
                 (= count (forward-thing thing count)))
        (error "No quoted string found"))
      (if (> dir 0) (setq end (point)) (setq beg (point)))
      ;; add whitespace
      (cond
       ((not inclusive) (setq beg (1+ beg) end (1- end)))
       ((not (eq inclusive 'quote-only))
        ;; try to add whitespace in forward direction
        (goto-char (if (> dir 0) end beg))
        (if (setq bnd (bounds-of-thing-at-point 'evil-space))
            (if (> dir 0) (setq end (cdr bnd)) (setq beg (car bnd)))
          ;; if not found try backward direction
          (goto-char (if (> dir 0) beg end))
          (if (and wsboth (setq bnd (bounds-of-thing-at-point 'evil-space)))
              (if (> dir 0) (setq beg (car bnd)) (setq end (cdr bnd)))))))
      (evil-range beg end
                  ;; HACK: fixes #583
                  ;; When not in visual state, an empty range is
                  ;; possible. However, this cannot be achieved with
                  ;; inclusive ranges, hence we use exclusive ranges
                  ;; in this case. In visual state the range must be
                  ;; inclusive because otherwise the selection would
                  ;; be wrong.
                  (if (evil-visual-state-p) 'inclusive 'exclusive)
                  :expanded t))))

(defun evil-select-quote (quote beg end type count &optional inclusive)
  "Return a range (BEG END) of COUNT quoted text objects.
QUOTE specifies the quotation delimiter. BEG END TYPE are the
currently selected (visual) range.

If INCLUSIVE is nil the previous selection is ignore. If there is
quoted string at point this object will be selected, otherwise
the following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)) is selected. If (/= (abs COUNT) 2) the delimiting quotes are not
contained in the range, otherwise they are contained in the range.

If INCLUSIVE is non-nil the selection depends on the previous
selection. If the currently selection contains at least one
character that is contained in a quoted string then the selection
is extended, otherwise it is thrown away. If there is a
non-selected object at point then this object is added to the
selection. Otherwise the selection is extended to the
following (if (> COUNT 0)) or preceeding object (if (< COUNT
0)). Any whitespace following (or preceeding if (< COUNT 0)) the
new selection is added to the selection. If no such whitespace
exists and the selection contains only one quoted string then the
preceeding (or following) whitespace is added to the range. "
  (let ((evil-forward-quote-char quote))
    (or (let ((bnd (or (bounds-of-thing-at-point 'evil-comment)
                       (bounds-of-thing-at-point 'evil-string))))
          (when (and bnd (< (point) (cdr bnd))
                     (/= (char-after (car bnd)) quote)
                     (/= (char-before (cdr bnd)) quote))
            (evil-with-restriction (car bnd) (cdr bnd)
              (ignore-errors (evil-select-quote-thing
                              'evil-quote-simple
                              beg end type
                              count
                              inclusive)))))
        (let ((evil-forward-quote-char quote))
          (evil-select-quote-thing 'evil-quote
                                   beg end type
                                   count
                                   inclusive)))))

(defun evil-select-xml-tag (beg end type &optional count inclusive)
  "Return a range (BEG END) of COUNT matching XML tags.
If INCLUSIVE is non-nil, the tags themselves are included
from the range."
  (cond
   ((and (not inclusive) (= (abs (or count 1)) 1))
    (let ((rng (evil-select-block #'evil-up-xml-tag beg end type count nil t)))
      (if (or (and beg (= beg (evil-range-beginning rng))
                   end (= end (evil-range-end rng))))
          (evil-select-block #'evil-up-xml-tag beg end type count t)
        rng)))
   (t
    (evil-select-block #'evil-up-xml-tag beg end type count inclusive))))

(defun evil-expand-range (range &optional copy)
  "Expand RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (evil-copy-range range)))
  (unless (plist-get (evil-range-properties range) :expanded)
    (setq range (evil-transform-range :expand range)))
  range)

(defun evil-contract-range (range &optional copy)
  "Contract RANGE according to its type.
Return a new range if COPY is non-nil."
  (evil-transform-range :contract range copy))

(defun evil-normalize-range (range &optional copy)
  "Normalize RANGE according to its type.
Return a new range if COPY is non-nil."
  (evil-transform-range :normalize range copy))

(defun evil-transform-range (transform range &optional copy)
  "Apply TRANSFORM to RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (evil-copy-range range)))
  (when (evil-type range)
    (apply #'evil-set-range range
           (apply #'evil-transform transform range)))
  range)

(defun evil-describe-range (range)
  "Return description of RANGE.
If no description is available, return the empty string."
  (apply #'evil-describe range))

;;; Undo

(defvar buffer-undo-tree)
(declare-function undo-tree-current "ext:undo-tree")
(declare-function undo-tree-node-next "ext:undo-tree")
(declare-function undo-tree-node-branch "ext:undo-tree")
(declare-function undo-tree-node-branch "ext:undo-tree")
(declare-function undo-tree-undo "ext:undo-tree")
(declare-function undo-tree-snip-node "ext:undo-tree")

(defun evil-start-undo-step (&optional continue)
  "Start a undo step.
All following buffer modifications are grouped together as a
single action. If CONTINUE is non-nil, preceding modifications
are included. The step is terminated with `evil-end-undo-step'."
  (when (and (listp buffer-undo-list)
             (not evil-in-single-undo))
    (if evil-undo-list-pointer
        (evil-refresh-undo-step)
      (unless (or continue (null (car-safe buffer-undo-list)))
        (undo-boundary))
      (setq evil-undo-list-pointer (or buffer-undo-list t)))))

(defun evil-end-undo-step (&optional continue)
  "End a undo step started with `evil-start-undo-step'.
Adds an undo boundary unless CONTINUE is specified."
  (when (and (listp buffer-undo-list)
             evil-undo-list-pointer
             (not evil-in-single-undo))
    (evil-refresh-undo-step)
    (unless (or continue (null (car-safe buffer-undo-list)))
      (undo-boundary))
    (setq evil-undo-list-pointer nil)))

(defun evil-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `evil-undo-list-pointer' are removed to
make the entries undoable as a single action. See
`evil-start-undo-step'."
  (when evil-undo-list-pointer
    (setq buffer-undo-list
          (evil-filter-list #'null buffer-undo-list evil-undo-list-pointer))
    (setq evil-undo-list-pointer (or buffer-undo-list t))))

(defmacro evil-with-undo (&rest body)
  "Execute BODY with enabled undo.
If undo is disabled in the current buffer, the undo information
is stored in `evil-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun) (debug t))
  (let ((undo-list (make-symbol "undo-list")))
    `(let ((,undo-list buffer-undo-list)
           (evil-undo-system evil-undo-system))
       (when (eq ,undo-list t) (setq buffer-undo-list nil
                                     evil-undo-system nil))
       (unwind-protect
           (progn ,@body)
         ;; ensure any new undo changes we've accumulated start with
         ;; exactly one undo boundary marker, i.e. nil
         (when (car-safe buffer-undo-list) (push nil buffer-undo-list))
         (if (eq ,undo-list t)
             ;; undo is disabled, so store undo information in
             ;; evil-temporary-undo
             (setq evil-temporary-undo buffer-undo-list
                   buffer-undo-list t)
           (setq evil-temporary-undo nil))))))

(defmacro evil-with-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (declare (indent defun) (debug t))
  `(let (evil-undo-list-pointer)
     (evil-with-undo
       (evil-start-undo-step)
       (unwind-protect
           (let ((evil-in-single-undo t)) ,@body)
         (evil-end-undo-step)))))

(defun evil-undo-pop ()
  "Undo and forget the last buffer change.
If undo is disabled in the current buffer, use the information
in `evil-temporary-undo' instead."
  (if (and (eq evil-undo-system 'undo-tree)
           (not (eq buffer-undo-list t)))
      (let (current)
        (undo-tree-undo)
        (setq current (undo-tree-current buffer-undo-tree)
              current (nth (undo-tree-node-branch current)
                           (undo-tree-node-next current)))
        ;; Remove only if leaf to not have to adjust child buffer positions
        (unless (undo-tree-node-next current) (undo-tree-snip-node current)))
    (let ((paste-undo (list nil))
          (undo-list (if (eq buffer-undo-list t)
                         evil-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (user-error "Can't undo previous change"))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)) ; remove nil
      (while (and undo-list (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo)))
        (evil-save-echo-area
          (undo)))
      (if (eq buffer-undo-list t)
          (setq evil-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

;;; Search
(defun evil-transform-regexp (regexp replacements-alist)
  (replace-regexp-in-string
   "\\\\+[^\\\\]"
   #'(lambda (txt)
       (let* ((b (match-beginning 0))
              (e (match-end 0))
              (ch (aref txt (1- e)))
              (repl (assoc ch replacements-alist)))
         (if (and repl (zerop (mod (length txt) 2)))
             (concat (substring txt b (- e 2))
                     (cdr repl))
           txt)))
   regexp nil t))

(defun evil-transform-magic (str magic quote transform &optional _start)
  "Transform STR with magic characters.
MAGIC is a regexp that matches all potential magic
characters. Each occurence of CHAR as magic character within str
is replaced by the result of calling the associated TRANSFORM
function. TRANSFORM is a function taking two arguments, the
character to be transformed and the rest of string after the
character. The function should return a triple (REPLACEMENT REST
. STOP) where REPLACEMENT is the replacement and REST is the rest
of the string that has not been transformed. If STOP is non-nil
then the substitution stops immediately.  The replacement starts
at position START, everything before that position is returned
literally.  The result is a pair (RESULT . REST). RESULT is a
list containing the transformed parts in order. If two
subsequents parts are both strings, they are concatenated. REST
is the untransformed rest string (usually \"\" but may be more if
TRANSFORM stopped the substitution). Which characters are
considered as magic characters (i.e. the transformation happens
if the character is NOT preceeded by a backslash) is determined
by `evil-magic'. The special tokens \\v, \\V, \\m and \\M have
always a special meaning (like in Vim) and should not be
contained in TRANSFORMS, otherwise their meaning is overwritten.

The parameter QUOTE is a quoting function applied to literal
transformations, usually `regexp-quote' or `replace-quote'."
  (save-match-data
    (let ((regexp (concat "\\(?:\\`\\|[^\\]\\)\\(\\\\\\(?:\\(" magic "\\)\\|\\(.\\)\\)\\|\\(" magic "\\)\\)"))
          (magic-chars (evil-get-magic evil-magic))
          (evil-magic evil-magic)
          (quote (or quote #'identity))
          result stop)
      (while (and (not stop) str (string-match regexp str))
        (unless (zerop (match-beginning 1))
          (push (substring str 0 (match-beginning 1)) result))
        (let ((char (or (match-string 2 str)
                        (match-string 3 str)
                        (match-string 4 str)))
              (rest (substring str (match-end 0))))
          (cond
           ((match-beginning 4)
            ;; magic character without backslash
            (if (string-match magic-chars char)
                ;; magic, do transform
                (let ((trans (funcall transform (aref char 0) rest)))
                  (push (car trans) result)
                  (setq str (cadr trans) stop (nthcdr 2 trans)))
              ;; non-magic, literal transformation
              (push (funcall quote char) result)
              (setq str rest)))
           ((match-beginning 2)
            ;; magic character with backslash
            (if (not (string-match magic-chars char))
                ;; non-magic, do transform
                (let ((trans (funcall transform (aref char 0) rest)))
                  (push (car trans) result)
                  (setq str (cadr trans) stop (nthcdr 2 trans)))
              ;; magic, literal transformation
              (push (funcall quote char) result)
              (setq str rest)))
           ((memq (aref char 0) '(?m ?M ?v ?V))
            (setq evil-magic (cdr (assq (aref char 0)
                                        '((?m . t)
                                          (?M . nil)
                                          (?v . very-magic)
                                          (?V . very-nomagic)))))
            (setq magic-chars (evil-get-magic evil-magic))
            (setq str rest))
           (t
            ;; non-magic char with backslash, literal transformation
            (push (funcall quote char) result)
            (setq str rest)))))
      (cond
       ((and str (not stop))
        (push str result)
        (setq str ""))
       ((not str)
        (setq str "")))
      ;; concatenate subsequent strings
      ;; note that result is in reverse order
      (let (repl)
        (while result
          (cond
           ((and (stringp (car result))
                 (zerop (length (car result))))
            (pop result))
           ((and (stringp (car result))
                 (stringp (cadr result)))
            (setq result (cons (concat (cadr result)
                                       (car result))
                               (nthcdr 2 result))))
           (t
            (push (pop result) repl))))
        (cons repl str)))))

(defconst evil-vim-regexp-replacements
  '((?n  . "\n")           (?r  . "\r")
    (?t  . "\t")           (?b  . "\b")
    (?s  . "[[:space:]]")  (?S  . "[^[:space:]]")
    (?d  . "[[:digit:]]")  (?D  . "[^[:digit:]]")
    (?x  . "[[:xdigit:]]") (?X  . "[^[:xdigit:]]")
    (?o  . "[0-7]")        (?O  . "[^0-7]")
    (?a  . "[[:alpha:]]")  (?A  . "[^[:alpha:]]")
    (?l  . "[a-z]")        (?L  . "[^a-z]")
    (?u  . "[A-Z]")        (?U  . "[^A-Z]")
    (?y  . "\\s")          (?Y  . "\\S")
    (?\( . "\\(")          (?\) . "\\)")
    (?{  . "\\{")          (?}  . "\\}")
    (?\[ . "[")            (?\] . "]")
    (?<  . "\\<")          (?>  . "\\>")
    (?_  . "\\_")
    (?*  . "*")            (?+  . "+")
    (??  . "?")            (?=  . "?")
    (?.  . ".")
    (?`  . "`")            (?^  . "^")
    (?$  . "$")            (?|  . "\\|")))

(defconst evil-regexp-magic "[][(){}<>_dDsSxXoOaAlLuUwWyY.*+?=^$`|nrtb]")

(defun evil-transform-vim-style-regexp (regexp)
  "Transform vim-style backslash codes to Emacs regexp.
This includes the backslash codes \\d, \\D, \\s, \\S, \\x, \\X,
\\o, \\O, \\a, \\A, \\l, \\L, \\u, \\U and \\w, \\W. The new
codes \\y and \\Y can be used instead of the Emacs code \\s and
\\S which have a different meaning in Vim-style."
  (car
   (car
    (evil-transform-magic
     regexp evil-regexp-magic #'regexp-quote
     #'(lambda (char rest)
         (let ((repl (assoc char evil-vim-regexp-replacements)))
           (if repl
               (list (cdr repl) rest)
             (list (concat "\\" (char-to-string char)) rest))))))))

;;; Substitute

(defun evil-downcase-first (str)
  "Return STR with the first letter downcased."
  (if (zerop (length str))
      str
    (concat (downcase (substring str 0 1))
            (substring str 1))))

(defun evil-upcase-first (str)
  "Return STR with the first letter upcased."
  (if (zerop (length str))
      str
    (concat (upcase (substring str 0 1))
            (substring str 1))))

(defun evil-get-magic (magic)
  "Return a regexp matching the magic characters according to MAGIC.
Depending on the value of MAGIC the following characters are
considered magic.
  t              [][{}*+?.&~$^
  nil            [][{}*+?$^
  `very-magic'   not 0-9A-Za-z_
  `very-nomagic' empty."
  (cond
   ((eq magic t) "[][}{*+?.&~$^]")
   ((eq magic 'very-magic) "[^0-9A-Za-z_]")
   ((eq magic 'very-nomagic) "\\\\")
   (t "[][}{*+?$^]")))

;; TODO: support magic characters in patterns
(defconst evil-replacement-magic "[eElLuU0-9&#,rnbt=]"
  "All magic characters in a replacement string")

(defun evil-compile-subreplacement (to &optional start)
  "Convert a regexp replacement TO to Lisp from START until \\e or \\E.
Return a pair (RESULT . REST). RESULT is a list suitable for
`perform-replace' if necessary, the original string if not.
REST is the unparsed remainder of TO."
  (let ((result
         (evil-transform-magic
          to evil-replacement-magic #'replace-quote
          #'(lambda (char rest)
              (cond
               ((eq char ?#)
                (list '(number-to-string replace-count) rest))
               ((eq char ?r) (list "\r" rest))
               ((eq char ?n) (list "\n" rest))
               ((eq char ?b) (list "\b" rest))
               ((eq char ?t) (list "\t" rest))
               ((memq char '(?e ?E))
                `("" ,rest . t))
               ((memq char '(?l ?L ?u ?U))
                (let ((result (evil-compile-subreplacement rest))
                      (func (cdr (assoc char
                                        '((?l . evil-downcase-first)
                                          (?L . downcase)
                                          (?u . evil-upcase-first)
                                          (?U . upcase))))))
                  (list `(,func
                          (replace-quote
                           (evil-match-substitute-replacement
                            ,(car result)
                            (not case-replace))))
                        (cdr result))))
               ((eq char ?=)
                (when (or (zerop (length rest))
                          (not (eq (aref rest 0) ?@)))
                  (user-error "Expected @ after \\="))
                (when (< (length rest) 2)
                  (user-error "Expected register after \\=@"))
                (list (evil-get-register (aref rest 1))
                      (substring rest 2)))
               ((eq char ?,)
                (let* ((obj (read-from-string rest))
                       (result `(replace-quote ,(car obj)))
                       (end
                        ;; swallow a space after a symbol
                        (if (and (or (symbolp (car obj))
                                     ;; swallow a space after 'foo,
                                     ;; but not after (quote foo)
                                     (and (eq (car-safe (car obj)) 'quote)
                                          (not (= ?\( (aref rest 0)))))
                                 (eq (string-match " " rest (cdr obj))
                                     (cdr obj)))
                            (1+ (cdr obj))
                          (cdr obj))))
                  (list result (substring rest end))))
               ((eq char ?0)
                (list "\\&" rest))
               (t
                (list (concat "\\" (char-to-string char)) rest))))
          start)))
    (let ((rest (cdr result))
          (result (car result)))
      (replace-match-string-symbols result)
      (cons (if (cdr result)
                (cons 'concat result)
              (or (car result) ""))
            rest))))

(defun evil-compile-replacement (to)
  "Maybe convert a regexp replacement TO to Lisp.
Return a list suitable for `perform-replace' if necessary, the
original string if not. Currently the following magic characters
in replacements are supported: 0-9&#lLuUrnbt,
The magic character , (comma) start an Emacs-lisp expression."
  (when (stringp to)
    (save-match-data
      (cons 'replace-eval-replacement
            (car (evil-compile-subreplacement to))))))

(defun evil-match-substitute-replacement (replacement &optional fixedcase string)
  "Return REPLACEMENT as it will be inserted by `evil-replace-match'.
If REPLACEMENT is an expression it will be evaluated to compute the
replacement text first."
  (match-substitute-replacement
   (if (stringp replacement)
       replacement
     (funcall (car replacement)
              (cdr replacement)
              0))
   fixedcase nil string))

;;; Alignment

(defun evil-justify-lines (beg end justify position)
  "Justify all lines in a range.
BEG and END specify the range of those lines to be
justified. JUSTIFY is either `left', `right' or `center' according
to the justification type. POSITION is the maximal text width for
right and center justification or the column at which the lines
should be left-aligned for left justification."
  (let ((fill-column position)
        adaptive-fill-mode fill-prefix)
    (evil-with-restriction
        (save-excursion
          (goto-char beg)
          (line-beginning-position))
        (save-excursion
          (goto-char end)
          (if (bolp)
              (line-end-position 0)
            (line-end-position)))
      (goto-char (point-min))
      (while (progn
               (if (eq justify 'left)
                   (indent-line-to position)
                 (when (re-search-forward "^[[:space:]]*" nil t)
                   (delete-region (match-beginning 0)
                                  (match-end 0)))
                 (justify-current-line justify nil t))
               (and (zerop (forward-line)) (bolp))))
      (goto-char (point-min))
      (back-to-indentation))))

;;; View helper

(defvar-local evil-list-view-select-action nil)

(define-derived-mode evil-list-view-mode tabulated-list-mode
  "Evil List View"
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun evil-list-view-goto-entry ()
  (interactive)
  (when (and evil-list-view-select-action
             (not (eobp)))
    (let* ((line (line-number-at-pos (point)))
           (entry (elt tabulated-list-entries (1- line))))
      (funcall evil-list-view-select-action (nth 1 entry)))))

(defun evil-list-view-quit ()
  (interactive)
  (quit-window 'kill))

(define-key evil-list-view-mode-map (kbd "q") #'evil-list-view-quit)
(define-key evil-list-view-mode-map [follow-link] nil) ;; allows mouse-1 to be activated
(define-key evil-list-view-mode-map [mouse-1] #'evil-list-view-goto-entry)
(define-key evil-list-view-mode-map [return] #'evil-list-view-goto-entry)

(defmacro evil-with-view-list (&rest properties)
  "Open a new list view buffer.
PROPERTIES is a property-list which supports the following properties:

:name          (required)   The name of the buffer.
:mode-name     (required)   The name for the mode line.
:format        (required)   The value for `tabulated-list-format'.
:entries       (required)   The value for `tabulated-list-entries'.
:select-action (optional)   A function for row selection.
                            It takes a single parameter, which is the
                            selected row's vector value that is
                            passed into `:entries'."
  (declare (indent defun) (debug t))
  `(let ((bufname (concat "*" ,(plist-get properties :name) "*"))
         (inhibit-read-only t))
     (and (get-buffer bufname)
          (kill-buffer bufname))
     (let ((buf (get-buffer-create bufname)))
       (with-current-buffer buf
         (setq tabulated-list-format ,(plist-get properties :format))
         (setq tabulated-list-entries ,(plist-get properties :entries))
         (setq evil-list-view-select-action ,(plist-get properties :select-action))
         (evil-list-view-mode)
         (setq mode-name ,(plist-get properties :mode-name))
         (evil-motion-state))
       (switch-to-buffer-other-window buf))))

(provide 'evil-common)

;;; evil-common.el ends here
