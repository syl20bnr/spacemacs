;;; tablist.el --- Extended tabulated-list-mode -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: extensions, lisp
;; Package: tablist
;; Version: 1.1
;; Package-Requires: ((emacs "24.3"))

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
;;
;; This package adds marks and filters to tabulated-list-mode.  It
;; also kind of puts a dired face on tabulated list buffers.
;;
;; It can be used by deriving from tablist-mode and some features by
;; using tablist-minor-mode inside a tabulated-list-mode buffer.
;;

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'tabulated-list)
(require 'dired)
(require 'tablist-filter)

;;
;; *Macros
;;

(defmacro tablist-save-marks (&rest body)
  "Eval body, while preserving all marks."
  (let ((marks (make-symbol "marks")))
    `(let (,marks)
       (save-excursion
         (goto-char (point-min))
         (let ((re "^\\([^ ]\\)"))
           (while (re-search-forward re nil t)
             (push (cons (tabulated-list-get-id)
                         (tablist-get-mark-state))
                   ,marks))))
       (unwind-protect
           (progn ,@body)
         (save-excursion
           (dolist (m ,marks)
             (let ((id (pop m)))
               (goto-char (point-min))
               (while (and id (not (eobp)))
                 (when (equal id (tabulated-list-get-id))
                   (tablist-put-mark-state m)
                   (setq id nil))
                 (forward-line)))))))))

(defmacro tablist-with-remembering-entry (&rest body)
  "Remember where body left of and restore previous position.

If the current entry is still visible, move to it. Otherwise move
to the next visible one after it.  If that also fails, goto to
the beginning of the buffer.  Finally move point to the major
column."
  (declare (indent 0) (debug t))
  (let ((re (make-symbol "re"))
        (id (make-symbol "id"))
        (col (make-symbol "col")))
    `(let ((,re
            (replace-regexp-in-string
             "[\t ]+" "[\t ]*" (regexp-quote
                                (or (thing-at-point 'line) ""))
             t t))
           (,id (tabulated-list-get-id))
           (,col (tablist-current-column)))
       (progn
         ,@body
         (let (success pos)
           (goto-char (point-min))
           (setq pos (point))
           (while (and (setq success (re-search-forward ,re nil t))
                       (> (point) (prog1 pos (setq pos (point))))
                       (forward-line -1)
                       (not (equal ,id (tabulated-list-get-id))))
             (forward-line))
           (unless success
             (goto-char (point-min))
             (while (and (not (eobp))
                         (not success))
               (if (equal (tabulated-list-get-id) ,id)
                   (setq success t)
                 (forward-line))))
           (unless (and success (not (invisible-p (point))))
             (goto-char (point-min)))
           (tablist-skip-invisible-entries)
           (tablist-move-to-column
            (or ,col (car (tablist-major-columns))))
           (dolist (win (get-buffer-window-list))
             (set-window-point win (point))))))))

(defmacro tablist-with-filter-displayed (&rest body)
  "Display the current filter in the mode while evalling BODY."
  (let ((state (make-symbol "state")))
    `(let ((,state (tablist-display-filter 'state)))
       (tablist-display-filter t)
       (unwind-protect
           (progn ,@body)
         (tablist-display-filter ,state)))))

;;
;; *Mode Maps
;;

(defvar tablist-mode-filter-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "p" #'tablist-pop-filter)
    (define-key kmap "r" #'tablist-push-regexp-filter)
    (define-key kmap "=" #'tablist-push-equal-filter)
    (define-key kmap "n" #'tablist-push-numeric-filter)
    (define-key kmap "!" #'tablist-negate-filter)
    (define-key kmap "t" #'tablist-toggle-first-filter-logic)
    (define-key kmap "/" #'tablist-display-filter)
    (define-key kmap "z" #'tablist-suspend-filter)

    (define-key kmap "a" #'tablist-push-named-filter)
    (define-key kmap "s" #'tablist-name-current-filter)
    (define-key kmap "D" #'tablist-delete-named-filter)
    (define-key kmap "d" #'tablist-deconstruct-named-filter)
    (define-key kmap "e" #'tablist-edit-filter)
    (define-key kmap "C" #'tablist-clear-filter)
    kmap))

(defvar tablist-mode-mark-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "c" #'tablist-change-marks)
    (define-key kmap "!" #'tablist-unmark-all-marks)
    (define-key kmap "r" #'tablist-mark-items-regexp)
    (define-key kmap "n" #'tablist-mark-items-numeric)
    (define-key kmap "m" #'tablist-mark-forward)
    kmap))

(defvar tablist-mode-regexp-map
  (let ((kmap (make-sparse-keymap)))
    ;; (define-key kmap "&" #'tablist-flag-gargabe-items)
    (define-key kmap "m" #'tablist-mark-items-regexp)
    kmap))

(defvar tablist-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "m" #'tablist-mark-forward)
    (define-key kmap (kbd "DEL") #'tablist-unmark-backward)
    (define-key kmap "k" #'tablist-do-kill-lines)
    (define-key kmap "U" #'tablist-unmark-all-marks)
    (define-key kmap "u" #'tablist-unmark-forward)
    (define-key kmap "t" #'tablist-toggle-marks)

    (define-key kmap (kbd "TAB") #'tablist-forward-column)
    (define-key kmap "\t" #'tablist-forward-column)
    (define-key kmap [backtab] #'tablist-backward-column)

    (define-key kmap "%" tablist-mode-regexp-map)
    (define-key kmap "*" tablist-mode-mark-map)
    (define-key kmap "/" tablist-mode-filter-map)

    ;; (define-key kmap "e" #'tablist-edit-column)
    ;; (define-key kmap "i" #'tablist-insert-entry)
    (define-key kmap "s" #'tablist-sort)
    (define-key kmap [remap back-to-indentation] #'tablist-move-to-major-column)
    (define-key kmap [remap next-line] #'tablist-next-line)
    (define-key kmap [remap previous-line] #'tablist-previous-line)
    (define-key kmap "<" #'tablist-shrink-column)
    (define-key kmap ">" #'tablist-enlarge-column)
    (define-key kmap "q" #'tablist-quit)
    (define-key kmap "G" #'tablist-revert)
    (define-key kmap (kbd "C-c C-e") #'tablist-export-csv)
    kmap))

(defvar tablist-mode-map
  (let ((kmap (copy-keymap tablist-minor-mode-map)))
    (set-keymap-parent kmap tabulated-list-mode-map)
    (define-key kmap "d" #'tablist-flag-forward)
    (define-key kmap (kbd "RET") #'tablist-find-entry)
    (define-key kmap "f" #'tablist-find-entry)
    ;; (define-key kmap "~" #'tablist-flag-gargabe-items)
    (define-key kmap "D" #'tablist-do-delete)
    ;; (define-key kmap "C" #'tablist-do-copy)
    ;; (define-key kmap "R" #'tablist-do-rename)
    (define-key kmap "x" #'tablist-do-flagged-delete)
    ;; (define-key kmap "F" #'tablist-find-marked-items)
    ;; (define-key kmap (kbd "C-o") #'tablist-display-item)
    kmap))

;;
;; *Variables
;;

;; Marking
(defvar tablist-umark-filtered-entries t)
(defvar tablist-marker-char dired-marker-char
  "The character used for marking.")
(defvar tablist-marker-face 'dired-mark
  "The face used for the mark character.")
(defvar tablist-marked-face  'dired-marked
  "The face used for marked major columns.")

;; Operations
(defvar-local tablist-operations-function nil
  "A function for handling operations on the entries.

The function is called with varying number of arguments, while
the first one is always a symbol describing one of the following
operations.

`supported-operations'

This is the only mandatory operation. There are no other
arguments and the function should return a list of symbols of
supported operations.

`delete'

The 2nd argument will be a list of entry ID's.  The function
should somehow delete these entries and update
`tabulated-list-entries'.

`find-entry'

The 2nd argument is the ID of an entry.  The function should
somehow find/display this entry, i.e. a kind of default
operation.

`edit-column'

The function is called with 3 further arguments: ID, INDEX and
NEW-COLUMN, where ID represents the entry to edit, INDEX is the index
of the column and NEW-COLUMN is the proposed new value for this
column.  It should either

i.  return a new edited complete entry and update
`tabulated-list-entries', or

ii. throw an error, if NEW-COLUMN is not a valid value for this
column.

`complete'

The function is called with 4 further arguments: ID, INDEX,
STRING and POS, where ID represents an entry, INDEX is the index
of the column to complete, STRING it's current value and POS an
offset of the current position of point into STRING.

The function should return a collection for this column, suitable
as argument for the function `completion-in-region'.")

;; Differentiating columns
(defvar-local tablist-major-columns nil
  "Columns used to mark and when querying.")

;; Filter
(defvar-local tablist-current-filter nil)
(defvar-local tablist-filter-suspended nil)
(defvar tablist-named-filter nil)

;; History variables
(defvar tablist-column-name-history nil)

;; Hooks
(defvar tablist-selection-changed-functions nil
  "A hook run when ever point moves to a different entry.")

;; Context Window
(defvar-local tablist-context-window nil)
(defvar-local tablist-context-window-function nil)
(defvar tablist-context-window-display-action
  `((display-buffer-reuse-window
     tablist-display-buffer-split-below-and-attach)
    (window-height . 0.25)
    (inhibit-same-window . t)))

;;
;; *Setup
;;

(defvar savehist-additional-variables)
(add-hook 'savehist-save-hook
          (lambda nil
            (add-to-list 'savehist-additional-variables 'tablist-named-filter)))

;;;###autoload
(define-minor-mode tablist-minor-mode
  "Toggle tablist minor mode."
  :global nil
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "Buffer is not in Tabulated List Mode"))
  (tablist-init (not tablist-minor-mode)))

;;;###autoload
(define-derived-mode tablist-mode tabulated-list-mode "TL"
  (tablist-init))

(defun tablist-init (&optional disable)
  (let ((cleaned-misc (cl-remove 'tablist-current-filter
                                 mode-line-misc-info :key #'car-safe)))
    (cond
     ((not disable)
      (set (make-local-variable 'mode-line-misc-info)
           (append
            (list
             (list 'tablist-current-filter
                   '(:eval (format " [%s]"
                                   (if tablist-filter-suspended
                                       "suspended"
                                     "filtered")))))))
      (add-hook 'post-command-hook
                'tablist-selection-changed-handler nil t)
      (add-hook 'tablist-selection-changed-functions
                'tablist-context-window-update nil t))
     (t
      (setq mode-line-misc-info cleaned-misc)
      (remove-hook 'post-command-hook
                   'tablist-selection-changed-handler t)
      (remove-hook 'tablist-selection-changed-functions
                   'tablist-context-window-update t)))))

(defun tablist-quit ()
  (interactive)
  (tablist-hide-context-window)
  (quit-window))

(defvar-local tablist-selected-id nil)
(defvar tablist-edit-column-minor-mode)

(defun tablist-selection-changed-handler ()
  (unless tablist-edit-column-minor-mode
    (let ((id tablist-selected-id)
          (selected (tabulated-list-get-id)))
      (unless (eq selected id)
        (setq tablist-selected-id selected)
        (run-hook-with-args
         'tablist-selection-changed-functions
         tablist-selected-id)))))

(defvar tablist-context-window-update--timer nil)

(defun tablist-context-window-update (&optional id)
  (when (and tablist-context-window-function
             (window-live-p tablist-context-window)
             (not tablist-edit-column-minor-mode))
    (unless id
      (setq id (tabulated-list-get-id)))
    (when (timerp tablist-context-window-update--timer)
      (cancel-timer tablist-context-window-update--timer))
    (setq tablist-context-window-update--timer
          (run-with-idle-timer 0.1 nil
                               (lambda (fn window)
                                 (when (window-live-p window)
                                   (with-selected-window window
                                     (set-window-dedicated-p nil nil)
                                     (save-selected-window
                                       (funcall fn id))
                                     (when (window-live-p (selected-window))
                                       (set-window-dedicated-p nil t)))))
                               tablist-context-window-function
                               tablist-context-window))))

(defun tablist-display-context-window ()
  (interactive)
  (unless tablist-context-window-function
    (error "No function for handling a context is defined"))
  (unless (window-live-p tablist-context-window)
    (setq tablist-context-window
          (display-buffer
           (current-buffer)
           tablist-context-window-display-action)))
  (prog1
      tablist-context-window
    (tablist-context-window-update)))

(defun tablist-hide-context-window ()
  (interactive)
  (when (window-live-p tablist-context-window)
    (let ((ignore-window-parameters t))
      (delete-window tablist-context-window)))
  (setq tablist-context-window nil))

(defun tablist-toggle-context-window ()
  (interactive)
  (if (window-live-p tablist-context-window)
      (tablist-hide-context-window)
    (tablist-display-context-window)))

;;
;; *Marking
;;

(defun tablist-revert ()
  "Revert the list with marks preserved, position kept."
  (interactive)
  (tablist-save-marks
   (tablist-with-remembering-entry
     (tabulated-list-revert))))

(defun tablist-major-columns ()
  (if (null tablist-major-columns)
      (number-sequence 0 (1- (length tabulated-list-format)))
    (if (numberp tablist-major-columns)
        (list tablist-major-columns)
      tablist-major-columns)))

(defun tablist-put-mark (&optional pos)
  "Put a mark before the entry at POS.

POS defaults to point. Use `tablist-marker-char',
`tablist-marker-face', `tablist-marked-face' and
`tablist-major-columns' to determine how to mark and what to put
a face on."
  (when (or (null tabulated-list-padding)
            (< tabulated-list-padding 1))
    (setq tabulated-list-padding 1)
    (tabulated-list-revert))
  (save-excursion
    (and pos (goto-char pos))
    (unless (tabulated-list-get-id)
      (error "No entry at this position"))
    (let ((inhibit-read-only t))
      (tabulated-list-put-tag
       (string tablist-marker-char))
      (put-text-property
       (point-at-bol)
       (1+ (point-at-bol))
       'face tablist-marker-face)
      (let ((columns (tablist-column-offsets)))
        (dolist (c (tablist-major-columns))
          (when (and (>= c 0)
                     (< c (length columns)))
            (let ((beg (+ (point-at-bol)
                          (nth c columns)))
                  (end (if (= c (1- (length columns)))
                           (point-at-eol)
                         (+ (point-at-bol)
                            (nth (1+ c) columns)))))
              (cond
               ((and tablist-marked-face
                     (not (eq tablist-marker-char ?\s)))
                (tablist--save-face-property beg end)
                (put-text-property
                 beg end 'face tablist-marked-face))
               (t (tablist--restore-face-property beg end))))))))))

(defun tablist-mark-forward (&optional arg interactive)
  "Mark ARG entries forward.

ARG is interpreted as a prefix-arg.  If interactive is non-nil,
maybe use the active region instead of ARG.

See `tablist-put-mark' for how entries are marked."
  (interactive (list current-prefix-arg t))
  (cond
   ;; Mark files in the active region.
   ((and interactive (use-region-p))
    (save-excursion
      (goto-char (region-beginning))
      (beginning-of-line)
      (tablist-repeat-over-lines
       (1+ (count-lines
            (point)
            (save-excursion
              (goto-char (region-end))
              (beginning-of-line)
              (point))))
       'tablist-put-mark)))
   ;; Mark the current (or next ARG) files.
   (t
    (tablist-repeat-over-lines
     (prefix-numeric-value arg)
     'tablist-put-mark))))

(defun tablist-mark-backward (&optional arg interactive)
  "Mark ARG entries backward.

See `tablist-mark-forward'."
  (interactive (list current-prefix-arg t))
  (tablist-mark-forward (- (prefix-numeric-value arg))
                        interactive))

(defun tablist-unmark-forward (&optional arg interactive)
  "Unmark ARG entries forward.

See `tablist-mark-forward'."
  (interactive (list current-prefix-arg t))
  (let ((tablist-marker-char ?\s)
        tablist-marked-face)
    (tablist-mark-forward arg interactive)))

(defun tablist-unmark-backward (&optional arg interactive)
  "Unmark ARG entries backward.

See `tablist-mark-forward'."
  (interactive (list current-prefix-arg t))
  (let ((tablist-marker-char ?\s)
        tablist-marked-face)
    (tablist-mark-backward arg interactive)))

(defun tablist-flag-forward (&optional arg interactive)
  "Flag ARG entries forward.

See `tablist-mark-forward'."
  (interactive (list current-prefix-arg t))
  (let ((tablist-marker-char ?D)
        (tablist-marked-face 'dired-flagged))
    (tablist-mark-forward arg interactive)))

(defun tablist-change-marks (old new)
  "Change all OLD marks to NEW marks.

OLD and NEW are both characters used to mark files."
  (interactive
   (let* ((cursor-in-echo-area t)
          (old (progn (message "Change (old mark): ") (read-char)))
          (new (progn (message  "Change %c marks to (new mark): " old)
                      (read-char))))
     (list old new)))
  (when (eq new ?\n)
    (error "Mark character \\n is not allowed"))
  (let ((default-mark-p (equal tablist-marker-char new))
        (tablist-marker-char old))
    (save-excursion
      (tablist-map-over-marks
       (lambda nil
         (pcase new
           (?D
            (tablist-flag-forward 1))
           (_
            (let ((tablist-marker-char new)
                  (tablist-marked-face
                   (and default-mark-p
                        tablist-marked-face)))
              (tablist-put-mark)))))))))

(defun tablist-unmark-all-marks (&optional marks interactive)
  "Remove all marks in MARKS.

MARKS should be a string of mark characters to match and defaults
to all marks.  Interactively, remove all marks, unless a prefix
arg was given, in which case ask about which ones to remove.
Give a message, if interactive is non-nil.

Returns the number of unmarked marks."
  (interactive
   (list (if current-prefix-arg
             (read-string "Remove marks: ")) t))
  (let ((re (if marks
                (tablist-marker-regexp marks)
              "^[^ ]"))
        (removed 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let ((tablist-marker-char ?\s)
              tablist-marker-face
              tablist-marked-face)
          (tablist-put-mark))
        (cl-incf removed)))
    (when interactive
      (message "Removed %d marks" removed))
    removed))

(defun tablist-toggle-marks ()
  "Unmark all marked and mark all unmarked entries.

See `tablist-put-mark'."
  (interactive)
  (let ((marked-re (tablist-marker-regexp))
        (not-marked-re
         (let ((tablist-marker-char ?\s))
           (tablist-marker-regexp))))
    (save-excursion
      (goto-char (point-min))
      (tablist-skip-invisible-entries)
      (while (not (eobp))
        (cond
         ((looking-at marked-re)
          (save-excursion (tablist-unmark-backward -1)))
         ((looking-at not-marked-re)
          (tablist-put-mark)))
        (tablist-forward-entry)))
    (tablist-move-to-major-column)))

(defun tablist-get-marked-items (&optional arg distinguish-one-marked)
  "Return marked or ARG entries."
  (let ((items (save-excursion
                 (tablist-map-over-marks
                  (lambda () (cons (tabulated-list-get-id)
                                   (tabulated-list-get-entry)))
                  arg nil distinguish-one-marked))))
    (if (and distinguish-one-marked
             (eq (car items) t))
        items
      (nreverse items))))

(defun tablist-mark-items-regexp (column-name regexp)
  "Mark entries matching REGEXP in column COLUMN-NAME."
  (interactive
   (tablist-read-regexp-filter "Mark" current-prefix-arg ))
  (tablist-map-with-filter
   'tablist-put-mark
   `(=~  ,column-name ,regexp)))

(defun tablist-mark-items-numeric (binop column-name operand)
  "Mark items fulfilling BINOP with arg OPERAND in column COLUMN-NAME.

First the column's value is coerced to a number N.  Then the test
proceeds as \(BINOP N OPERAND\)."
  (interactive
   (tablist-read-numeric-filter "Mark" current-prefix-arg))
  (tablist-map-with-filter
   'tablist-put-mark
   `(,binop ,column-name ,operand)))

(defun tablist-map-over-marks (fn &optional arg show-progress
                                  distinguish-one-marked)
  (prog1
      (cond
       ((and arg (integerp arg))
        (let (results)
          (tablist-repeat-over-lines
           arg
           (lambda ()
             (if show-progress (sit-for 0))
             (push (funcall fn) results)))
          (if (< arg 0)
              (nreverse results)
            results)))
       (arg
        ;; non-nil, non-integer ARG means use current item:
        (tablist-skip-invisible-entries)
        (unless (eobp)
          (list (funcall fn))))
       (t
        (cl-labels ((search (re)
                            (let (success)
                              (tablist-skip-invisible-entries)
                              (while (and (setq success
                                                (re-search-forward re nil t))
                                          (invisible-p (point)))
                                (tablist-forward-entry))
                              success)))
          (let ((regexp (tablist-marker-regexp))
                next-position results found)
            (save-excursion
              (goto-char (point-min))
              ;; remember position of next marked file before BODY
              ;; can insert lines before the just found file,
              ;; confusing us by finding the same marked file again
              ;; and again and...
              (setq next-position (and (search regexp)
                                       (point-marker))
                    found (not (null next-position)))
              (while next-position
                (goto-char next-position)
                (if show-progress (sit-for 0))
                (push (funcall fn) results)
                ;; move after last match
                (goto-char next-position)
                (forward-line 1)
                (set-marker next-position nil)
                (setq next-position (and (search regexp)
                                         (point-marker)))))
            (if (and distinguish-one-marked (= (length results) 1))
                (setq results (cons t results)))
            (if found
                results
              (unless (or (eobp) (invisible-p (point)))
                (list (funcall fn))))))))
    (tablist-move-to-major-column)))

(defun tablist-marker-regexp (&optional marks)
  "Return a regexp matching marks in MARKS.

MARKS should be a string of mark characters to match and defaults
to the current value of `tablist-marker-char' as a string."
  (concat (format "^[%s]"
                  (or marks (string tablist-marker-char)))))

(defun tablist-get-mark-state ()
  "Return the mark state of the entry at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([^ ]\\)")
      (let ((mark (buffer-substring
                   (match-beginning 1)
                   (match-end 1))))
        (tablist-move-to-major-column)
        (list (aref mark 0)
              (get-text-property 0 'face mark)
              (get-text-property (point) 'face))))))

(defun tablist-put-mark-state (state)
  "Set the mark of the entry at point according to STATE.

STATE is a return value of `tablist-get-mark-state'."
  (cl-destructuring-bind (tablist-marker-char
                          tablist-marker-face
                          tablist-marked-face)
      state
    (tablist-put-mark)))

(defun tablist-mark-prompt (arg items)
  "Return a string suitable for use in a tablist prompt."
  ;; distinguish-one-marked can cause the first element to be just t.
  (if (eq (car items) t) (setq items (cdr items)))
  (let ((count (length items)))
    (if (= count 1)
        (car items)
      ;; more than 1 item:
      (if (integerp arg)
          ;; abs(arg) = count
          ;; Perhaps this is nicer, but it also takes more screen space:
          ;;(format "[%s %d items]" (if (> arg 0) "next" "previous")
          ;;                        count)
          (format "[next %d item%s]"
                  arg (dired-plural-s arg))
        (format "%c [%d item%s]" dired-marker-char count
                (dired-plural-s count))))))

;;
;; *Movement
;;

(defun tablist-forward-entry (&optional n)
  "Move past the next N unfiltered entries."
  (unless n (setq n 1))
  (while (and (> n 0)
              (not (eobp)))
    (forward-line)
    (when (invisible-p (point))
      (tablist-skip-invisible-entries))
    (cl-decf n))
  (while (and (< n 0)
              (not (bobp)))
    (forward-line -1)
    (when (invisible-p (point))
      (tablist-skip-invisible-entries t))
    (cl-incf n))
  n)

(defun tablist-next-line (&optional n)
  (interactive "p")
  (when (and (< n 0)
             (save-excursion
               (end-of-line 0)
               (tablist-skip-invisible-entries t)
               (bobp)))
    (signal 'beginning-of-buffer nil))
  (when (and (> n 0)
             (save-excursion
               (tablist-forward-entry)
               (eobp)))
    (signal 'end-of-buffer nil))

  (let ((col (tablist-current-column)))
    (tablist-forward-entry (or n 1))
    (if col
        (tablist-move-to-column col)
      (tablist-move-to-major-column))))

(defun tablist-previous-line (&optional n)
  (interactive "p")
  (tablist-next-line (- (or n 1))))

(defun tablist-repeat-over-lines (arg function)
  "Call FUNCTION for the next ARG entries."
  ;; Move out of potentially invisible area.
  (tablist-skip-invisible-entries)
  (let ((pos (make-marker)))
    (while (and (> arg 0)
                (not (eobp)))
      (cl-decf arg)
      (save-excursion
        (tablist-forward-entry)
        (move-marker pos (1+ (point))))
      (unless (eobp)
        (save-excursion (funcall function)))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (cl-incf arg)
      (tablist-forward-entry -1)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (tablist-move-to-major-column)))

(defun tablist-move-to-column (n)
  "Move to the N'th list column."
  (interactive "p")
  (when (tabulated-list-get-id)
    (let ((columns (tablist-column-offsets)))
      (when (or (< n 0)
                (>= n (length columns)))
        (error "No such column: %s" n))
      (beginning-of-line)
      (forward-char (nth n columns))
      (when (and (plist-get (nthcdr 3 (elt tabulated-list-format n))
                            :right-align)
                 (not (= n (1- (length columns)))))
        (forward-char (1- (car (cdr (elt tabulated-list-format n)))))))))

(defun tablist-move-to-major-column (&optional first-skip-invisible-p)
  "Move to the first major column."
  (interactive (list t))
  (when first-skip-invisible-p
    (tablist-skip-invisible-entries))
  (tablist-move-to-column (car (tablist-major-columns))))

(defun tablist-forward-column (n)
  "Move n columns forward, while wrapping around."
  (interactive "p")
  (unless (tabulated-list-get-id)
    (error "No entry on this line"))
  (let* ((columns (tablist-column-offsets))
         (current (1- (length columns))))
    ;; find current column
    (while (and (>= current 0)
                (> (nth current columns)
                   (current-column)))
      (cl-decf current))
    ;; there may be an invisible spec here
    (when (bolp)
      (forward-char))
    ;; before any columns
    (when (< current 0)
      (goto-char (+ (point-at-bol) (if (> n 0)
                                       (car columns)
                                     (car (last columns)))))
      (setq n (* (cl-signum n) (1- (abs n)))))
    (when (/= n 0)
      (tablist-move-to-column
       (mod (+ current n) (length columns))))))

(defun tablist-backward-column (n)
  "Move n columns backward, while wrapping around."
  (interactive "p")
  (tablist-forward-column (- n)))

;;
(defun tablist-skip-invisible-entries (&optional backward)
  "Skip invisible entries BACKWARD or forward.

Do nothing, if the entry at point is visible.  Otherwise move to
the beginning of the next visible entry in the direction
determined by BACKWARD.

Return t, if point is now in a visible area."

  (cond
   ((and backward
         (not (bobp))
         (get-text-property (point) 'invisible))
    (when (get-text-property (1- (point)) 'invisible)
      (goto-char (previous-single-property-change
                  (point)
                  'invisible nil (point-min))))
    (forward-line -1))
   ((and (not backward)
         (not (eobp))
         (get-text-property (point) 'invisible))
    (goto-char (next-single-property-change
                (point)
                'invisible nil (point-max)))))
  (not (invisible-p (point))))

;;
;; *Operations
;;

(defun tablist-yes-or-no-p (operation arg items)
  "Query the user whether to proceed with some operation.

Operation should be a symbol or string describing the operation,
ARG the prefix-arg of the command used in
`tablist-get-marked-items' or elsewhere, to get the ITEMS."

  (let ((pp-items (mapcar 'tablist-pretty-print-entry
                          (mapcar 'cdr items)))
        dired-no-confirm
        (op-str (upcase-initials
                 (if (stringp operation)
                     operation
                   (symbol-name operation)))))
    (dired-mark-pop-up
     (format " *%s*" op-str) nil
     pp-items
     dired-deletion-confirmer
     (format "%s %s "
             op-str
             (tablist-mark-prompt arg pp-items)))))

(defun tablist-operation-available-p (op)
  (and (functionp tablist-operations-function)
       (memq op (funcall tablist-operations-function
                         'supported-operations))))

(defun tablist-do-delete (&optional arg)
  "Delete ARG entries."
  (interactive "P")
  (unless (tablist-operation-available-p 'delete)
    (error "Deleting entries is not available in this buffer"))
  (let ((items (tablist-get-marked-items arg)))
    (when (tablist-yes-or-no-p 'delete arg items)
      (tablist-do-kill-lines arg)
      (funcall tablist-operations-function
               'delete (mapcar 'car items))
      (tablist-move-to-major-column))))

(defun tablist-do-flagged-delete (&optional interactive)
  "Delete all entries marked with a D."
  (interactive "p")
  (let* ((tablist-marker-char ?D))
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward (tablist-marker-regexp) nil t))
        (tablist-do-delete)
      (or (not interactive)
          (message "(No deletions requested)")))))

(defun tablist-do-kill-lines (&optional arg interactive)
  "Remove ARG lines from the display."
  (interactive (list current-prefix-arg t))
  (save-excursion
    (let ((positions
           (tablist-map-over-marks 'point arg))
          (inhibit-read-only t))
      (dolist (pos positions)
        (goto-char pos)
        (tabulated-list-delete-entry))
      (when interactive
        (message (format "Killed %d line%s"
                         (length positions)
                         (dired-plural-s (length positions))))))))

(defun tablist-do-operation (arg fn operation &optional delete-p revert-p)
  "Operate on marked items.

ARG should be the `current-prefix-arg', FN is a function of two
arguments \(ID ENTRY\) handling the operation.  It gets called
repeatedly with all marked items.  OPERATION is a symbol or string
describing the operation, it is used for display.

Optional non-nil DELETE-P means, remove the items from the display.
Optional REVERT-P means, revert the display afterwards."
  (let ((items (tablist-get-marked-items arg)))
    (unless items
      (error "No items marked"))
    (when (tablist-yes-or-no-p operation arg items)
      (when delete-p
        (tablist-do-kill-lines arg))
      (dolist (item items)
        (funcall fn (car item)))
      (when revert-p
        (tablist-revert))
      (tablist-move-to-major-column))))

;;
;; *Editing
;;
(defvar tablist-edit-column-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap (current-global-map))
    (define-key kmap [remap self-insert-command] #'self-insert-command)
    (define-key kmap "\r" #'tablist-edit-column-commit)
    (define-key kmap (kbd "C-g") #'tablist-edit-column-quit)
    (define-key kmap (kbd "C-c C-c") #'tablist-edit-column-commit)
    (define-key kmap (kbd "C-c C-q") #'tablist-edit-column-quit)
    (define-key kmap "\t" #'tablist-edit-column-complete)
    (define-key kmap (kbd "TAB") #'tablist-edit-column-complete)
    (define-key kmap [remap end-of-buffer] #'end-of-line)
    (define-key kmap [remap beginning-of-buffer] #'beginning-of-line)
    (define-key kmap [remap mark-whole-buffer] #'tablist-edit-column-mark-field)
    kmap))

(define-minor-mode tablist-edit-column-minor-mode
  "Toggle tablist-edit-column minor mode."
  :global nil
  (unless (or tablist-minor-mode
              (derived-mode-p 'tablist-mode))
    (error "Not in a tablist buffer"))
  (cond
   (tablist-edit-column-minor-mode
    (add-to-list 'mode-line-misc-info
                 '(tablist-edit-column-minor-mode "[edit]"))
    (add-hook 'post-command-hook 'tablist-edit-column-constrain-point nil t)
    (read-only-mode -1))
   (t
    (remove-hook 'post-command-hook 'tablist-edit-column-constrain-point t)
    (read-only-mode 1))))

(defun tablist-edit-column (&optional n)
  (interactive "P")
  (unless n (setq n (tablist-current-column)))
  (tablist-assert-column-editable n)
  (let* ((offsets (append (tablist-column-offsets)
                          (list (- (point-at-eol)
                                   (point-at-bol)))))
         (beg (+ (point-at-bol)
                 (nth n offsets)))
         (end (+ (point-at-bol)
                 (nth (1+ n) offsets)))
         (entry (tabulated-list-get-entry beg))
         (inhibit-read-only t)
         (inhibit-field-text-motion t)
         (alist `((entry . ,entry)
                  (column . ,n)
                  (id . ,(tabulated-list-get-id beg))))
         ov)
    (goto-char beg)
    (delete-region beg end)
    (add-text-properties
     (point-at-bol) (point-at-eol)
     '(read-only t field t))
    (unless (= beg (point-at-bol))
      (put-text-property (1- beg) beg 'rear-nonsticky t))
    (save-excursion
      ;; Keep one read-only space at the end for keeping text
      ;; properties.
      (insert
       (propertize
        (concat
         (tablist-nth-entry n entry)
         (propertize " "
                     'display `(space :align-to ,(- end (point-at-bol)))))
        'field nil
        'front-sticky '(tablist-edit)
        'rear-nonsticky '(read-only field)
        'tablist-edit alist))
      (setq end (point)))
    (add-text-properties
     (1- end) end '(read-only t field 'tablist-edit-end))
    (setq ov (make-overlay beg end))
    (overlay-put ov 'priority 9999)
    (overlay-put ov 'face '(:background "deep sky blue" :foreground "white"))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'tablist-edit t)
    (tablist-edit-column-minor-mode 1)))

(defun tablist-edit-column-quit ()
  (interactive)
  (tablist-edit-column-commit t))

(defun tablist-edit-column-commit (&optional abandon-edit)
  (interactive (list current-prefix-arg))
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t)
        bounds)
    (condition-case nil
        (setq bounds (tablist-edit-column-bounds))
      (error
       (tablist-edit-column-minor-mode -1)
       (tabulated-list-revert)
       (put-text-property (point-min) (point-max)
                          'tablist-edit nil)
       (error "Unable to complete the edit")))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (alist (get-text-property beg 'tablist-edit))
           (column (cdr (assq 'column alist)))
           (id (cdr (assq 'id alist)))
           (entry (cdr (assq 'entry alist)))
           (item (buffer-substring-no-properties beg (1- end))))

      (unless abandon-edit
        ;; Throws an error, if item is invalid.
        (setq entry (funcall tablist-operations-function
                             'edit-column id column item)))
      (tablist-edit-column-minor-mode -1)
      (remove-overlays beg end 'tablist-edit t)
      (put-text-property beg end 'tablist-edit nil)
      (delete-region (point-at-bol) (1+ (point-at-eol)))
      (save-excursion
        (tabulated-list-print-entry id entry))
      (forward-char (nth column (tablist-column-offsets))))))

(defun tablist-edit-column-complete ()
  (interactive)
  (unless (tablist-operation-available-p 'complete)
    (error "Completion not available"))
  (cl-destructuring-bind (beg &rest end)
      (tablist-edit-column-bounds t)
    (let* ((string (buffer-substring-no-properties
                    beg end))
           (alist (get-text-property beg 'tablist-edit))
           (completions (funcall tablist-operations-function
                                 'complete
                                 (cdr (assq 'id alist))
                                 (cdr (assq 'column alist))
                                 string
                                 (- (point) beg))))
      (unless completions
        (error "No completions available"))
      (completion-in-region beg end completions))))

(defun tablist-column-editable (n)
  (and (tablist-operation-available-p 'edit-column)
       (not (tablist-column-property n :read-only))))

(defun tablist-assert-column-editable (n)
  (unless (and (>= n 0)
               (< n (length tabulated-list-format)))
    (error "Invalid column number: %s" n))
  (unless (tablist-operation-available-p 'edit-column)
    (error "Editing columns not enabled in this buffer"))
  (when (tablist-column-property n :read-only)
    (error "This column is read-only")))

(defun tablist-edit-column-constrain-point ()
  (unless tablist-edit-column-minor-mode
    (error "Not editing a column"))
  (unless (get-text-property (point) 'tablist-edit)
    (let ((bounds (tablist-edit-column-bounds)))
      (when bounds
        (if (> (point) (cdr bounds))
            (goto-char (1- (cdr bounds)))
          (goto-char (car bounds)))
        (point)))))

(defun tablist-edit-column-bounds (&optional skip-final-space)
  (unless tablist-edit-column-minor-mode
    (error "Not editing a column"))
  (let ((pos (next-single-property-change
              (point) 'tablist-edit))
        beg end)
    (cond
     ((null pos)
      (setq end (previous-single-property-change
                 (point-max) 'tablist-edit)
            beg (previous-single-property-change
                 end 'tablist-edit)))
     ((get-text-property pos 'tablist-edit)
      (setq beg pos
            end (next-single-property-change
                 pos 'tablist-edit)))
     (pos
      (setq end pos
            beg (previous-single-property-change
                 pos 'tablist-edit))))

    (unless (and beg end (get-text-property beg 'tablist-edit))
      (error "Unable to locate edited text"))
    (cons beg (if skip-final-space (1- end) end))))

(defun tablist-edit-column-mark-field ()
  (interactive)
  (push-mark (field-beginning))
  (push-mark (field-end) nil t)
  (goto-char (field-beginning)))

(defun tablist-find-entry (&optional id)
  (interactive)
  (unless (tablist-operation-available-p 'find-entry)
    (error "Finding entries not supported in this buffer"))
  (funcall tablist-operations-function
           'find-entry
           (or id (tabulated-list-get-id))))

;;
;; *Utility
;;

(defun tablist-column-property (n prop)
  (plist-get
   (nthcdr 3 (aref tabulated-list-format n))
   prop))

(defun tablist-current-column ()
  "Return the column number at point.

Returns nil, if point is before the first column."
  (let ((column
         (1- (cl-position
              (current-column)
              (append (tablist-column-offsets)
                      (list most-positive-fixnum))
              :test (lambda (column offset) (> offset column))))))
    (when (>= column 0)
      column)))

(defun tablist-column-offsets ()
  "Return a list of column positions.

This is a list of offsets from the beginning of the line."
  (let ((cc tabulated-list-padding)
        columns)
    (dotimes (i (length tabulated-list-format))
      (let* ((c (aref tabulated-list-format i))
             (len (nth 1 c))
             (pad (or (plist-get (nthcdr 3 c) :pad-right)
                      1)))
        (push cc columns)
        (when (numberp len)
          (cl-incf cc len))
        (when pad
          (cl-incf cc pad))))
    (nreverse columns)))

(defun tablist-pretty-print-entry (item)
  (mapconcat (lambda (i)
               (tablist-nth-entry i item))
             (tablist-major-columns) " "))

(defun tablist--save-face-property (beg end)
  ;; We need to distinguish ,,not set'' from ''no face''.
  (unless (and (text-property-not-all beg end 'face nil)
               (< beg end))
    (put-text-property beg (1+ beg) 'face 'default))
  (unless (text-property-not-all beg end 'tablist-saved-face nil)
    (tablist-copy-text-property beg end 'face 'tablist-saved-face)))

(defun tablist--restore-face-property (beg end)
  (when (text-property-not-all beg end 'tablist-saved-face nil)
    (tablist-copy-text-property beg end 'tablist-saved-face 'face)))

(defun tablist-copy-text-property (beg end from to)
  "Copy text property FROM to TO in region BEG to END."
  (let ((inhibit-read-only t))
    (save-excursion
      (while (< beg end)
        (goto-char beg)
        (put-text-property
         (point)
         (setq beg (next-single-property-change
                    (point) from nil end))
         to
         (get-text-property (point) from))))))

;;
(defun tablist-read-column-name (arg &optional prompt default)
  "Read the name of a column using ARG.

If ARG is a number, return column ARG.
If ARG is nil, return DEFAULT or the current column.
Else ask the user, using PROMPT and DEFAULT."
  (cond
   ((numberp arg)
    (or (tablist-column-name
         (prefix-numeric-value arg))
        (error "No such column: %d" (prefix-numeric-value arg))))
   ((null arg)
    (or default
        (tablist-column-name
         (or (tablist-current-column)
             (car (tablist-major-columns))
             0))))
   (t
    (let* ((default (or default
                        (tablist-column-name
                         (car (tablist-major-columns)))))
           (result
            (completing-read
             (format "%s %s: "
                     (or prompt "Use column")
                     (if default
                         (format "(default %s)"
                                 default)
                       ""))
             (tablist-column-names)
             nil t nil 'tablist-column-name-history)))
      (if (> (length result) 0)
          result
        (if (not default)
            (error "No column selected")
          default))))))

(defun tablist-column-name (n)
  "Return the name of column N."
  (when (and n
             (>= n 0)
             (< n (length tabulated-list-format)))
    (substring-no-properties
     (car (elt tabulated-list-format n)) 0)))

(defun tablist-column-names ()
  "Return a list of all column-names."
  (mapcar 'tablist-column-name
          (number-sequence 0 (1- (length tabulated-list-format)))))

(defun tablist-nth-entry (n &optional entry)
  (unless entry (setq entry (tabulated-list-get-entry)))
  (when (and entry
             (>= n 0)
             (< n (length entry)))
    (let ((str (elt entry n)))
      (if (stringp str)
          str
        (car str)))))

(defun tablist-major-column-name ()
  "Return a list of the major column names."
  (tablist-column-name (car (tablist-major-columns))))

(defun tablist-export-csv (&optional separator always-quote-p
                                     invisible-p out-buffer display-p)
  "Export a tabulated list to a CSV format.

Use SEPARATOR (or ;) and quote if necessary (or always if
ALWAYS-QUOTE-P is non-nil).  Only consider non-filtered entries,
unless invisible-p is non-nil.  Create a buffer for the output or
insert it after point in OUT-BUFFER.  Finally if DISPLAY-P is
non-nil, display this buffer.

Return the output buffer."

  (interactive (list nil t nil nil t))
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "Not in Tabulated List Mode"))
  (unless (stringp separator)
    (setq separator (string (or separator ?\;))))
  (let* ((outb (or out-buffer
                   (get-buffer-create
                    (format "%s.csv" (buffer-name)))))
         (escape-re (format "[%s\"\n]" separator))
         (header (tablist-column-names)))
    (unless (buffer-live-p outb)
      (error "Expected a live buffer: %s" outb))
    (cl-labels
        ((printit (entry)
                  (insert
                   (mapconcat
                    (lambda (e)
                      (unless (stringp e)
                        (setq e (car e)))
                      (if (or always-quote-p
                              (string-match escape-re e))
                          (concat "\""
                                  (replace-regexp-in-string "\"" "\"\"" e t t)
                                  "\"")
                        e))
                    entry separator))
                  (insert ?\n)))
      (with-current-buffer outb
        (let ((inhibit-read-only t))
          (erase-buffer)
          (printit header)))
      (save-excursion
        (goto-char (point-min))
        (unless invisible-p
          (tablist-skip-invisible-entries))
        (while (not (eobp))
          (let* ((entry (tabulated-list-get-entry)))
            (with-current-buffer outb
              (let ((inhibit-read-only t))
                (printit entry)))
            (if invisible-p
                (forward-line)
              (tablist-forward-entry)))))
      (if display-p
          (display-buffer outb))
      outb)))

;;

(defun tablist-enlarge-column (&optional column width)
  "Enlarge column COLUMN by WIDTH.

This function is lazy and therefore pretty slow."
  (interactive
   (list nil (* (prefix-numeric-value current-prefix-arg)
                3)))
  (unless column (setq column (tablist-current-column)))
  (unless column
    (error "No column given and no entry at point"))
  (unless width (setq width 1))
  (when (or (not (numberp column))
            (< column 0)
            (>= column (length tabulated-list-format)))
    (error "No such column: %d" column))
  (when (= column (1- (length tabulated-list-format)))
    (error "Can't resize last column"))

  (let* ((cur-width (cadr (elt tabulated-list-format column))))
    (setcar (cdr (elt tabulated-list-format column))
            (max 3 (+ cur-width width)))
    (tablist-with-remembering-entry
      (tablist-save-marks
       (tabulated-list-init-header)
       (tabulated-list-print)))))

(defun tablist-shrink-column (&optional column width)
  (interactive
   (list nil (* (prefix-numeric-value current-prefix-arg)
                3)))
  (tablist-enlarge-column column (- (or width 1))))

;; *Sorting
;;

(defun tablist-sort (&optional column)
  "Sort the tabulated-list by COLUMN.

COLUMN may be either a name or an index.  The default compare
function is given by the `tabulated-list-format', which see.

This function saves the current sort column and the inverse
sort-direction in the variable `tabulated-list-sort-key', which
also determines the default COLUMN and direction.

The main difference to `tabulated-list-sort' is, that this
function sorts the buffer in-place and it ignores a nil sort
entry in `tabulated-list-format' and sorts on the column
anyway (why not ?)."

  (interactive
   (list
    (if (null current-prefix-arg)
        (tablist-column-name
         (or (tablist-current-column)
             (car (tablist-major-columns))
             0))
      (tablist-read-column-name
       '(4) "Sort by column"
       (tablist-column-name (car (tablist-major-columns)))))))

  (unless column
    (setq column (or (car tabulated-list-sort-key)
                     (tablist-column-name (car (tablist-major-columns)))
                     (tablist-column-name 0))))
  (when (numberp column)
    (let ((column-name (tablist-column-name column)))
      (unless column-name
        (error "No such column: %d" column))
      (setq column column-name)))

  (setq tabulated-list-sort-key
        (cons column
              (if (equal column (car tabulated-list-sort-key))
                  (cdr tabulated-list-sort-key))))

  (let* ((entries (if (functionp tabulated-list-entries)
                      (funcall tabulated-list-entries)
                    tabulated-list-entries))
         (reverse (cdr tabulated-list-sort-key))
         (n (tabulated-list--column-number ;;errors if column is n/a
             (car tabulated-list-sort-key)))
         (compare-fn (nth 2 (aref tabulated-list-format n))))

    (when (or (null compare-fn)
              (eq compare-fn t))
      (setq compare-fn
            (lambda (a b)
              (setq a (aref (cadr a) n))
              (setq b (aref (cadr b) n))
              (string< (if (stringp a) a (car a))
                       (if (stringp b) b (car b))))))

    (unless compare-fn
      (error "This column cannot be sorted: %s" column))

    (setcdr tabulated-list-sort-key (not reverse))
    ;; Presort the entries and hash the result and sort the buffer.
    (setq entries (sort (copy-sequence entries) compare-fn))
    (let ((hash (make-hash-table :test 'equal)))
      (dotimes (i (length entries))
        (puthash (caar entries) i hash)
        (setq entries (cdr entries)))
      (tablist-with-remembering-entry
        (goto-char (point-min))
        (tablist-skip-invisible-entries)
        (let ((inhibit-read-only t))
          (sort-subr
           nil 'tablist-forward-entry 'end-of-line
           (lambda ()
             (gethash (tabulated-list-get-id) hash 0))
           nil (if reverse '< '>))))
      (tablist-move-to-column n)
      ;; Make the sort arrows display.
      (tabulated-list-init-header))))

;;
;; *Filter
;;

(defun tablist-read-filter-name (prompt)
  (let ((filter (cdr (assq major-mode tablist-named-filter))))
    (unless filter
      (error "No filter defined for %s mode" mode-name))
    (let ((name (completing-read
                 (format "%s: " prompt)
                 filter
                 nil t)))
      (unless (> (length name) 0)
        (error "No filter selected"))
      name)))

(defun tablist-apply-filter (&optional filter)
  "Apply FILTER to the current tabulated list.

FILTER defaults to `tablist-current-filter'."
  (unless filter (setq filter tablist-current-filter))
  (tablist-filter-unhide-buffer)
  (when (and filter
             (null tablist-filter-suspended))
    (tablist-with-remembering-entry
      (tablist-map-with-filter
       (lambda nil
         (if tablist-umark-filtered-entries
             (save-excursion (tablist-unmark-forward)))
         (tablist-filter-hide-entry))
       (tablist-filter-negate filter))))
  (force-mode-line-update))

(defadvice tabulated-list-print (after tabulated-list activate)
  "Reapply the filter."
  (when (or tablist-minor-mode
            (derived-mode-p 'tablist-mode))
    (tablist-apply-filter)))

(defun tablist-eval-filter (filter)
  (tablist-filter-eval
   filter
   (tabulated-list-get-id)
   (tabulated-list-get-entry)
   (cdr (assq major-mode tablist-named-filter))))

(defun tablist-map-with-filter (fn filter &optional show-progress
                                   distinguish-one-marked)
  "Call FN for every unfiltered entry matching FILTER."
  (prog1
      (cl-labels ((search ()
                          (tablist-skip-invisible-entries)
                          (while (and (not (eobp))
                                      (not (tablist-eval-filter filter)))
                            (tablist-forward-entry))
                          (unless (eobp)
                            (point-marker))))
        (let (next-position results)
          (save-excursion
            (goto-char (point-min))
            (setq next-position (search))
            (while next-position
              (goto-char next-position)
              (if show-progress (sit-for 0))
              (push (funcall fn) results)
              ;; move after last match
              (goto-char next-position)
              (forward-line 1)
              (set-marker next-position nil)
              (setq next-position (search)))
            (if (and distinguish-one-marked (= (length results) 1))
                (setq results (cons t results))))))))

;;
;; **Filter Commands
;;
(defun tablist-push-filter (filter &optional interactive or-p)
  (setq tablist-current-filter
        (tablist-filter-push
         tablist-current-filter
         filter or-p))
  (tablist-apply-filter)
  (when interactive
    (tablist-display-filter-temporarily)))

(defun tablist-pop-filter (&optional n interactive)
  "Remove the first N filter components."
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (while (and tablist-current-filter
              (> n 0))
    (setq tablist-current-filter
          (tablist-filter-pop
           tablist-current-filter))
    (cl-decf n))
  (tablist-apply-filter)
  (when interactive
    (when (> n 0)
      (message "The filter is empty."))
    (tablist-display-filter-temporarily))
  n)

(defun tablist-negate-filter (&optional interactive)
  "Negate the current filter."
  (interactive (list t))
  (setq tablist-current-filter
        (tablist-filter-negate
         tablist-current-filter))
  (tablist-apply-filter)
  (when interactive
    (tablist-display-filter-temporarily)))

(defun tablist-toggle-first-filter-logic ()
  "Toggle between and/or for the first filter operand."
  (interactive)
  (setq tablist-current-filter
        (pcase tablist-current-filter
          (`(or ,x1 ,x2)
           `(and ,x1 ,x2))
          (`(and ,x1 ,x2)
           `(or ,x1 ,x2))
          (`(not ,x) x)
          (x `(not ,x))))
  (tablist-apply-filter)
  (tablist-display-filter-temporarily))

(defun tablist-suspend-filter (&optional flag)
  "Temporarily disable filtering according to FLAG.

Interactively, this command toggles filtering."
  (interactive
   (list (not tablist-filter-suspended)))
  (let ((state tablist-filter-suspended))
    (unless (eq (not (not state))
                (not (not flag)))
      (set (make-local-variable 'tablist-filter-suspended) flag)
      (tablist-apply-filter))))

(defun tablist-read-regexp-filter (operation arg)
  (let ((column-name (tablist-read-column-name arg)))
    (list
     column-name
     (let ((re
            (read-regexp (format "%s where %s matches: " operation column-name))))
       (unless (> (length re) 0)
         (error "No regexp given"))
       re))))

(defun tablist-read-equal-filter (operation arg)
  (let ((column-name (tablist-read-column-name arg)))
    (list
     column-name
     (read-string (format "%s where %s equals: " operation column-name)))))

(defun tablist-read-numeric-filter (operation arg)
  (let* ((entry (tabulated-list-get-entry 1))
         (default (cl-some
                   (lambda (idx)
                     (let ((value (tablist-nth-entry idx entry)))
                       (when (or (not (eq 0 (string-to-number value)))
                                 (equal "0" value))
                         (tablist-column-name idx))))
                   (number-sequence 0 (length entry))))
         (column-name (tablist-read-column-name arg nil default))
         (op (completing-read
              (format "%s %s matching binary op: " operation column-name)
              '("=" "<" ">" "<=" ">=") nil t))
         oper)

    (when (equal "" op)
      (error "No operation selected"))
    (setq op (intern op))
    (setq oper (number-to-string
                (read-number
                 (format "%s where %s %s " operation column-name op))))

    (list op column-name oper)))

(defun tablist-push-regexp-filter (column-name regexp)
  "Add a new filter matching REGEXP in COLUMN-NAME.

The filter is and'ed with the current filter.  Use
`tablist-toggle-first-filter-logic' to change this."
  (interactive
   (tablist-with-filter-displayed
    (tablist-read-regexp-filter "Filter" current-prefix-arg)))
  (tablist-push-filter
   `(=~ ,column-name ,regexp)
   (called-interactively-p 'any)))

(defun tablist-push-equal-filter (column-name string)
  "Add a new filter whre string equals COLUMN-NAME's value.

The filter is and'ed with the current filter.  Use
`tablist-toggle-first-filter-logic' to change this."
  (interactive
   (tablist-with-filter-displayed
    (tablist-read-equal-filter "Filter" current-prefix-arg)))
  (tablist-push-filter
   `(== ,column-name ,string)
   (called-interactively-p 'any)))

(defun tablist-push-numeric-filter (op column-name 2nd-arg)
  "Add a new filter matching a numeric predicate.

The filter is and'ed with the current filter.  Use
`tablist-toggle-first-filter-logic' to change this."
  (interactive
   (tablist-with-filter-displayed
    (tablist-read-numeric-filter "Filter" current-prefix-arg)))
  (tablist-push-filter
   `(,op ,column-name ,2nd-arg)
   (called-interactively-p 'any)))

(defun tablist-push-named-filter (name)
  "Add a named filter called NAME.

Named filter are saved in the variable `tablist-named-filter'."
  (interactive
   (tablist-with-filter-displayed
    (list
     (tablist-read-filter-name "Add filter"))))
  (when (and name (symbolp name))
    (setq name (symbol-name name)))
  (tablist-push-filter name (called-interactively-p 'any)))

(defun tablist-delete-named-filter (name &optional mode)
  (interactive
   (tablist-with-filter-displayed
    (list
     (tablist-read-filter-name "Delete filter"))))
  (setq tablist-current-filter
        (tablist-filter-map
         (lambda (f)
           (when (equal f name)
             (setq f (tablist-get-named-filter f)))
           f)
         tablist-current-filter))
  (unless mode (setq mode major-mode))
  (let ((mode-filter
         (assq mode tablist-named-filter)))
    (when mode-filter
      (setcdr mode-filter
              (cl-remove name (cdr mode-filter)
                         :test 'equal :key 'car)))))

(defun tablist-name-current-filter (name)
  (interactive
   (list (tablist-with-filter-displayed
          (read-string
           "Add name for current filter: "))))
  (unless tablist-current-filter
    (error "Filter is empty"))
  (unless (> (length name) 0)
    (error "No name given"))
  (tablist-put-named-filter
   name (if (stringp tablist-current-filter)
            (tablist-get-named-filter
             tablist-current-filter)
          tablist-current-filter))
  (setq tablist-current-filter name)
  (force-mode-line-update))

(defun tablist-deconstruct-named-filter ()
  (interactive)
  (let (found)
    (setq tablist-current-filter
          (tablist-filter-map
           (lambda (f)
             (when (and (not found)
                        (stringp f))
               (setq found t)
               (let ((df (tablist-get-named-filter f)))
                 (unless df
                   (error "Filter is not defined: %s" f))
                 (setq f df)))
             f)
           tablist-current-filter))
    (unless found
      (error "No named filter found"))
    (force-mode-line-update)))

(defun tablist-filter-names (&optional mode)
  (mapcar 'car (cdr (assq (or mode major-mode)
                          tablist-named-filter))))

(defun tablist-get-named-filter (name &optional mode)
  (cdr (assoc name
              (cdr (assq (or mode major-mode)
                         tablist-named-filter)))))

(defun tablist-put-named-filter (name filter &optional mode)
  (unless mode (setq mode major-mode))
  (let ((mode-filter
         (assq mode tablist-named-filter)))
    (unless mode-filter
      (setq mode-filter (cons mode nil))
      (push mode-filter tablist-named-filter))
    (let ((entry (assoc name mode-filter)))
      (if entry
          (setcdr entry filter)
        (setcdr mode-filter
                (list (cons name filter)))))))

(defun tablist-validate-named-filter (filter)
  (tablist-filter-map
   (lambda (f)
     (when (and (stringp f)
                (null (tablist-get-named-filter f)))
       (error "Undefined named filter: %s (defined: %s)" f
              (mapconcat 'identity (tablist-filter-names) ","))))
   filter))

(defun tablist-edit-filter ()
  (interactive)
  (setq tablist-current-filter
        (tablist-with-filter-displayed
         (tablist-filter-edit-filter
          "Edit filter: "
          tablist-current-filter
          nil
          'tablist-validate-named-filter)))
  (tablist-apply-filter))

(defun tablist-clear-filter ()
  (interactive)
  (setq tablist-current-filter nil)
  (tablist-apply-filter))

;; **Displaying filter
;;

(defconst tablist-display-filter-mode-line-tag nil)

(defun tablist-display-filter (&optional flag)
  "Display the current filter according to FLAG.

If FLAG has the value 'toggle, toggle it's visibility.
If FLAG has the 'state, then do nothing but return the current
visibility."
  (interactive (list 'toggle))
  (let* ((tag 'tablist-display-filter-mode-line-tag)
         (displayed-p (not (not (assq tag mode-line-format)))))
    (if (eq flag 'state)
        displayed-p
      (let ((display-p (not (not (if (eq flag 'toggle)
                                     (not displayed-p)
                                   flag)))))
        (unless (eq displayed-p display-p)
          (setq mode-line-format
                (if display-p
                    (list (cons tag mode-line-format)
                          '(:eval
                            (replace-regexp-in-string
                             "%" "%%"
                             (concat
                              (propertize "Filter: " 'face 'minibuffer-prompt)
                              (and tablist-filter-suspended
                                   "[suspended] ")
                              (if tablist-current-filter
                                  (tablist-filter-unparse
                                   tablist-current-filter t)
                                "[none]")))))
                  (cdr (assq tag mode-line-format)))))
        (force-mode-line-update)
        display-p))))

(defun tablist-display-filter-temporarily ()
  (tablist-with-filter-displayed
   (sit-for 9999)))

;;
;; **Hiding/Unhiding Entries
;;
(defun tablist-filter-set-entry-hidden (flag &optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (let ((inhibit-read-only t))
      (add-text-properties
       (point-at-bol)
       (1+ (point-at-eol))
       `(invisible ,flag)))))

(defun tablist-filter-hide-entry (&optional pos)
  (interactive)
  (tablist-filter-set-entry-hidden t pos))

(defun tablist-filter-unhide-entry (&optional pos)
  (tablist-filter-set-entry-hidden nil pos))

(defun tablist-filter-unhide-buffer ()
  (let ((inhibit-read-only t))
    (remove-text-properties
     (point-min) (point-max)
     '(invisible))))

(defun tablist-window-attach (awindow &optional window)
  "Attach AWINDOW to WINDOW.

This has the following effect.  Whenever WINDOW, defaulting to
the selected window, stops displaying the buffer it currently
displays (e.g., by switching buffers or because it was deleted)
AWINDOW is deleted."
  (unless window (setq window (selected-window)))
  (let ((buffer (window-buffer window))
        (hook (make-symbol "window-attach-hook")))
    (fset hook
          (lambda ()
            (when (or (not (window-live-p window))
                      (not (eq buffer (window-buffer window))))
              (remove-hook 'window-configuration-change-hook
                           hook)
              ;; Deleting windows inside wcch may cause errors in
              ;; windows.el .
              (run-with-timer
               0 nil (lambda (win)
                       (when (and (window-live-p win)
                                  (not (eq win (selected-window))))
                         (delete-window win)))
               awindow))))
    (add-hook 'window-configuration-change-hook hook)))

(defun tablist-display-buffer-split-below-and-attach (buf alist)
  "Display buffer action using `tablist-window-attach'."
  (let ((window (selected-window))
        (height (cdr (assq 'window-height alist)))
        newwin)
    (when height
      (when (floatp height)
        (setq height (round (* height (frame-height)))))
      (setq height (- (max height window-min-height))))
    (setq newwin (window--display-buffer
                  buf
                  (split-window-below height)
                  'window alist))
    (tablist-window-attach newwin window)
    newwin))

(defun tablist-generate-sorter (column compare-fn &optional read-fn)
  "Generate a sort function for `tabulated-list' entries.

Example:

     \(tablist-generate-sorter 0 '< 'string-to-number\)

would create a sort function sorting `tabulated-list-entries' on
the 0-th column as numbers by the less-than relation."

  (lambda (e1 e2)
    (funcall compare-fn
             (funcall (or read-fn 'identity)
                      (aref (cadr e1) column))
             (funcall (or read-fn 'identity)
                      (aref (cadr e2) column)))))

(provide 'tablist)
;; Local Variables:
;; outline-regexp: ";;\\(\\(?:[;*]+ \\| \\*+\\)[^\s\t\n]\\|###autoload\\)\\|("
;; indent-tabs-mode: nil
;; End:
;;; tablist.el ends here
