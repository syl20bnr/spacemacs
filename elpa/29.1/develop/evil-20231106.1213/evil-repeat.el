;;; evil-repeat.el --- Repeat system -*- lexical-binding: t -*-

;; Author: Frank Fischer <frank.fischer at mathematik.tu-chemnitz.de>
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

;;; Commentary:

;; A repeat begins when leaving Normal state; it ends when re-entering
;; Normal state. The diagram below shows possible routes between
;; Normal state (N), Insert state (I), Visual state (V),
;; Operator-Pending state (O) and Replace state (R). (Emacs state
;; is an exception: nothing is repeated in that state.)
;;                              ___
;;                             /   \
;;                             | R |
;;                             \___/
;;                             ^   |
;;                             |   |
;;               ___           |___V           ___
;;              /   \ <------- /   \ -------> /   \
;;              | V |          | N |          | O |
;;              \___/ -------> \___/ <------- \___/
;;                  |          |   ^          |
;;                  |          |   |          |
;;                  |          V___|          |
;;                  |          /   \          |
;;                  +--------> | I | <--------+
;;                             \___/
;;
;; The recording of a repeat is started in one of two cases: Either a
;; command is about to be executed (in pre-command-hook) or normal
;; state is exited. The recording is stopped whenever a command has
;; been completed and evil is in normal state afterwards. Therefore,
;; a non-inserting command in normal-state is recorded as a single
;; repeat unit. In contrast, if the command leaves normal state and
;; starts insert-state, all commands that are executed until
;; insert-state is left and normal state is reactivated are recorded
;; together in one repeat unit. In other words, a repeat unit consists
;; of all commands that are executed starting and ending in normal
;; state.
;;
;; Not all commands are recorded. There are several commands that are
;; completely ignored and other commands that even abort the currently
;; active recording, e.g., commands that switch buffer.
;;
;; During recording the repeat information is appended to the variable
;; `evil-repeat-info', which is cleared when the recording
;; starts. This accumulated repeat information is put into the
;; `evil-repeat-ring' when the recording is finished. The dot command,
;; `\[evil-repeat]' (`evil-repeat') replays the most recent entry in
;; the ring, preceeding repeats can be replayed using
;; `\[evil-repeat-pop]' (`evil-repeat-pop').
;;
;; Repeat information can be stored in almost arbitrary form. How the
;; repeat information for each single command is recorded is
;; determined by the :repeat property of the command. This property
;; has the following interpretation:
;;
;; t         record commands by storing the key-sequence that invoked it
;; nil       ignore this command completely
;; ignore    synonym to nil
;; motion    command is recorded by storing the key-sequence but only in
;;           insert state, otherwise it is ignored.
;; abort     stop recording of repeat information immediately
;; change    record commands by storing buffer changes
;; SYMBOL    if SYMBOL is contained as key in `evil-repeat-types' call
;;           the corresponding (function-)value, otherwise call the
;;           function associated with SYMBOL. In both cases the
;;           function should take exactly one argument which is either
;;           `pre', `pre-read-key-sequence' or `post' specifying on
;;           whether the function is called before or after the
;;           execution of the command.
;;
;; Therefore, using a certain SYMBOL one can write specific repeation
;; functions for each command.
;;
;; Each value of ring `evil-repeat-info', i.e., each single repeat
;; information must be one of the following two possibilities:
;; If element is a sequence, it is regarded as a key-sequence to
;; be repeated. Otherwise the element must be a list
;; (FUNCTION PARAMS ...) which will be called using
;; (apply FUNCTION PARAMS) whenever this repeat is being executed.
;;
;; A user supplied repeat function can use the functions
;; `evil-record-repeat' to append further repeat-information of the
;; form described above to `evil-repeat-info'. See the implementation
;; of `evil-repeat-keystrokes' and `evil-repeat-changes' for examples.
;;
;; The repeat information is executed with `evil-execute-repeat-info',
;; which passes key-sequence elements to `execute-kbd-macro' and
;; executes other elements as defined above.  A special version is
;; `evil-execute-repeat-info-with-count'.  This function works as
;; `evil-execute-repeat-info', but replaces the count of the first
;; command. This is done by parsing the key-sequence, ignoring all
;; calls to `digit-prefix-argument' and `negative-argument', and
;; prepending the count as a string to the vector of the remaining
;; key-sequence.

;;; Code:

(require 'evil-states)

(defmacro evil-without-repeat (&rest body)
  (declare (indent defun)
           (debug t))
  `(let ((pre-command-hook (remq 'evil-repeat-pre-hook pre-command-hook))
         (post-command-hook (remq 'evil-repeat-post-hook post-command-hook)))
     ,@body
     (evil-repeat-abort)))

(defsubst evil-repeat-recording-p ()
  "Return non-nil iff a recording is in progress."
  (eq evil-recording-repeat t))

(defun evil-repeat-start ()
  "Start recording a new repeat into `evil-repeat-info'."
  (evil-repeat-reset t)
  (evil-repeat-record-buffer)
  (when (evil-visual-state-p)
    (let* ((range (evil-visual-range))
           (beg (evil-range-beginning range))
           (end (1- (evil-range-end range)))
           (nfwdlines (evil-count-lines beg end)))
      (evil-repeat-record
       (cond
        ((eq evil-visual-selection 'char)
         (list #'evil-repeat-visual-char
               nfwdlines
               (- end
                  (if (zerop nfwdlines)
                      beg
                    (save-excursion
                      (goto-char end)
                      (line-beginning-position))))))
        ((eq evil-visual-selection 'line)
         (list #'evil-repeat-visual-line nfwdlines))
        ((eq evil-visual-selection 'block)
         (list #'evil-repeat-visual-block
               nfwdlines
               (abs (- (evil-column beg) (evil-column end))))))))))

(defun evil-repeat-stop ()
  "Stop recording a repeat.
Update `evil-repeat-ring' with the accumulated changes
in `evil-repeat-info' and clear variables."
  (unwind-protect
      (when (evil-repeat-recording-p)
        (setq evil-repeat-info (evil-normalize-repeat-info evil-repeat-info))
        (when (and evil-repeat-info evil-repeat-ring)
          (ring-insert evil-repeat-ring evil-repeat-info)))
    (evil-repeat-reset nil)))

(defun evil-repeat-abort ()
  "Abort current repeation."
  (evil-repeat-reset 'abort))

(defun evil-repeat-reset (flag)
  "Clear all repeat recording variables.
Set `evil-recording-repeat' to FLAG."
  (setq evil-recording-repeat flag
        evil-repeat-info nil
        evil-repeat-buffer nil))

(defsubst evil-repeat-record-position (&optional pos)
  "Set `evil-repeat-pos' to POS or point."
  (setq evil-repeat-pos (or pos (point))))

(defun evil-repeat-record-buffer ()
  "Set `evil-repeat-buffer' to the current buffer."
  (unless (minibufferp)
    (setq evil-repeat-buffer (current-buffer))))

(defmacro evil-save-repeat-info (&rest body)
  "Execute BODY, protecting the values of repeat variables."
  (declare (indent defun)
           (debug t))
  `(let (evil-repeat-ring
         evil-recording-repeat
         evil-recording-current-command
         evil-repeat-info
         evil-repeat-changes
         evil-repeat-pos
         evil-repeat-keys
         evil-repeat-buffer
         this-command
         last-command)
     ,@body))

(defun evil-repeat-different-buffer-p (&optional strict)
  "Whether the buffer has changed in a repeat.
If STRICT is non-nil, returns t if the previous buffer
is unknown; otherwise returns t only if the previous
buffer is known and different from the current buffer."
  (and (or (buffer-live-p evil-repeat-buffer) strict)
       (not (minibufferp))
       (not (eq (current-buffer) evil-repeat-buffer))))

(defvar evil-repeat-types
  '((t . evil-repeat-keystrokes)
    (change . evil-repeat-changes)
    (motion . evil-repeat-motion)
    (insert-at-point . evil-repeat-insert-at-point)
    (ignore . nil))
  "Alist of defined repeat-types.")

(defun evil--repeat-type (command)
  "Return the :repeat property of COMMAND."
  (when (functionp command) ; ignore keyboard macros
    (let* ((type (evil-get-command-property command :repeat t))
           (repeat-type (assq type evil-repeat-types)))
      (if repeat-type (cdr repeat-type) type))))

(defun evil-repeat-type (command &optional default)
  "Return the :repeat property of COMMAND.
If COMMAND doesn't have this property, return DEFAULT."
  (declare (obsolete evil--repeat-type "1.15.0"))
  (when (functionp command) ; ignore keyboard macros
    (let* ((type (evil-get-command-property command :repeat default))
           (repeat-type (assq type evil-repeat-types)))
      (if repeat-type (cdr repeat-type) type))))

(defun evil-repeat-force-abort-p (repeat-type)
  "Whether the current command should abort the recording of repeat information.
Return non-nil if so."
  (or (evil-repeat-different-buffer-p)  ; ... buffer changed
      (eq repeat-type 'abort)           ; ... explicitely forced
      (eq evil-recording-repeat 'abort) ; ... already aborted
      (evil-emacs-state-p)              ; ... in Emacs state
      (and (evil-mouse-events-p         ; ... mouse events
            (this-command-keys-vector))
           (eq repeat-type nil))
      (minibufferp)))                   ; ... minibuffer activated

(defun evil-repeat-record (info)
  "Append INFO to `evil-repeat-info'."
  (when (evil-repeat-recording-p)
    (setq evil-repeat-info (nconc evil-repeat-info (list info)))))

;; called from `pre-command-hook'
(defun evil-repeat-pre-hook ()
  "Prepare the current command for recording the repeation."
  (when evil-local-mode
    (let ((repeat-type (evil--repeat-type this-command)))
      (cond
       ;; abort the repeat
       ((evil-repeat-force-abort-p repeat-type)
        ;; We mark the current record as being aborted, because there
        ;; may be further pre-hooks following before the post-hook is
        ;; called.
        (evil-repeat-abort))
       ;; ignore those commands completely
       ((or (null repeat-type)
            (evil-mouse-events-p (this-command-keys-vector))))
       ;; record command
       (t
        ;; In normal-state or visual state, each command is a single
        ;; repeation, therefore start a new repeation.
        (when (or (evil-normal-state-p)
                  (evil-visual-state-p))
          (evil-repeat-start))
        (setq evil-recording-current-command t)
        (funcall repeat-type 'pre))))))
(put 'evil-repeat-pre-hook 'permanent-local-hook t)

;; called from `post-command-hook'
(defun evil-repeat-post-hook ()
  "Finish recording of repeat-information for the current-command."
  (when (and evil-local-mode evil-recording-repeat)
    (let ((repeat-type (evil--repeat-type this-command)))
      (cond
       ;; abort the repeat
       ((evil-repeat-force-abort-p repeat-type)
        ;; The command has been aborted but is complete, so just reset
        ;; the recording state.
        (evil-repeat-reset nil))
       ;; ignore if command should not be recorded or the current
       ;; command is not being recorded
       ((or (null repeat-type)
            (not evil-recording-current-command)))
       ;; record command
       (t
        (funcall repeat-type 'post)
        ;; In normal state, the repeat sequence is complete, so record it.
        (when (evil-normal-state-p)
          (evil-repeat-stop)))))
    ;; done with recording the current command
    (setq evil-recording-current-command nil)))
(put 'evil-repeat-post-hook 'permanent-local-hook t)

(defun evil-clear-command-keys ()
  "Clear `this-command-keys' and all information about the current command keys.
Calling this function prevents further recording of the keys that
invoked the current command"
  (clear-this-command-keys t)
  (setq evil-repeat-keys ""))

(defun evil-this-command-keys (&optional post-cmd)
  "Version of `this-command-keys' with finer control over prefix args."
  (vconcat
   (let ((arg (if post-cmd current-prefix-arg prefix-arg)))
     (and (numberp arg)
          ;; Only add prefix if no repeat info recorded yet
          (null evil-repeat-info)
          (number-to-string arg)))
   (this-single-command-keys)))

(defun evil-repeat-keystrokes (flag)
  "Repeation recording function for commands that are repeated by keystrokes."
  (cond
   ((eq flag 'pre)
    (when evil-this-register
      (evil-repeat-record
       `(set evil-this-register ,evil-this-register)))
    (setq evil-repeat-keys (evil-this-command-keys)))
   ((memq flag '(post pre-read-key-sequence))
    (evil-repeat-record (if (zerop (length (evil-this-command-keys t)))
                            evil-repeat-keys
                          (evil-this-command-keys t)))
    ;; erase commands keys to prevent double recording
    (evil-clear-command-keys))))

(defun evil-repeat-motion (flag)
  "Repetition for motions.
Motions are recorded by keystroke but only in Insert state."
  (when (memq evil-state '(insert replace))
    (evil-repeat-keystrokes flag)))

(defun evil-repeat-changes (flag)
  "Repeation recording function for commands that are repeated by buffer changes."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'evil-repeat-change-hook nil t)
    (evil-repeat-start-record-changes))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'evil-repeat-change-hook t)
    (evil-repeat-finish-record-changes))))

;; called from the `after-change-functions' hook
(defun evil-repeat-change-hook (beg end length)
  "Record change information for current command."
  (let ((repeat-type (evil--repeat-type this-command)))
    (when (and (evil-repeat-recording-p)
               (eq repeat-type 'evil-repeat-changes)
               (not (evil-emacs-state-p))
               (not (evil-repeat-different-buffer-p t))
               evil-state)
      (unless (evil-repeat-recording-p)
        (evil-repeat-start))
      (evil-repeat-record-change (- beg evil-repeat-pos)
                                 (buffer-substring beg end)
                                 length))))
(put 'evil-repeat-change-hook 'permanent-local-hook t)

(defun evil-repeat-record-change (relpos ins ndel)
  "Record the current buffer changes during a repeat.
If CHANGE is specified, it is added to `evil-repeat-changes'."
  (when (evil-repeat-recording-p)
    (push (list relpos ins ndel) evil-repeat-changes)))

(defun evil-repeat-start-record-changes ()
  "Start the recording of a new set of buffer changes."
  (setq evil-repeat-changes nil)
  (evil-repeat-record-position))

(defun evil-repeat-finish-record-changes ()
  "Finish the recording of buffer changes and record them as repeat."
  (when (evil-repeat-recording-p)
    (evil-repeat-record `(evil-execute-change
                          ,(nreverse evil-repeat-changes)
                          ,(- (point) evil-repeat-pos)))
    (setq evil-repeat-changes nil)))

(defun evil-repeat-insert-at-point (flag)
  "Repeation recording function for commands that insert text in region.
For example `mouse-yank-primary'. This records text insertion when a command
inserts some text in a buffer between (point) and (mark)."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'evil-repeat-insert-at-point-hook nil t))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'evil-repeat-insert-at-point-hook t))))

(defun evil-repeat-insert-at-point-hook (beg end _length)
  (let ((repeat-type (evil--repeat-type this-command)))
    (when (and (evil-repeat-recording-p)
               (eq repeat-type 'evil-repeat-insert-at-point)
               (not (evil-emacs-state-p))
               (not (evil-repeat-different-buffer-p t))
               evil-state)
      (setq evil-repeat-pos beg)
      (evil-repeat-record (list 'insert (buffer-substring beg end))))))
(put 'evil-repeat-insert-at-point-hook 'permanent-local-hook t)

(defun evil-normalize-repeat-info (repeat-info)
  "Concatenate consecutive arrays in REPEAT-INFO.
Return a single array."
  (let* ((result (cons nil nil))
         (result-last result)
         cur cur-last)
    (dolist (rep repeat-info)
      (cond
       ((null rep))
       ((arrayp rep)
        (setq rep (listify-key-sequence rep))
        (cond
         (cur
          (setcdr cur-last (cons rep nil))
          (setq cur-last (cdr cur-last)))
         (t (setq cur (cons rep nil)
                  cur-last cur))))
       (t
        (when cur
          (setcdr result-last (cons (apply #'vconcat cur) nil))
          (setq result-last (cdr result-last)
                cur nil))
        (setcdr result-last (cons rep nil))
        (setq result-last (cdr result-last)))))
    (when cur
      (setcdr result-last (cons (apply #'vconcat cur) nil)))
    (cdr result)))

(defun evil-repeat-visual-char (nfwdlines nfwdchars)
  "Restore a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (evil-visual-char)
  (when (> nfwdlines 0)
    (forward-line nfwdlines))
  (forward-char nfwdchars))

(defun evil-repeat-visual-line (nfwdlines)
  "Restore a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (evil-visual-line)
  (forward-line nfwdlines))

(defun evil-repeat-visual-block (nfwdlines nfwdchars)
  "Restore a character visual selection.
If the selection is in a single line, the restored visual
selection covers the same number of characters. If the selection
covers several lines, the restored selection covers the same
number of lines and the same number of characters in the last
line as the original selection."
  (evil-visual-block)
  (let ((col (current-column)))
    (forward-line nfwdlines)
    (move-to-column (+ col nfwdchars) t)))

(defun evil-execute-change (changes rel-point)
  "Execute as list of changes.

CHANGES is a list of triples (REL-BEG INSERT-TEXT NDEL).
REL-BEG is the relative position (to point) where the change
takes place. INSERT-TEXT is the text to be inserted at that
position and NDEL the number of characters to be deleted at that
position before insertion.

REL-POINT is the relative position to point before the changed
where point should be placed after all changes."
  (evil-save-repeat-info
    (let ((point (point)))
      (dolist (change changes)
        (goto-char (+ point (nth 0 change)))
        (delete-char (nth 2 change))
        (insert (nth 1 change)))
      (goto-char (+ point rel-point)))))

(defun evil-execute-repeat-info (repeat-info)
  "Execute a repeat-information REPEAT-INFO."
  (evil-save-repeat-info
    (dolist (rep repeat-info)
      (cond
       ((or (arrayp rep) (stringp rep))
        (let ((input-method current-input-method)
              (evil-input-method nil))
          (deactivate-input-method)
          (unwind-protect
              (execute-kbd-macro rep)
            (activate-input-method input-method))))
       ((consp rep)
        (when (and (= 3 (length rep))
                   (eq (nth 0 rep) 'set)
                   (eq (nth 1 rep) 'evil-this-register)
                   (>= (nth 2 rep) ?0)
                   (< (nth 2 rep) ?9))
          (setcar (nthcdr 2 rep) (1+ (nth 2 rep))))
        (apply (car rep) (cdr rep)))
       (t (error "Unexpected repeat-info: %S" rep))))))

;; TODO: currently we prepend the replacing count before the
;; key-sequence that calls the command. Can we use direct
;; modification of prefix-arg instead? Does it work in
;; conjunction with `execute-kbd-macro'?
(defun evil-execute-repeat-info-with-count (count repeat-info)
  "Repeat the repeat-information REPEAT-INFO with the count of
the first command replaced by COUNT. The count is replaced if
and only if COUNT is non-nil."
  (evil-save-repeat-info
    (cond
     ;; do nothing (zero repeating)
     ((and count (zerop count)))
     ;; replace count
     (count
      (let ((evil-repeat-count count)
            done)
        (while (and repeat-info
                    (arrayp (car repeat-info))
                    (not done))
          (let* ((count-and-cmd (evil-extract-count (pop repeat-info))))
            (push (vconcat (number-to-string count)
                           (nth 2 count-and-cmd)
                           (nth 3 count-and-cmd))
                  repeat-info)
            (setq done t)))
        (evil-execute-repeat-info repeat-info)))
     ;; repeat with original count
     (t (evil-execute-repeat-info repeat-info)))))

;; Keep the compiler happy - this is a buffer local var
(defvar evil--execute-normal-return-state)

(evil-define-command evil-repeat (count &optional save-point)
  "Repeat the last editing command with count replaced by COUNT.
If SAVE-POINT is non-nil, do not move point."
  :repeat ignore
  :suppress-operator t
  (interactive (list current-prefix-arg
                     (not evil-repeat-move-cursor)))
  (cond
   ((null evil-repeat-ring)
    (error "Already executing repeat"))
   (save-point
    (save-excursion
      (evil-repeat count)))
   (t
    (unwind-protect
        (let ((evil-last-find-temp evil-last-find)
              (confirm-kill-emacs t)
              (kill-buffer-hook
               (cons #'(lambda ()
                         (user-error "Cannot delete buffer in repeat command"))
                     kill-buffer-hook))
              (undo-pointer buffer-undo-list))
          (evil-with-single-undo
            (setq evil-last-repeat (list (point) count undo-pointer))
            (evil-execute-repeat-info-with-count
             count (ring-ref evil-repeat-ring 0))
            (setq evil-last-find evil-last-find-temp)))
      (if (eq 'evil-execute-in-normal-state last-command)
          (evil-change-state evil--execute-normal-return-state)
        (evil-normal-state))))))

;; TODO: the same issue concering disabled undos as for `evil-paste-pop'
(evil-define-command evil-repeat-pop (count &optional save-point)
  "Replace the just repeated command with a previously executed command.
Only allowed after `evil-repeat', `evil-repeat-pop' or
`evil-repeat-pop-next'. Uses the same repeat count that
was used for the first repeat.

The COUNT argument inserts the COUNT-th previous kill.
If COUNT is negative, this is a more recent kill."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not evil-repeat-move-cursor)))
  (cond
   ((not (and (eq last-command #'evil-repeat)
              evil-last-repeat))
    (user-error "Previous command was not evil-repeat: %s" last-command))
   (save-point
    (save-excursion
      (evil-repeat-pop count)))
   (t
    (unless (eq buffer-undo-list (nth 2 evil-last-repeat))
      (evil-undo-pop))
    (goto-char (car evil-last-repeat))
    ;; rotate the repeat-ring
    (while (> count 0)
      (when evil-repeat-ring
        (ring-insert-at-beginning evil-repeat-ring
                                  (ring-remove evil-repeat-ring 0)))
      (setq count (1- count)))
    (while (< count 0)
      (when evil-repeat-ring
        (ring-insert evil-repeat-ring
                     (ring-remove evil-repeat-ring)))
      (setq count (1+ count)))
    (setq this-command #'evil-repeat)
    (evil-repeat (cadr evil-last-repeat)))))

(evil-define-command evil-repeat-pop-next (count &optional save-point)
  "Same as `evil-repeat-pop', but with negative COUNT."
  :repeat nil
  :suppress-operator t
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not evil-repeat-move-cursor)))
  (evil-repeat-pop (- count) save-point))

(defun evil--read-key-sequence-advice ()
  "Record `this-command-keys' before it is overwritten."
  (when (and (evil-repeat-recording-p)
             evil-recording-current-command)
    (let ((repeat-type (evil--repeat-type this-command)))
      (when (functionp repeat-type)
        (funcall repeat-type 'pre-read-key-sequence)))))

(defadvice read-key-sequence (before evil activate)
  (evil--read-key-sequence-advice))
(defadvice read-key-sequence-vector (before evil activate)
  (evil--read-key-sequence-advice))

(provide 'evil-repeat)

;;; evil-repeat.el ends here
