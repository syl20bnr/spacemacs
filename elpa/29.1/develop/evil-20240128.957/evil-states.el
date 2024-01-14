;;; evil-states.el --- States -*- lexical-binding: t -*-

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

(require 'evil-core)

;;; Normal state

(evil-define-state normal
  "Normal state.
AKA \"Command\" state."
  :tag " <N> "
  :enable (motion)
  (cond
   ((evil-normal-state-p)
    (overwrite-mode -1)
    (add-hook 'post-command-hook #'evil-normal-post-command nil t))
   (t
    (remove-hook 'post-command-hook #'evil-normal-post-command t))))

(defun evil-normal-post-command (&optional command)
  "Reset command loop variables in Normal state.
Also prevent point from reaching the end of the line.
If the region is activated, enter Visual state."
  (unless (null this-command)
    (setq command (or command this-command))
    (when (evil-normal-state-p)
      (setq evil-this-type nil
            evil-this-operator nil
            evil-this-motion nil
            evil-this-motion-count nil
            evil-inhibit-operator nil
            evil-inhibit-operator-value nil)
      (unless (memq command '(evil-use-register
                              digit-argument
                              negative-argument
                              universal-argument
                              universal-argument-minus
                              universal-argument-more
                              universal-argument-other-key))
        (setq evil-this-register nil))
      (evil-adjust-cursor))))
(put 'evil-normal-post-command 'permanent-local-hook t)

;;; Insert state

(defun evil-maybe-remove-spaces (&optional do-remove)
  "Remove space from newly opened empty line.
This function removes (indentation) spaces that have been
inserted by opening a new empty line. The behavior depends on the
variable `evil-maybe-remove-spaces'. If this variable is nil the
function does nothing. Otherwise the behavior depends on
DO-REMOVE.  If DO-REMOVE is non-nil the spaces are
removed. Otherwise `evil-maybe-remove-spaces' is set to nil
unless the last command opened yet another new line.

This function should be added as a post-command-hook to track
commands opening a new line."
  (cond
   ((not evil-maybe-remove-spaces)
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces))
   (do-remove
    (when (save-excursion
            (beginning-of-line)
            (looking-at "^\\s-*$"))
      (delete-region (line-beginning-position)
                     (line-end-position)))
    (setq evil-maybe-remove-spaces nil)
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces))
   ((not (memq this-command
               '(evil-open-above
                 evil-open-below
                 evil-append
                 evil-append-line
                 evil-change-whole-line
                 newline
                 newline-and-indent
                 indent-and-newline)))
    (setq evil-maybe-remove-spaces nil)
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces))))

(evil-define-state insert
  "Insert state."
  :tag " <I> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-insert-state-p)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (setq evil-maybe-remove-spaces t)
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step)))
   (t
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (evil-maybe-remove-spaces t)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless (eq evil-want-fine-undo t)
      (evil-end-undo-step))
    (when (or (evil-normal-state-p evil-next-state)
              (evil-motion-state-p evil-next-state))
      (evil-move-cursor-back
       (and (eolp) (not evil-move-beyond-eol)))))))

(defun evil-insert-repeat-hook ()
  "Record insertion keys in `evil-insert-repeat-info'."
  (setq evil-insert-repeat-info (last evil-repeat-info))
  (remove-hook 'pre-command-hook #'evil-insert-repeat-hook))
(put 'evil-insert-repeat-hook 'permanent-local-hook t)

(declare-function evil-execute-repeat-info "evil-repeat")
(defun evil-cleanup-insert-state ()
  "Called when Insert or Replace state is about to be exited.
Handles the repeat-count of the insertion command."
  (when evil-insert-count
    (dotimes (_ (1- evil-insert-count))
      (when evil-insert-lines
        (evil-insert-newline-below)
        (when evil-auto-indent
          (indent-according-to-mode)))
      (evil-execute-repeat-info (cdr evil-insert-repeat-info))))
  (when evil-insert-vcount
    (let ((buffer-invisibility-spec
           (if (listp buffer-invisibility-spec)
               ;; make all lines hidden by hideshow temporarily visible
               (cl-remove-if (lambda (x) (eq (or (car-safe x) x) 'hs))
                             buffer-invisibility-spec)
             buffer-invisibility-spec)))
      (cl-destructuring-bind (line col vcount) evil-insert-vcount
        (save-excursion
          (dotimes (v (1- vcount))
            (goto-char (point-min))
            (forward-line (+ line v))
            (when (or (not evil-insert-skip-empty-lines)
                      (not (integerp col))
                      (save-excursion
                        (evil-move-end-of-line)
                        (>= (current-column) col)))
              (if (integerp col)
                  (move-to-column col t)
                (funcall col))
              (dotimes (_ (or evil-insert-count 1))
                (evil-execute-repeat-info (cdr evil-insert-repeat-info))))))))))

;;; Visual state

;; Visual selections are implemented in terms of types, and are
;; compatible with the Emacs region. This is achieved by "translating"
;; the region to the selected text right before a command is executed.
;; If the command is a motion, the translation is postponed until a
;; non-motion command is invoked (distinguished by the :keep-visual
;; command property).
;;
;; Visual state activates the region, enabling Transient Mark mode if
;; not already enabled. This is only temporay: if Transient Mark mode
;; was disabled before entering Visual state, it is disabled when
;; exiting Visual state. This allows Visual state to harness the
;; "transient" behavior of many commands without overriding the user's
;; preferences in other states.

(defmacro evil-define-visual-selection (selection doc &rest body)
  "Define a Visual selection SELECTION.
Creates a command evil-visual-SELECTION for enabling the selection.
DOC is the function's documentation string. The following keywords
may be specified in BODY:

:message STRING         Status message when enabling the selection.
:type TYPE              Type to use (defaults to SELECTION).

Following the keywords is optional code which is executed each time
the selection is enabled.

\(fn SELECTION DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name stringp
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((name (intern (format "evil-visual-%s" selection)))
         (message (intern (format "%s-message" name)))
         (tagvar (intern (format "%s-tag" name)))
         (type selection)
         (tag " <V> ")
         arg key string)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :message)
        (setq string arg))
       ((eq key :type)
        (setq type arg))
       ((eq key :tag)
        (setq tag arg))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-visual-alist (cons ',selection ',name))
       (defvar ,name ',type ,(format "*%s" doc))
       (defvar ,message ,string ,doc)
       (defvar ,tagvar ,tag ,doc)
       (evil-define-command ,name (&optional mark point type message)
         ,@(when doc `(,doc))
         :keep-visual t
         :repeat nil
         (interactive
          (list nil nil
                (if (and (evil-visual-state-p)
                         (eq evil-visual-selection ',selection))
                    'exit ,name) t))
         (setq evil--region-from-mouse nil)
         (if (eq type 'exit)
             (evil-exit-visual-state)
           (setq type (or type ,name)
                 evil-visual-selection ',selection)
           (evil-visual-make-region mark point type message)
           ,@body))
       ',selection)))

(evil-define-visual-selection char
  "Characterwise selection."
  :type inclusive
  :message "-- VISUAL --"
  :tag " <V> ")

(evil-define-visual-selection line
  "Linewise selection."
  :message "-- VISUAL LINE --"
  :tag " <Vl> ")

(evil-define-visual-selection screen-line
  "Linewise selection in `visual-line-mode'."
  :message "-- SCREEN LINE --"
  :tag " <Vs> ")

(evil-define-visual-selection block
  "Blockwise selection."
  :message "-- VISUAL BLOCK --"
  :tag " <Vb> "
  (evil-transient-mark -1)
  ;; refresh the :corner property
  (setq evil-visual-properties
        (plist-put evil-visual-properties :corner
                   (evil-visual-block-corner))))

(evil-define-state visual
  "Visual state."
  :tag 'evil-visual-tag
  :enable (motion normal)
  :message 'evil-visual-message
  (cond
   ((evil-visual-state-p)
    (evil-save-transient-mark-mode)
    (setq select-active-regions nil)
    (cond
     ((region-active-p)
      (if (< (evil-visual-direction) 0)
          (evil-visual-select (region-beginning) (region-end)
                              evil-visual-char
                              (evil-visual-direction))
        (evil-visual-make-selection (mark t) (point)
                                    evil-visual-char))
      (evil-visual-highlight))
     (t
      (evil-visual-make-region (point) (point) evil-visual-char)))
    (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
    (add-hook 'post-command-hook #'evil-visual-post-command nil t)
    (add-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook nil t))
   (t
    ;; Postpone deactivation of region if next state is Insert.
    ;; This gives certain insertion commands (auto-pairing characters,
    ;; for example) an opportunity to access the region.
    (if (and (eq evil-next-state 'insert)
             (eq evil-visual-selection 'char))
        (add-hook 'evil-normal-state-entry-hook
                  #'evil-visual-deactivate-hook nil t)
      (evil-visual-deactivate-hook))
    (setq evil-visual-region-expanded nil)
    (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
    (remove-hook 'post-command-hook #'evil-visual-post-command t)
    (remove-hook 'deactivate-mark-hook #'evil-visual-deactivate-hook t)
    (evil-visual-highlight -1))))

(defun evil-visual-pre-command (&optional command)
  "Run before each COMMAND in Visual state.
Expand the region to the selection unless COMMAND is a motion."
  (when (evil-visual-state-p)
    (setq command (or command this-command))
    (if (and evil-transient-mouse-selection
             evil--region-from-mouse
             (not (rassq command evil-visual-alist)))
        (progn (setq evil--region-from-mouse nil)
               (evil-exit-visual-state))
      (when (and evil-transient-mouse-selection
                 evil--region-from-mouse
                 (rassq command evil-visual-alist))
        (setq evil-visual-selection nil ;; maintain visual state
              evil--region-from-mouse nil))
      (when evil-visual-x-select-timer
        (cancel-timer evil-visual-x-select-timer))
      (unless (evil-get-command-property command :keep-visual)
        (evil-visual-update-x-selection)
        (evil-visual-expand-region
         ;; exclude final newline from linewise selection
         ;; unless the command has real need of it
         (and (eq (evil-visual-type) 'line)
              (evil-get-command-property command :exclude-newline)))))))

(put 'evil-visual-pre-command 'permanent-local-hook t)

(defun evil-visual-post-command (&optional command)
  "Run after each COMMAND in Visual state.
If COMMAND is a motion, refresh the selection;
otherwise exit Visual state."
  (when (evil-visual-state-p)
    (setq command (or command this-command))
    (if (or quit-flag
            (eq command #'keyboard-quit)
            ;; Is `mark-active' nil for an unexpanded region?
            deactivate-mark
            (and (not evil-visual-region-expanded)
                 (not (region-active-p))
                 (not (eq evil-visual-selection 'block))
                 transient-mark-mode))
        (progn
          (evil-exit-visual-state)
          (evil-adjust-cursor))
      (if evil-visual-region-expanded
          (evil-visual-contract-region)
        (evil-visual-refresh))
      (setq evil-visual-x-select-timer
            (run-with-idle-timer evil-visual-x-select-timeout nil
                                 #'evil-visual-update-x-selection
                                 (current-buffer)))
      (evil-visual-highlight))))
(put 'evil-visual-post-command 'permanent-local-hook t)

(defun evil-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region of BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (when (and evil-visual-update-x-selection-p
               (buffer-live-p buf)
               (evil-visual-state-p)
               (display-selections-p)
               (not (eq evil-visual-selection 'block)))
      (with-current-buffer buf
        (evil-set-selection 'PRIMARY (buffer-substring-no-properties
                                      evil-visual-beginning
                                      evil-visual-end))))))

(defun evil-visual-activate-hook (&optional _command)
  "Enable Visual state if the region is activated."
  (unless (evil-visual-state-p)
    (evil-with-delay nil
        (post-command-hook nil t "evil-activate-visual-state")
      ;; the activation may only be momentary, so re-check
      ;; in `post-command-hook' before entering Visual state
      (unless (or (evil-visual-state-p)
                  (evil-insert-state-p)
                  (evil-emacs-state-p))
        (when (and (region-active-p)
                   (not deactivate-mark))
          (evil-visual-state))))))
(put 'evil-visual-activate-hook 'permanent-local-hook t)

(defun evil-visual-deactivate-hook (&optional command)
  "Deactivate the region and restore Transient Mark mode."
  (setq command (or command this-command))
  (remove-hook 'deactivate-mark-hook
               #'evil-visual-deactivate-hook t)
  (remove-hook 'evil-normal-state-entry-hook
               #'evil-visual-deactivate-hook t)
  (cond
   ((and (evil-visual-state-p) command
         (not (evil-get-command-property command :keep-visual)))
    (setq evil-visual-region-expanded nil)
    (evil-exit-visual-state))
   ((not (evil-visual-state-p))
    (evil-active-region -1)
    (evil-restore-transient-mark-mode))))
(put 'evil-visual-deactivate-hook 'permanent-local-hook t)

(evil-define-command evil-exit-visual-state (&optional later buffer)
  "Exit from Visual state to the previous state.
If LATER is non-nil, exit after the current command."
  :keep-visual t
  :repeat abort
  (with-current-buffer (or buffer (current-buffer))
    (when (evil-visual-state-p)
      (if later
          (setq deactivate-mark t)
        (when evil-visual-region-expanded
          (evil-visual-contract-region))
        (setq evil-this-register nil)
        (evil-change-to-previous-state)))))

(defun evil-visual-tag (&optional selection)
  "Return a mode-line tag for SELECTION.
SELECTION is a kind of selection as defined by
`evil-define-visual-selection', such as `char', `line'
or `block'."
  (setq selection (or selection evil-visual-selection))
  (when selection
    (symbol-value (intern (format "evil-visual-%s-tag" selection)))))

(defun evil-visual-message (&optional selection)
  "Create an echo area message for SELECTION.
SELECTION is a kind of selection as defined by
`evil-define-visual-selection', such as `char', `line'
or `block'."
  (unless selection (setq selection evil-visual-selection))
  (when selection
    (let ((message (symbol-value (intern (format "evil-visual-%s-message"
                                                 selection)))))
      (cond
       ((functionp message) (funcall message))
       ((stringp message) (evil-echo "%s" message))))))

(defun evil-visual-select (beg end &optional type dir message)
  "Create a Visual selection of type TYPE from BEG to END.
Point and mark are positioned so that the resulting selection
has the specified boundaries. If DIR is negative, point precedes mark,
otherwise it succedes it. To specify point and mark directly,
use `evil-visual-make-selection'."
  (let* ((range (evil-contract beg end type))
         (mark (evil-range-beginning range))
         (point (evil-range-end range))
         (dir (or dir 1)))
    (when (< dir 0)
      (evil-swap mark point))
    (evil-visual-make-selection mark point type message)))

(defun evil-visual-make-selection (mark point &optional type message)
  "Create a Visual selection with point at POINT and mark at MARK.
The boundaries of the selection are inferred from these
and the current TYPE. To specify the boundaries and infer
mark and point, use `evil-visual-select' instead."
  (let* ((selection (evil-visual-selection-for-type type))
         (func (evil-visual-selection-function selection))
         (prev (and (evil-visual-state-p) evil-visual-selection))
         (mark (evil-normalize-position mark))
         (point (evil-normalize-position point))
         (state evil-state))
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (setq evil-visual-selection selection)
    (funcall func mark point type
             ;; signal a message when changing the selection
             (when (or (not (evil-visual-state-p state))
                       (not (eq selection prev)))
               message))))

(defun evil-visual-make-region (mark point &optional type message)
  "Create an active region from MARK to POINT.
If TYPE is given, also set the Visual type.
If MESSAGE is given, display it in the echo area."
  (interactive)
  (let* ((point (or point (point)))
         (mark (or mark
                   (when (or (evil-visual-state-p)
                             (region-active-p))
                     (mark t))
                   point)))
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-active-region 1)
    (setq evil-visual-region-expanded nil)
    (evil-visual-refresh mark point type)
    (cond
     ((null evil-echo-state))
     ((stringp message)
      (evil-echo "%s" message))
     (message
      (cond
       ((stringp evil-visual-state-message)
        (evil-echo "%s" evil-visual-state-message))
       ((functionp evil-visual-state-message)
        (funcall evil-visual-state-message)))))))

(defun evil-visual-expand-region (&optional exclude-newline)
  "Expand the region to the Visual selection.
If EXCLUDE-NEWLINE is non-nil and the selection ends with a newline,
exclude that newline from the region."
  (when (and (evil-visual-state-p)
             (not evil-visual-region-expanded))
    (let ((mark evil-visual-beginning)
          (point evil-visual-end))
      (when (< evil-visual-direction 0)
        (evil-swap mark point))
      (setq evil-visual-region-expanded t)
      (evil-visual-refresh mark point)
      (when (and exclude-newline
                 (save-excursion
                   (goto-char evil-visual-end)
                   (and (bolp) (not (bobp)))))
        (if (< evil-visual-direction 0)
            (evil-move-mark (max point (1- (mark))))
          (goto-char (max mark (1- (point)))))))))

(defun evil-visual-contract-region ()
  "The inverse of `evil-visual-expand-region'.
Create a Visual selection that expands to the current region."
  (evil-visual-refresh)
  (setq evil-visual-region-expanded nil)
  (evil-visual-refresh evil-visual-mark evil-visual-point))

(defun evil-visual-refresh (&optional mark point type &rest properties)
  "Refresh point, mark and Visual variables.
Refreshes `evil-visual-beginning', `evil-visual-end',
`evil-visual-mark', `evil-visual-point', `evil-visual-selection',
`evil-visual-direction', `evil-visual-properties' and `evil-this-type'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t) point))
         (dir (evil-visual-direction))
         (type (or type (evil-visual-type evil-visual-selection)
                   (evil-visual-type)))
         range)
    (evil-move-mark mark)
    (goto-char point)
    (setq evil-visual-beginning
          (or evil-visual-beginning
              (let ((marker (make-marker)))
                (move-marker marker (min point mark))))
          evil-visual-end
          (or evil-visual-end
              (let ((marker (make-marker)))
                (set-marker-insertion-type marker t)
                (move-marker marker (max point mark))))
          evil-visual-mark
          (or evil-visual-mark
              (let ((marker (make-marker)))
                (move-marker marker mark)))
          evil-visual-point
          (or evil-visual-point
              (let ((marker (make-marker)))
                (move-marker marker point))))
    (setq evil-visual-properties
          (evil-concat-plists evil-visual-properties properties))
    (cond
     (evil-visual-region-expanded
      (setq type (or (evil-visual-type) type))
      (move-marker evil-visual-beginning (min point mark))
      (move-marker evil-visual-end (max point mark))
      ;; if the type is one-to-one, we can safely refresh
      ;; the unexpanded positions as well
      (when (evil-type-property type :one-to-one)
        (setq range (apply #'evil-contract point mark type
                           evil-visual-properties)
              mark (evil-range-beginning range)
              point (evil-range-end range))
        (when (< dir 0)
          (evil-swap mark point))
        (move-marker evil-visual-mark mark)
        (move-marker evil-visual-point point)))
     (t
      (setq range (apply #'evil-expand point mark type
                         evil-visual-properties)
            type (evil-type range type))
      (move-marker evil-visual-beginning (evil-range-beginning range))
      (move-marker evil-visual-end (evil-range-end range))
      (move-marker evil-visual-mark mark)
      (move-marker evil-visual-point point)))
    (setq evil-visual-direction dir
          evil-this-type type)))

(defun evil-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on the Visual type.
With negative ARG, disable highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (when evil-visual-overlay
      (delete-overlay evil-visual-overlay)
      (setq evil-visual-overlay nil))
    (when evil-visual-block-overlays
      (mapc #'delete-overlay evil-visual-block-overlays)
      (setq evil-visual-block-overlays nil)))
   ((eq evil-visual-selection 'block)
    (when evil-visual-overlay
      (evil-visual-highlight -1))
    (evil-visual-highlight-block
     evil-visual-beginning
     evil-visual-end))
   (t
    (when evil-visual-block-overlays
      (evil-visual-highlight -1))
    (if evil-visual-overlay
        (move-overlay evil-visual-overlay
                      evil-visual-beginning evil-visual-end)
      (setq evil-visual-overlay
            (make-overlay evil-visual-beginning evil-visual-end)))
    (overlay-put evil-visual-overlay 'face 'region)
    (overlay-put evil-visual-overlay 'priority 99))))

(defun evil-visual-highlight-block (beg end &optional overlays)
  "Highlight rectangular region from BEG to END.
Do this by putting an overlay on each line within the rectangle.
Each overlay extends across all the columns of the rectangle.
Reuse overlays where possible to prevent flicker."
  (let* ((point (point))
         (overlays (or overlays 'evil-visual-block-overlays))
         (old (symbol-value overlays))
         beg-col end-col new nlines overlay window-beg window-end)
    (save-excursion
      ;; calculate the rectangular region represented by BEG and END,
      ;; but put BEG in the upper-left corner and END in the
      ;; lower-right if not already there
      (setq beg-col (evil-column beg)
            end-col (evil-column end))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (evil-swap beg-col end-col))
        (setq beg (progn (goto-char beg) (evil-move-to-column beg-col))
              end (progn (goto-char end) (evil-move-to-column end-col 1))))
      ;; maybe extend end column to EOL
      (and (memq this-command '(next-line previous-line))
           (eq temporary-goal-column most-positive-fixnum)
           (setq end-col most-positive-fixnum))
      ;; force a redisplay so we can do reliable window
      ;; BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (_ nlines)
        (let (before after row-beg row-end)
          ;; beginning of row
          (evil-move-to-column beg-col)
          (when (< (current-column) beg-col)
            ;; prepend overlay with virtual spaces if unable to
            ;; move directly to the first column
            (setq before
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\s)
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; end of row
          (evil-move-to-column end-col)
          (when (and (not (eolp))
                     (< (current-column) end-col))
            ;; append overlay with virtual spaces if unable to
            ;; move directly to the last column
            (setq after
                  (propertize
                   (make-string
                    (if (= (point) row-beg)
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\s) 'face 'region))
            ;; place cursor on one of the virtual spaces
            (if (= point row-beg)
                (put-text-property
                 0 (min (length after) 1)
                 'cursor t after)
              (put-text-property
               (max 0 (1- (length after))) (length after)
               'cursor t after)))
          (setq row-end (min (point) (line-end-position)))
          ;; trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (overlay-start overlay) row-beg)
                      (/= (overlay-end overlay) row-end))
            (delete-overlay overlay)
            (setq old (cdr old)))
          ;; reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (overlay-start overlay) row-beg)
                     (= (overlay-end overlay) row-end)))
            (move-overlay overlay row-beg row-end)
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (make-overlay row-beg row-end))
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; display overlays
      (dolist (overlay new)
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'priority 99))
      ;; trim old overlays
      (dolist (overlay old)
        (delete-overlay overlay))
      (set overlays (nreverse new)))))

(defun evil-visual-range ()
  "Return the Visual selection as a range.
This is a list (BEG END TYPE PROPERTIES...), where BEG is the
beginning of the selection, END is the end of the selection,
TYPE is the selection's type, and PROPERTIES is a property list
of miscellaneous selection attributes."
  (apply #'evil-range
         evil-visual-beginning evil-visual-end
         (evil-visual-type)
         :expanded t
         evil-visual-properties))

(defun evil-visual-direction ()
  "Return direction of Visual selection.
The direction is -1 if point precedes mark and 1 otherwise.
See also the variable `evil-visual-direction', which holds
the direction of the last selection."
  (let* ((point (point))
         (mark (or (mark t) point)))
    (if (< point mark) -1 1)))

(defun evil-visual-type (&optional selection)
  "Return the type of the Visual selection.
If SELECTION is specified, return the type of that instead."
  (or (and (null selection) (evil-visual-state-p) evil-this-type)
      (symbol-value (cdr (assq (or selection evil-visual-selection)
                               evil-visual-alist)))))

(defun evil-visual-goto-end ()
  "Go to the last line of the Visual selection.
This position may differ from `evil-visual-end' depending on
the selection type, and is contained in the selection."
  (let ((range (evil-contract-range (evil-visual-range))))
    (goto-char (evil-range-end range))))

(defun evil-visual-alist ()
  "Return an association list from types to selection symbols."
  (mapcar #'(lambda (e)
              (cons (symbol-value (cdr-safe e)) (cdr-safe e)))
          evil-visual-alist))

(defun evil-visual-selection-function (selection)
  "Return a selection function for TYPE.
Default to `evil-visual-make-region'."
  (or (cdr-safe (assq selection evil-visual-alist))
      ;; generic selection function
      'evil-visual-make-region))

(defun evil-visual-selection-for-type (type)
  "Return a Visual selection for TYPE."
  (catch 'done
    (dolist (selection evil-visual-alist)
      (when (eq (symbol-value (cdr selection)) type)
        (throw 'done (car selection))))))

(defun evil-visual-block-corner (&optional corner point mark)
  "Block corner corresponding to POINT, with MARK in opposite corner.
Depending on POINT and MARK, the return value is `upper-left',
`upper-right', `lower-left' or `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

One-column or one-row blocks are ambiguous. In such cases,
the horizontal or vertical component of CORNER is used.
CORNER defaults to `upper-left'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t)))
         (corner (or corner
                     (when evil-visual-overlay
                       (overlay-get evil-visual-overlay :corner))
                     'upper-left))
         (point-col (evil-column point))
         (mark-col (evil-column mark))
         (upperp (if (save-excursion (goto-char (min point mark))
                                     (search-forward "\n" (max point mark) t))
                     (< point mark)
                   (memq corner '(upper-left upper-right))))
         (leftp (if (= point-col mark-col)
                    (memq corner '(upper-left lower-left))
                  (< point-col mark-col))))
    (if upperp
        (if leftp 'upper-left 'upper-right)
      (if leftp 'lower-left 'lower-right))))

;;; Operator-Pending state

(evil-define-state operator
  "Operator-Pending state."
  :tag " <O> "
  :cursor evil-half-cursor
  :enable (evil-operator-shortcut-map operator motion normal))

(evil-define-keymap evil-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq evil-operator-shortcut-map (make-sparse-keymap))
  (evil-initialize-local-keymaps))

;; the half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size
(defun evil-half-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (let ((height (/ (window-pixel-height) (* (window-height) 2))))
    (setq cursor-type (cons 'hbar height))))

;;; Replace state

(evil-define-state replace
  "Replace state."
  :tag " <R> "
  :cursor hbar
  :message "-- REPLACE --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-replace-state-p)
    (overwrite-mode 1)
    (add-hook 'pre-command-hook #'evil-replace-pre-command nil t)
    (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step)))
   (t
    (overwrite-mode -1)
    (remove-hook 'pre-command-hook #'evil-replace-pre-command t)
    (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless (eq evil-want-fine-undo t)
      (evil-end-undo-step))
    (evil-move-cursor-back)))
  (setq evil-replace-alist nil))

(defun evil-replace-pre-command ()
  "Remember the character under point."
  (when (evil-replace-state-p)
    (let ((char (unless (eolp) (char-after)))
          (prev (assq (point) evil-replace-alist)))
      (if prev
          (setcdr prev char)
        (push (cons (point) char) evil-replace-alist)))))
(put 'evil-replace-pre-command 'permanent-local-hook t)

(defun evil-replace-backspace ()
  "Restore character under cursor."
  (interactive)
  (backward-char)
  (let* ((prev (assq (point) evil-replace-alist))
         (char (cdr prev))
         (this-command #'evil-replace-backspace))
    (when prev
      (delete-char 1)
      (when char (save-excursion (insert char))))))

(defun evil-update-replace-alist (opoint count chars-to-delete &optional offset)
  "Add CHARS-TO-DELETE chars to evil-replace-alist, starting at OPOINT.
If COUNT is greater than CHARS-TO-DELETE, pad the alist with nils.
Decrement recorded position by optional offset, or 0."
  (when (evil-replace-state-p)
    (dotimes (c count)
      (let ((pos (+ c opoint)))
        (add-to-list 'evil-replace-alist
                     (cons (- pos (or offset 0)) (when (< c chars-to-delete)
                                                   (char-after pos))))))))

;;; Motion state

(evil-define-state motion
  "Motion state."
  :tag " <M> "
  :suppress-keymap t)

;;; Emacs state

(evil-define-state emacs
  "Emacs state."
  :tag " <E> "
  :message "-- EMACS --"
  :input-method t
  :intercept-esc nil)

(provide 'evil-states)

;;; evil-states.el ends here
