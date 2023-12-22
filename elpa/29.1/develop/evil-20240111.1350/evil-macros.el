;;; evil-macros.el --- Macros -*- lexical-binding: t -*-

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

(require 'evil-common)
(require 'evil-states)
(require 'evil-repeat)

(declare-function evil-ex-p "evil-ex")
(declare-function evil-line-or-visual-line "evil-commands")

;; set some error codes
(put 'beginning-of-line 'error-conditions '(beginning-of-line error))
(put 'beginning-of-line 'error-message "Beginning of line")
(put 'end-of-line 'error-conditions '(end-of-line error))
(put 'end-of-line 'error-message "End of line")
;; we don't want line boundaries to trigger the debugger
;; when `debug-on-error' is t
(cl-pushnew 'beginning-of-line debug-ignored-errors)
(cl-pushnew 'end-of-line debug-ignored-errors)

(defun evil-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (let ((opoint   (point))
        (omark    (mark t))
        (obuffer  (current-buffer))
        (evil-motion-marker (move-marker (make-marker) (point)))
        range)
    (evil-with-transient-mark-mode
      (evil-narrow-to-field
        (unwind-protect
            (let ((current-prefix-arg count)
                  ;; Store type in global variable `evil-this-type'.
                  ;; If necessary, motions can change their type
                  ;; during execution by setting this variable.
                  (evil-this-type
                   (or type (evil-get-command-property motion :type 'exclusive))))
              (condition-case err
                  (let ((repeat-type (evil--repeat-type motion)))
                    (when (functionp repeat-type) (funcall repeat-type 'pre))
                    (unless (with-local-quit
                              (setq range (call-interactively motion))
                              t)
                      (evil-repeat-abort)
                      (setq quit-flag t))
                    (when (functionp repeat-type) (funcall repeat-type 'post)))
                (error
                 (evil-repeat-abort)
                 ;; some operators depend on succeeding motions, in
                 ;; particular for `evil-forward-char' (e.g., used by
                 ;; `evil-substitute'), therefore we let end-of-line
                 ;; and end-of-buffer pass
                 (if (memq (car err) '(end-of-line end-of-buffer))
                     (message (error-message-string err))
                   (signal (car err) (cdr err)))))
              (cond
               ;; the motion returned a range
               ((evil-range-p range))
               ;; the motion made a Visual selection
               ((evil-visual-state-p)
                (setq range (evil-visual-range)))
               ;; the motion made an active region
               ((region-active-p)
                (setq range (evil-range (region-beginning) (region-end)
                                        evil-this-type)))
               ;; default: range from previous position to current
               (t (setq range (evil-expand-range
                               (evil-normalize evil-motion-marker
                                               (point)
                                               evil-this-type)))))
              (unless (or (null type) (eq (evil-type range) type))
                (evil-set-type range type)
                (evil-expand-range range))
              (evil-set-range-properties range nil)
              range)
          ;; restore point and mark like `save-excursion',
          ;; but only if the motion hasn't disabled the operator
          (unless evil-inhibit-operator
            (set-buffer obuffer)
            (evil-move-mark omark)
            (goto-char opoint))
          ;; delete marker so it doesn't slow down editing
          (move-marker evil-motion-marker nil))))))

(defmacro evil-define-motion (motion args &rest body)
  "Define a motion command MOTION.
ARGS is a list of arguments.  Motions can have any number of
arguments, but the first (if any) has the predefined meaning of
count.  BODY must execute the motion by moving point.

Optional keyword arguments are:
- `:type' - determines how the motion works after an operator (one of
  `inclusive', `line', `block' and `exclusive', or a self-defined
  motion type)
- `:jump' - if non-nil, the previous position is stored in the jump
  list, so that it can be restored with \
\\<evil-motion-state-map>\\[evil-jump-backward]

\(fn MOTION (COUNT ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let (arg doc interactive key keys)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            ;; the count is either numerical or nil
            interactive '("<c>")))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car body)) #'format)
                   (stringp (car body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :repeat 'motion))
    (while (keywordp (car body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; collect `interactive' specification
    (when (eq (car-safe (car body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       ;; refresh echo area in Eldoc mode
       (when ',motion
         (eval-after-load 'eldoc
           '(and (fboundp 'eldoc-add-command)
                 (eldoc-add-command ',motion))))
       (evil-define-command ,motion (,@args)
         ,@(when doc `(,doc))         ; avoid nil before `interactive'
         ,@keys
         :keep-visual t
         (interactive ,@interactive)
         ,@body))))

(defmacro evil-narrow-to-line (&rest body)
  "Narrow BODY to the current line.
BODY will signal `beginning-of-line' or `end-of-line' upon reaching
the beginning or end of the current line."
  (declare (indent defun) (debug t))
  `(cl-destructuring-bind (beg end &rest) (evil-line-expand (point) (point))
     (when (save-excursion (goto-char end) (bolp))
       (setq end (max beg (1- end))))
     ;; Do not include the newline in Normal state
     (and (not evil-move-beyond-eol)
          (not (evil-visual-state-p))
          (not (evil-operator-state-p))
          (setq end (max beg (1- end))))
     (evil-with-restriction beg end
       (condition-case err
           (progn ,@body)
         (beginning-of-buffer
          (if (= (point-min) beg)
              (signal 'beginning-of-line nil)
            (signal (car err) (cdr err))))
         (end-of-buffer
          (if (= (point-max) end)
              (signal 'end-of-line nil)
            (signal (car err) (cdr err))))))))

(defun evil-eobp (&optional pos)
  "Whether point is at end-of-buffer with regard to end-of-line."
  (save-excursion
    (when pos (goto-char pos))
    (cond
     ((eobp))
     ;; the rest only pertains to Normal state
     ((not (evil-normal-state-p)) nil)
     ;; at the end of the last line
     ((eolp)
      (forward-char)
      (eobp))
     ;; at the last character of the last line
     (t
      (forward-char)
      (cond
       ((eobp))
       ((eolp)
        (forward-char)
        (eobp)))))))

(defun evil-move-beginning (count forward &optional backward)
  "Move to the beginning of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall backward 1))
        (unless (zerop count)
          (goto-char (point-min)))))
     ((> count 0)
      (when (evil-eobp)
        (signal 'end-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (<= (save-excursion
                  (funcall forward 1)
                  (funcall backward 1)
                  (point))
                opoint)
        (setq count (1+ count)))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall forward 1))
        (if (zerop count)
            ;; go back to beginning of object
            (funcall backward 1)
          (goto-char (point-max)))))
     (t count))))

(defun evil-move-end (count forward &optional backward inclusive)
  "Move to the end of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument.
If INCLUSIVE is non-nil, then point is placed at the last character
of the object; otherwise it is placed at the end of the object."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (>= (save-excursion
                  (funcall backward 1)
                  (funcall forward 1)
                  (point))
                (if inclusive
                    (1+ opoint)
                  opoint))
        (setq count (1- count)))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall backward 1))
        (if (not (zerop count))
            (goto-char (point-min))
          ;; go to end of object
          (funcall forward 1)
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (evil-normal-state-p)
                    (evil-motion-state-p))
            (evil-adjust-cursor)))))
     ((> count 0)
      (when (evil-eobp)
        (signal 'end-of-buffer nil))
      (when inclusive
        (forward-char))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall forward 1))
        (if (not (zerop count))
            (goto-char (point-max))
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (evil-normal-state-p)
                    (evil-motion-state-p))
            (evil-adjust-cursor)))))
     (t count))))

(defun evil-text-object-make-linewise (range)
  "Turn the text object selection RANGE to linewise.
The selection is adjusted in a sensible way so that the selected
lines match the user intent. In particular, whitespace-only parts
at the first and last lines are omitted. This function returns
the new range."
  ;; Bug #607
  ;; If new type is linewise and the selection of the
  ;; first line consists of whitespace only, the
  ;; beginning is moved to the start of the next line. If
  ;; the selections of the last line consists of
  ;; whitespace only, the end is moved to the end of the
  ;; previous line.
  (if (eq (evil-type range) 'line)
      range
    (let ((expanded (plist-get (evil-range-properties range) :expanded))
          (newrange (evil-expand-range range t)))
      (save-excursion
        ;; skip whitespace at the beginning
        (goto-char (evil-range-beginning newrange))
        (skip-chars-forward " \t")
        (when (and (not (bolp)) (eolp))
          (evil-set-range-beginning newrange (1+ (point))))
        ;; skip whitepsace at the end
        (goto-char (evil-range-end newrange))
        (skip-chars-backward " \t")
        (when (and (not (eolp)) (bolp))
          (evil-set-range-end newrange (1- (point))))
        ;; only modify range if result is not empty
        (if (> (evil-range-beginning newrange)
               (evil-range-end newrange))
            range
          (unless expanded
            (evil-contract-range newrange))
          newrange)))))

(defmacro evil-define-text-object (object args &rest body)
  "Define a text object command OBJECT.
BODY should return a range (BEG END) to the right of point
if COUNT is positive, and to the left of it if negative.

Optional keyword arguments:
- `:type' - determines how the range applies after an operator
  (`inclusive', `line', `block', and `exclusive', or a self-defined
  motion type).
- `:extend-selection' - if non-nil (default), the text object always
  enlarges the current selection.  Otherwise, it replaces the current
  selection.

\(fn OBJECT (COUNT) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((args (delq '&optional args))
         (count (or (pop args) 'count))
         (args (when args `(&optional ,@args)))
         (interactive '(interactive "<c><v>"))
         doc keys)
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :extend-selection t))
    (while (keywordp (car body))
      (setq keys (plist-put keys (pop body) (pop body))))
    ;; interactive
    (when (eq (car-safe (car body)) 'interactive)
      (setq interactive (pop body)))
    ;; macro expansion
    `(evil-define-motion ,object (,count ,@args)
       ,@(when doc `(,doc))
       ,@keys
       ,interactive
       (setq ,count (or ,count 1))
       (when (/= ,count 0)
         ;; FIXME: These let-bindings shadow variables in args
         (let ((type (evil-type ',object evil-visual-char))
               (extend (and (evil-visual-state-p)
                            (evil-get-command-property
                             ',object :extend-selection
                             ',(plist-get keys :extend-selection))))
               (dir evil-visual-direction)
               mark point range selection)
           (cond
            ;; Visual state: extend the current selection
            ((and (evil-visual-state-p)
                  (called-interactively-p 'any))
             ;; if we are at the beginning of the Visual selection,
             ;; go to the left (negative COUNT); if at the end,
             ;; go to the right (positive COUNT)
             (setq dir evil-visual-direction
                   ,count (* ,count dir))
             (setq range (progn ,@body))
             (when (evil-range-p range)
               (setq range (evil-expand-range range))
               (evil-set-type range (evil-type range type))
               (setq range (evil-contract-range range))
               ;; the beginning is mark and the end is point
               ;; unless the selection goes the other way
               (setq mark  (evil-range-beginning range)
                     point (evil-range-end range)
                     type  (evil-type
                            (if evil-text-object-change-visual-type
                                range
                              (evil-visual-range))))
               (when (and (eq type 'line)
                          (not (eq type (evil-type range))))
                 (let ((newrange (evil-text-object-make-linewise range)))
                   (setq mark (evil-range-beginning newrange)
                         point (evil-range-end newrange))))
               (when (< dir 0)
                 (evil-swap mark point))
               ;; select the union
               (evil-visual-make-selection mark point type)))
            ;; not Visual state: return a pair of buffer positions
            (t
             (setq range (progn ,@body))
             (unless (evil-range-p range)
               (setq ,count (- ,count)
                     range (progn ,@body)))
             (when (evil-range-p range)
               (setq selection (evil-range (point) (point) type))
               (if extend
                   (setq range (evil-range-union range selection))
                 (evil-set-type range (evil-type range type)))
               ;; possibly convert to linewise
               (when (eq evil-this-type-modified 'line)
                 (setq range (evil-text-object-make-linewise range)))
               (evil-set-range-properties range nil)
               range))))))))

(defmacro evil-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.
The operator acts on the range of characters BEG through
END. BODY must execute the operator by potentially manipulating
the buffer contents, or otherwise causing side effects to happen.

Optional keyword arguments are:
- `:type' - force the input range to be of a given type (`inclusive',
  `line', `block', and `exclusive', or a self-defined motion type).
- `:motion' - use a predetermined motion instead of waiting for one
  from the keyboard.  This does not affect the behavior in visual
  state, where selection boundaries are always used.
- `:repeat' - if non-nil (default), then \
  \\<evil-normal-state-map>\\[evil-repeat] will repeat the
  operator.
- `:move-point' - if non-nil (default), the cursor will be moved to
  the beginning of the range before the body executes
- `:keep-visual' - if non-nil, the selection is not disabled when the
  operator is executed in visual state.  By default, visual state is
  exited automatically.
- `:restore-point' - if non-nil, point is restored when the
  operator is executed from ex.

\(fn OPERATOR (BEG END ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let* ((args (delq '&optional args))
         (interactive (if (> (length args) 2) '("<R>") '("<r>")))
         (args (if (> (length args) 2)
                   `(,(nth 0 args) ,(nth 1 args)
                     &optional ,@(nthcdr 2 args))
                 args))
         (end-marker (make-symbol "end-marker"))
         arg doc key keys visual)
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :move-point t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :keep-visual)
        (setq visual arg))
       (t
        (setq keys (plist-put keys key arg)))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr-safe (pop body))))
    ;; transform extended interactive specs
    (setq interactive (apply #'evil-interactive-form interactive))
    (setq keys (evil-concat-plists keys (cdr-safe interactive))
          interactive (car-safe interactive))
    ;; macro expansion
    `(evil-define-command ,operator ,args
       ,@(when doc `(,doc))
       ,@keys
       :keep-visual t
       :suppress-operator t
       (interactive
        (let* ((props (evil-command-properties ',operator))
               (evil-operator-range-motion
                (let ((p (plist-member props :motion)))
                  ;; :motion nil is equivalent to :motion undefined
                  (when p (or (cadr p) #'undefined))))
               (evil-operator-range-type (plist-get props :type))
               evil-operator-range-beginning evil-operator-range-end
               evil-inhibit-operator)
          (setq evil-inhibit-operator-value nil
                evil-this-operator this-command
                evil-operator-start-col (current-column))
          (prog1 ,interactive
            (setq evil-inhibit-operator-value evil-inhibit-operator)
            (if ,visual
                (when (evil-visual-state-p) (evil-visual-expand-region))
              (setq deactivate-mark t))
            (cond
             ((evil-visual-state-p)
              (evil-visual-rotate 'upper-left))
             ((plist-get props :move-point)
              (when evil-operator-range-beginning
                (goto-char evil-operator-range-beginning)))))))
       (unwind-protect
           (let ((evil-inhibit-operator evil-inhibit-operator-value)
                 (,end-marker (make-marker)))
             (set-marker ,end-marker ,(cadr args))
             (unless (and evil-inhibit-operator
                          (called-interactively-p 'any))
               ,@body)
             (evil-set-marker ?\[ (or ,(car args) (point-max)))
             (evil-set-marker ?\] (max (or ,(car args) (point-max))
                                       (1- (or (marker-position ,end-marker) (point-max)))))
             (set-marker ,end-marker nil))
         (setq evil-inhibit-operator-value nil)))))

;; this is used in the `interactive' specification of an operator command
(defun evil-operator-range (&optional return-type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END), or (BEG END TYPE) if
RETURN-TYPE is non-nil."
  (let ((motion (or evil-operator-range-motion
                    (when evil-called-from-ex-p 'evil-line)))
        (type evil-operator-range-type)
        range count)
    (setq evil-this-type-modified nil)
    (cond
     ;; Ex command
     ((and evil-called-from-ex-p evil-ex-range)
      (setq range evil-ex-range))
     ;; Visual selection
     ((and (not evil-called-from-ex-p) (evil-visual-state-p))
      (setq range (evil-visual-range)))
     ;; active region
     ((and (not evil-called-from-ex-p) (region-active-p))
      (setq range (evil-range (region-beginning) (region-end)
                              (or evil-this-type 'exclusive))))
     ;; motion
     (t
      (evil-save-echo-area
        (evil-save-state
          (unless motion
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (evil-extract-count (this-command-keys)))))
              (evil-change-state 'operator)
              (cl-loop for keys on (listify-key-sequence keys) do
                       (define-key evil-operator-shortcut-map
                         (vconcat keys) #'evil-line-or-visual-line)))
            ;; read motion from keyboard
            (let ((command (evil-read-motion motion)))
              (setq motion (car command)
                    count (cadr command)
                    type (or type (nth 2 command)))))
          (cond
           ((eq motion #'undefined)
            (setq range (list nil nil)
                  motion nil))
           ((or (null motion) ; keyboard-quit
                (evil-get-command-property motion :suppress-operator))
            (evil-repeat-abort)
            (setq quit-flag t
                  range (evil-range (point) (point)) ; zero-len range
                  motion nil))
           (evil-repeat-count
            (setq count evil-repeat-count
                  ;; only the first operator's count is overwritten
                  evil-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (let ((evil-state 'operator)
                  mark-active)
              ;; calculate motion range
              (setq range (evil-motion-range motion count type))))
          ;; update global variables
          (setq evil-this-motion motion
                evil-this-motion-count count
                type (evil-type range type)
                evil-this-type type)))))
    (unless (or (null type) (eq (evil-type range) type))
      (evil-contract-range range)
      (evil-set-range-type range type)
      (evil-expand-range range))
    (setq evil-operator-range-beginning (evil-range-beginning range)
          evil-operator-range-end (evil-range-end range)
          evil-operator-range-type (evil-type range))
    (setcdr (cdr range)
            (when return-type (list (evil-type range))))
    range))

(defmacro evil-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.

Optional keyword arguments:
- `:expand' - expansion function.  This function should accept two
  positions in the current buffer, BEG and END,and return a pair of
  expanded buffer positions.
- `:contract' - the opposite of `:expand'.  Optional.
- `:one-to-one' - non-nil if expansion is one-to-one.  This means that
  `:expand' followed by `:contract' always return the original range.
- `:normalize' - normalization function.  This function should accept
  two unexpanded positions and adjust them before expansion.  May be
  used to deal with buffer boundaries.
- `:string' - description function.  Takes two buffer positions and
  returns a human-readable string.  For example \"2 lines\"

If further keywords and functions are specified, they are assumed to
be transformations on buffer positions, like `:expand' and `:contract'.

\(fn TYPE DOC [[KEY FUNC]...])"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (let ((plist (list :one-to-one t)) ; standard values
        args defun-forms func key name string sym val)
    ;; keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            val (pop body))
      (if (plist-member plist key)      ; not a function
          (setq plist (plist-put plist key val))
        (setq func val
              sym (intern (replace-regexp-in-string
                           "^:" "" (symbol-name key)))
              name (intern (format "evil-%s-%s" type sym))
              args (car (cdr-safe func))
              string (cadr (cdr-safe func))
              string (if (stringp string)
                         (format "%s\n\n" string) "")
              plist (plist-put plist key `',name))
        (push
         (cond
          ((eq key :string)
           `(defun ,name (beg end &rest properties)
              ,(format "Return size of %s from BEG to END \
with PROPERTIES.\n\n%s%s" type string doc)
              (let ((type ',type)
                    plist range)
                (when (and beg end)
                  (evil-sort beg end)
                  (save-excursion
                    (unless (plist-get properties :expanded)
                      (setq range (apply #'evil-expand
                                         beg end type properties)
                            beg (evil-range-beginning range)
                            end (evil-range-end range)
                            type (evil-type range type)
                            plist (evil-range-properties range))
                      (setq properties
                            (evil-concat-plists properties plist)))
                    (or (apply #',func beg end
                               (when ,(> (length args) 2)
                                 properties))
                        ""))))))
          (t
           `(defun ,name (beg end &rest properties)
              ,(format "Perform %s transformation on %s from BEG to END \
with PROPERTIES.\n\n%s%s" sym type string doc)
              (let ((type ',type)
                    plist range)
                (when (and beg end)
                  (evil-sort beg end)
                  (save-excursion
                    (when (memq ,key '(:expand :contract))
                      (setq properties
                            (plist-put properties
                                       :expanded ,(eq key :expand))))
                    (setq range (or (apply #',func beg end
                                           (when ,(> (length args) 2)
                                             properties))
                                    (apply #'evil-range
                                           beg end type properties))
                          beg (evil-range-beginning range)
                          end (evil-range-end range)
                          type (evil-type range type)
                          plist (evil-range-properties range))
                    (setq properties
                          (evil-concat-plists properties plist))
                    (apply #'evil-range beg end type properties)))))))
         defun-forms)))
    ;; :one-to-one requires both or neither of :expand and :contract
    (when (plist-get plist :expand)
      (setq plist (plist-put plist :one-to-one
                             (and (plist-get plist :contract)
                                  (plist-get plist :one-to-one)))))
    `(progn
       (evil--add-to-alist evil-type-properties ',type (list ,@plist))
       ,@defun-forms
       ',type)))

(defmacro evil-define-interactive-code (code &rest body)
  "Define an interactive code.
PROMPT, if given, is the remainder of the interactive string
up to the next newline. Command properties may be specified
via KEY-VALUE pairs. BODY should evaluate to a list of values.

\(fn CODE (PROMPT) [[KEY VALUE]...] BODY...)"
  (declare (indent defun))
  (let* ((args (and (> (length body) 1) (listp (car body))
                    (pop body)))
         (doc (when (stringp (car body)) (pop body)))
         (properties
          (cl-loop while (keywordp (car body))
                   collect (pop body) collect (pop body)))
         (func (if args
                   `(lambda ,args
                      ,@(when doc `(,doc))
                      ,@body)
                 `',(macroexp-progn body))))
    `(eval-and-compile
       (evil--add-to-alist
        evil-interactive-alist ,code (cons ,func ',properties))
       ,code)))

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   ;; Match all `evil-define-' forms except `evil-define-key'.
   ;; (In the interests of speed, this expression is incomplete
   ;; and does not match all three-letter words.)
   '(("(\\(evil-\\(?:ex-\\)?define-\
\\(?:[^ k][^ e][^ y]\\|[-[:word:]]\\{4,\\}\\)\\)\
\\>[ \f\t\n\r\v]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(evil-\\(?:delay\\|narrow\\|signal\\|save\\|with\\(?:out\\)?\\)\
\\(?:-[-[:word:]]+\\)?\\)\\>\[ \f\t\n\r\v]+"
      1 font-lock-keyword-face)
     ("(\\(evil-\\(?:[-[:word:]]\\)*loop\\)\\>[ \f\t\n\r\v]+"
      1 font-lock-keyword-face))))

(provide 'evil-macros)

;;; evil-macros.el ends here
