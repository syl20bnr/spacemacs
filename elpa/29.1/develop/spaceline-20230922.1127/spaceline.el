;;; spaceline.el --- Modeline configuration library for powerline

;; Copyright (C) 2015 TheBB
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; URL: https://github.com/TheBB/spaceline
;; Version: 2.0.1
;; Keywords: mode-line powerline spacemacs
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (powerline "2.3") (dash "2.11.0") (s "1.10.0"))

;; This file is not part of GNU Emacs.

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

;; Spaceline is a modular mode-line library built on the powerline package,
;; designed to make it easy to build beautiful mode-lines.
;;
;; It was originally used in the Spacemacs distribution, but has since been
;; extracted as a stand-alone package.

;;; Code:

(require 'dash)
(require 'gv)
(require 'powerline)
(require 'cl-lib)
(require 'subr-x)

(defvar evil-previous-state)
(defvar evil-state)
(defvar evil-visual-selection)

(defvar spaceline-byte-compile t
  "Whether to byte compile the modeline.")

(defvar spaceline-responsive t
  "If true, the modeline responds to narrow windows by
dynamically hiding segments.")

(defvar spaceline--mode-lines nil
  "Alist of modelines.
Each CAR is a symbol naming the modeline, and the CDR is a cons
cell (LEFT . RIGHT) where LEFT and RIGHT are lists of segments.
See `spaceline-compile' for a description of segments.")

(defvar spaceline-inflation nil
  "A factor by which to artificially pad the modeline. Note that
this does not currently also impact the size of the powerline
separators. Those must be set separately.")

(defvar spaceline-pre-hook nil
  "Hook run before the modeline is rendered.")

(defvar spaceline-separator-dir-left nil
  "The separator directions to use for the left side.
Cons of the form (DIR . DIR) where DIR is one of left and right,
or nil, in which case the best separators are chosen depending on
the separator style.")

(defvar spaceline-separator-dir-right nil
  "The separator directions to use for the right side.
Cons of the form (DIR . DIR) where DIR is one of left and right,
or nil, in which case the best separators are chosen depending on
the separator style.")

(defvar spaceline-directed-separators '(arrow arrow-fade brace butt curve roundstub utf-8)
  "List of directed powerline separators.
Unless the directions are explicitly set in
`spaceline-separator-dir-left' or
`spaceline-separator-dir-right', these are the separators for
which Spaceline will choose different directions on the left and
right sides.")

(defvar spaceline-highlight-face-func 'spaceline-highlight-face-default
  "The function that decides the highlight face.
Superseded by `spaceline-face-func' if that variable is set.")

(defvar spaceline-face-func nil
  "The function that defines all faces.
Must be a function that accepts two arguments: FACE and ACTIVE,
where FACE is `face1', `face2' `line' or `highlight', and ACTIVE
determines whether the window in question is active.  It should
return a face to use.

This variable supersedes `spaceline-highlight-face-func' if set.")

(defvar spaceline-always-show-segments nil
  "When true, show all the segments that would otherwise be
hidden in inactive windows.")

(defun spaceline--get-separator-dirs (side)
  "Gets the preconfigured separator directions for SIDE, or the \"best\" ones,
if not specified."
  (or (if (eq 'l side)
          spaceline-separator-dir-left
        spaceline-separator-dir-right)
      (cond
       ((memq powerline-default-separator spaceline-directed-separators)
        (if (eq 'l side) '(left . left) '(right . right)))
       (t '(left . right)))))

(defun spaceline--get-face (face active)
  "Universal function to get the right face.
FACE and ACTIVE have the same meanings as in
`spaceline-face-func'.  It delegates the work to
`spaceline-face-func' if it is given, otherwise falls back to
default configuration."
  (if spaceline-face-func
      (funcall spaceline-face-func face active)
    (cond
     ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
     ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
     ((eq 'line face) (if active 'powerline-active2 'powerline-inactive2))
     ((eq 'highlight face) (if active
                               (funcall spaceline-highlight-face-func)
                             'powerline-inactive1)))))

(defface spaceline-highlight-face
  `((t (:background "DarkGoldenrod2"
        :foreground "#3E3D31"
        :inherit 'mode-line)))
  "Default highlight face for spaceline."
  :group 'spaceline)

;; Define various other highlight faces
(dolist (s '((spaceline-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (spaceline-evil-insert "chartreuse3" "Evil insert state face.")
             (spaceline-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (spaceline-evil-replace "chocolate" "Evil replace state face.")
             (spaceline-evil-visual "gray" "Evil visual state face.")
             (spaceline-evil-motion "plum3" "Evil motion state face.")
             (spaceline-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (spaceline-modified "SkyBlue2" "Modified buffer face.")
             (spaceline-read-only "plum3" "Read-only buffer face.")))
  (eval `(defface ,(nth 0 s)
           `((t (:background ,(nth 1 s)
                 :foreground "#3E3D31"
                 :inherit 'mode-line)))
           ,(nth 2 s)
           :group 'spaceline)))

(defun spaceline-highlight-face-default ()
  "The default highlight face function.
Set `spaceline-highlight-face-func' to
`spaceline-highlight-face-default' to use this."
  'spaceline-highlight-face)

(defvar spaceline-evil-state-faces
  '((normal . spaceline-evil-normal)
    (insert . spaceline-evil-insert)
    (emacs . spaceline-evil-emacs)
    (replace . spaceline-evil-replace)
    (visual . spaceline-evil-visual)
    (motion . spaceline-evil-motion))
  "Association list mapping evil states to their corresponding highlight faces.
Is used by `spaceline-highlight-face-evil-state'.")

(defun spaceline-highlight-face-evil-state ()
  "Set the highlight face depending on the evil state.
Set `spaceline-highlight-face-func' to
`spaceline-highlight-face-evil-state' to use this."
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (if (eq 'operator evil-state) evil-previous-state evil-state))
             (face (assq state spaceline-evil-state-faces)))
        (if face (cdr face) (spaceline-highlight-face-default)))
    (spaceline-highlight-face-default)))

(defun spaceline-highlight-face-modified ()
  "Set the highlight face depending on the buffer modified status.
Set `spaceline-highlight-face-func' to
`spaceline-highlight-face-modified' to use this."
  (cond
   (buffer-read-only 'spaceline-read-only)
   ((buffer-modified-p) 'spaceline-modified)
   (t 'spaceline-unmodified)))

(defun spaceline--imagep (object)
  "Test whether the given OBJECT is an image.
An image is a list whose first element is the symbol `image'."
  (and (listp object)
       object
       (eq 'image (car object))))

(defun spaceline--intersperse (separator seq)
  "Intersperses SEPARATOR between each element of SEQ.
This function does not run in-place.  It returns a new list."
  (cond
   ((not seq) nil)
   ((not (cdr seq)) seq)
   (t (append (list (car seq) separator)
              (spaceline--intersperse separator (cdr seq))))))

(defun spaceline--mode-line-nonempty (seg)
  "Check whether a modeline segment SEG is nonempty."
  (let ((val (format-mode-line seg)))
    (cond ((listp val) val)
          ((stringp val) (< 0 (length val)))
          (t))))

(defmacro spaceline--parse-segment-spec (spec &rest body)
  "Destructure the segment specification SPEC and then run BODY.
The following bindings are available in BODY:
- `segment': The segment itself, either a symbol or a literal
  value, or a list of such.
- `sym': The function that evaluates `segment', if it is a symbol.
- `sym-form': The form that evaluates the segment, if it is a
  symbol.
- `input-props': The property list part of SPEC, if present.
- `props': The full property list (including those bound to the
  symbol, if applicable)."
  (declare (indent 1))
  `(let* ((input (if (and (listp ,spec)
                          (cdr ,spec)
                          (keywordp (cadr ,spec)))
                     ,spec
                   (cons ,spec nil)))
          (segment (car input))
          (sym (when (symbolp segment) (intern (format "spaceline-%s-p" segment))))
          (sym-form (when (symbolp segment) (get sym :code)))
          (input-props (cdr input))
          (props (append input-props
                         (when (symbolp segment) (symbol-plist sym)))))
     ,@body))

(defun spaceline--gen-separator (face side)
  "Generate the code for producing a separator, if needed.
Generates a separator with faces SEPARATOR-FACE (should be bound
where the code runs) and FACE.  SIDE is l or r."
  `((when needs-separator
      ,(if (eq 'l side)
           `(push (funcall default-sep separator-face ,face) result)
         `(push (funcall default-sep ,face separator-face) result))
      (cl-rotatef default-sep other-sep)
      (setq needs-separator nil))))

(defun spaceline--gen-produce (face side)
  "Generate pre-production code.
This code must run immediately before any segment produces
output, if and only if it actually produces output.  This will
1. Generate a separator with the correct FACE and SIDE.
   (see `spaceline--gen-separator')
2. Output the value of PRIOR, if given.
3. Reset the value of PRIOR to NEXT-PRIOR.
4. Set SEPARATOR-FACE for the next separator."
  `(,@(spaceline--gen-separator face side)
    (when prior
      (push prior result))
    (setq prior next-prior)
    (setq separator-face ,face)))

(defun spaceline--gen-segment (segment-spec side hidden &optional outer-props deep-or-fallback deep)
  "Generate the code for evaluating a segment.
SEGMENT-SPEC is a valid Spaceline segment.  See
`spaceline-compile'.  SIDE is either l or r.  HIDDEN is a form
that evaluates to true if the segment should be hidden, nil
otherwise.  OUTER-PROPS is a property list with properties
inherited from parent segments.

DEEP-OR-FALLBACK is nil if this segment is a top level segment or
a fallback for a top level segment.

DEEP is nil if and only if this segment is a top level segment.

This function should only be called from outside code with
OUTER-PROPS, DEEP-OR-FALLBACK and DEEP set to nil.

Returns a list of forms."
  (spaceline--parse-segment-spec segment-spec
    (let* (;; Assemble the properties in the correct order
           (props (append props outer-props))

           ;; Explicitly set the fallback property for child segments to nil,
           ;; as it should not be inherited
           (fallback (plist-get props :fallback))
           (nest-props (append '(:fallback nil) input-props outer-props))

           (condition (if (plist-member props :when)
                          (plist-get props :when) t))
           (face (or (plist-get props :face) 'default-face))
           (face (if (memq face '(default-face other-face highlight-face line-face))
                     face `(,@face)))
           (separator `(powerline-raw ,(or (plist-get props :separator) " ") ,face))
           (tight-left (or (plist-get props :tight)
                           (plist-get props :tight-left)))
           (tight-right (or (plist-get props :tight)
                            (plist-get props :tight-right)))

           ;; A temporary variable to check whether this segment has produced
           ;; output. It is uninterned to prevent it from clashing with those
           ;; of children or parent segments.
           (previous-result (make-symbol "spaceline--previous-result"))

           ;; A temporary variable to store the setting of `needs-separator',
           ;; in case we update it and need to change it back
           (previous-needs-separator (make-symbol "spaceline--previous-needs-separator"))

           ;; A temporary variable to store the setting of `separator-face',
           ;; in case we update it and need to change it back
           (previous-separator-face (make-symbol "spaceline--previous-separator-face"))

           clean-up-make
           clean-up-code)

      ;; On the right we output we produce output in the reverse direction,
      ;; so the meanings of left and right are interchanged
      (when (eq 'r side) (cl-rotatef tight-left tight-right))

      ;; Clean-up-make and clean-up-code runs for top level segments that produce output
      ;; Anything that modifies `result' goes in clean-up-make
      (setq clean-up-make
            ;; Add padding unless the segment is tight
            (unless tight-right `((push (propertize " " 'face ,face) result))))
      (setq clean-up-code
            `(;; Rotate the faces for the next top level segment
              ,@(unless (plist-get props :skip-alternate)
                  '((cl-rotatef default-face other-face)))
              ;; We need a new separator at the next producing segment
              (setq needs-separator ,(not tight-right))))

      `(;; Store the current result pointer in the temp variable
        (let ((,previous-result result)
              (,previous-needs-separator needs-separator)
              (,previous-separator-face separator-face))

          ;; Don't produce a separator if the segment is tight
          ,@(when tight-left `((setq needs-separator nil)))

          ;; Top-level non-tight segments need padding
          ,@(unless (or deep-or-fallback tight-left)
              `((setq prior (propertize " " 'face ,face))))

          ;; Evaluate the segment
          (when ,condition
            ,@(cond
               ((listp segment)
                ;; List segments can potentially have a new separator between
                ;; their elements, but not before the first one; therefore we
                ;; set NEXT-PRIOR but leave PRIOR alone
                `((let ((next-prior ,separator))
                    ,@(apply 'append
                             (mapcar (lambda (s)
                                       (spaceline--gen-segment s side hidden nest-props 'deep-or-fallback 'deep))
                                     (if (eq 'r side) (reverse segment) segment))))
                  ;; Since PRIOR may have been disrupted, we reset it here
                  (setq prior next-prior)))
               ((symbolp segment)
                `((-when-let (value ,sym-form)
                    ;; Symbol segments are assumed to not produce output unless
                    ;; they evaluate to nil or an empty string
                    (unless (and (stringp value) (string= "" value))
                      ,@(spaceline--gen-produce face side))
                    (cond
                     ;; Images are lists, so they must be treated first
                     ((spaceline--imagep value) (push value result))
                     ((listp value)
                      (dolist (r ,(if (eq 'l side)
                                      `(spaceline--intersperse ,separator value)
                                    `(reverse (spaceline--intersperse ,separator value))))
                        (push (if (spaceline--imagep r) r (powerline-raw r ,face)) result)))
                     ((and (stringp value) (string= "" value)))
                     (t (push (powerline-raw value ,face) result))))))
               (t
                ;; Literal segments are assumed to always produce; no check for
                ;; empty string here
                `(,@(spaceline--gen-produce face side)
                  (push (powerline-raw (format "%s" ,segment) ,face) result)))))

          ;; If the segment failed to produce, but has a fallback, we call the fallback
          ,@(when fallback
              `((when (eq ,previous-result result)
                  ,@(spaceline--gen-segment fallback side hidden nest-props deep-or-fallback 'deep))))

          ;; Compute the length of the segment, and possibly run clean-up code
          ,@(unless deep
              `((if (eq ,previous-result result)
                    ;; The segment didn't produce, so just set it to zero
                    (setq segment-length 0)
                  ;; The segment produced. First, do any clean-up that might alter the length
                  ,@clean-up-make
                  ;; Compute the length
                  (let ((len (string-width (format-mode-line (powerline-render result)))))
                    (setq segment-length (- len result-length))
                    ;; If the segment is hidden, forcibly reset the result pointer
                    ;; If it's not hidden, update the result length and perform the rest of the cleanup
                    (if ,hidden
                        (setq result ,previous-result
                              needs-separator ,previous-needs-separator
                              separator-face ,previous-separator-face)
                      ,@clean-up-code
                      (setq result-length len)))))))))))

(defun spaceline-compile (&rest args)
  "Compile a modeline.

This function accepts a number of calling conventions:
- With three arguments, TARGET, LEFT and RIGHT, it compiles a
  modeline named TARGET, with segment lists LEFT and RIGHT for
  the left and right sides respectively.
- With two arguments, LEFT and RIGHT, the target takes the
  default value `main'.
- With one argument, TARGET, it recompiles the modeline named
  TARGET with the same segments as it was originally compiled.
- With no arguments, it recompiles all existing modelines with
  the same segments as they were originally compiled.

In all cases, a function called `spaceline-ml-TARGET' is defined,
which evaluates the modeline. It can then be used as a modeline
by setting `mode-line-format' to

    (\"%e\" (:eval (spaceline-ml-TARGET)))

If `spaceline-byte-compile' is non-nil, this function will be
byte-compiled. This is recommended for regular usage as it
improves performance significantly.

If the segments are known statically at compile time, consider
using `spaceline-generate' instead.

Each element in LEFT and RIGHT must be a valid segment. Namely,
- A literal string, integer or floating point number; or
- a symbol, which has been defined with
  `spaceline-define-segment'; or
- a list of segments; or
- a list where the first element is a segment, and the rest of
  the list is a plist.

The supported properties are
- `:priority', a number representing the priority of appearance of that
  segment over the others, the higher the number the higher the priority.
- `:when', a form that must evaluate to non-nil for the segment to
  show (default t)
- `:face', the face with which to render the segment; may either
  one of the variables `default-face', `other-face' or `highlight-face'
  (default `default-face') or a form evaluating to a face. Thus any
  face symbol which is not either of the above three must be quoted.
- `:separator', a string inserted between each element in a list
  segment (default \" \")
- `:tight-left', non-nil if the segment should have no padding on
  the left side (default nil)
- `:tight-right', non-nil if the segment should have no padding on
  the right side (default nil)
- `:tight', non-nil if the segment should have no padding on
  either side (default nil)
- `:fallback', another segment that will be called if no output
  is produced"
  (declare (indent defun))
  (interactive)
  (if (not args)
      ;; Recompile all modelines
      (dolist (target spaceline--mode-lines)
        (spaceline-compile (car target)))
    (let* (;; Handle the different calling conventions
           (nargs (length args))
           (target (if (cl-oddp nargs) (pop args) 'main))
           (left-segs (if (> nargs 1) (pop args)
                        (cadr (assq target spaceline--mode-lines))))
           (right-segs (if (> nargs 1) (pop args)
                         (cddr (assq target spaceline--mode-lines))))
           (target-func (intern (format "spaceline-ml-%s" target))))

      (eval (macroexpand-all `(spaceline-generate ,target ,left-segs ,right-segs)))

      (when spaceline-byte-compile
        (let ((byte-compile-warnings nil))
          (byte-compile target-func)))

      ;; This is not strictly required, but it lets people use
      ;; `spaceline-compile' as an all-in-one fix-everything button
      (powerline-reset))))

(defalias 'spaceline-install 'spaceline-compile)

(make-obsolete-variable 'spaceline-install 'spaceline-compile "2.0.2")

(defmacro spaceline-generate (&rest args)
  "Compile a modeline.

This is a macro-version of `spaceline-compile', useful for
generating a modeline function when the segments are known
statically at compile time.

This macro accepts two calling conventions:
- With three arguments, TARGET, LEFT and RIGHT, it compiles a
  modeline named TARGET, with segment lists LEFT and RIGHT for
  the left and right sides respectively.
- With two arguments, LEFT and RIGHT, the target takes the
  default value `main'.

In all cases, a function called `spaceline-ml-TARGET' is defined,
which evaluates the modeline. It can then be used as a modeline
by setting `mode-line-format' to

    (\"%e\" (:eval (spaceline-ml-TARGET)))

See the documentation for `spaceline-compile' for how to specify
LEFT and RIGHT."
  (declare (indent defun))
  (let* (;; Handle the different calling conventions
         (nargs (length args))
         (target (if (cl-oddp nargs) (pop args) 'main))
         (left-segs (car args))
         (right-segs (cadr args))
         (target-func (intern (format "spaceline-ml-%s" target)))
         ;; Special support for the global segment: compile list of excludes
         (global-excludes (append (spaceline--global-excludes left-segs)
                                  (spaceline--global-excludes right-segs)))
         ;; Symbols for runtime data
         (left-symbol (intern (format "spaceline--segments-code-%s-left" target)))
         (right-symbol (intern (format "spaceline--segments-code-%s-right" target)))
         (priority-symbol (intern (format "spaceline--runtime-data-%s" target)))
         (left-code `(spaceline--code-for-side
                      ,global-excludes ,left-symbol ,left-segs l))
         (right-code `(spaceline--code-for-side
                       ,global-excludes ,right-symbol ,right-segs r)))
    `(progn

       ;; Declare global runtime defaults
       (spaceline--declare-runtime ,left-segs ,right-segs ,left-symbol ,right-symbol ,priority-symbol)

       ;; Update stored segments so that recompilation will work
       (unless (assq ',target spaceline--mode-lines)
         (push '(,target) spaceline--mode-lines))
       (setcdr (assq ',target spaceline--mode-lines)
               '(,left-segs . ,right-segs))

       ;; Define the function that Emacs will call to generate the mode-line's
       ;; format string every time the mode-line is refreshed.
       (defun ,target-func ()
         ;; Initialize the local runtime if necessary
         (unless ,priority-symbol
           (spaceline--init-runtime ,left-symbol ,right-symbol ,priority-symbol))
         ;; Render the modeline
         (let ((fmt (spaceline--render-mode-line ,left-code ,right-code)))
           (and spaceline-responsive
                (spaceline--adjust-to-window ,priority-symbol fmt)
                (setq fmt (spaceline--render-mode-line ,left-code ,right-code)))
           fmt)))))

(defmacro spaceline--code-for-side
    (global-excludes runtime-symbol segments side)
  "Generate the code that will evaluate all segments for one side.

GLOBAL-EXCLUDES is used for the global segment, see `spaceline-define-segment'.

RUNTIME-SYMBOL is a symbol storing the runtime data for this side, one
three-element vector for each top-level segment, see
`spaceline--declare-runtime' and `spaceline--init-runtime'.

SEGMENTS is a list of segment specifications (see `spaceline--compile') and SIDE
is either l or r, respectively for the left and the right side."
  (let ((sep-style (format "powerline-%s" powerline-default-separator))
        (sep-dirs (spaceline--get-separator-dirs side)))
    `(let* ((default-face face1)
            (other-face face2)
            (default-sep ',(intern (format "%s-%s" sep-style (car sep-dirs))))
            (other-sep ',(intern (format "%s-%s" sep-style (cdr sep-dirs))))
            (global-excludes ',global-excludes)
            (result-length 0)
            (segment-length 0)
            (runtime-pointer ,runtime-symbol)
            prior
            next-prior
            needs-separator
            separator-face
            result)
       ,@(--map `(let ((runtime-data (pop runtime-pointer)))
                   ,@(spaceline--gen-segment it side '(not (spaceline--shown runtime-data)))
                   (spaceline--set-length runtime-data segment-length))
                (if (eq 'l side) segments (reverse segments)))
       ,@(spaceline--gen-separator 'line-face side)
       ,(if (eq side 'l) '(reverse result) 'result))))

(defmacro spaceline--priority (val) `(aref ,val 0))
(defmacro spaceline--length (val) `(aref ,val 1))
(defmacro spaceline--shown (val) `(aref ,val 2))
(defmacro spaceline--set-length (vec val) `(aset ,vec 1 ,val))
(defmacro spaceline--set-shown (vec val) `(aset ,vec 2 ,val))

(defmacro spaceline--declare-runtime
    (segments-left segments-right left-symbol right-symbol priority-symbol)
  "Initialize the global runtime data for a modeline.

The runtime consist of a three-element vector for each top-level
segment in the modeline. The elements are:
- priority: The priority of the segment (derived from its `:priority' property)
- length: The rendered length of the segment
- shown: Whether the segment is displayed or not

The effect of this function is to create the default values for
these vectors, and store them in the varibles LEFT-SYMBOL and
RIGHT-SYMBOL, respectively, which are lists. The variable
PRIORITY-SYMBOL is initialized with default value nil.

See `spaceline--init-runtime' for more information."
  (let ((left (--map (spaceline--parse-segment-spec it
                       (vector (or (plist-get props :priority) 0) 0 t))
                     segments-left))
        (right (--map (spaceline--parse-segment-spec it
                        (vector (or (plist-get props :priority) 0) 0 t))
                      segments-right)))
    `(progn
       ;; We use both defvar and setq so that recompilation will work.
       (defvar-local ,left-symbol nil "See `spaceline--declare-runtime'.")
       (setq-default ,left-symbol ',left)
       (defvar-local ,right-symbol nil "See `spaceline--declare-runtime'.")
       (setq-default ,right-symbol ',(reverse right))
       (defvar-local ,priority-symbol nil "See `spaceline--declare-runtime'.")

       ;; Since we have possibly changed the defaults of these variables, kill
       ;; any local bindings that may exist.
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (kill-local-variable ',left-symbol)
           (kill-local-variable ',right-symbol)
           (kill-local-variable ',priority-symbol))))))

(defmacro spaceline--render-mode-line (left-code right-code)
  "Call powerline to generate the mode-line format string.
LEFT-CODE and RIGHT-CODE are the code that will be used "
  `(progn
     (run-hooks 'spaceline-pre-hook)
     (let* ((active-strict (powerline-selected-window-active))
            (active (or spaceline-always-show-segments active-strict))
            (line-face (spaceline--get-face 'line active-strict))
            (highlight-face (spaceline--get-face 'highlight active-strict))
            (face1 (spaceline--get-face 'face1 active-strict))
            (face2 (spaceline--get-face 'face2 active-strict))
            (lhs ,left-code)
            (rhs ,right-code))
       (concat
        ,@(when spaceline-inflation
            `((propertize "\u200b" 'display
                          '((raise ,(/ (1- spaceline-inflation) -2.0))
                            (height ,spaceline-inflation)))))
        (powerline-render lhs)
        (powerline-fill line-face (powerline-width rhs))
        (powerline-render rhs)))))

(defmacro spaceline--init-runtime (left-symbol right-symbol priority-symbol)
  "Initialize data structures used for the responsiveness of the modeline.

This function
- creates local deep copies of the global values of LEFT-SYMBOL and
  RIGHT-SYMBOL,
- initializes PRIORITY-SYMBOl, a reordering of the same vectors whose order
  (by priority) decides the order of segments disappearance / reappearance with
  the size of the window.

Note that the changes in the resulting PRIORITY-SYMBOL list are
visible from LEFT-SYMBOL and RIGHT-SYMBOL, and vice versa. This
creates a data structure that is efficiently accessible both in
order of priority and order of segments."
  `(let ((left (--map (copy-tree it) (default-value ',left-symbol)))
         (right (--map (copy-tree it) (default-value ',right-symbol)))
         priority)
     (set (make-local-variable ',left-symbol) left)
     (set (make-local-variable ',right-symbol) right)
     (while (or left right)
       (when left (push (pop left) priority))
       (when right (push (pop right) priority)))
     (set (make-local-variable ',priority-symbol)
          (sort priority 'spaceline--compare-priorities))))

(defmacro spaceline--adjust-to-window (responsiveness-runtime-data format)
  "Adjust the spaceline to the window by hiding or showing segments.

RESPONSIVENESS-RUNTIME-DATA is a list of segments runtime data used to hide or
show segments, see `spaceline--declare-runtime' for more info about
how responsiveness works.

FMT is the rendered modeline with the current visibility settings.

Returns a truthy value if the visibility of any segment changed."
  ;; `1-' to not count the space generated by `powerline-fill'
  `(let ((total-length (1- (string-width (format-mode-line ,format))))
         (width (+ (window-width)
                   (or (cdr (window-margins)) 0)
                   (or (car (window-margins)) 0)))
         changed)
     (if (> total-length width)
         ;; The modeline is too long, so try to hide some segments that are shown
         (let ((to-hide (--drop-while (not (spaceline--shown it))
                                      ,responsiveness-runtime-data)))
           (--each-while to-hide
               (< width total-length)
             (cl-decf total-length (spaceline--length it))
             (spaceline--set-shown it nil)
             (setq changed t)))
       ;; The modeline is shorter than it could be, so try to show some hidden segments
       (let ((to-show (--drop-while (spaceline--shown it)
                                    (reverse ,responsiveness-runtime-data))))
         (--each-while to-show
             (> width (+ total-length (spaceline--length it)))
           (cl-incf total-length (spaceline--length it))
           (spaceline--set-shown it t)
           (setq changed t))))
     changed))

(defun spaceline--compare-priorities (first-alist second-alist)
  "Comparison predicate for sorting the segments runtime data by priority.
Used as a predicate for `sort' in `spaceline--init-runtime'."
  (let ((first (spaceline--priority first-alist))
        (second (spaceline--priority second-alist)))
    (< first second)))

(defmacro spaceline-define-segment (name value &rest props)
  "Define a modeline segment called NAME with value VALUE and properties PROPS.

Its value is computed by the form VALUE. The segment will not
produce output if VALUE evaluates to nil or an empty string. All
other values are assumed truthy.

This macro defines a variable `spaceline--NAME-p' whose value can
be used to switch the segment on or off. Its initial value is
given by the optional keyword argument `:enabled', which defaults
to true.

If the segment is intended as a replacement for data which is
otherwise inserted into `global-mode-string' (typically by
another package), you can use the keyword argument
`:global-override' to disable that. Its value is a single element
or a list of elements which will be removed from
`global-mode-string' before evaluation of the `global' segment.
For modelines that do not use the `global' segment, this has no
effect.

All properties accepted in `spaceline-compile' are also accepted
here. They are stored in a plist attached to the symbol
`spaceline--NAME-p' to be inspected at compilation time by
`spaceline-compile'.

When a segment is redefined, the modelines must be recompiled for
the changes to take effect."
  (declare (indent 1)
           (doc-string 2))
  (let* ((wrapper-func (intern (format "spaceline--segment-%S" name)))
         (toggle-var (intern (format "spaceline-%S-p" name)))
         (toggle-func (intern (format "spaceline-toggle-%S" name)))
         (toggle-func-on (intern (format "spaceline-toggle-%S-on" name)))
         (toggle-func-off (intern (format "spaceline-toggle-%S-off" name)))
         (docstring (when (stringp value)
                      (prog1 value
                        (setq value (car props)
                              props (cdr props)))))
         (value `(when ,toggle-var ,value))
         (enabled (if (plist-member props :enabled)
                      (plist-get props :enabled)
                    t))
         (global-override (plist-get props :global-override))
         (global-override (if (listp global-override)
                              global-override
                            (list global-override))))
    `(progn
       (defvar ,toggle-var ,enabled
         ,(format "True if modeline segment %S is enabled." name))

       (defun ,toggle-func () (interactive) (setq ,toggle-var (not ,toggle-var)))
       (defun ,toggle-func-on () (interactive) (setq ,toggle-var t))
       (defun ,toggle-func-off () (interactive) (setq ,toggle-var nil))

       ;; Explicitly set the plist, in case the segment is redefined
       (let ((doc (get ',toggle-var 'variable-documentation)))
         (setplist ',toggle-var ',props)
         (put ',toggle-var 'variable-documentation doc))

       ;; These properties must be explicitly set
       (put ',toggle-var :code ',value)
       (put ',toggle-var :global-override ',global-override))))

(defun spaceline--global-excludes (segments)
  "Compute global overrides from the segment list SEGMENTS."
  (let (excludes)
    (dolist (s-spec segments)
      (spaceline--parse-segment-spec s-spec
        (setq excludes (append (plist-get props :global-override) excludes))
        (when (listp segment)
          (setq excludes (append (spaceline--global-excludes segment) excludes)))))
    excludes))

(spaceline-define-segment global
  (let ((global (if (listp global-mode-string)
                    (cons "" (-difference global-mode-string global-excludes))
                  global-mode-string)))
    (when (spaceline--mode-line-nonempty global)
      (string-trim (powerline-raw global)))))

(defun spaceline--string-trim-from-center (str len)
  "Return STR with its center chars trimmed for it to be a maximum length of LEN.
When characters are trimmed, they are replaced with '...'."
  (if (> (length str) len)
      (let ((mid (/ (- len 3) 2)))
        (concat (substring str 0 mid)
                (apply #'propertize "..." (text-properties-at (- mid 1) str))
                (substring str (- (1+ mid)) nil)))
    str))

(provide 'spaceline)

;;; spaceline.el ends here
