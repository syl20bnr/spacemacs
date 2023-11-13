;;; polymode-methods.el --- Methods for polymode classes -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013-2022  Free Software Foundation, Inc.
;; Author: Vitalie Spinu
;; URL: https://github.com/polymode/polymode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Code:

(require 'polymode-core)


;;; INITIALIZATION

(cl-defgeneric pm-initialize (object)
  "Initialize current buffer with OBJECT.")

(defun pm--instantiate-innermodes (config)
  "Instantiate CONFIG's innermodes respecting inheritance."
  (let ((inner-syms (delete-dups
                     (delq :inherit
                           (apply #'append
                                  (pm--collect-parent-slots
                                   config 'innermodes
                                   (lambda (obj)
                                     (memq :inherit
                                           (eieio-oref obj 'innermodes)))))))))
    (oset config -innermodes
          (mapcar (lambda (sub-name)
                    (clone (symbol-value sub-name)))
                  inner-syms))))

(cl-defmethod pm-initialize ((config pm-polymode))
  "Initialization of host buffers.
Ran by the polymode mode function."
  ;; Not calling config's '-minor-mode in hosts because this pm-initialize is
  ;; called from minor-mode itself in base buffers.
  (let* ((hostmode-name (eieio-oref config 'hostmode))
         (hostmode (if hostmode-name
                       (clone (symbol-value hostmode-name))
                     (pm-host-chunkmode :name "ANY" :mode nil))))
    (let ((pm-initialization-in-progress t)
          ;; Set if nil! This allows unspecified host chunkmodes to be used in
          ;; minor modes.
          (host-mode (or (eieio-oref hostmode 'mode)
                         (oset hostmode :mode major-mode))))
      ;; FIXME: mode hooks and local var hacking happens here. Need to move it
      ;; to the end.
      (pm--mode-setup host-mode)
      (oset hostmode -buffer (current-buffer))
      (oset config -hostmode hostmode)
      (setq pm--core-buffer-name (buffer-name)
            pm/polymode config
            pm/chunkmode hostmode
            pm/current t
            pm/type nil)
      (pm--instantiate-innermodes config)
      (pm--common-setup)
      (add-hook 'after-save-hook #'polymode-after-save nil t)
      (add-hook 'before-save-hook #'polymode-before-save nil t))
    (pm--run-init-hooks hostmode 'host 'polymode-init-host-hook)
    ;; (run-mode-hooks) ;; FIXME
    ))

(cl-defmethod pm-initialize ((chunkmode pm-inner-chunkmode) &optional type mode)
  "Initialization of the innermodes' (indirect) buffers."
  ;; run in chunkmode indirect buffer
  (setq mode (or mode (pm--get-innermode-mode chunkmode type)))
  (let* ((pm-initialization-in-progress t)
         (post-fix (replace-regexp-in-string "poly-\\|-mode" "" (symbol-name mode)))
         (core-name (format "%s[%s]" (buffer-name (pm-base-buffer))
                            (or (cdr (assoc post-fix polymode-mode-abbrev-aliases))
                                post-fix)))
         (new-name (generate-new-buffer-name core-name)))
    (rename-buffer new-name)
    ;; FIXME: Mode hooks and local var hacking happens here. Need to move it to
    ;; the end. But then font-lock is not activated and buffers not installed
    ;; correctly.
    ;; FIXME: One severe problem is that --*- mode: poly-xyz; does not
    ;; currently work. See poly-noweb/samples/hello.nw.
    (pm--mode-setup mode)
    (pm--move-vars '(pm/polymode buffer-file-coding-system) (pm-base-buffer))
    ;; FIXME: This breaks if different chunkmodes use same-mode buffer. Even for
    ;; head/tail the value of pm/type will be wrong for tail
    (setq pm--core-buffer-name core-name
          pm/chunkmode chunkmode
          pm/type (pm-true-span-type chunkmode type))
    ;; FIXME: should not be here?
    (vc-refresh-state)
    (pm--common-setup)
    (add-hook 'syntax-propertize-extend-region-functions
              #'polymode-syntax-propertize-extend-region-in-host
              -90 t)
    (pm--move-vars polymode-move-these-vars-from-base-buffer (pm-base-buffer))
    ;; If this rename happens before the mode setup font-lock doesn't work in
    ;; inner buffers.
    (when pm-hide-implementation-buffers
      (rename-buffer (generate-new-buffer-name (concat " " pm--core-buffer-name)))))
  (pm--run-init-hooks chunkmode type 'polymode-init-inner-hook)
  ;; Call polymode mode for the sake of the keymap and hook. Same minor mode
  ;; which runs in the host buffer but without recursive call to `pm-initialize'.
  (funcall (eieio-oref pm/polymode '-minor-mode))
  ;; finally run the mode's native hooks (FIXME)
  ;; (run-mode-hooks)
  )

(defvar poly-lock-allow-fontification)
(defun pm--mode-setup (mode &optional buffer)
  ;; General major-mode install. Should work for both indirect and base buffers.
  ;; PM objects are not yet initialized (pm/polymode, pm/chunkmode, pm/type)
  (with-current-buffer (or buffer (current-buffer))
    ;; don't re-install if already there; polymodes can be used as minor modes.
    (unless (eq major-mode mode)
      (let ((polymode-mode t)           ;major-modes might check this
            (base (buffer-base-buffer))
            ;; Some modes (or minor-modes which are run in their hooks) call
            ;; font-lock functions directly on the entire buffer (#212 for an
            ;; example). They were inhibited here before, but these variables
            ;; are designed to be set by modes, so our setup doesn't have an
            ;; effect in those cases and we get "Making xyz buffer-local while
            ;; locally let-bound!" warning which seems to be harmless but
            ;; annoying. The only solution seems to be to advice those
            ;; functions, particularly `font-lock-fontify-region`.
            ;; (font-lock-flush-function 'ignore)
            ;; (font-lock-ensure-function 'ignore)
            ;; (font-lock-fontify-buffer-function 'ignore)
            ;; (font-lock-fontify-region-function 'ignore)
            (font-lock-function 'ignore)
            ;; Mode functions can do arbitrary things. We inhibt all PM hooks
            ;; because PM objects have not been setup yet.
            (pm-allow-after-change-hook nil)
            (poly-lock-allow-fontification nil))
        ;; run-mode-hooks needs buffer-file-name, so we transfer base vars twice
        (when base
          (pm--move-vars polymode-move-these-vars-from-base-buffer base))
        (condition-case-unless-debug err
            ;; !! run-mode-hooks and hack-local-variables run here
            (funcall mode)
          (error (message "Polymode error (pm--mode-setup '%s): %s"
                          mode (error-message-string err))))
        ;; In emacs 27 this is called from run-mode-hooks
        (and (bound-and-true-p syntax-propertize-function)
             (not (local-variable-p 'parse-sexp-lookup-properties))
             (setq-local parse-sexp-lookup-properties t))))
    (setq polymode-mode t)
    (current-buffer)))

(defvar syntax-ppss-wide)
(defun pm--common-setup (&optional buffer)
  "Run common setup in BUFFER.
Runs after major mode and core polymode structures have been
initialized. Return the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (object-add-to-list pm/polymode '-buffers (current-buffer))

    ;; INDENTATION
    ;; If poly-minor-mode is called twice don't overwrite the original (#289)
    (unless pm--indent-line-function-original
      (setq-local pm--indent-line-function-original
                  (if (memq indent-line-function '(nil indent-relative indent-relative-maybe))
                      #'pm--indent-line-basic
                    indent-line-function)))
    (setq-local indent-line-function #'pm-indent-line-dispatcher)
    (unless pm--indent-region-function-original
      (setq-local pm--indent-region-function-original
                  (if (memq indent-region-function '(nil indent-region-line-by-line))
                      #'pm--indent-region-line-by-line
                    indent-region-function)))
    (setq-local indent-region-function #'pm-indent-region)

    ;; FILL
    (unless pm--fill-forward-paragraph-original
      (setq-local pm--fill-forward-paragraph-original fill-forward-paragraph-function))
    (setq-local fill-forward-paragraph-function #'polymode-fill-forward-paragraph)

    ;; HOOKS
    (add-hook 'kill-buffer-hook #'polymode-after-kill-fixes nil t)
    (add-hook 'pre-command-hook #'polymode-pre-command -99 t)
    (add-hook 'post-command-hook #'polymode-post-command 99 t)
    (add-hook 'before-change-functions #'polymode-before-change -95 t)
    (add-hook 'after-change-functions #'polymode-after-change 95 t)

    ;; FONT LOCK (see poly-lock.el)
    (setq-local font-lock-function #'poly-lock-mode)
    ;; Font lock is a globalized minor mode and is thus initialized in
    ;; `after-change-major-mode-hook' within `run-mode-hooks'. As a result
    ;; poly-lock won't get installed if polymode is installed as a minor mode or
    ;; interactively. We add font/poly-lock in all buffers (because this is how
    ;; inner buffers are installed) but use `poly-lock-allow-fontification' to
    ;; disallow fontification in buffers which don't want font-lock (aka those
    ;; buffers where `turn-on-font-lock-if-desired' doesn't activate font-lock).
    ;; FIXME: can poly-lock-mode be used here instead?
    (setq-local poly-lock-allow-fontification font-lock-mode)
    ;; Make sure to re-install with our font-lock-function as
    ;; `turn-on-font-lock-if-desired' from above might actually not call it.
    (font-lock-mode t)
    ;; (font-lock-flush)

    ;; SYNTAX (must be done after font-lock for after-change order)
    (with-no-warnings
      ;; [OBSOLETE as of 25.1 but we still protect it]
      (pm-around-advice syntax-begin-function 'pm-override-output-position))
    ;; (advice-remove 'c-beginning-of-syntax #'pm-override-output-position)

    ;; Ideally this should be called in some hook to avoid minor-modes messing
    ;; it up. Setting even if syntax-propertize-function is nil to have more
    ;; control over syntax-propertize--done.
    (unless (eq syntax-propertize-function #'polymode-syntax-propertize)
      (setq-local pm--syntax-propertize-function-original syntax-propertize-function)
      (setq-local syntax-propertize-function #'polymode-syntax-propertize))
    (setq-local syntax-ppss-wide (cons nil nil))
    ;; Flush ppss in all buffers. Must be done in first after-change (see
    ;; https://lists.gnu.org/archive/html/emacs-devel/2019-03/msg00500.html)
    ;; TODO: Consider just advising syntax-ppss-flush-cache once the above is
    ;; fixed in emacs.
    (add-hook 'after-change-functions #'polymode-flush-syntax-ppss-cache -99 t)

    (current-buffer)))


;;; BUFFER CREATION

(cl-defgeneric pm-get-buffer-create (chunkmode &optional type)
  "Get the indirect buffer associated with SUBMODE and SPAN-TYPE.
Create and initialize the buffer if does not exist yet.")

(cl-defmethod pm-get-buffer-create ((chunkmode pm-host-chunkmode) &optional type)
  (when type
    (error "Cannot create host buffer of type '%s'" type))
  (let ((buff (eieio-oref chunkmode '-buffer)))
    (if (buffer-live-p buff)
        buff
      (error "Cannot create host buffer for host chunkmode %s" (eieio-object-name chunkmode)))))

(cl-defmethod pm-get-buffer-create ((chunkmode pm-inner-chunkmode) &optional type)
  (let ((buff (cl-case type
                (body (eieio-oref chunkmode '-buffer))
                (head (eieio-oref chunkmode '-head-buffer))
                (tail (eieio-oref chunkmode '-tail-buffer))
                (t (error "Don't know how to select buffer of type '%s' for chunkmode '%s'"
                          type (eieio-object-name chunkmode))))))
    (if (buffer-live-p buff)
        buff
      (let ((new-buff (pm--get-innermode-buffer-create chunkmode type)))
        (pm--set-innermode-buffer chunkmode type new-buff)))))

(defun pm--get-innermode-buffer-create (chunkmode type &optional force-new)
  (let ((mode (pm--get-innermode-mode chunkmode type)))
    (or
     ;; 1. search through the existing buffer list
     (unless force-new
       (cl-loop for bf in (eieio-oref pm/polymode '-buffers)
                when (let ((out (and (buffer-live-p bf)
                                     (eq mode (buffer-local-value 'major-mode bf)))))
                       out)
                return bf))
     ;; 2. create new
     (with-current-buffer (pm-base-buffer)
       (let* ((new-name (generate-new-buffer-name (buffer-name)))
              (new-buffer (make-indirect-buffer (current-buffer) new-name)))
         (with-current-buffer new-buffer
           (pm-initialize chunkmode type mode))
         new-buffer)))))

(defun pm-get-buffer-of-mode (mode)
  (let ((mode (pm--true-mode-symbol mode)))
    (or
     ;; 1. search through the existing buffer list
     (cl-loop for bf in (eieio-oref pm/polymode '-buffers)
              when (and (buffer-live-p bf)
                        (eq mode (buffer-local-value 'major-mode bf)))
              return bf)
     ;; 2. create new if body mode matched
     (cl-loop for imode in (eieio-oref pm/polymode '-innermodes)
              when (eq mode (eieio-oref imode 'mode))
              return (pm--get-innermode-buffer-create imode 'body 'force)))))

(defun pm--set-innermode-buffer (obj type buff)
  "Assign BUFF to OBJ's slot(s) corresponding to TYPE."
  (with-slots (-buffer head-mode -head-buffer tail-mode -tail-buffer) obj
    (pcase (list type head-mode tail-mode)
      (`(body body ,(or `nil `body))
       (setq -buffer buff
             -head-buffer buff
             -tail-buffer buff))
      (`(body ,_ body)
       (setq -buffer buff
             -tail-buffer buff))
      (`(body ,_ ,_ )
       (setq -buffer buff))
      (`(head ,_ ,(or `nil `head))
       (setq -head-buffer buff
             -tail-buffer buff))
      (`(head ,_ ,_)
       (setq -head-buffer buff))
      (`(tail ,_ ,(or `nil `head))
       (setq -tail-buffer buff
             -head-buffer buff))
      (`(tail ,_ ,_)
       (setq -tail-buffer buff))
      (_ (error "Type must be one of 'body, 'head or 'tail")))))


;;; SPAN MANIPULATION

(cl-defgeneric pm-get-span (chunkmode &optional pos)
  "Ask the CHUNKMODE for the span at point.
Return a list of three elements (TYPE BEG END OBJECT) where TYPE
is a symbol representing the type of the span surrounding
POS (head, tail, body). BEG and END are the coordinates of the
span. OBJECT is a suitable object which is `responsible' for this
span. This is an object that could be dispatched upon with
`pm-select-buffer'. Should return nil if there is no SUBMODE
specific span around POS. Not to be used in programs directly;
use `pm-innermost-span'.")

(cl-defmethod pm-get-span (chunkmode &optional _pos)
  "Return nil.
Host modes usually do not compute spans."
  (unless chunkmode
    (error "Dispatching `pm-get-span' on a nil object"))
  nil)

(cl-defmethod pm-get-span ((chunkmode pm-inner-chunkmode) &optional pos)
  "Return a list of the form (TYPE POS-START POS-END SELF).
TYPE can be `body', `head' or `tail'. SELF is the CHUNKMODE."
  (with-slots (head-matcher tail-matcher head-mode tail-mode) chunkmode
    (let ((span (pm--span-at-point head-matcher tail-matcher pos
                                   (eieio-oref chunkmode 'can-overlap))))
      (when span
        (append span (list chunkmode))))))

(cl-defmethod pm-get-span ((_chunkmode pm-inner-auto-chunkmode) &optional _pos)
  (let ((span (cl-call-next-method)))
    (if (null (car span))
        span
      (setf (nth 3 span) (apply #'pm--get-auto-chunkmode span))
      span)))

;; (defun pm-get-chunk (ichunkmode &optional pos)
;;   (with-slots (head-matcher tail-matcher head-mode tail-mode) ichunkmode
;;     (pm--span-at-point
;;      head-matcher tail-matcher (or pos (point))
;;      (eieio-oref ichunkmode 'can-overlap)
;;      t)))


(cl-defgeneric pm-next-chunk (chunkmode &optional pos)
  "Ask the CHUNKMODE for the chunk after POS.
Return a list of five elements (CHUNKMODE HEAD-BEG HEAD-END
TAIL-BEG TAIL-END).")

(cl-defmethod pm-next-chunk (_chunkmode &optional _pos)
  nil)

(cl-defmethod pm-next-chunk ((chunkmode pm-inner-chunkmode) &optional pos)
  (with-slots (head-matcher tail-matcher head-mode tail-mode) chunkmode
    (let ((raw-chunk (pm--next-chunk
                      head-matcher tail-matcher (or pos (point))
                      (eieio-oref chunkmode 'can-overlap))))
      (when raw-chunk
        (cons chunkmode raw-chunk)))))

(cl-defmethod pm-next-chunk ((chunkmode pm-inner-auto-chunkmode) &optional pos)
  (with-slots (head-matcher tail-matcher head-mode tail-mode) chunkmode
    (let ((raw-chunk (pm--next-chunk
                      head-matcher tail-matcher (or pos (point))
                      (eieio-oref chunkmode 'can-overlap))))
      (when raw-chunk
        (cons (pm--get-auto-chunkmode 'head (car raw-chunk) (cadr raw-chunk) chunkmode)
              raw-chunk)))))

;; FIXME: cache somehow?
(defun pm--get-auto-chunkmode (type beg end proto)
  (save-excursion
    (goto-char beg)
    (unless (eq type 'head)
      (goto-char end)     ; fixme: add multiline matchers to micro-optimize this
      (let ((matcher (pm-fun-matcher (eieio-oref proto 'head-matcher))))
        ;; can be multiple incomplete spans within a span
        (while (< beg (goto-char (car (funcall matcher -1)))))))
    (let* ((str (let ((matcher (eieio-oref proto 'mode-matcher)))
                  (when (stringp matcher)
                    (setq matcher (cons matcher 0)))
                  (cond  ((consp matcher)
                          (re-search-forward (car matcher) (point-at-eol) t)
                          (match-string-no-properties (cdr matcher)))
                         ((functionp matcher)
                          (funcall matcher)))))
           (mode (pm-get-mode-symbol-from-name str (eieio-oref proto 'fallback-mode))))
      (if (eq mode 'host)
          (oref pm/polymode -hostmode)
        ;; chunkname:MODE serves as ID (e.g. `markdown-fenced-code:emacs-lisp-mode`).
        ;; Head/tail/body indirect buffers are shared across chunkmodes and span
        ;; types.
        (let ((automodes (eieio-oref pm/polymode '-auto-innermodes)))
          (if (memq proto automodes)
              ;; a. if proto already part of the list return
              proto
            (let ((name (concat (pm-object-name proto) ":" (symbol-name mode))))
              (or
               ;; b. loop through installed inner modes
               (cl-loop for obj in automodes
                        when (equal name (pm-object-name obj))
                        return obj)
               ;; c. create new
               (let ((innermode (clone proto :name name :mode mode)))
                 (object-add-to-list pm/polymode '-auto-innermodes innermode)
                 innermode)))))))))


;;; INDENT

;; indent-region-line-by-line for polymode buffers (more efficient, works on
;; emacs 25, but no progress reporter)
(defun pm--indent-region-line-by-line (start end)
  (save-excursion
    ;; called from pm--indent-raw; so we know we are in the same span with
    ;; buffer set and narrowed to span if 'protect-indent is non-nil
    (let ((span (pm-innermost-span start)))
      (setq end (copy-marker end))
      (goto-char start)
      (while (< (point) end)
        (unless (and (bolp) (eolp))
          ;; fixme: html-erb jumps line here; need save-excursion. why?
          (save-excursion (pm-indent-line (nth 3 span) span)))
        (forward-line 1))
      (move-marker end nil))))

(defun pm--indent-line-basic ()
  "Used as `indent-line-function' for modes with tab indent."
  ;; adapted from indent-according-to-mode
  (let ((column (save-excursion
                  (beginning-of-line)
                  (if (bobp) 0
                    (beginning-of-line 0)
                    (if (looking-at "[ \t]*$") 0 (current-indentation))))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to column)
      (save-excursion (indent-line-to column)))))

(defun pm--indent-raw (span fn-sym &rest args)
  ;; fixme: do save-excursion instead of this?
  (let ((point (point)))
    ;; do fast synchronization here
    (save-current-buffer
      (pm-set-buffer span)
      (goto-char point)
      (let ((fn (symbol-value fn-sym)))
        (when fn
          (if (eieio-oref (nth 3 span) 'protect-indent)
              (pm-with-narrowed-to-span span
                (apply fn args))
            (apply fn args))))
      (setq point (point)))
    (goto-char point)))

(defun pm--indent-line-raw (span)
  (pm--indent-raw span 'pm--indent-line-function-original)
  (pm--reindent-with+-indent span (point-at-bol) (point-at-eol)))

(defun pm--indent-region-raw (span beg end)
  (pm--indent-raw span 'pm--indent-region-function-original beg end)
  (pm--reindent-with+-indent span beg end))

(defun pm-indent-region (beg end)
  "Indent region between BEG and END in polymode buffers.
Function used for `indent-region-function'."
  ;; (message "(pm-indent-region %d %d)" beg end)
  ;; cannot use pm-map-over-spans here because of the buffer modifications
  (let ((inhibit-point-motion-hooks t)
        (end (copy-marker end)))
    (save-excursion
      (while (< beg end)
        (goto-char beg)
        (back-to-indentation)
        (setq beg (point))
        (let ((span (pm-innermost-span beg 'no-cache)))
          (let* ((end-span (copy-marker (nth 2 span)))
                 (end1 (min end end-span)))
            (goto-char beg)
            ;; (pm-switch-to-buffer)
            ;; indent first line separately
            (pm-indent-line (nth 3 span) span)
            (beginning-of-line 2)
            (when (< (point) end1)
              ;; we know that span end was moved, hard reset without recomputation
              (setf (nth 2 span) end-span)
              (pm--indent-region-raw span (point) end1))
            (setq beg (max end1 (point)))))))
    (move-marker end nil)))

(defun pm-indent-line-dispatcher (&optional span)
  "Dispatch `pm-indent-line' methods on current SPAN.
Value of `indent-line-function' in polymode buffers."
  ;; NB: No buffer switching in indentation functions. See comment at
  ;; pm-switch-to-buffer.
  (let ((span (or span (pm-innermost-span
                        (save-excursion (back-to-indentation) (point)))))
        (inhibit-read-only t))
    (pm-indent-line (nth 3 span) span)))

(cl-defgeneric pm-indent-line (chunkmode &optional span)
  "Indent current line.
Protect and call original indentation function associated with
the chunkmode.")

(cl-defmethod pm-indent-line ((_chunkmode pm-chunkmode) span)
  (let ((pos (point))
        (delta))
    (back-to-indentation)
    (setq delta (- pos (point)))
    (let* ((bol (point-at-bol))
           (span (or span (pm-innermost-span)))
           (prev-span-pos)
           (first-line (save-excursion
                         (goto-char (nth 1 span))
                         (unless (bobp)
                           (setq prev-span-pos (1- (point))))
                         (forward-line)
                         (<= bol (point)))))
      (pm--indent-line-raw span)
      (when (and first-line prev-span-pos)
        (pm--reindent-with-extra-offset (pm-innermost-span prev-span-pos)
                                        'post-indent-offset)))
    (when (and delta (> delta 0))
      (goto-char (+ (point) delta)))))

(cl-defmethod pm-indent-line ((_chunkmode pm-inner-chunkmode) span)
  "Indent line in inner chunkmodes.
When point is at the beginning of head or tail, use parent chunk
to indent."
  (let ((pos (point))
        (delta))
    (back-to-indentation)
    (setq delta (- pos (point)))
    (unwind-protect
        (cond

         ;; 1. HEAD or TAIL (we assume head or tail fits in one line for now)
         ((or (eq 'head (car span))
              (eq 'tail (car span)))
          (goto-char (nth 1 span))
          (when (not (bobp))
            ;; ind-point need not be in prev-span; there might be other spans in between
            (let ((prev-span (pm-innermost-span (1- (point)))))
              (if (eq 'tail (car span))
                  (indent-line-to (pm--head-indent prev-span))
                ;; head indent and adjustments
                ;; (pm-indent-line (nth 3 prev-span) prev-span)
                (pm--indent-line-raw prev-span)
                (let ((prev-tail-pos (save-excursion
                                       (beginning-of-line)
                                       (skip-chars-backward " \t\n")
                                       (if (bobp) (point) (1- (point))))))
                  (setq prev-span (pm-innermost-span prev-tail-pos)))
                (pm--reindent-with-extra-offset prev-span 'post-indent-offset)
                (pm--reindent-with-extra-offset span 'pre-indent-offset)))))

         ;; 2. BODY
         (t
          (if (< (point) (nth 1 span))
              ;; first body line in the same line with header (re-indent at indentation)
              (pm-indent-line-dispatcher)
            (let ((fl-indent (pm--first-line-indent span)))
              (if fl-indent
                  ;; We are not on the 1st line
                  (progn
                    ;; thus indent according to mode
                    (pm--indent-line-raw span)
                    (when (bolp)
                      ;; When original mode's indented to bol, match with the
                      ;; first line indent. Otherwise it's a continuation
                      ;; indentation and we assume the original function did it
                      ;; correctly with respect to previous lines.
                      (indent-to fl-indent)))
                ;; On the first line. Indent with respect to header line.
                (let ((delta (save-excursion
                               (goto-char (nth 1 span))
                               (+
                                (pm--oref-value (nth 3 span) 'body-indent-offset)
                                (cond
                                 ;; empty line
                                 ((looking-at-p "[ \t]*$") 0)
                                 ;; inner span starts at bol; honor +-indent cookie
                                 ((= (point) (point-at-bol))
                                  (pm--+-indent-offset-on-this-line span))
                                 ;; code after header
                                 (t
                                  (end-of-line)
                                  (skip-chars-forward "\t\n")
                                  (pm--indent-line-raw span)
                                  (- (point) (point-at-bol))))))))
                  (indent-line-to
                   ;; indent with respect to header line
                   (+ delta (pm--head-indent span)))))))))

      ;; keep point on same characters
      (when (and delta (> delta 0))
        (goto-char (+ (point) delta))))))

(defun pm--first-line-indent (&optional span)
  "Return indentation of first line if not on a first line."
  (setq span (or span (pm-innermost-span)))
  (let ((pos (point)))
    (save-excursion
      (goto-char (nth 1 span))
      (when (not (bolp)) ; for spans which don't start at bol, first line is next line
        (forward-line 1))
      (skip-chars-forward " \t\n\r")
      (when (< (point-at-eol) pos)
        ;; not on first line -> compute indent of the first line
        (goto-char (nth 1 span))
        (skip-chars-forward " \t\n\r")
        (back-to-indentation)
        (when (< (point-at-eol) pos)
          (- (point) (point-at-bol)))))))

;; SPAN is a body span; do nothing if narrowed to body
(defun pm--head-indent (&optional span)
  (save-restriction
    (widen)
    (save-excursion
      (let* ((span (or span (pm-innermost-span)))
             ;; span is innermost, thus can be truncated due to nested innermodes
             (span (pm-get-span (nth 3 span) (nth 1 span)))
             (sbeg (nth 1 span)))
        (goto-char sbeg)
        (backward-char 1)
        (let ((head-span (pm-innermost-span)))
          (if (eq (car head-span) 'head)
              (goto-char (nth 1 head-span))
            ;; body span is not preceded by a head span. We don't have such
            ;; practical cases yet, but headless spans are real - indented blocks
            ;; for instance.
            (goto-char sbeg)))
        (back-to-indentation)
        (current-column)))))

(defun pm--+-indent-offset-on-this-line (span)
  (if (re-search-forward "\\([+-]\\)indent" (point-at-eol) t)
      (let ((basic-offset (pm--oref-value (nth 3 span) 'indent-offset)))
        (if (string= (match-string 1) "-")
            (- basic-offset)
          basic-offset))
    0))

(defun pm--reindent-with+-indent (span beg end)
  (save-excursion
    (goto-char beg)
    (let ((basic-offset (pm--oref-value (nth 3 span) 'indent-offset)))
      (while (and (< (point) end)
                  (re-search-forward "\\([+-]\\)indent" end t))
        (let ((offset (if (string= (match-string 1) "-")
                          (- basic-offset)
                        basic-offset)))
          (indent-line-to (max 0 (+ (current-indentation) offset)))
          (forward-line))))))

(defun pm--reindent-with-extra-offset (span offset-type &optional offset2)
  (let ((offset (eieio-oref (nth 3 span) offset-type)))
    (unless (and (numberp offset) (= offset 0))
      (let ((pos (nth (if (eq offset-type 'post-indent-offset) 2 1) span)))
        (save-excursion
          (goto-char pos)
          (setq offset (pm--object-value offset)))
        (indent-line-to (max 0 (+ (current-indentation) offset (or offset2 0))))))))


;;; FACES
(cl-defgeneric pm-get-adjust-face (chunkmode type))

(cl-defmethod pm-get-adjust-face ((chunkmode pm-chunkmode) _type)
  (eieio-oref chunkmode 'adjust-face))

(cl-defmethod pm-get-adjust-face ((chunkmode pm-inner-chunkmode) type)
  (cond ((eq type 'head)
         (eieio-oref chunkmode 'head-adjust-face))
        ((eq type 'tail)
         (or (eieio-oref chunkmode 'tail-adjust-face)
             (eieio-oref chunkmode 'head-adjust-face)))
        (t (eieio-oref chunkmode 'adjust-face))))

(provide 'polymode-methods)

;;; polymode-methods.el ends here
