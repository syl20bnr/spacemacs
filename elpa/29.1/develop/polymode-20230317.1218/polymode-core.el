;; polymode-core.el --- Core initialization and utilities for polymode -*- lexical-binding: t -*-
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

(require 'gv)
(require 'font-lock)
(require 'color)
(require 'polymode-classes)
(require 'format-spec)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib)
  (require 'derived))


;;; ESSENTIAL DECLARATIONS
(defvar *span* nil)
(defvar-local pm/polymode nil)
(put 'pm/polymode 'permanent-local t)
(defvar-local pm/chunkmode nil)
(defvar-local pm/current nil) ;; fixme: unused
(defvar-local pm/type nil) ;; fixme: remove this
(defvar-local polymode-mode nil
  "Non-nil if current \"mode\" is a polymode.")
(defvar pm--emacs>26 (version<= "26" emacs-version))

;; overwrites
(defvar-local pm--indent-region-function-original nil)
(defvar-local pm--fill-forward-paragraph-original nil)
(defvar-local pm--indent-line-function-original nil)
(defvar-local pm--syntax-propertize-function-original nil)

;; silence the compiler
(defvar pm--output-file nil)
(defvar pm--input-buffer nil)
(defvar pm--input-file nil)
(defvar pm--export-spec nil)
(defvar pm--input-not-real nil)
(defvar pm--output-not-real nil)

;; methods api from polymode-methods.el
(declare-function pm-initialize "polymode-methods")
(declare-function pm-get-buffer-of-mode "polymode-methods")
(declare-function pm-get-buffer-create "polymode-methods")
(declare-function pm-get-adjust-face "polymode-methods")
(declare-function pm-get-span "polymode-methods")
(declare-function pm-next-chunk "polymode-methods")

;; eieio silence "unknown slot"
;; http://emacs.1067599.n8.nabble.com/Fixing-quot-Unknown-slot-quot-warnings-td419119.html
(eval-when-compile
  (defclass dummy ()
    ((function) (from-to))))

(defun pm-object-name (obj)
  ;; gives warnings on e25,26 but fine in e27
  (with-no-warnings
    (eieio-object-name-string obj)))

;; SHIELDS

(defvar pm-allow-after-change-hook t)
(defvar pm-allow-before-change-hook t)

(defvar pm-allow-pre-command-hook t)
(defvar pm-allow-post-command-hook t)
(defun polymode-disable-post-command ()
  (when polymode-mode
    (setq pm-allow-post-command-hook nil)))
(defun polymode-enable-post-command ()
  (when polymode-mode
    (setq pm-allow-post-command-hook t)))

;; We need this during cascaded call-next-method in pm-initialize. -innermodes
;; are initialized after the hostmode setup has taken place. This means that
;; pm-get-span and all the functionality that relies on it will fail to work
;; correctly during the initialization in the call-next-method. This is
;; particularly relevant to font-lock setup and user hooks.
(defvar pm-initialization-in-progress nil)

(defvar pm-hide-implementation-buffers t)
(defvar-local pm--core-buffer-name nil)

(defun pm--hidden-buffer-name ()
  (generate-new-buffer-name (concat " " pm--core-buffer-name)))

(defun pm--visible-buffer-name ()
  (generate-new-buffer-name
   (replace-regexp-in-string "^ +" "" pm--core-buffer-name)))



;;; CUSTOM

;;;###autoload
(defvar-local polymode-default-inner-mode nil
  "Inner mode for chunks with unspecified modes.
Intended to be used as local variable in polymode buffers. A
special value `host' means use the host mode.")
;;;###autoload
(put 'polymode-default-inner-mode 'safe-local-variable #'symbolp)

(defgroup polymode nil
  "Object oriented framework for multiple modes based on indirect buffers"
  :link '(emacs-commentary-link "polymode")
  :group 'tools)

(defgroup poly-modes nil
  "Polymode Configuration Objects"
  :group 'polymode)

(defgroup poly-hostmodes nil
  "Polymode Host Chunkmode Objects"
  :group 'polymode)

(defgroup poly-innermodes nil
  "Polymode Chunkmode Objects"
  :group 'polymode)

(defcustom polymode-display-output-file t
  "Whether to display woven and exported output buffers.
When non-nil automatically visit and call `display-buffer' on
output files from processor engines (e.g. weavers and exporters).
Can also be a function, in which case it is called with the
output file name as the only argument. If this function returns
non-nil, the file is visited and displayed with `display-buffer'.
See `display-buffer-alist' for how to customize the display."
  :group 'polymode
  :type '(choice (const t) (const nil) function))

(defcustom polymode-display-process-buffers t
  "When non-nil, display weaving and exporting process buffers."
  :group 'polymode
  :type 'boolean)

(defcustom polymode-skip-processing-when-unmodified t
  "If non-nil, consider modification times of input and output files.
Skip weaving or exporting process when output file is more recent
than the input file."
  :group 'polymode
  :type 'boolean)

(define-obsolete-variable-alias 'polymode-mode-name-override-alist 'polymode-mode-name-aliases "2018-08")
(define-obsolete-variable-alias 'polymode-mode-name-alias-alist 'polymode-mode-name-aliases "2019-04")
(defcustom polymode-mode-name-aliases
  '(
    (asymptote  . asy-mode)
    (bash       . sh-mode)
    (cpp        . c++-mode)
    (ditaa      . artist-mode)
    (el         . emacs-lisp)
    (elisp      . emacs-lisp)
    (ocaml      . tuareg)
    (screen     . shell-script-mode)
    (shell      . sh-mode)
    (sqlite     . sql-mode)
    )
  "An alist of inner mode overrides.
When inner mode is automatically detected from the header of the
inner chunk (such as in markdown mode), the detected symbol might
not correspond to the desired mode. This alist maps discovered
symbols into desired modes. For example

  (add-to-list 'polymode-mode-name-aliases '(julia . ess-julia))

will cause installation of `ess-julia-mode' in markdown ```julia chunks."
  :group 'polymode
  :type 'alist)

(defvar polymode-mode-abbrev-aliases nil
  "An alist of abbreviation mappings from mode names to their abbreviations.
Used to compute mode post-fixes in buffer names. Example:

  (add-to-list \\='polymode-mode-abbrevs-aliases \\='(\"ess-r\" . \"R\"))")

(defvar polymode-before-switch-buffer-hook nil
  "Hook run just before switching to a different polymode buffer.
Each function is run with two arguments `old-buffer' and
`new-buffer'. This hook is commonly used to transfer state
between buffers. Hook is run before transfer of variables, modes
and overlays.")

(define-obsolete-variable-alias 'polymode-switch-buffer-hook 'polymode-after-switch-buffer-hook "v0.2")
(defvar polymode-after-switch-buffer-hook nil
  "Hook run after switching to a different polymode buffer.
Each function is run with two arguments `old-buffer' and
`new-buffer'. This hook is commonly used to transfer state
between buffers. Slot :switch-buffer-functions in `pm-polymode'
and `pm-chunkmode' objects provides same functionality for
narrower scope.")

(defvar polymode-init-host-hook nil
  "Hook run on initialization of every hostmode.
Ran in a base buffer from `pm-initialze'
methods. Slot :init-functions in `pm-polymode' objects provides
similar hook for more focused scope. See
`polymode-init-inner-hook' and :init-functions slot in
`pm-chunkmode' objects for similar hooks for inner chunkmodes.")

(defvar polymode-init-inner-hook nil
  "Hook run on initialization of every `pm-chunkmode' object.
The hook is run in chunkmode's body buffer from `pm-initialze'
`pm-chunkmode' methods. Slot :init-functions `pm-chunkmode'
objects provides same functionality for narrower scope. See also
`polymode-init-host-hook'.")


;;; Mode Macros

(defun polymode--define-chunkmode (constructor name parent doc key-args)
  (let* ((type (format "%smode"
                       (replace-regexp-in-string
                        "-.*$" "" (replace-regexp-in-string "^pm-" "" (symbol-name constructor)))))
         (sname (symbol-name name))
         (root-name (replace-regexp-in-string (format "poly-\\|-%s" type) "" sname)))
    (when (keywordp parent)
      (progn
        (push doc key-args)
        (push parent key-args)
        (setq doc nil parent nil)))

    (unless (stringp doc)
      (when (keywordp doc)
        (push doc key-args))
      (setq doc (format "%s for %s chunks." (capitalize type) root-name)))

    (unless (string-match-p (format "-%s$" type) sname)
      (error "%s must end in '-%s'" (capitalize type) type))
    (unless (symbolp parent)
      ;; fixme: check inheritance
      (error "PARENT must be a name of an `%s'" type))

    `(progn
       (makunbound ',name)
       (defvar ,name
         ,(if parent
              `(pm--safe-clone ',constructor ,parent :name ,root-name ,@key-args)
            `(,constructor :name ,root-name ,@key-args))
         ,doc))
    ;; `(progn
    ;;    (defvar ,name)
    ;;    (defcustom ,name nil
    ;;      ,doc
    ;;      :group ',(intern (format "poly-%ss" type))
    ;;      :type 'object)
    ;;    (setq ,name
    ;;          ,(if parent
    ;;               `(clone ,parent :name ,root-name ,@key-args)
    ;;             `(,constructor :name ,root-name ,@key-args))))
    ))

;;;###autoload
(defmacro define-hostmode (name &optional parent doc &rest key-args)
  "Define a hostmode with name NAME.
Optional PARENT is a name of a hostmode to be derived (cloned)
from. If missing, the optional documentation string DOC is
generated automatically. KEY-ARGS is a list of key-value pairs.
See the documentation of the class `pm-host-chunkmode' for
possible values."
  (declare (doc-string 3) (indent defun))
  (polymode--define-chunkmode 'pm-host-chunkmode name parent doc key-args))

;;;###autoload
(defmacro define-innermode (name &optional parent doc &rest key-args)
  "Ddefine an innermode with name NAME.
Optional PARENT is a name of a innermode to be derived (cloned)
from. If missing the optional documentation string DOC is
generated automatically. KEY-ARGS is a list of key-value pairs.
See the documentation of the class `pm-inner-chunkmode' for
possible values."
  (declare (doc-string 3) (indent defun))
  (polymode--define-chunkmode 'pm-inner-chunkmode name parent doc key-args))

;;;###autoload
(defmacro define-auto-innermode (name &optional parent doc &rest key-args)
  "Ddefine an auto innermode with name NAME.
Optional PARENT is a name of an auto innermode to be
derived (cloned) from. If missing the optional documentation
string DOC is generated automatically. KEY-ARGS is a list of
key-value pairs. See the documentation of the class
`pm-inner-auto-chunkmode' for possible values."
  (declare (doc-string 3) (indent defun))
  (polymode--define-chunkmode 'pm-inner-auto-chunkmode name parent doc key-args))



;;; MESSAGES

(defvar pm-extra-span-info nil)

(defun pm-format-span (&optional span prefixp)
  (let* ((span (cond
                ((number-or-marker-p span) (pm-innermost-span span))
                ((null span) (pm-innermost-span))
                (span)))
         (message-log-max nil)
         (beg (nth 1 span))
         (end (nth 2 span))
         (type (and span (or (car span) 'host)))
         (oname (if span
                    (eieio-object-name (nth 3 span))
                  (current-buffer)))
         (extra (if pm-extra-span-info
                    (format (if prefixp "%s " " (%s)") pm-extra-span-info)
                  "")))
    (if prefixp
        (format "%s[%s %s-%s %s]" extra type beg end oname)
      (format "[%s %s-%s %s]%s" type beg end oname extra))))


;;; SPANS

(defsubst pm-base-buffer ()
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

(defun pm-span-mode (&optional span)
  "Retrieve the major mode associated with SPAN."
  (pm--true-mode-symbol
   (buffer-local-value 'major-mode (pm-span-buffer span))))

(defun pm-span-buffer (&optional span)
  "Retrieve the buffer associated with SPAN."
  (setq span (or span (pm-innermost-span)))
  (let* ((chunkmode (nth 3 span))
         (type (pm-true-span-type span)))
    (if type
        (pm-get-buffer-create chunkmode type)
      ;; ignore span's chunkmode as inner spans can request host span
      (pm-get-buffer-create (oref pm/polymode -hostmode)))))

(defun pm-true-span-type (chunkmode &optional type)
  "Retrieve the TYPE of buffer to be installed for CHUNKMODE.
`pm-innermost-span' returns a raw type (head, body or tail) but
the actual type installed depends on the values of :host-mode and
:tail-mode of the CHUNKMODE object. Always return nil if TYPE is
nil (aka a host span). CHUNKMODE could also be a span, in which
case TYPE is ignored."
  ;; fixme: this works on inner modes only. Fix naming.
  (when (listp chunkmode)
    ;; a span
    (setq type (car chunkmode)
          chunkmode (nth 3 chunkmode)))
  (when (object-of-class-p chunkmode 'pm-inner-chunkmode)
    (unless (or (null type) (eq type 'host))
      (with-slots (mode head-mode tail-mode fallback-mode) chunkmode
        (cond ((eq type 'body)
               (unless (or (eq mode 'host)
                           ;; for efficiency don't check if modes are valid
                           (and (null mode)
                                (if polymode-default-inner-mode
                                    (eq polymode-default-inner-mode 'host)
                                  (eq fallback-mode 'host))))
                 'body))
              ((eq type 'head)
               (cond ((eq head-mode 'host) nil)
                     ((eq head-mode 'body) 'body)
                     (t 'head)))
              ((eq type 'tail)
               (cond ((eq tail-mode 'host) nil)
                     ((eq tail-mode 'body) 'body)
                     (t 'tail)))
              (t (error "Type must be one of nil, 'host, 'head, 'tail or 'body")))))))

(defvar pm-use-cache t)

(defun pm-cache-span (span)
  ;; cache span
  (when pm-use-cache
    (unless pm-initialization-in-progress
      (with-silent-modifications
        ;; (message "caching: %s %s" (car span) (pm-span-to-range span))
        (let ((sbeg (nth 1 span))
              (send (nth 2 span)))
          (put-text-property sbeg send :pm-span span)
          (put-text-property sbeg send :pm-mode (pm-span-mode span)))))))

(defun pm-flush-span-cache (beg end &optional buffer)
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(:pm-span) buffer)))

(defun pm--outspan-p (span thespan)
  "Non-nil if SPAN outspans THESPAN.
Return non-nil if SPAN contains THESPAN's chunk (strictly from
the front)."
  (let ((type (car thespan))
        (beg (nth 1 thespan))
        (end (nth 2 thespan))
        (sbeg (nth 1 span))
        (send (nth 2 span)))
    ;; The following check is to ensure that the outer span really
    ;; spans outside of the entire thespan's chunk (poly-markdown#6)
    (and
     (< sbeg beg)
     (cond
      ((eq type 'body)
       (and (let ((hspan (pm-get-span (nth 3 thespan) (1- beg))))
              (< sbeg (nth 1 hspan)))
            ;; Ends might coincide due to eob
            (if (< end send)
                (let ((tspan (pm-get-span (nth 3 thespan) (1+ end))))
                  (<= (nth 2 tspan) send))
              (= end send))))
      ((eq type 'tail)
       (let ((bspan (pm-get-span (nth 3 thespan) (1- beg))))
         (when (< sbeg (nth 1 bspan))
           (let ((hspan (pm-get-span (nth 3 thespan) (1- (nth 1 bspan)))))
             (< sbeg (nth 1 hspan))))))
      ;; Ends might coincide due to eob
      ((eq type 'head)
       (if (< end send)
           (let ((bspan (pm-get-span (nth 3 thespan) (1+ end))))
             (if (< (nth 2 bspan) send)
                 (let ((tspan (pm-get-span (nth 3 thespan) (1+ (nth 2 bspan)))))
                   (<= (nth 2 tspan) send))
               (= (nth 2 bspan) send)))
         (= end send)))))))

(defun pm--intersect-spans (thespan span)
  ;; ASSUMPTION: first thespan should be of the form (nil MIN MAX HOSTMODE)
  (when span
    (let ((allow-nested (eieio-oref (nth 3 span) 'allow-nested))
          (is-host (null (car span))))
      (cond
       ;; 1. nil means host and it can be an intersection of spans returned by
       ;; two neighboring inner chunkmodes. When `allow-nested` is 'always the
       ;; innermode behaves like the host-mode (i.e. nest other spans regardless
       ;; of :can-nest slot)
       ((or is-host (eq allow-nested 'always))
        (if (car thespan)
            ;; 1) inner thespan:
            ;;   a) inner span [thespan ..|.. [span ...] ...]
            ;;   b) outer span [thespan ..|..] ... [span ...]
            ;;   c) host-like span [span ... [thespan ..|..] ]
            (setq thespan
                  (list (car thespan)
                        (max (nth 1 span) (nth 1 thespan))
                        (min (nth 2 span) (nth 2 thespan))
                        (nth 3 thespan)))
          ;; 2) host thespan
          ;;    a) hosts span [thespan ...] ..|.. [span ..]
          ;;    b) host-like span [span ..|.. [thespan ...] ..]
          (setq thespan
                (list (car span)
                      (max (nth 1 span) (nth 1 thespan))
                      (min (nth 2 span) (nth 2 thespan))
                      ;; first host span has precedence for clarity
                      (nth 3 (if is-host thespan span))))))
       ;; 2. Inner span
       ((and (>= (nth 1 span) (nth 1 thespan))
             (<= (nth 2 span) (nth 2 thespan)))
        ;; Accepted only nested spans. In case of crossing (incorrect spans),
        ;; first span wins.
        (when (or (null (car thespan))
                  (eieio-oref (nth 3 span) 'can-nest))
          (setq thespan span)))
       ;; 3. Outer span; overwrite previous span if nesting is not allowed.
       ;; This case is very hard because it can result in big invalid span
       ;; when a head occurs within a inner-chunk. For example $ for inline
       ;; latex can occur within R or python. The hard way to fix this would
       ;; require non-local information (e.g. checking if outer span's
       ;; extremities are within a host span) and still might not be the full
       ;; proof solution. Instead, make use of 'allow-nested property.
       ((and (eq allow-nested t)
             (car thespan)              ; span is an inner span
             (not (eieio-oref (nth 3 thespan) 'can-nest))
             (pm--outspan-p span thespan))
        (setq thespan span)))))
  thespan)

(defun pm--get-intersected-span (config &optional pos)
  ;; fixme: host should be last, to take advantage of the chunkmodes computation?
  (let* ((start (point-min))
         (end (point-max))
         (pos (or pos (point)))
         (hostmode (oref config -hostmode))
         (chunkmodes (cons hostmode (oref config -innermodes)))
         (thespan (list nil start end hostmode)))
    (dolist (cm chunkmodes)
      ;; Optimization opportunity: this searches till the end of buffer but the
      ;; outermost pm-get-span caller has computed a few spans already so we can
      ;; pass limits or narrow to pre-computed span.
      (setq thespan (pm--intersect-spans thespan (pm-get-span cm pos))))

    (unless (and (<= start end) (<= pos end) (>= pos start))
      (error "Bad polymode selection: span:%s pos:%s"
             (list start end) pos))
    (pm-cache-span thespan)
    thespan))

(defun pm--chop-span (span beg end)
  ;; destructive!
  (when (> beg (nth 1 span))
    (setcar (cdr span) beg))
  (when (< end (nth 2 span))
    (setcar (cddr span) end))
  span)

(defun pm--innermost-span (config &optional pos)
  (let ((pos (or pos (point)))
        (omin (point-min))
        (omax (point-max))
        ;; `re-search-forward' and other search functions trigger full
        ;; `internal--syntax-propertize' on the whole buffer on every
        ;; single buffer modification. This is a small price to pay for a
        ;; much improved efficiency in modes which heavily rely on
        ;; `syntax-propertize' like `markdown-mode'.
        (parse-sexp-lookup-properties nil)
        (case-fold-search t))
    (save-excursion
      (save-restriction
        (widen)
        (let ((span (pm--get-intersected-span config pos)))
          (if (= omax pos)
              (when (and (= omax (nth 1 span))
                         (> omax omin))
                ;; When pos == point-max and it's beg of span, return the
                ;; previous span. This occurs because the computation of
                ;; pm--get-intersected-span is done on a widened buffer.
                (setq span (pm--get-intersected-span config (1- pos))))
            (when (= pos (nth 2 span))
              (error "Span ends at %d in (pm--inermost-span %d) %s"
                     pos pos (pm-format-span span))))
          (pm--chop-span span omin omax))))))

(defun pm--cached-span (&optional pos)
  ;; fixme: add basic miss statistics
  (unless pm-initialization-in-progress
    (let* ((omin (point-min))
           (omax (point-max))
           (pos (or pos (point)))
           (pos (if (= pos omax)
                    (max (point-min) (1- pos))
                  pos))
           (span (get-text-property pos :pm-span)))
      (when span
        (save-restriction
          (widen)
          (let* ((beg (nth 1 span))
                 (end (1- (nth 2 span))))
            (when (and (< end (point-max)) ; buffer size might have changed
                       (<= pos end)
                       (<= beg pos)
                       (eq span (get-text-property beg :pm-span))
                       (eq span (get-text-property end :pm-span))
                       (not (eq span (get-text-property (1+ end) :pm-span)))
                       (or (= beg (point-min))
                           (not (eq span (get-text-property (1- beg) :pm-span)))))
              (pm--chop-span (copy-sequence span) omin omax))))))))

(define-obsolete-function-alias 'pm-get-innermost-span #'pm-innermost-span "2018-08")
(defun pm-innermost-span (&optional pos no-cache)
  "Get span object at POS.
If NO-CACHE is non-nil, don't use cache and force re-computation
of the span. Return a cons (type start end chunkmode). POS
defaults to point. Guarantied to return a non-empty span."
  (when (and pos (or (< pos (point-min)) (> pos (point-max))))
    (signal 'args-out-of-range
            (list :pos pos
                  :point-min (point-min)
                  :point-max (point-max))))
  (save-match-data
    (or (when (and pm-use-cache (not no-cache))
          (pm--cached-span pos))
        (pm--innermost-span pm/polymode pos))))

(defun pm-span-to-range (span)
  (and span (cons (nth 1 span) (nth 2 span))))

(define-obsolete-function-alias 'pm-get-innermost-range #'pm-innermost-range "2018-08")
(defun pm-innermost-range (&optional pos no-cache)
  (pm-span-to-range (pm-innermost-span pos no-cache)))

(defun pm-fun-matcher (matcher)
  "Make a function matcher given a MATCHER.
MATCHER is one of the forms accepted by \=`pm-inner-chunkmode''s
:head-matcher slot."
  (cond
   ((stringp matcher)
    (lambda (ahead)
      (if (< ahead 0)
          (if (re-search-backward matcher nil t)
              (cons (match-beginning 0) (match-end 0)))
        (if (re-search-forward matcher nil t)
            (cons (match-beginning 0) (match-end 0))))))
   ((functionp matcher)
    matcher)
   ((consp matcher)
    (lambda (ahead)
      (when (re-search-forward (car matcher) nil t ahead)
        (cons (match-beginning (cdr matcher))
              (match-end (cdr matcher))))))
   (t (error "Head and tail matchers must be either regexp strings, cons cells or functions"))))

(defun pm-forward-sexp-tail-matcher (_arg)
  "A simple tail matcher for a common closing-sexp character.
Use this matcher if an inner mode is delimited by a closing
construct like ${...}, xyz[...], html! {...} etc. In order to
match the tail `forward-sexp' is matched from HEAD-END - 1
position. ARG is ignored - always match forward."
  (when (> (point) 0)
    (backward-char 1)
    (ignore-errors
      (forward-sexp 1)
      (cons (1- (point)) (point)))))

(defun pm-same-indent-tail-matcher (_arg)
  "Get the end position of block with the higher indent than the current column.
Used as tail matcher for blocks identified by same indent. See
function `poly-slim-mode' for examples. ARG is ignored; always search
forward."
  ;; we are at the head end; so either use head indent or this code indent
  (let* ((cur-indent (current-indentation))
         (cur-col (current-column))
         (block-col (if (< cur-indent cur-col)
                        cur-indent
                      (1- cur-indent)))
         (end (point-at-eol)))
    (forward-line 1)
    (while (and (not (eobp))
                (or (looking-at-p "[ \t]*$")
                    (and (> (current-indentation) block-col)
                         (setq end (point-at-eol)))))
      (forward-line 1))
    ;; end at bol for the sake of indentation
    (setq end (min (point-max) (1+ end)))
    (cons end end)))

(defun pm--get-property-nearby (property accessor ahead)
  (let ((ahead (> ahead 0)))
    (let* ((pos (if ahead
                    (if (get-text-property (point) property)
                        (point)
                      (next-single-property-change (point) property))
                  (previous-single-property-change (point) property)))
           (val (when pos
                  (or (get-text-property pos property)
                      (and (setq pos (previous-single-property-change pos property nil (point-min)))
                           (get-text-property pos property))))))
      (when val
        (if accessor
            (let ((val (save-excursion
                         (goto-char pos)
                         (funcall accessor val))))
              (cond
               ((numberp val) (cons val val))
               ((consp val) (cons (car val) (if (listp (cdr val))
                                                (cadr val)
                                              (cdr val))))
               (t (cons pos (next-single-property-change pos property nil (point-max))))))
          (cons pos (next-single-property-change pos property nil (point-max))))))))

(defun pm-make-text-property-matcher (property &optional accessor)
  "Return a head or tail matcher for PROPERTY with ACCESSOR.
ACCESSOR is either a function or a keyword. When a function it is
applied to the PROPERTY's value to retrieve the position of the
head in the buffer. It should return either a number in which
case head has 0 length, a cons of the form (BEG . END), or a
list (BEG END). ACCESSOR is called at the beginning of the
PROPERTY region. When ACCESSOR is nil the head span is the region
covered by the same value of PROPERTY. When ACCESSOR is a keyword
the property is searched as when ACCESSOR is nil but is adapted
according to the keyword. Currently :inc-end means increment the
END of the span, when :dec-beg, decrement the beginning of the
span."
  (lambda (ahead)
    (if (keywordp accessor)
        (let ((loc (pm--get-property-nearby property nil ahead)))
          (when loc
            (cond
             ((eq accessor :inc-end) (setcdr loc (1+ (cdr loc))))
             ((eq accessor :dec-beg) (setcar loc (1- (cdr loc))))
             (t (error "Invalid ACCESSOR keyword")))
            loc))
      (pm--get-property-nearby property accessor ahead))))

(defun pm--span-at-point (head-matcher tail-matcher &optional pos can-overlap do-chunk)
  "Span detector with head and tail matchers.
HEAD-MATCHER and TAIL-MATCHER is as in :head-matcher slot of
`pm-inner-chunkmode' object. POS defaults to (point). When
CAN-OVERLAP is non-nil nested chunks of this type are allowed.

Return a list of the form (TYPE SPAN-START SPAN-END) where TYPE
is one of the following symbols:
  nil   - pos is between ‘point-min’ and head-matcher, or between
          tail-matcher and ‘point-max’
  body  - pos is between head-matcher and tail-matcher (exclusively)
  head  - head span
  tail  - tail span

Non-nil DO-CHUNK makes this function return a list of the
form (TYPE HEAD-START HEAD-END TAIL-START TAIL-END)."
  (setq pos (or pos (point)))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (let* ((at-max (= pos (point-max)))
             (head-matcher (pm-fun-matcher head-matcher))
             (tail-matcher (pm-fun-matcher tail-matcher))
             (head1 (funcall head-matcher -1)))
        (if head1
            (if (or (< pos (cdr head1))
                    (and at-max (= (cdr head1) pos)))
                ;;      -----|
                ;; host)[head)           ; can occur with sub-head == 0 only
                (if do-chunk
                    (pm--find-tail-from-head pos head1 head-matcher tail-matcher can-overlap 'head)
                  (list 'head (car head1) (cdr head1)))
              ;;            ------------------------
              ;; host)[head)[body)[tail)[host)[head)[body)
              (pm--find-tail-from-head pos head1 head-matcher tail-matcher can-overlap do-chunk))
          ;; ----------
          ;; host)[head)[body)[tail)[host
          (goto-char (point-min))
          (let ((head2 (funcall head-matcher 1)))
            (if head2
                (if (< pos (car head2))
                    ;; ----
                    ;; host)[head)[body)[tail)[host
                    (if do-chunk
                        (list nil (point-min) (point-min) (car head2) (car head2))
                      (list nil (point-min) (car head2)))
                  (if (< pos (cdr head2))
                      ;;      -----
                      ;; host)[head)[body)[tail)[host
                      (if do-chunk
                          (pm--find-tail-from-head pos head2 head-matcher tail-matcher can-overlap 'head)
                        (list 'head (car head2) (cdr head2)))
                    ;;            -----------------
                    ;; host)[head)[body)[tail)[host
                    (pm--find-tail-from-head pos head2 head-matcher tail-matcher can-overlap do-chunk)))
              ;; no span found
              nil)))))))

;; fixme: find a simpler way with recursion where head-matcher and tail-matcher could be reversed
(defun pm--find-tail-from-head (pos head head-matcher tail-matcher can-overlap do-chunk)
  (goto-char (cdr head))
  (let ((tail (funcall tail-matcher 1))
        (at-max (= pos (point-max)))
        (type 'tail))
    (when can-overlap
      (save-excursion
        ;; search for next head and pick the earliest
        (goto-char (cdr head))
        (let ((match (funcall head-matcher 1)))
          (when (or (null tail)
                    (and match (< (car match) (car tail))))
            (setq tail match
                  type 'head)))))
    (if tail
        (if (< pos (car tail))
            ;;            -----
            ;; host)[head)[body)[tail)[host)[head)
            (if do-chunk
                (list (if (eq do-chunk t) 'body do-chunk)
                      (car head) (cdr head) (car tail) (cdr tail))
              (list 'body (cdr head) (car tail)))
          (if (or (< pos (cdr tail))
                  (and at-max (= pos (cdr tail))))
              ;;                  -----
              ;; host)[head)[body)[tail)[host)[head)
              (if do-chunk
                  (if (eq type 'tail)
                      (list (if (eq do-chunk t) 'tail do-chunk)
                            (car head) (cdr head) (car tail) (cdr tail))
                    ;; can-overlap case
                    (pm--find-tail-from-head pos tail head-matcher tail-matcher can-overlap do-chunk))
                (list type (car tail) (cdr tail)))
            (goto-char (cdr tail))
            ;;                        -----------
            ;; host)[head)[body)[tail)[host)[head)
            (let ((match (funcall head-matcher 1))
                  (type 'head))
              (when can-overlap
                (save-excursion
                  ;; search for next head and pick the earliest
                  (goto-char (cdr tail))
                  (let ((match2 (funcall tail-matcher 1)))
                    (when (or (null match)
                              (and match2 (< (car match2) (car match))))
                      (setq match match2
                            type 'tail)))))
              (if match
                  (if (< pos (car match))
                      ;;                        -----
                      ;; host)[head)[body)[tail)[host)[head)
                      (if do-chunk
                          (list nil (cdr tail) (cdr tail) (car match) (car match))
                        (list nil (cdr tail) (car match)))
                    (if (or (< pos (cdr match))
                            (and at-max (= pos (cdr match))))
                        ;;                              -----
                        ;; host)[head)[body)[tail)[host)[head)[body
                        (if do-chunk
                            (if (eq type 'tail)
                                ;; can-overlap case
                                (list (if (eq do-chunk t) 'tail do-chunk)
                                      (car head) (cdr head) (car match) (cdr match))
                              (pm--find-tail-from-head pos match head-matcher tail-matcher can-overlap 'head))
                          (list type (car match) (cdr match)))
                      ;;                                    ----
                      ;; host)[head)[body)[tail)[host)[head)[body
                      (pm--find-tail-from-head pos match head-matcher tail-matcher can-overlap do-chunk)))
                ;;                        -----
                ;; host)[head)[body)[tail)[host)
                (if do-chunk
                    (list nil (cdr tail) (cdr tail) (point-max) (point-max))
                  (list nil (cdr tail) (point-max)))))))
      ;;            -----
      ;; host)[head)[body)
      (if do-chunk
          (list (if (eq do-chunk t) 'body do-chunk) (cdr head) (cdr head) (point-max) (point-max))
        (list 'body (cdr head) (point-max))))))

(defun pm--next-chunk (head-matcher tail-matcher &optional pos can-overlap)
  "Forward only span detector.
For HEAD-MATCHER, TAIL-MATCHER, POS and CAN-OVERLAP see
`pm--span-at-point'. Return a list of the form (HEAD-START
HEAD-END TAIL-START TAIL-END). Can return nil if there are no
forward spans from pos."
  (setq pos (or pos (point)))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (let ((parse-sexp-lookup-properties nil)
            (case-fold-search t)
            (head-matcher (pm-fun-matcher head-matcher))
            (tail-matcher (pm-fun-matcher tail-matcher))
            (head nil))
        ;; start from bol !! ASSUMPTION !!
        (forward-line 0)
        (setq head (funcall head-matcher 1))
        (while (and head (< (car head) pos))
          (setq head (funcall head-matcher 1)))
        (when head
          (goto-char (cdr head))
          (let ((tail (or (funcall tail-matcher 1)
                          (cons (point-max) (point-max)))))
            (when can-overlap
              (goto-char (cdr head))
              (when-let ((hbeg (car (funcall head-matcher 1))))
                (when (< hbeg (car tail))
                  (setq tail (cons hbeg hbeg)))))
            (list (car head) (cdr head) (car tail) (cdr tail))))))))

(defun pm-goto-span-of-type (type N)
  "Skip to N - 1 spans of TYPE and stop at the start of a span of TYPE.
TYPE is either a symbol or a list of symbols of span types."
  (let* ((sofar 0)
         (types (if (symbolp type)
                    (list type)
                  type))
         (back (< N 0))
         (N (if back (- N) N))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max))))
    (unless (memq (car (pm-innermost-span)) types)
      (setq sofar 1))
    (condition-case nil
        (pm-map-over-spans
         (lambda (span)
           (when (memq (car span) types)
             (goto-char (nth 1 span))
             (when (>= sofar N)
               (signal 'quit nil))
             (setq sofar (1+ sofar))))
         beg end nil back)
      (quit nil))
    sofar))


;;; OBJECT HOOKS

(defun pm--run-derived-mode-hooks ()
  ;; Minor modes run-hooks, major-modes run-mode-hooks. Polymodes is a minor
  ;; mode but with major-mode flavor. We run hooks of all modes stored in
  ;; '-minor-mode slot of all parent objects in parent-first order.
  (let* ((this-mode (eieio-oref pm/polymode '-minor-mode))
         (this-state (symbol-value this-mode)))
    (mapc (lambda (mm)
            (let ((old-state (symbol-value mm)))
              (unwind-protect
                  (progn
                    (set mm this-state)
                    (run-hooks (derived-mode-hook-name mm)))
                (set mm old-state))))
          (pm--collect-parent-slots pm/polymode '-minor-mode))))

(defun pm--run-init-hooks (object type &optional global-hook)
  (unless pm-initialization-in-progress
    (when global-hook
      (run-hooks global-hook))
    (pm--run-hooks object :init-functions (or type 'host))))

(defun pm--collect-parent-slots (object slot &optional do-when inclusive)
  "Descend into parents of OBJECT and return a list of SLOT values.
Returned list is in parent first order. If non-nil DO-WHEN must
be a function which would take an object and return non-nil if
the recursion should descend into the parent. When nil, all
parents are descended. If INCLUSIVE is non-nil, include the slot
of the first object for which DO-WHEN failed."
  (let ((inst object)
        (vals nil)
        (failed nil))
    (while inst
      (if (not (slot-boundp inst slot))
          (setq inst (and (slot-boundp inst :parent-instance)
                          (eieio-oref inst 'parent-instance)))
        (push (eieio-oref inst slot) vals)
        (setq inst (and
                    (or (null do-when)
                        (if failed
                            (progn (setq failed nil) t)
                          (or (funcall do-when inst)
                              (and inclusive
                                   (setq failed t)))))
                    (slot-boundp inst :parent-instance)
                    (eieio-oref inst 'parent-instance)))))
    vals))

(defun pm--run-hooks (object slot &rest args)
  "Run hooks from SLOT of OBJECT and its parent instances.
Parents' hooks are run first."
  (let ((funs (delete-dups
               (copy-sequence
                (apply #'append
                       (pm--collect-parent-slots object slot))))))
    (if args
        (mapc (lambda (fn)
                (apply fn args))
              funs)
      (mapc #'funcall funs))))


;;; BUFFER SELECTION

;; Transfer of the buffer-undo-list is managed internally by emacs
(define-obsolete-variable-alias 'pm-move-vars-from-base 'polymode-move-these-vars-from-base-buffer "v0.1.6")
(defvar polymode-move-these-vars-from-base-buffer
  '(buffer-file-name
    ;; ideally this one should be merged across all buffers
    buffer-display-table
    outline-regexp
    outline-level
    polymode-default-inner-mode
    tab-width)
  "Variables transferred from base buffer on switch to inner mode buffer.")

(define-obsolete-variable-alias 'pm-move-vars-from-old-buffer 'polymode-move-these-vars-from-old-buffer "v0.1.6")
(defvar polymode-move-these-vars-from-old-buffer
  '(buffer-face-mode
    buffer-face-mode-face
    buffer-face-mode-remapping
    buffer-invisibility-spec
    buffer-read-only
    buffer-undo-list
    buffer-undo-tree
    display-line-numbers
    face-remapping-alist
    isearch-mode ; this seems to be enough to avoid isearch glitching
    line-move-visual
    left-margin-width
    right-margin-width
    overwrite-mode
    selective-display
    text-scale-mode
    text-scale-mode-amount
    ;; transient-mark-mode stores here the state of selection
    ;; when the shift-select-mode is enabled
    transient-mark-mode
    truncate-lines
    truncate-partial-width-windows
    word-wrap
    ;; multiple-cursors stores here a command in a pre-command-hook
    ;; and executes it for all cursors in a post-command-hook so we
    ;; need to transfer in case the buffer was switched.
    mc--this-command
    char-property-alias-alist)
  "Variables transferred from old buffer to new buffer on buffer switch.")

(defvar polymode-move-these-minor-modes-from-base-buffer nil
  "Minor modes to move from base buffer on buffer switch.")
(defvar polymode-move-these-minor-modes-from-old-buffer
  '(linum-mode
    visual-line-mode
    visual-fill-column-mode
    writeroom-mode
    multiple-cursors-mode)
  "Minor modes to move from the old buffer during buffer switch.")

(defun pm-own-buffer-p (&optional buffer)
  "Return t if BUFFER is owned by polymode.
Owning a buffer means that the BUFFER is either the base buffer
or an indirect implementation buffer. If nil, the buffer was
created outside of polymode with `clone-indirect-buffer'."
  (when pm/polymode
    (memq (or buffer (current-buffer))
          (eieio-oref pm/polymode '-buffers))))

(defun pm-select-buffer (span &optional visibly)
  "Select the buffer associated with SPAN.
Install a new indirect buffer if it is not already installed.
Chunkmode's class should define `pm-get-buffer-create' method. If
VISIBLY is non-nil perform extra adjustment for \"visual\" buffer
switch."
  (let ((buffer (pm-span-buffer span))
        (own (pm-own-buffer-p))
        (cbuf (current-buffer)))
    ;; FIXME: investigate why this one is still needed.
    ;; polymode-syntax-propertize should have taken care of it.
    (with-current-buffer buffer
      (pm--reset-ppss-cache span))
    (when (and own visibly)
      ;; always sync to avoid issues with tooling working in different buffers
      (pm--synchronize-points cbuf)
      (let ((mode (or (eieio-oref (nth 3 span) 'keep-in-mode)
                      (eieio-oref pm/polymode 'keep-in-mode))))
        (setq buffer (cond
                      ((null mode) buffer)
                      ((eq mode 'host) (pm-base-buffer))
                      (mode (or (pm-get-buffer-of-mode mode)
                                ;; not throwing because in auto-modes mode might not
                                ;; be installed yet and there is no way install it
                                ;; from here
                                buffer))))))
    ;; no further action if BUFFER is already the current buffer
    (unless (eq buffer cbuf)
      (when (and own visibly)
        (run-hook-with-args 'polymode-before-switch-buffer-hook
                            cbuf buffer))
      (pm--move-vars polymode-move-these-vars-from-base-buffer
                     (pm-base-buffer) buffer)
      (pm--move-vars polymode-move-these-vars-from-old-buffer
                     cbuf buffer)
      (if visibly
          ;; Slow, visual selection. Don't perform in foreign indirect buffers.
          (when own
            (pm--select-existing-buffer-visibly buffer))
        (set-buffer buffer)))))

(defvar text-scale-mode)
(defvar text-scale-mode-amount)
(defun pm--select-existing-buffer-visibly (new-buffer)
  (let ((old-buffer (current-buffer))
        (point (point))
        (window-start (window-start))
        (visible (pos-visible-in-window-p))
        (ractive (region-active-p))
        (mkt (mark t))
        (hlf header-line-format))

    (when pm-hide-implementation-buffers
      (rename-buffer (pm--hidden-buffer-name)))

    (setq pm/current nil)

    (pm--move-minor-modes polymode-move-these-minor-modes-from-base-buffer
                          (pm-base-buffer) new-buffer)
    (pm--move-minor-modes polymode-move-these-minor-modes-from-old-buffer
                          old-buffer new-buffer)

    (pm--move-overlays old-buffer new-buffer)

    (switch-to-buffer new-buffer)
    (bury-buffer-internal old-buffer)
    (set-window-prev-buffers nil (assq-delete-all old-buffer (window-prev-buffers nil)))

    ;; if header line is active in some modes, make it active everywhere
    (unless header-line-format
      (when hlf
        (setq header-line-format '(""))))

    (setq pm/current t)

    ;; fixme: what is the right way to do this ... activate-mark-hook?
    (if (not ractive)
        (deactivate-mark)
      (set-mark mkt)
      (activate-mark))

    (when pm-hide-implementation-buffers
      (rename-buffer (pm--visible-buffer-name)))

    ;; avoid display jumps
    (goto-char point)
    (when visible
      (set-window-start (get-buffer-window new-buffer t) window-start))

    (run-hook-with-args 'polymode-after-switch-buffer-hook old-buffer new-buffer)
    (pm--run-hooks pm/polymode :switch-buffer-functions old-buffer new-buffer)
    (pm--run-hooks pm/chunkmode :switch-buffer-functions old-buffer new-buffer)))

(defun pm--move-overlays (from-buffer to-buffer)
  (with-current-buffer from-buffer
    (mapc (lambda (o)
            (unless (or (overlay-get o 'linum-str)
                        (overlay-get o 'yas--snippet))
              (move-overlay o (overlay-start o) (overlay-end o) to-buffer)))
          (overlays-in 1 (1+ (buffer-size))))))

(defun pm--move-vars (vars from-buffer &optional to-buffer)
  (let ((to-buffer (or to-buffer (current-buffer))))
    (unless (eq to-buffer from-buffer)
      (with-current-buffer to-buffer
        (dolist (var vars)
          (when (default-boundp var)
            (make-local-variable var)
            (set var (buffer-local-value var from-buffer))))))))

(defun pm--move-minor-modes (modes from-buffer &optional to-buffer)
  (let ((to-buffer (or to-buffer (current-buffer))))
    (unless (eq to-buffer from-buffer)
      (with-current-buffer to-buffer
        (dolist (m modes)
          (when (default-boundp m)
            (let ((from-state (buffer-local-value m from-buffer)))
              (unless (equal from-state (symbol-value m))
                (funcall (symbol-function m) (if from-state 1 -1))))))))))

(defun pm-set-buffer (&optional pos-or-span)
  "Set buffer to polymode buffer appropriate for POS-OR-SPAN.
This is done with `set-buffer' and no visual adjustments (like
overlay transport) are done. See `pm-switch-to-buffer' for a more
comprehensive alternative."
  (let ((span (if (or (null pos-or-span)
                      (number-or-marker-p pos-or-span))
                  (pm-innermost-span pos-or-span)
                pos-or-span)))
    (pm-select-buffer span)))

;; NB: Polymode functions used in emacs utilities should not switch buffers
;; under any circumstances. Switching should happen only in post-command. For
;; example `pm-indent-line-dispatcher' used to switch buffers, but it was called
;; from electric-indent-post-self-insert-function in post-self-insert-hook which
;; was triggered by self-insert-command called from `newline'. `newline' sets a
;; temporary local post-self-insert-hook and makes the assumption that buffer
;; stays the same. It was moved away from original buffer by polymode's
;; indentation dispatcher its post-self-insert-hook hanged permanently in the
;; old buffer (#226).
(defun pm-switch-to-buffer (&optional pos-or-span)
  "Bring the appropriate polymode buffer to front.
POS-OR-SPAN can be either a position in a buffer or a span. All
expensive adjustment for a visible switch (like overlay
transport) are performed."
  (let ((span (if (or (null pos-or-span)
                      (number-or-marker-p pos-or-span))
                  (pm-innermost-span pos-or-span)
                pos-or-span)))
    (pm-select-buffer span 'visibly)))

;; NB: save-excursion saves window-point only when current buffer is the
;; selected buffer. Thus when we iterate from a non-window buffer, and within
;; some of the iterations are performed in selected-buffer the point is moved
;; which might results in undesirable consequences (#295). Thus `save-excursion`
;; must be applied on each iteration.

;; TOTHINK: This function is used for font-lock, and thus we cannot rely on
;; cached spans. For other use-cases relying on cached spans would be faster.
;; Without cache `pm-get-span' is less efficient than this function which is
;; essentially a forward search of spans.
(defun pm-map-over-modes (fn beg end)
  "Apply function FN for each major mode between BEG and END.
FN is a function of two arguments mode-beg and mode-end. This is
different from `pm-map-over-spans' which maps over polymode
spans. Two adjacent spans might have same major mode, thus
`pm-map-over-modes' will iterate over same or bigger regions than
`pm-map-over-spans'."
  (when (< beg end)
    (save-restriction
      (widen)
      (let* ((hostmode (eieio-oref pm/polymode '-hostmode))
             (pos beg)
             (ttype 'dummy)
             (span (pm-innermost-span beg))
             (nspan span)
             (ttype (pm-true-span-type span))
             (nttype ttype))
        ;; 1. Use pm-innermost-span to get to the first tail. From there on rely
        ;; on `pm-next-chunk' for efficiency.
        (setq beg (nth 1 span)
              pos (nth 2 span))
        (while (and (< pos end)
                    (memq (car span) '(head body)))
          (while (and (< pos end)
                      (eq ttype nttype))
            (setq pos (nth 2 nspan)
                  nspan (pm-innermost-span pos)
                  nttype (pm-true-span-type nspan)))
          (with-current-buffer (pm-span-buffer span)
            (funcall fn beg pos))
          (setq span nspan
                ttype nttype
                beg (nth 1 nspan)
                pos (nth 2 nspan)))
        ;; 2. Forward chunk search
        (when (< pos end)
          ;; Extended chunks: car is the original innermode. Cannot use
          ;; autochunk modes (i.e. markdwon fortran-inner-mode) in calls to
          ;; pm-next-chunk. It would return fortran chunks.
          (let ((echunks (cl-loop for im in (eieio-oref pm/polymode '-innermodes)
                                  collect (cons im nil)))
                spans)
            (while (< pos end)
              ;; 1. Recompute outdated chunks - if pos behind a chunk, replace
              ;; this chunk with next chunk of the same type.
              (let (tchunks)
                (dolist (echunk echunks)
                  (if (and (cdr echunk)
                           (< pos (nth 5 echunk)))
                      (push echunk tchunks)
                    (let ((nchunk (pm-next-chunk (car echunk) pos)))
                      (if nchunk
                          (push (cons (car echunk) nchunk) tchunks)
                        ;; If nil, chunk is the last of this type in the buffer,
                        ;; or there are no such chunks at all (on 1st iteration).
                        ;; Keep it in the list in order to correctly compute last
                        ;; intersections with nested innermodes.
                        (when (cdr echunk)
                          (push echunk tchunks))))))
                (setq echunks (reverse tchunks)))
              ;; 2. Compute all (next) spans from spans
              (setq spans nil)
              (dolist (echunk echunks)
                (let ((chunk (cdr echunk)))
                  (let ((s (cond
                            ((< pos (nth 1 chunk)) (list nil pos (nth 1 chunk) (car chunk)))
                            ((< pos (nth 2 chunk)) (list 'head (nth 1 chunk) (nth 2 chunk) (car chunk)))
                            ((< pos (nth 3 chunk)) (list 'body (nth 2 chunk) (nth 3 chunk) (car chunk)))
                            ((< pos (nth 4 chunk)) (list 'tail (nth 3 chunk) (nth 4 chunk) (car chunk)))
                            (t (list nil (nth 4 chunk) (point-max) (car chunk))))))
                    (push s spans))))
              (setq spans (nreverse spans))
              ;; 3. Intersect the spans
              (setq nspan (list nil pos (point-max) hostmode))
              (dolist (s spans)
                (setq nspan (pm--intersect-spans nspan s)))
              ;; NB: If there is a bug in the core, this caching is likely
              ;; causing major issues (runs in font-lock). Disable during
              ;; debugging.
              (pm-cache-span nspan)
              (setq nttype (pm-true-span-type nspan))
              ;; 4. funcall on (previous) region if type changed
              (unless (eq ttype nttype)
                (with-current-buffer (pm-span-buffer span)
                  (funcall fn beg pos))
                (setq ttype nttype
                      beg (nth 1 nspan)))
              (setq span nspan
                    pos (nth 2 nspan)))))
        ;; 5. funcall on last region
        (with-current-buffer (pm-span-buffer span)
          (funcall fn beg pos))))))

;; ;; do not delete: speed and consistency checks
;; (defvar pm--span-counter 0)
;; (defvar pm--mode-counter 0)
;; (defun pm-debug-map-over-modes-test (&optional beg end)
;;   (interactive)
;;   (setq pm--span-counter 0)
;;   (setq pm--mode-counter 0)
;;   (pm-map-over-modes
;;    (lambda (beg end)
;;      (setq pm--mode-counter (1+ pm--mode-counter)))
;;    (or beg (point-min))
;;    (or end (point-max)))
;;   (cons pm--span-counter pm--mode-counter))
;; (defun pm-debug-map-over-spans-test (&optional beg end)
;;   (interactive)
;;   (setq pm--span-counter 0)
;;   (pm-map-over-spans
;;    (lambda (span)
;;      (setq pm--span-counter (1+ pm--span-counter)))
;;    (or beg (point-min))
;;    (or end (point-max)))
;;   pm--span-counter)

(defun pm-map-over-spans (fun &optional beg end count backwardp visibly no-cache)
  "For all spans between BEG and END, execute FUN.
FUN is a function of one argument a span object (also available
in a dynamic variable *span*). Buffer is *not* narrowed to the
span, nor point is moved. If COUNT is non-nil, jump at most that
many times. If BACKWARDP is non-nil, map backwards. If VISIBLY is
non-nil select buffers with the full synchronization (as if
performed by the user), otherwise point synchronization across
indirect buffers is not taken care of. Modification of the buffer
during mapping is an undefined behavior."
  ;; Important! Don't forget to save-excursion when calling map-overs-spans and
  ;; synchronize points if needed. Mapping can end in different buffer and
  ;; invalidate the caller assumptions.
  (save-restriction
    (widen)
    (setq beg (or beg (point-min))
          end (if end
                  (min end (point-max))
                (point-max)))
    (unless count
      (setq count most-positive-fixnum))
    (let* ((nr 0)
           (pos (if backwardp end beg))
           (*span* (pm-innermost-span pos no-cache)))
      (while *span*
        (setq nr (1+ nr))
        (pm-select-buffer *span* visibly)
        ;; FUN might change buffer and invalidate our *span*. Should we care or
        ;; reserve pm-map-over-spans for "read-only" actions only? Does
        ;; after-change run immediately or after this function ends?
        (funcall fun *span*)
        ;; enter previous/next chunk
        (setq pos
              (if backwardp
                  (max 1 (1- (nth 1 *span*)))
                (min (point-max) (nth 2 *span*))))
        (setq *span*
              (and (if backwardp
                       (> pos beg)
                     (< pos end))
                   (< nr count)
                   (pm-innermost-span pos no-cache)))))))

(defun pm-narrow-to-span (&optional span)
  "Narrow to current SPAN."
  (interactive)
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm-innermost-span))))
      (let ((sbeg (nth 1 span))
            (send (nth 2 span)))
        (unless pm--emacs>26
          (pm--reset-ppss-cache span))
        (narrow-to-region sbeg send)))))

(defmacro pm-with-narrowed-to-span (span &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(save-restriction
     (pm-narrow-to-span ,span)
     ,@body))



;;; HOOKS
;; There is also `poly-lock-after-change' in poly-lock.el
(defun polymode-flush-syntax-ppss-cache (beg end _)
  "Run `syntax-ppss-flush-cache' from BEG to END in all polymode buffers.
Placed with high priority in `after-change-functions' hook."
  ;; Modification hooks are run only in current buffer and not in other (base or
  ;; indirect) buffers. Thus some actions like flush of ppss cache must be taken
  ;; care explicitly. We run some safety hooks checks here as well.
  (dolist (buff (oref pm/polymode -buffers))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        ;; micro-optimization to avoid calling the flush twice
        (when (memq #'syntax-ppss-flush-cache before-change-functions)
          (remove-hook 'before-change-functions #'syntax-ppss-flush-cache t))
        ;; need to be the first to avoid breaking preceding hooks
        (unless (eq (car after-change-functions)
                    #'polymode-flush-syntax-ppss-cache)
          (delq #'polymode-flush-syntax-ppss-cache after-change-functions)
          (setq after-change-functions (cons #'polymode-flush-syntax-ppss-cache
                                             after-change-functions)))
        (syntax-ppss-flush-cache beg end)
        ;; Check if something has changed our hooks. (Am I theoretically paranoid or
        ;; this is indeed needed?) `fontification-functions' (and others?) should be
        ;; checked as well I guess.
        ;; (when (memq 'font-lock-after-change-function after-change-functions)
        ;;   (remove-hook 'after-change-functions 'font-lock-after-change-function t))
        ;; (when (memq 'jit-lock-after-change after-change-functions)
        ;;   (remove-hook 'after-change-functions 'jit-lock-after-change t))
        ))))

(defun pm--run-other-hooks (allow syms hook &rest args)
  (when (and allow polymode-mode pm/polymode)
    (dolist (sym syms)
      (dolist (buf (eieio-oref pm/polymode '-buffers))
        (when (buffer-live-p buf)
          (unless (eq buf (current-buffer))
            (with-current-buffer buf
              (when (memq sym (symbol-value hook))
                (if args
                    (apply sym args)
                  (funcall sym))))))))))

;; BUFFER SAVE
;; TOTHINK: add auto-save-hook?
(defvar polymode-run-these-before-save-functions-in-other-buffers nil
  "Beore-save functions to run in indirect buffers.
Saving happens from the base buffer, thus only `before-save-hook'
declared in the base buffer is triggered.")

(defvar polymode-run-these-after-save-functions-in-other-buffers nil
  "After-save functions to run in indirect buffers.
Saving happens from the base buffer, thus only `after-save-hook'
declared in the base buffer is triggered.")

(defun polymode-before-save ()
  "Run after-save-hooks in indirect buffers.
Only those in `polymode-run-these-after-save-functions-in-other-buffers'
are triggered if present."
  (pm--run-other-hooks t
                       polymode-run-these-before-save-functions-in-other-buffers
                       'after-save-hook))

(defun polymode-after-save ()
  "Run after-save-hooks in indirect buffers.
Only those in `polymode-run-these-after-save-functions-in-other-buffers'
are triggered if present."
  (pm--run-other-hooks t
                       polymode-run-these-after-save-functions-in-other-buffers
                       'after-save-hook))


;; change hooks
(defvar polymode-run-these-before-change-functions-in-other-buffers nil
  "Before-change functions to run in all other buffers.")
(defvar polymode-run-these-after-change-functions-in-other-buffers nil
  "After-change functions to run in all other buffers.")

(defun polymode-before-change (beg end)
  "Polymode before-change fixes.
Run `polymode-run-these-before-change-functions-in-other-buffers'.
Placed with low priority in `before-change-functions' hook."
  (pm--prop-put :before-change-range (cons beg end))
  ;; FIXME: LSP specific move this out somehow
  (when (boundp 'lsp-mode)
    (dolist (buf (eieio-oref pm/polymode '-buffers))
      (with-current-buffer buf
        (when lsp-mode
          (setq pm--lsp-before-change-end-position (pm--lsp-position end))))))
  (pm--run-other-hooks pm-allow-before-change-hook
                       polymode-run-these-before-change-functions-in-other-buffers
                       'before-change-functions
                       beg end))

(defun polymode-after-change (beg end len)
  "Polymode after-change fixes.
Run `polymode-run-these-after-change-functions-in-other-buffers'.
Placed with low priority in `after-change-functions' hook."
  (pm--run-other-hooks pm-allow-after-change-hook
                       polymode-run-these-after-change-functions-in-other-buffers
                       'after-change-functions
                       beg end len))

(defvar polymode-run-these-pre-commands-in-other-buffers nil
  "These commands, if present in `pre-command-hook', are run in other bufers.")
(defvar polymode-run-these-post-commands-in-other-buffers nil
  "These commands, if present in `post-command-hook', are run in other bufers.")

(defun polymode-pre-command ()
  "Synchronize state between buffers and run pre-commands in other buffers.
Currently synchronize points and runs
`polymode-run-these-pre-commands-in-other-buffers' if any. Runs in
local `pre-command-hook' with very high priority."
  (pm--synchronize-points (current-buffer))
  (condition-case err
      (pm--run-other-hooks pm-allow-pre-command-hook
                           polymode-run-these-pre-commands-in-other-buffers
                           'pre-command-hook)
    (error (message "error polymode-pre-command run other hooks: (%s) %s"
                    (point) (error-message-string err)))))

(defun polymode-post-command ()
  "Select the buffer relevant buffer and run post-commands in other buffers.
Run all the `post-command-hooks' in the new buffer and those
command defined in
`polymode-run-these-post-commands-in-other-buffers' whenever
appropriate. This function is placed into local
`post-command-hook' with very low priority."
  (when (and pm-allow-post-command-hook
             polymode-mode
             pm/polymode)
    (let ((cbuf (current-buffer)))
      (condition-case err
          (pm-switch-to-buffer)
        (error (message "error in polymode-post-command: (pm-switch-to-buffer %s): %s"
                        (point) (error-message-string err))))
      (condition-case err
          (if (eq cbuf (current-buffer))
              ;; 1. same buffer, run hooks in other buffers
              (pm--run-other-hooks pm-allow-post-command-hook
                                   polymode-run-these-post-commands-in-other-buffers
                                   'post-command-hook)
            ;; 2. Run all hooks in this (newly switched to) buffer
            (run-hooks 'post-command-hook))
        (error (message "error in polymode-post-command run other hooks: (%s) %s"
                        (point) (error-message-string err)))))))

(defvar-local pm--killed nil)
(defun polymode-after-kill-fixes ()
  "Various fixes for polymode indirect buffers."
  (when (pm-own-buffer-p)
    (let ((base (pm-base-buffer)))
      (set-buffer-modified-p nil)
      ;; Prevent various tools like `find-file' to re-find this file.
      ;;
      ;; We use buffer-list instead of `-buffers' slot here because on some
      ;; occasions there are other indirect buffers (e.g. switch from polymode
      ;; to other mode and then back, or when user or a tool (e.g. org-capture)
      ;; creates an indirect buffer manually).
      (dolist (b (buffer-list))
        (when (and (buffer-live-p b)
                   (eq (buffer-base-buffer b) base))
          (with-current-buffer b
            (setq pm--killed t)
            (setq buffer-file-name nil)
            (setq buffer-file-number nil)
            (setq buffer-file-truename nil)))))))

(defun pm-turn-polymode-off (&optional new-mode)
  "Remove all polymode indirect buffers and install NEW-MODE if any.
NEW-MODE can be t in which case mode is picked from the
`pm/polymode' object."
  (when pm/polymode
    (let* ((base (pm-base-buffer))
           (mmode (buffer-local-value 'major-mode base))
           (kill-buffer-hook (delete 'polymode-after-kill-fixes (copy-sequence kill-buffer-hook))))
      ;; remove only our own indirect buffers
      (dolist (b (eieio-oref pm/polymode '-buffers))
        (unless (eq b base)
          (kill-buffer b)))
      (with-current-buffer base
        (setq pm/polymode nil)
        (when new-mode
          (if (eq new-mode t)
              (funcall mmode)
            (funcall new-mode)))))))

(defun polymode-after-change-major-mode-cleanup ()
  "Remove all polymode implementation buffers on mode change."
  ;; pm/polymode is permanent local. Nil polymode-mode means that the user
  ;; called another mode on top of polymode.
  (when (and pm/polymode (not polymode-mode))
    ;; if another mode was called from an innermode, it was installed in a wrong place
    (let* ((base (pm-base-buffer))
           (mmode (unless (eq base (current-buffer))
                    major-mode)))
      (unless (eq base (current-buffer))
        (when (eq (window-buffer) (current-buffer))
          (switch-to-buffer base)))
      (pm-turn-polymode-off mmode))))

(add-hook 'after-change-major-mode-hook #'polymode-after-change-major-mode-cleanup)




;;; CORE ADVICE

(defun pm-around-advice (fun advice)
  "Apply around ADVICE to FUN.
If FUN is a list, apply ADVICE to each element of it."
  (cond ((listp fun)
         (dolist (el fun) (pm-around-advice el advice)))
        ((and (symbolp fun)
              (not (advice-member-p advice fun)))
         (advice-add fun :around advice))))

(defun polymode-inhibit-during-initialization (orig-fun &rest args)
  "Don't run ORIG-FUN (with ARGS) during polymode setup."
  (unless pm-initialization-in-progress
    (apply orig-fun args)))

(defun polymode-inhibit-in-indirect-buffers (orig-fun &rest args)
  "Don't run ORIG-FUN (with ARGS) in polymode indirect buffers (aka inner modes).
Use this function to around advice delicate functions:
   (advice-add \\='xyz :around #\\='polymode-inhibit-in-indirect-buffers)
or with `pm-around-advice' which allows for multiple advises at once:
   (pm-around-advice \\='(foo bar) #\\='polymode-inhibit-in-indirect-buffers)"
  (unless (and polymode-mode (buffer-base-buffer))
    (apply orig-fun args)))

(defun polymode-with-current-base-buffer (orig-fun &rest args)
  "Switch to base buffer and apply ORIG-FUN to ARGS.
Use this function to around advice of functions that should run
in base buffer only like this:
   (advice-add \\='foo :around #\\='polymode-with-current-base-buffer)
or with `pm-around-advice' which allows for multiple advises at
once:
   (pm-around-advice \\='(foo bar) #\\='polymode-with-current-base-buffer)"
  (if (and polymode-mode
           (not pm--killed)
           (buffer-live-p (buffer-base-buffer)))
      (let (;; (pm-initialization-in-progress t) ; just in case
            (cur-buf (current-buffer))
            (base (buffer-base-buffer))
            (first-arg (car-safe args)))
        (prog1 (with-current-buffer base
                 (if (or (eq first-arg cur-buf)
                         (equal first-arg (buffer-name cur-buf)))
                     (apply orig-fun base (cdr args))
                   (apply orig-fun args)))
          ;; The sync of points doesn't work as expected in the following corner
          ;; case: if current buffer is an indirect one and a function operates
          ;; on the base buffer (like save-buffer) and somehow inadvertently
          ;; moves points in the indirect buffer then we synchronize wrong point
          ;; (from the current indirect buffer). For unclear reasons the very
          ;; low level scan-lists moves points in indirect buffers (FIXME: EMACS
          ;; bug, report ASAP). Unfortunately save-excursion protects only from
          ;; point moves in the current buffer.
          (when pm/polymode
            (pm--synchronize-points base))))
    (apply orig-fun args)))

;; Most importat Core
;; (pm-around-advice #'kill-buffer #'polymode-with-current-base-buffer)
(pm-around-advice 'find-alternate-file #'polymode-with-current-base-buffer)
(pm-around-advice 'write-file #'polymode-with-current-base-buffer)
(pm-around-advice 'basic-save-buffer #'polymode-with-current-base-buffer)


;;; FILL

;; FIXME: this is an incomplete heuristic and breaks on adjacent multi-span
;; fill-region depending on the mode's fill-forward-paragraph-function. For a
;; complete solution one might likely need to define fill-paragraph-function as
;; well.
(defun polymode-fill-forward-paragraph (&optional arg)
  "Function for `fill-forward-paragraph-function'.
ARG is the same as in `forward-paragraph'"
  (let* ((neg (< arg 0))
         (cur-span (pm-innermost-span (if neg (1- (point)) (point))))
         (cur-mode (pm-span-mode cur-span))
         (out (funcall (or pm--fill-forward-paragraph-original
                           #'forward-paragraph)
                       arg))
         (new-mode (pm-span-mode (pm-innermost-span (point)))))
    (unless (eq cur-mode new-mode)
      ;; adjust to the most recent span border and hope for the best
      (pm-goto-span-of-type (car cur-span) (if neg 1 -1)))
    out))


;;; SYNTAX

(defun pm--call-syntax-propertize-original (start end)
  (condition-case err
      (save-excursion
        (funcall pm--syntax-propertize-function-original start end))
    (error
     (message "ERROR: (%s %d %d) -> %s"
              (if (symbolp pm--syntax-propertize-function-original)
                  pm--syntax-propertize-function-original
                (format "polymode-syntax-propertize:%s" major-mode))
              start end
              ;; (backtrace)
              (error-message-string err)))))

(defun polymode-syntax-propertize-extend-region-in-host (start end)
  (let ((base (pm-base-buffer))
        (min (point-min))
        (max (point-max)))
    (when base
      (with-current-buffer base
        (save-restriction
          (narrow-to-region min max)
          ;; Relevant part from syntax-propertize
          (let ((funs syntax-propertize-extend-region-functions)
                (extended nil))
            (while funs
              (let* ((syntax-propertize--done most-positive-fixnum)
                     (fn (pop funs))
                     (new (unless (eq fn 'syntax-propertize-wholelines)
                            (funcall fn start end))))
                (when (and new
                           (or (< (car new) start)
                               (> (cdr new) end)))
                  (setq extended t
                        start (car new)
                        end (cdr new))
                  ;; If there's been a change, we should go through the list again
                  ;; since this new position may warrant a different answer from
                  ;; one of the funs we've already seen.
                  (unless (eq funs (cdr syntax-propertize-extend-region-functions))
                    (setq funs syntax-propertize-extend-region-functions)))))
            (when extended (cons start end))))))))

;; used for hard debugging of syntax properties in batch mode
(defun pm--syntax-after (pos)
  (let ((syntax (syntax-after pos)))
    (with-temp-buffer
      (internal-describe-syntax-value syntax)
      (buffer-string))))

;; called from syntax-propertize and thus at the beginning of syntax-ppss
(defun polymode-syntax-propertize (beg end)
  ;; (message "SP:%d-%d" beg end)
  (unless pm-initialization-in-progress
    (save-restriction
      (widen)
      (save-excursion

        ;; some modes don't save data in their syntax propertize functions
        (save-match-data
          (let ((base (pm-base-buffer))
                (protect-host (with-current-buffer (pm-base-buffer)
                                (eieio-oref pm/chunkmode 'protect-syntax))))

            ;; 1. host if no protection
            (unless protect-host
              (with-current-buffer base
                (set 'syntax-propertize--done end)
                ;; (message "sp:%s:%d-%d" major-mode beg end)
                (when pm--syntax-propertize-function-original
                  ;; For syntax matchers the host mode syntax prioritization
                  ;; should be smart enough to install relevant elements around
                  ;; end for the followup map-over-modes to work correctly.
                  (pm--call-syntax-propertize-original beg end))))

            ;; 2. all others
            (let ((last-ppss nil))
              (pm-map-over-modes
               (lambda (mbeg mend)
                 ;; Cannot set this earlier because some buffers might not be
                 ;; created when this function is called. One major reason to
                 ;; set this here is to avoid recurring into syntax-propertize
                 ;; when propertize functions call syntax-ppss. `setq' doesn't
                 ;; have an effect because the var is let bound but `set'
                 ;; works.
                 (set 'syntax-propertize--done (max end mend))
                 ;; (message "sp:%s:%d-%d" major-mode (max beg mbeg) mend)
                 (if (eq base (current-buffer))
                     (when protect-host
                       (pm--reset-ppss-cache-0 mbeg last-ppss)
                       (when pm--syntax-propertize-function-original
                         (pm--call-syntax-propertize-original (max beg mbeg) mend))
                       (setq last-ppss (syntax-ppss mend)))
                   (pm--reset-ppss-cache-0 mbeg)
                   (when pm--syntax-propertize-function-original
                     (pm--call-syntax-propertize-original (max beg mbeg) mend))))
               beg end))))))))

(defvar syntax-ppss-wide)
(defvar syntax-ppss-last)
(defvar syntax-ppss-cache)
(defun pm--reset-ppss-cache (span)
  "Reset `syntax-ppss-last' cache if it was recorded before SPAN's start."
  (let ((sbeg (nth 1 span))
        new-ppss)
    (unless (car span)
      ;; Host chunk is special. Pick ppss from end of last span. Body chunks
      ;; with nested inner chunks should be treated the same but no practical
      ;; example showed so far.
      (save-restriction
        (widen)
        (save-excursion
          (let ((pos sbeg))
            (while (and (null new-ppss)
                        (not (= pos (point-min))))
              (let ((prev-span (pm-innermost-span (1- pos))))
                (if (null (car prev-span))
                    (setq new-ppss (syntax-ppss pos))
                  (setq pos (nth 1 prev-span)))))))))
    (pm--reset-ppss-cache-0 sbeg new-ppss)))

(defun pm--reset-ppss-cache-0 (pos &optional new-ppss)
  (unless new-ppss
    (setq new-ppss (list 0 nil pos nil nil nil 0 nil nil nil nil)))
  ;; in emacs 26 there are two caches syntax-ppss-wide and
  ;; syntax-ppss-narrow. The latter is reset automatically each time a
  ;; different narrowing is in place so we don't deal with it for now.
  (let ((cache (if pm--emacs>26
                   (cdr syntax-ppss-wide)
                 syntax-ppss-cache)))
    (while (and cache (>= (caar cache) pos))
      (setq cache (cdr cache)))
    (setq cache (cons (cons pos new-ppss) cache))
    (if pm--emacs>26
        ;; syntax-ppss involves an aggressive cache cleaning; protect for one
        ;; such cleaning by double entry
        (setq syntax-ppss-wide (cons (car cache) cache))
      (setq syntax-ppss-cache cache)
      (setq syntax-ppss-last (cons pos new-ppss))))
  new-ppss)


;; (defun polymode-reset-ppss-cache (&optional pos)
;;   "Reset `syntax-ppss' cache to POS in polymode buffers.
;; Used in :before advice of `syntax-ppss'."
;;   (when polymode-mode
;;     (pm--reset-ppss-cache (pm-innermost-span pos))))

;; (advice-add 'syntax-ppss :before #'polymode-reset-ppss-cache)
;; (unless pm--emacs>26
;;   (advice-add 'syntax-ppss :before #'polymode-reset-ppss-cache))

;; (defun polymode-restrict-syntax-propertize-extension (orig-fun beg end)
;;   (if (and polymode-mode pm/polymode)
;;       (let ((span (pm-innermost-span beg)))
;;         (if (eieio-oref (nth 3 span) 'protect-syntax)
;;             (let ((range (pm-span-to-range span)))
;;               (if (and (eq beg (car range))
;;                        (eq end (cdr range)))
;;                   ;; in the most common case when span == beg-end, simply return
;;                   range
;;                 (let ((be (funcall orig-fun beg end)))
;;                   (and be
;;                        (cons (max (car be) (car range))
;;                              (min (cdr be) (cdr range)))))))
;;           (funcall orig-fun beg end)))
;;     (funcall orig-fun beg end)))


;;; INTERNAL UTILITIES

(defun pm--set-transient-map (commands)
  "Set transient map with COMMANDS.
COMMANDS is a list of commands which are bound to their
accessible keys as well as the basic event of those keys. Used
for \"cycling\" commands."
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (cmd)
            (mapc (lambda (vec)
                    (define-key map vec cmd)
                    (let ((basic-ev (elt vec (1- (length vec)))))
                      (define-key map (vector basic-ev) cmd)))
                  (where-is-internal cmd)))
          commands)
    (set-transient-map map)))

(defun pm--display-file (ofile)
  (when ofile
    ;; errors might occur (most notably with open-with package errors are intentional)
    ;; We need to catch those if we want to display multiple files like with Rmarkdown
    (condition-case-unless-debug err
        (let ((buff (get-file-buffer ofile)))
          (when buff
            (with-current-buffer buff
              (with-demoted-errors "Error while reverting: %s"
                ;; FIXME: something is not right with pdflatex export with
                ;; pdf-tools viewer within emacs
                (revert-buffer t t))))
          (when (if (functionp polymode-display-output-file)
                    (funcall polymode-display-output-file ofile)
                  polymode-display-output-file)
            (if (string-match-p "html\\|htm$" ofile)
                (browse-url ofile)
              (display-buffer (find-file-noselect ofile 'nowarn)))))
      (error (message "Error while displaying '%s': %s"
                      (file-name-nondirectory ofile)
                      (error-message-string err))))))

(defun pm--symbol-name (str-or-symbol)
  (if (symbolp str-or-symbol)
      (symbol-name str-or-symbol)
    str-or-symbol))

(defun pm--true-mode-symbol (mode)
  "Resolve aliases of MODE and return the true MODE name."
  (while (and mode (symbolp (symbol-function mode)))
    (setq mode (symbol-function mode)))
  mode)

(defun pm--get-existing-mode (mode fallback)
  "Return MODE symbol if it's defined and is a valid function.
If so, return it, otherwise check in turn
`polymode-default-inner-mode', the FALLBACK and ultimately
`poly-fallback-mode'."
  (pm--true-mode-symbol
   (cond ((fboundp mode) mode)
         ((eq polymode-default-inner-mode 'host) (buffer-local-value 'major-mode (pm-base-buffer)))
         ((fboundp polymode-default-inner-mode) polymode-default-inner-mode)
         ((eq fallback 'host) (buffer-local-value 'major-mode (pm-base-buffer)))
         ((fboundp fallback) fallback)
         (t 'poly-fallback-mode))))

(defun pm--get-innermode-mode (chunkmode type)
  "Retrieve the mode name of for inner CHUNKMODE for span of TYPE."
  (pm--get-existing-mode
   (cl-case (pm-true-span-type chunkmode type)
     (body (eieio-oref chunkmode 'mode))
     (head (eieio-oref chunkmode 'head-mode))
     (tail (eieio-oref chunkmode 'tail-mode))
     (t (error "Invalid type (%s); must be one of body, head tail" type)))
   (eieio-oref chunkmode 'fallback-mode)))

;; Used in auto innermode detection only and can return symbol 'host as that's
;; needed in pm--get-auto-chunkmode.
(defun pm-get-mode-symbol-from-name (name &optional fallback)
  "Guess and return mode function from short NAME.
Return FALLBACK if non-nil, otherwise the value of
`polymode-default-inner-mode' if non-nil, otherwise value of slot
:fallback-mode which globally defaults to `poly-fallback-mode'."
  (pm--true-mode-symbol
   (cond
    ;; anonymous chunk
    ((or (null name)
         (and (stringp name) (= (length name) 0)))
     (or
      (when (or (eq polymode-default-inner-mode 'host)
                (fboundp polymode-default-inner-mode))
        polymode-default-inner-mode)
      (when (or (eq fallback 'host)
                (fboundp fallback))
        fallback)
      'poly-fallback-mode))
    ;; proper mode symbol
    ((and (symbolp name) (fboundp name) name))
    ;; compute from name
    ((let* ((str (pm--symbol-name
                  (or (cdr (assq (intern (pm--symbol-name name))
                                 polymode-mode-name-aliases))
                      name)))
            (mname (if (string-match-p "-mode$" str)
                       str
                     (concat str "-mode"))))
       (or
        ;; direct search
        (let ((mode (intern mname)))
          (when (fboundp mode)
            mode))
        ;; downcase
        (let ((mode (intern (downcase mname))))
          (when (fboundp mode)
            mode))
        ;; auto-mode alist
        (let ((dummy-file (concat "a." str)))
          (cl-loop for (k . v) in auto-mode-alist
                   if (and (string-match-p k dummy-file)
                           (not (string-match-p "^poly-" (symbol-name v))))
                   return v))
        (when (or (eq polymode-default-inner-mode 'host)
                  (fboundp polymode-default-inner-mode))
          polymode-default-inner-mode)
        (when (or (eq fallback 'host)
                  (fboundp fallback))
          fallback)
        'poly-fallback-mode))))))

(defun pm--oref-with-parents (object slot)
  "Merge slots SLOT from the OBJECT and all its parent instances."
  (let (VALS)
    (while object
      (setq VALS (append (and (slot-boundp object slot) ; don't cascade
                              (eieio-oref object slot))
                         VALS)
            object (and (slot-boundp object :parent-instance)
                        (eieio-oref object 'parent-instance))))
    VALS))

(defun pm--abrev-names (abrev-regexp list)
  "Abbreviate names in LIST by erasing ABREV-REGEXP matches.
Elements of LIST can be either strings or symbols."
  (mapcar (lambda (nm)
            (let* ((str-nm (if (symbolp nm)
                               (symbol-name nm)
                             nm))
                   (prefix (replace-regexp-in-string "^poly-[^-]+\\(.+\\)" "" str-nm nil nil 1))
                   (is-lib (or (string= prefix "poly-r") ; ugly special case as the library is called poly-R
                               (featurep (intern prefix)))))
              (cons (replace-regexp-in-string abrev-regexp ""
                                              (if is-lib
                                                  (replace-regexp-in-string "^poly-[^-]+-" "" str-nm)
                                                str-nm))
                    str-nm)))
          list))

(defun pm--object-value (obj)
  (cond
   ((functionp obj)
    (funcall obj))
   ((symbolp obj)
    (symbol-value obj))
   (t obj)))

(defun pm--oref-value (object slot)
  (pm--object-value (eieio-oref object slot)))

(defun pm--prop-put (key val &optional object)
  (oset (or object pm/polymode) -props
        (plist-put (oref (or object pm/polymode) -props) key val)))

(defun pm--prop-get (key &optional object)
  (plist-get (oref (or object pm/polymode) -props) key))

(defun pm--comment-region (beg end)
  ;; mark as syntactic comment
  (when (> end 1)
    (with-silent-modifications
      (let ((beg (or beg (region-beginning)))
            (end (or end (region-end))))
        (let ((ch-beg (char-after beg))
              (ch-end (char-before end)))
          (add-text-properties beg (1+ beg)
                               (list 'syntax-table (cons 11 ch-beg)
                                     'rear-nonsticky t
                                     'polymode-comment 'start))
          (add-text-properties (1- end) end
                               (list 'syntax-table (cons 12 ch-end)
                                     'rear-nonsticky t
                                     'polymode-comment 'end)))))))

(defun pm--uncomment-region (beg end)
  ;; Remove all syntax-table properties.
  ;; fixme: this beggs for problems
  (when (> end 1)
    (with-silent-modifications
      (let ((props '(syntax-table nil rear-nonsticky nil polymode-comment nil)))
        (remove-text-properties (max beg (point-min)) (min end (point-max)) props)
        ;; (remove-text-properties beg (1+ beg) props)
        ;; (remove-text-properties end (1- end) props)
        ))))

(defun pm--synchronize-points (&optional buffer)
  "Synchronize the point in polymode buffers with the point in BUFFER."
  (setq buffer (current-buffer))
  (when (and polymode-mode
             (buffer-live-p buffer))
    (let* ((bufs (eieio-oref pm/polymode '-buffers))
           ;; (buffer (or buffer
           ;;             (cl-loop for b in bufs
           ;;                      if (and (buffer-live-p b)
           ;;                              (buffer-local-value 'pm/current b))
           ;;                      return b)
           ;;             (current-buffer)))
           (pos (with-current-buffer buffer (point))))
      (dolist (b bufs)
        (when (buffer-live-p b)
          (with-current-buffer b
            (goto-char pos)))))))

(defun pm--completing-read (prompt collection &optional predicate require-match
                                   initial-input hist def inherit-input-method)
  ;; Wrapper for `completing-read'.
  ;; Take care when collection is an alist of (name . meta-info). If
  ;; so, asks for names, but returns meta-info for that name. Enforce
  ;; require-match = t. Also takes care of adding the most relevant
  ;; DEF from history.
  (if (and (listp collection)
           (listp (car collection)))
      (let* ((candidates (mapcar #'car collection))
             (thirst (and hist
                          (delq nil (mapcar (lambda (x) (car (member x candidates)))
                                            (symbol-value hist)))))
             (def (or def (car thirst) (car candidates))))
        (assoc (completing-read prompt candidates predicate t initial-input hist def inherit-input-method)
               collection))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))


;;; WEAVING and EXPORTING
;; fixme: move all these into separate polymode-process.el?
(defvar polymode-exporter-output-file-format)
(defvar polymode-weaver-output-file-format)
(declare-function pm-export "polymode-export")
(declare-function pm-weave "polymode-weave")
(declare-function comint-exec "comint")
(declare-function comint-mode "comint")

(defun pm--wrap-callback (processor slot _ifile)
  ;; replace processor :sentinel or :callback temporally in order to export-spec as a
  ;; followup step or display the result
  (let ((sentinel1 (eieio-oref processor slot))
        (cur-dir default-directory)
        (exporter (symbol-value (eieio-oref pm/polymode 'exporter)))
        (obuffer (current-buffer)))
    (if pm--export-spec
        ;; 2-stage weaver->exporter
        (let ((espec pm--export-spec))
          (lambda (&rest args)
            (with-current-buffer obuffer
              (let ((wfile (apply sentinel1 args))
                    (pm--export-spec nil)
                    (pm--input-not-real t))
                ;; If no wfile, probably errors occurred. So we stop.
                (when wfile
                  (when (listp wfile)
                    ;; In an unlikely situation weaver can generate multiple
                    ;; files. Pick the first one.
                    (setq wfile (car wfile)))
                  (pm-export exporter (car espec) (cdr espec) wfile))))))
      (lambda (&rest args)
        (with-current-buffer obuffer
          (let ((ofile (apply sentinel1 args)))
            (when ofile
              (let ((ofiles (if (listp ofile) ofile (list ofile))))
                (dolist (f ofiles)
                  (pm--display-file (expand-file-name f cur-dir)))))))))))

(defun pm--file-mod-time (file)
  (and (stringp file)
       (file-exists-p file)
       (nth 5 (file-attributes file))))

(defvar-local pm--process-buffer nil)
;; Simplified version of TeX-run-TeX. Run shell COMMAND interactively in BUFFER.
;; Run COMMAND in a buffer (in comint-shell-mode) in order to be able to accept
;; user interaction.
(defun pm--run-shell-command (command sentinel buff-name message)
  (require 'comint)
  (let* ((buffer (get-buffer-create buff-name))
         (process nil)
         ;; weave/export buffers are re-usable; need to transfer some vars
         (dd default-directory)
         ;; (command (shell-quote-argument command))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (setq-local default-directory dd)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert message)
      (comint-exec buffer buff-name shell-file-name nil
                   (list shell-command-switch command))
      (setq process (get-buffer-process buffer))
      (comint-mode)
      (goto-address-mode 1)
      (set-process-sentinel process sentinel)
      (setq pm--process-buffer t)
      (set-marker (process-mark process) (point-max))
      ;; for communication with sentinel
      (process-put process :output-file pm--output-file)
      (process-put process :output-file-mod-time (pm--file-mod-time pm--output-file))
      (process-put process :input-file pm--input-file)
      (when polymode-display-process-buffers
        (display-buffer buffer `(nil . ((inhibit-same-window . ,pop-up-windows)))))
      nil)))

(defun pm--make-shell-command-sentinel (action)
  (lambda (process _name)
    "Sentinel built with `pm--make-shell-command-sentinel'."
    (let ((buff (process-buffer process))
          (status (process-exit-status process)))
      (if (> status 0)
          (progn
            (message "Errors during %s; process exit status %d" action status)
            (ding) (sit-for 1)
            nil)
        (with-current-buffer buff
          (let ((ofile (process-get process :output-file)))
            (cond
             ;; 1. output-file guesser
             ((functionp ofile) (funcall ofile))
             ;; 2. string
             (ofile
              (let ((otime (process-get process :output-file-mod-time))
                    (ntime (pm--file-mod-time ofile)))
                (if (or (null ntime)
                        (and otime
                             (not (time-less-p otime ntime))))
                    ;; mod time didn't change
                    ;; tothink: shall we still return ofile for display?
                    (progn
                      (display-buffer (current-buffer))
                      (message "Output file unchanged. Either input unchanged or errors during %s." action)
                      (ding) (sit-for 1)
                      ofile)
                  ;; else, all is good, we return the file name
                  ;; (display-buffer (current-buffer))
                  (message "Done with %s" action)
                  ofile)))
             ;; 3. output file is not known; display process buffer
             (t (display-buffer (current-buffer)) nil))))))))

(fset 'pm-default-shell-export-sentinel (pm--make-shell-command-sentinel "export"))
(fset 'pm-default-shell-weave-sentinel (pm--make-shell-command-sentinel "weaving"))

(defun pm--make-selector (specs elements)
  (cond ((functionp elements) elements)
        ((listp elements)
         (let ((spec-alist (cl-mapcar #'cons specs elements)))
           (lambda (selsym &rest _ignore)
             (cdr (assoc selsym spec-alist)))))
        (t (error "Elements argument must be either a list or a function"))))

(defun pm--selector (processor type id)
  (let ((spec (or (assoc id (eieio-oref processor type))
                  (error "%s spec '%s' cannot be found in '%s'"
                         (symbol-name type) id (eieio-object-name processor))))
        (names (cond
                ;; exporter slots
                ((eq type :from) '(regexp doc command))
                ((eq type :to) '(ext doc t-spec))
                ;; weaver slot
                ((eq type :from-to) '(regexp ext doc command))
                (t (error "Invalid type '%s'" type)))))
    (cons id (pm--make-selector names (cdr spec)))))

(defun pm--selector-match (el &optional file)
  (let* ((id (car el))
         (regexp (funcall (cdr el) 'regexp id)))
    (or (funcall (cdr el) 'match id file)
        (and regexp
             (string-match-p regexp (or file buffer-file-name))))))

(defun pm--matched-selectors (translator slot)
  (let ((translator (if (symbolp translator)
                        (symbol-value translator)
                      translator)))
    (cl-loop for el in (pm--selectors translator slot)
             when (pm--selector-match el)
             collect el)))

(defun pm--selectors (processor type)
  (let ((ids (mapcar #'car (eieio-oref processor type))))
    (mapcar (lambda (id) (pm--selector processor type id)) ids)))

(defun pm--output-command.file (output-file-format sfrom &optional sto quote)
  ;; !!Must be run in input buffer!!
  (cl-flet ((squote (arg) (or (and (stringp arg)
                                   (if quote (shell-quote-argument arg) arg))
                              "")))
    (let* ((el (or sto sfrom))
           (base-ofile (or (funcall (cdr el) 'output-file (car el))
                           (let ((ext (funcall (cdr el) 'ext (car el))))
                             (when ext
                               (concat (format output-file-format
                                               (file-name-base buffer-file-name))
                                       "." ext)))))
           (ofile (and (stringp base-ofile)
                       (expand-file-name base-ofile)))
           (oname (and (stringp base-ofile)
                       (file-name-base base-ofile)))
           (t-spec (and sto (funcall (cdr sto) 't-spec (car sto))))
           (command-w-formats (or (and sto (funcall (cdr sto) 'command (car sto)))
                                  (and (listp t-spec) (car t-spec))
                                  (funcall (cdr sfrom) 'command (car sfrom))))
           (command (format-spec command-w-formats
                                 (list (cons ?i (squote (file-name-nondirectory buffer-file-name)))
                                       (cons ?I (squote buffer-file-name))
                                       (cons ?o (squote base-ofile))
                                       (cons ?O (squote ofile))
                                       (cons ?b (squote oname))
                                       (cons ?t (squote t-spec))))))
      (cons command (or ofile base-ofile)))))

(defun pm--process-internal (processor from to ifile &optional callback quote)
  (let ((is-exporter (object-of-class-p processor 'pm-exporter)))
    (if is-exporter
        (unless (and from to)
          (error "For exporter both FROM and TO must be supplied (from: %s, to: %s)" from to))
      (unless from
        ;; it represents :from-to slot
        (error "For weaver FROM must be supplied (from: %s)" from)))
    (let* ((sfrom (if is-exporter
                      (pm--selector processor :from from)
                    (pm--selector processor :from-to from)))
           (sto (and is-exporter (pm--selector processor :to to)))
           (ifile (or ifile buffer-file-name))
           ;; fixme: nowarn is only right for inputs from weavers, you need to
           ;; save otherwise
           (ibuffer (if pm--input-not-real
                        ;; for exporter input we silently re-fetch the file
                        ;; even if it was modified
                        (find-file-noselect ifile t)
                      ;; if real user file, get it or fetch it
                      (or (get-file-buffer ifile)
                          (find-file-noselect ifile))))
           (output-format (if is-exporter
                              polymode-exporter-output-file-format
                            polymode-weaver-output-file-format)))
      (when (buffer-live-p ibuffer)
        (with-current-buffer ibuffer
          ;; FIXME: could be deleted buffer in weaver->exporter pipeline?
          (save-buffer)
          (let ((comm.ofile (pm--output-command.file output-format sfrom sto quote)))
            (let* ((pm--output-file (cdr comm.ofile))
                   (pm--input-file ifile)
                   ;; skip weaving step if possible
                   ;; :fixme this should not happen after weaver/exporter change
                   ;; or after errors in previous exporter
                   (omt (and polymode-skip-processing-when-unmodified
                             (stringp pm--output-file)
                             (pm--file-mod-time pm--output-file)))
                   (imt (and omt (pm--file-mod-time pm--input-file)))
                   (ofile (if (and imt (time-less-p imt omt))
                              (progn
                                (message "Not re-%s as input file '%s' hasn't changed"
                                         (if is-exporter "exporting" "weaving")
                                         (file-name-nondirectory ifile))
                                pm--output-file)
                            (message "%s '%s' with '%s' ..."
                                     (if is-exporter "EXPORTING" "WEAVING")
                                     (file-name-nondirectory ifile)
                                     (eieio-object-name processor))
                            (let ((fn (with-no-warnings
                                        (eieio-oref processor 'function)))
                                  ;; `to` is nil for weavers
                                  (args (delq nil (list from to)))
                                  (comm (car comm.ofile)))
                              (if callback
                                  ;; the display is handled within the
                                  ;; callback and return value of :function
                                  ;; slot is ignored
                                  (progn (apply fn comm callback args)
                                         nil)
                                (apply fn comm args))))))
              (when ofile
                (if pm--export-spec
                    ;; same logic as in pm--wrap-callback
                    (let ((pm--input-not-real t)
                          (espec pm--export-spec)
                          (pm--export-spec nil))
                      (when (listp ofile)
                        (setq ofile (car ofile)))
                      (pm-export (symbol-value (eieio-oref pm/polymode 'exporter))
                                 (car espec) (cdr espec)
                                 ofile))
                  (pm--display-file ofile))))))))))

;; (defun replace-poly-spec ()
;;   (interactive)
;;   (when (re-search-forward "defcustom +pm-\\(inner\\|host\\|poly\\)/\\([^ \n]+\\)" nil t)
;;     (let* ((mode (match-string 2))
;;            (type (match-string 1))
;;            (new-name (format "poly-%s-%smode" mode type)))
;;       (previous-line 1)
;;       (insert (format "(define-obsolete-variable-alias 'pm-%s/%s '%s \"v0.2\")\n" type mode new-name))
;;       (insert (format "(define-%smode %s\n)" type new-name)))))

(provide 'polymode-core)
;;; polymode-core.el ends here
