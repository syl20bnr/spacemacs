;;; polymode.el --- Extensible framework for multiple major modes -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu <spinuvit@gmail.com>
;; Copyright (C) 2013-2022  Free Software Foundation, Inc.
;; Version: 0.2.2
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/polymode/polymode
;; Keywords: languages, multi-modes, processes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Documentation at https://polymode.github.io
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-core)
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-compat)
(require 'polymode-export)
(require 'polymode-weave)
(require 'polymode-base)
(require 'poly-lock)
(require 'easymenu)
(require 'derived)

(defvar polymode-prefix-key nil
  "[Obsoleted] Prefix key for the polymode mode keymap.
Not effective after loading the polymode library.")
(make-obsolete-variable 'polymode-prefix-key "Unbind in `polymode-mode-map'" "v0.1.6")

(defvar polymode-map
  (let ((map (define-prefix-command 'polymode-map)))
    ;; eval
    (define-key map "v" 'polymode-eval-map)
    ;; navigation
    (define-key map "\C-n" #'polymode-next-chunk)
    (define-key map "\C-p" #'polymode-previous-chunk)
    (define-key map "\C-\M-n" #'polymode-next-chunk-same-type)
    (define-key map "\C-\M-p" #'polymode-previous-chunk-same-type)
    ;; chunk manipulation
    (define-key map "\M-k" #'polymode-kill-chunk)
    (define-key map "\M-m" #'polymode-mark-or-extend-chunk)
    (define-key map "\C-t" #'polymode-toggle-chunk-narrowing)
    ;; backends
    (define-key map "e" #'polymode-export)
    (define-key map "E" #'polymode-set-exporter)
    (define-key map "w" #'polymode-weave)
    (define-key map "W" #'polymode-set-weaver)
    (define-key map "t" #'polymode-tangle)
    (define-key map "T" #'polymode-set-tangler)
    (define-key map "$" #'polymode-show-process-buffer)
    map)
  "Polymode prefix map.
Lives on `polymode-prefix-key' in polymode buffers.")

(defvaralias 'polymode-mode-map 'polymode-minor-mode-map)
(defvar polymode-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (or polymode-prefix-key "\M-n") 'polymode-map)
    map)
  "The minor mode keymap which is inherited by all polymodes.")

(easy-menu-define polymode-menu polymode-minor-mode-map
  "Menu for polymode."
  '("Polymode"
    ["Next chunk" polymode-next-chunk]
    ["Previous chunk" polymode-previous-chunk]
    ["Next chunk same type" polymode-next-chunk-same-type]
    ["Previous chunk same type" polymode-previous-chunk-same-type]
    ["Mark or extend chunk" polymode-mark-or-extend-chunk]
    ["Kill chunk" polymode-kill-chunk]
    "--"
    ["Weave" polymode-weave]
    ["Set Weaver" polymode-set-weaver]
    "--"
    ["Export" polymode-export]
    ["Set Exporter" polymode-set-exporter]))


;;; NAVIGATION

(defun polymode-next-chunk (&optional N)
  "Go N chunks forwards.
Return the number of actually moved over chunks. This command is
a \"cycling\" command (see `polymode-next-chunk-same-type' for an
example)."
  (interactive "p")
  (pm-goto-span-of-type '(nil body) N)
  ;; If head/tail end before eol we move to the next line
  (when (looking-at "\\s *$")
    (forward-line 1))
  (pm--set-transient-map (list #'polymode-previous-chunk
                               #'polymode-next-chunk)))

;;fixme: problme with long chunks .. point is recentered
;;todo: merge into next-chunk
(defun polymode-previous-chunk (&optional N)
  "Go N chunks backwards.
This command is a \"cycling\" command (see
`polymode-next-chunk-same-type' for an example). Return the
number of chunks jumped over."
  (interactive "p")
  (polymode-next-chunk (- N)))

(defun polymode-next-chunk-same-type (&optional N)
  "Go to next N chunk.
Return the number of chunks of the same type moved over. This
command is a \"cycling\" command in the sense that you can repeat
the basic key without the prefix multiple times to invoke the
command multiple times."
  (interactive "p")
  (let* ((sofar 0)
         (back (< N 0))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max)))
         (N (if back (- N) N))
         (orig-pos (point))
         (pos (point))
         this-type this-name)
    (condition-case-unless-debug nil
        (pm-map-over-spans
         (lambda (span)
           (unless (memq (car span) '(head tail))
             (when (and (equal this-name
                               (eieio-object-name-string (nth 3 span)))
                        (eq this-type (car span)))
               (setq pos (nth 1 span))
               (setq sofar (1+ sofar)))
             (unless this-name
               (setq this-name (eieio-object-name-string (nth 3 span))
                     this-type (car span)))
             (when (>= sofar N)
               (signal 'quit nil))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line))))
    (goto-char pos)
    (when (or (eobp) (bobp) (eq pos orig-pos))
      (message "No more chunks of type %s" this-name)
      (ding))
    (pm--set-transient-map (list #'polymode-previous-chunk-same-type
                                 #'polymode-next-chunk-same-type))
    sofar))

(defun polymode-previous-chunk-same-type (&optional N)
  "Go to previous N chunk.
Return the number of chunks of the same type moved over."
  (interactive "p")
  (polymode-next-chunk-same-type (- N)))


;;; KILL and NARROWING

(defun pm--kill-span (types)
  (let ((span (pm-innermost-span)))
    (when (memq (car span) types)
      (delete-region (nth 1 span) (nth 2 span)))))

(defun polymode-kill-chunk ()
  "Kill current chunk."
  (interactive)
  (pcase (pm-innermost-span)
    (`(,(or `nil `host) ,beg ,end ,_) (delete-region beg end))
    (`(body ,beg ,_ ,_)
     (goto-char beg)
     (pm--kill-span '(body))
     (pm--kill-span '(head tail))
     (pm--kill-span '(head tail)))
    (`(tail ,beg ,end ,_)
     (if (eq beg (point-min))
         (delete-region beg end)
       (goto-char (1- beg))
       (polymode-kill-chunk)))
    (`(head ,_ ,end ,_)
     (goto-char end)
     (polymode-kill-chunk))
    (_ (error "Canoot find chunk to kill"))))

(defun polymode-toggle-chunk-narrowing ()
  "Toggle narrowing of the body of current chunk."
  (interactive)
  (if (buffer-narrowed-p)
      (progn (widen) (recenter))
    (pcase (pm-innermost-span)
      (`(head ,_ ,end ,_)
       (goto-char end)
       (pm-narrow-to-span))
      (`(tail ,beg ,_ ,_)
       (if (eq beg (point-min))
           (error "Invalid chunk")
         (goto-char (1- beg))
         (pm-narrow-to-span)))
      (_ (pm-narrow-to-span)))))

(defun pm-chunk-range (&optional pos)
  "Return a range (BEG . END) for a chunk at POS."
  (setq pos (or pos (point)))
  (let ((span (pm-innermost-span pos))
        (pmin (point-min))
        (pmax (point-max)))
    (cl-case (car span)
      ((nil) (pm-span-to-range span))
      (body (cons (if (= pmin (nth 1 span))
                      pmin
                    (nth 1 (pm-innermost-span (1- (nth 1 span)))))
                  (if (= pmax (nth 2 span))
                      pmax
                    (nth 2 (pm-innermost-span (nth 2 span))))))
      (head (if (= pmax (nth 2 span))
                (pm-span-to-range span)
              (pm-chunk-range (nth 2 span))))
      (tail (if (= pmin (nth 1 span))
                (pm-span-to-range span)
              (pm-chunk-range (1- (nth 1 span))))))))

(defun polymode-mark-or-extend-chunk ()
  "DWIM command to repeatedly mark chunk or extend region.
When no region is active, mark the current span if in body of a
chunk or the whole chunk if in head or tail. On repeated
invocation extend the region either forward or backward. You need
not use the prefix key on repeated invocation. For example
assuming we are in the body of the inner chunk and this command
is bound on M\\=-n M\\=-m (the default)

  [M\\=-n M\\=-m M\\=-m M\\=-m] selects body, expand selection to chunk then
                    expand selection to previous chunk

  [M\\=-n M\\=-m C\\=-x C\\=-x M\\=-m] selects body, expand selection to chunk,
                    then reverse point and mark, then extend the
                    selection to the following chunk"
  (interactive)
  (let ((span (pm-innermost-span)))
    (if (region-active-p)
        (if (< (mark) (point))
            ;; forward extension
            (if (eobp)
                (user-error "End of buffer")
              (if (eq (car span) 'head)
                  (goto-char (cdr (pm-chunk-range)))
                (goto-char (nth 2 span))
                ;; special dwim when extending from body
                (when (and (eq (car span) 'tail)
                           (not (= (point-min) (nth 1 span))))
                  (let ((body-span (pm-innermost-span (1- (nth 1 span)))))
                    (when (and (= (nth 1 body-span) (mark))
                               (not (= (nth 1 body-span) (point-min))))
                      (let ((head-span (pm-innermost-span (1- (nth 1 body-span)))))
                        (when (eq (car head-span) 'head)
                          (set-mark (nth 1 head-span)))))))))
          ;; backward extension
          (if (bobp)
              (user-error "Beginning of buffer")
            (goto-char (car (if (= (point) (nth 1 span))
                                (pm-chunk-range (1- (point)))
                              (pm-chunk-range (point)))))
            ;; special dwim when extending from body
            (when (and (eq (car span) 'body)
                       (= (nth 2 span) (mark)))
              (let ((tail-span (pm-innermost-span (nth 2 span))))
                (when (eq (car tail-span) 'tail)
                  (set-mark (nth 2 tail-span)))))))
      (let ((range (if (memq (car span) '(nil body))
                       (pm-span-to-range span)
                     (pm-chunk-range))))
        (set-mark (cdr range))
        (goto-char (car range)))))
  (let ((map (make-sparse-keymap)))
    (define-key map (vector last-command-event) #'polymode-mark-or-extend-chunk)
    (define-key map (car (where-is-internal #'exchange-point-and-mark)) #'exchange-point-and-mark)
    (let ((ev (event-basic-type last-command-event)))
      (define-key map (vector ev) #'polymode-mark-or-extend-chunk))
    (set-transient-map map (lambda () (eq this-command 'exchange-point-and-mark)))))

(defun polymode-show-process-buffer ()
  "Show the process buffer used by weaving and exporting programs."
  (interactive)
  (let ((buf (cl-loop for b being the buffers
                      if (buffer-local-value 'pm--process-buffer b)
                      return b)))
    (if buf
        (pop-to-buffer buf `(nil . ((inhibit-same-window . ,pop-up-windows))))
      (message "No polymode process buffers found."))))


;;; EVALUATION

(defvar polymode-eval-map
  (let (polymode-eval-map)
    (define-prefix-command 'polymode-eval-map)
    (define-key polymode-eval-map "v" #'polymode-eval-region-or-chunk)
    (define-key polymode-eval-map "b" #'polymode-eval-buffer)
    (define-key polymode-eval-map "u" #'polymode-eval-buffer-from-beg-to-point)
    (define-key polymode-eval-map "d" #'polymode-eval-buffer-from-point-to-end)
    (define-key polymode-eval-map (kbd "<up>") #'polymode-eval-buffer-from-beg-to-point)
    (define-key polymode-eval-map (kbd "<down>") #'polymode-eval-buffer-from-point-to-end)
    polymode-eval-map)
  "Keymap for polymode evaluation commands.")

(defvar-local polymode-eval-region-function nil
  "Function taking three arguments which does mode specific evaluation.
First two arguments are BEG and END of the region. The third
argument is the message describing the evaluation type. If the
value of this variable is non-nil in the host mode then all inner
spans are evaluated within the host buffer and values of this
variable for the inner modes are ignored.")

(defun polymode-eval-region (beg end &optional msg)
  "Eval all spans within region defined by BEG and END.
MSG is a message to be passed to `polymode-eval-region-function';
defaults to \"Eval region\"."
  (interactive "r")
  (save-excursion
    (let* ((base (pm-base-buffer))
           (host-fun (buffer-local-value 'polymode-eval-region-function base))
           (msg (or msg "Eval region"))
           evalled mapped)
      (if host-fun
          (pm-map-over-spans
           (lambda (span)
             (when (eq (car span) 'body)
               (with-current-buffer base
                 (funcall host-fun (max beg (nth 1 span)) (min end (nth 2 span)) msg))))
           beg end)
        (pm-map-over-spans
         (lambda (span)
           (when (eq (car span) 'body)
             (setq mapped t)
             (when polymode-eval-region-function
               (setq evalled t)
               (funcall polymode-eval-region-function
                        (max beg (nth 1 span))
                        (min end (nth 2 span))
                        msg))))
         beg end)
        (unless mapped
          (user-error "No inner spans in the region"))
        (unless evalled
          (user-error "None of the inner spans have `polymode-eval-region-function' defined"))))))

(defun polymode-eval-chunk (span-or-pos &optional no-error)
  "Eval the body span of the inner chunk at point.
SPAN-OR-POS is either a span or a point. When NO-ERROR is
non-nil, don't throw if `polymode-eval-region-function' is nil."
  (interactive "d")
  (let* ((span (if (number-or-marker-p span-or-pos)
                   (pm-innermost-span span-or-pos)
                 span-or-pos))
         (body-span (pcase (car span)
                      ('head (pm-innermost-span (nth 2 span)))
                      ('tail (pm-innermost-span (1- (nth 1 span))))
                      ('body span)
                      (_ (user-error "Not in an inner chunk"))))
         (base (pm-base-buffer))
         (host-fun (buffer-local-value 'polymode-eval-region-function base))
         (msg "Eval chunk"))
    (save-excursion
      (pm-set-buffer body-span)
      (if host-fun
          (with-current-buffer base
            (funcall host-fun (nth 1 body-span) (nth 2 body-span) msg))
        (if polymode-eval-region-function
            (funcall polymode-eval-region-function (nth 1 body-span) (nth 2 body-span) msg)
          (unless no-error
            (error "Undefined `polymode-eval-region-function' in buffer %s" (current-buffer))))))))

(defun polymode-eval-region-or-chunk ()
  "Eval all inner chunks in region if active, or current chunk otherwise."
  (interactive)
  (if (use-region-p)
      (polymode-eval-region (region-beginning) (region-end))
    (polymode-eval-chunk (point))))

(defun polymode-eval-buffer ()
  "Eval all inner chunks in the buffer."
  (interactive)
  (polymode-eval-region (point-min) (point-max) "Eval buffer"))

(defun polymode-eval-buffer-from-beg-to-point ()
  "Eval all inner chunks from beginning of buffer till point."
  (interactive)
  (polymode-eval-region (point-min) (point) "Eval buffer till point"))

(defun polymode-eval-buffer-from-point-to-end ()
  "Eval all inner chunks from point to the end of buffer."
  (interactive)
  (polymode-eval-region (point) (point-max) "Eval buffer till end"))


;;; DEFINE

(defun pm--config-name (symbol &optional must-exist)
  (let* ((poly-name (replace-regexp-in-string "pm-poly/\\|poly-\\|-mode\\|-polymode\\|-minor-mode" ""
                                              (symbol-name symbol)))
         (config-name
          (if (and (boundp symbol)
                   (symbol-value symbol)
                   (object-of-class-p (symbol-value symbol) 'pm-polymode))
              symbol
            (intern (concat "poly-" poly-name "-polymode")))))
    (when must-exist
      (unless (boundp config-name)
        (let ((old-config-name (intern (concat "pm-poly/" poly-name))))
          (if (boundp old-config-name)
              (setq config-name old-config-name)
            (error "No pm-polymode config object with name `%s'" config-name))))
      (unless (object-of-class-p (symbol-value config-name) 'pm-polymode)
        (error "`%s' is not a `pm-polymode' config object" config-name)))
    config-name))

(defun pm--get-keylist.keymap-from-parent (keymap parent-conf)
  (let ((keylist (copy-sequence keymap))
        (pi parent-conf)
        (parent-map))
    (while pi
      (let ((map (and (slot-boundp pi :keylist)
                      (eieio-oref pi 'keylist))))
        (when map
          (if (and (symbolp map)
                   (keymapp (symbol-value map)))
              ;; if one of the parent's :keylist is a keymap, use it as our
              ;; parent-map and stop further descent
              (setq parent-map map
                    pi nil)
            ;; list, descend to next parent and append the key list to keylist
            (setq pi (and (slot-boundp pi :parent-instance)
                          (eieio-oref pi 'parent-instance))
                  keylist (append map keylist))))))
    (when (and parent-map (symbolp parent-map))
      (setq parent-map (symbol-value parent-map)))
    (cons (reverse keylist)
          (or parent-map polymode-minor-mode-map))))

;;;###autoload
(defmacro define-polymode (mode &optional parent doc &rest body)
  "Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
which becomes t when MODE is active and nil otherwise.

MODE command can be used as both major and minor mode. Using
polymodes as minor modes makes sense when :hostmode (see below)
is not specified, in which case polymode installs only inner
modes and doesn't touch current major mode.

Standard hook MODE-hook is run at the end of the initialization
of each polymode buffer (both indirect and base buffers).

This macro also defines the MODE-map keymap from the :keymap
argument and PARENT-map (see below) and poly-[MODE-NAME]-polymode
variable which holds an object of class `pm-polymode' which holds
the entire configuration for this polymode.

PARENT is either the polymode configuration object or a polymode
mode (there is 1-to-1 correspondence between config
objects (`pm-polymode') and mode functions). The new polymode
MODE inherits alll the behavior from PARENT except for the
overwrites specified by the keywords (see below). The new MODE
runs all the hooks from the PARENT-mode and inherits its MODE-map
from PARENT-map.

DOC is an optional documentation string. If present PARENT must
be provided, but can be nil.

BODY is executed after the complete initialization of the
polymode but before MODE-hook. It is executed once for each
polymode buffer - host buffer on initialization and every inner
buffer subsequently created.

Before the BODY code keyword arguments (i.e. alternating keywords
and values) are allowed. The following special keywords
controlling the behavior of the new MODE are supported:

:lighter Optional LIGHTER is displayed in the mode line when the
   mode is on. If omitted, it defaults to the :lighter slot of
   CONFIG object.

:keymap If nil, a new MODE-map keymap is created what directly
  inherits from the PARENT's keymap. The last keymap in the
  inheritance chain is always `polymode-minor-mode-map'. If a
  keymap it is used directly as it is. If a list of binding of
  the form (KEY . BINDING) it is merged the bindings are added to
  the newly create keymap.

:after-hook A single form which is evaluated after the mode hooks
  have been run. It should not be quoted.

Other keywords are added to the `pm-polymode' configuration
object and should be valid slots in PARENT config object or the
root config `pm-polymode' object if PARENT is nil. By far the
most frequently used slots are:

:hostmode Symbol pointing to a `pm-host-chunkmode' object
  specifying the behavior of the hostmode. If missing or nil,
  MODE will behave as a minor-mode in the sense that it will
  reuse the currently installed major mode and will install only
  the inner modes.

:innermodes List of symbols pointing to `pm-inner-chunkmode'
  objects which specify the behavior of inner modes (or submodes)."
  (declare
   (indent defun)
   (doc-string 3)
   (debug (&define name
                   [&optional [&not keywordp] name]
                   [&optional stringp]
                   [&rest [keywordp sexp]]
                   def-body)))

  (let* ((last-message (make-symbol "last-message"))
         (mode-name (symbol-name mode))
         (config-name (pm--config-name mode))
         (root-name (replace-regexp-in-string "poly-\\|-mode" "" mode-name))
         (keymap-name (intern (concat mode-name "-map")))
         keymap keylist slots after-hook keyw lighter)

    (if (keywordp parent)
        (progn
          (push doc body)
          (push parent body)
          (setq doc nil
                parent nil))
      (unless (stringp doc)
        (push doc body)
        (setq doc (format "Polymode for %s." root-name))))

    (unless (symbolp parent)
      (error "PARENT must be a name of a `pm-polymode' config or a polymode mode function"))

    ;; Check keys
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:lighter (setq lighter (purecopy (pop body))))
        (:keymap (setq keymap (pop body)))
        (:after-hook (setq after-hook (pop body)))
        (:keylist (setq keylist (pop body)))
        (_ (push (pop body) slots) (push keyw slots))))


    `(progn

       ;; Define the variable to enable or disable the mode.
       (defvar-local ,mode nil ,(format "Non-nil if `%s' polymode is enabled." mode))

       (let* ((parent ',parent)
              (keymap ,keymap)
              (keylist ,keylist)
              (parent-conf-name (and parent (pm--config-name parent 'must-exist)))
              (parent-conf (and parent-conf-name (symbol-value parent-conf-name))))

         ;; define the minor-mode's keymap
         (makunbound ',keymap-name)
         (defvar ,keymap-name
           (if (keymapp keymap)
               keymap
             (let ((parent-map (unless (keymapp keymap)
                                 ;; keymap is either nil or a list
                                 (cond
                                  ;; 1. if parent is config object, merge all list
                                  ;; keymaps from parents
                                  ((eieio-object-p (symbol-value parent))
                                   (let ((klist.kmap (pm--get-keylist.keymap-from-parent
                                                      keymap (symbol-value parent))))
                                     (setq keymap (append keylist (car klist.kmap)))
                                     (cdr klist.kmap)))
                                  ;; 2. If parent is polymode function, take the
                                  ;; minor-mode from the parent config
                                  (parent
                                   (symbol-value
                                    (derived-mode-map-name
                                     (eieio-oref parent-conf '-minor-mode))))
                                  ;; 3. nil
                                  (t polymode-minor-mode-map)))))
               (easy-mmode-define-keymap keymap nil nil (list :inherit parent-map))))
           ,(format "Keymap for %s." mode-name))


         ,@(unless (eq parent config-name)
             `((makunbound ',config-name)
               (defvar ,config-name
                 (if parent-conf-name
                     (clone parent-conf
                            :name ,(symbol-name config-name)
                            '-minor-mode ',mode
                            ,@slots)
                   (pm-polymode :name ,(symbol-name config-name)
                                '-minor-mode ',mode
                                ,@slots))
                 ,(format "Configuration object for `%s' polymode." mode))))

         ;; The actual mode function:
         (defun ,mode (&optional arg)
           ,(format "%s\n\n\\{%s}"
                    ;; fixme: add inheretance info here and warning if body is
                    ;; non-nil (like in define-mirror-mode)
                    doc keymap-name)
           (interactive)
           (let ((,last-message (current-message))
                 (state (cond
                         ((numberp arg) (> arg 0))
                         (arg t)
                         ((not ,mode)))))
             (setq ,mode state)
             (if state
                 (unless (buffer-base-buffer)
                   ;; Call in host (base) buffers only.
                   (when ,mode
                     (let ((obj (clone ,config-name)))
                       ;; (eieio-oset obj '-minor-mode ',mode)
                       (pm-initialize obj))
                     ;; when host mode is reset in pm-initialize we end up with new
                     ;; minor mode in hosts
                     (setq ,mode t)))
               (let ((base (pm-base-buffer)))
                 (pm-turn-polymode-off t)
                 (switch-to-buffer base)))
             ;; `body` and `hooks` are executed in all buffers; pm/polymode has been set
             ,@body
             (when state
               (pm--run-derived-mode-hooks)
               ,@(when after-hook `(,after-hook)))
             (unless (buffer-base-buffer)
               ;; Avoid overwriting a message shown by the body,
               ;; but do overwrite previous messages.
               (when (and (called-interactively-p 'any)
                          (or (null (current-message))
                              (not (equal ,last-message
                                          (current-message)))))
                 (message ,(concat root-name " polymode %s")
                          (if state "enabled" "disabled"))))
             (force-mode-line-update))
           ;; Return the new state
           ,mode)

         (add-minor-mode ',mode ,(or lighter " PM") ,keymap-name)))))

(define-minor-mode polymode-minor-mode
  "Polymode minor mode, used to make everything work."
  :lighter " PM")

(define-derived-mode poly-head-tail-mode prog-mode "HeadTail"
  "Default major mode for polymode head and tail spans."
  (let ((base (pm-base-buffer)))
    ;; (#119) hideshow needs comment regexp and throws if not found. We are
    ;; using these values from the host mode which should have been installed
    ;; already.
    (setq-local comment-start (buffer-local-value 'comment-start base))
    (setq-local comment-end (buffer-local-value 'comment-end base))))

(define-derived-mode poly-fallback-mode prog-mode "FallBack"
  ;; fixme:
  ;; 1. doesn't work as fallback for hostmode
  ;; 2. highlighting is lost (Rnw with inner fallback)
  "Default major mode for modes which were not found.
This is better than fundamental-mode because it allows running
globalized minor modes and can run user hooks.")

;; indulge elisp font-lock (FIXME: check if this is needed; why host/inner defs work?)
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\(define-polymode\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(provide 'polymode)
;;; polymode.el ends here
