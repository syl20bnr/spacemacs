;;; poly-lock.el --- Font lock sub-system for polymode -*- lexical-binding: t -*-
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Code:

;; FONT-LOCK COMPONENTS:
;;
;; All * functions are lazy in poly-lock and jit-lock because they just mark
;; 'fontified nil.
;;
;;  fontification-functions ->                                           jit-lock-function  / poly-lock-function
;;  font-lock-ensure ->           font-lock-ensure-function ->           jit-lock-fontify-now/poly-lock-fontify-now
;; *font-lock-flush  ->           font-lock-flush-function  ->           jit-lock-refontify / poly-lock-flush
;; *font-lock-fontify-buffer ->   font-lock-fontify-buffer-function ->   jit-lock-refontify / poly-lock-flush
;;  font-lock-fontify-region ->   font-lock-fontify-region-function ->   font-lock-default-fontify-region
;;  font-lock-unfontify-region -> font-lock-unfontify-region-function -> font-lock-default-unfontify-region
;;  font-lock-unfontify-buffer -> font-lock-unfontify-buffer-function -> font-lock-default-unfontify-buffer
;;
;; Jit-lock components:
;; fontification-functions (called by display engine)
;;   --> jit-lock-function
;;     --> jit-lock-fontify-now (or deferred through timer/text-properties)
;;       --> jit-lock--run-functions
;;         --> jit-lock-functions (font-lock-fontify-region bug-reference-fontify etc.)
;;
;;
;; Poly-lock components:
;; fontification-functions
;;   --> poly-lock-function
;;    --> poly-lock-fontify-now
;;      --> jit-lock-fontify-now
;;      ...
;;
;; `font-lock-mode' call graph:
;; -> font-lock-function <---- replaced by `poly-lock-mode'
;;   -> font-lock-default-function
;;     -> font-lock-mode-internal
;;        -> font-lock-turn-on-thing-lock
;;           -> font-lock-turn-on-thing-lock
;;             -> (setq font-lock-flush-function jit-lock-refontify)
;;             -> (setq font-lock-ensure-function jit-lock-fontify-now)
;;             -> (setq font-lock-fontify-buffer-function jit-lock-refontify)
;;             -> (jit-lock-register #'font-lock-fontify-region)
;;               -> (add-hook 'jit-lock-functions #'font-lock-fontify-region nil t)
;;               -> jit-lock-mode

(require 'jit-lock)
(require 'polymode-core)

(defvar poly-lock-allow-fontification t)
(defvar poly-lock-allow-background-adjustment t)
(defvar poly-lock-fontification-in-progress nil)
(defvar poly-lock-defer-after-change t)
(defvar-local poly-lock-mode nil)

(eval-and-compile
  (defmacro with-buffer-prepared-for-poly-lock (&rest body)
    "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
    (declare (debug (body)) (indent 1))
    `(let ((inhibit-point-motion-hooks t))
       (with-silent-modifications
         ,@body))))

;; FIXME: Can this hack be avoided if poly-lock is registered in
;; `font-lock-support-mode'?
(defun poly-lock-no-jit-lock-in-polymode-buffers (fun arg)
  "Don't activate FUN in `polymode' buffers.
When not in polymode buffers apply FUN to ARG."
  (unless polymode-mode
    (funcall fun arg)))
(pm-around-advice 'jit-lock-mode #'poly-lock-no-jit-lock-in-polymode-buffers)
;; see the comment in pm--mode-setup for these
(pm-around-advice 'font-lock-fontify-region #'polymode-inhibit-during-initialization)
(pm-around-advice 'font-lock-fontify-buffer #'polymode-inhibit-during-initialization)
(pm-around-advice 'font-lock-ensure #'polymode-inhibit-during-initialization)

(defun poly-lock-mode (arg)
  "This is the value of `font-lock-function' in all polymode buffers.
Mode activated when ARG is positive; happens when font-lock is
switched on."
  (unless polymode-mode
    (error "Calling `poly-lock-mode' in a non-polymode buffer (%s)" (current-buffer)))

  (setq poly-lock-mode arg)

  (if arg
      (progn
        ;; a lot of the following is inspired by what jit-lock does in
        ;; `font-lock-turn-on-thing-lock'

        (setq-local font-lock-support-mode 'poly-lock-mode)
        (setq-local font-lock-dont-widen t)

        ;; Re-use jit-lock registration. Some minor modes (adaptive-wrap)
        ;; register extra functionality. [Unfortunately `jit-lock-register'
        ;; calls `jit-lock-mode' which we don't want. Hence the advice. TOTHINK:
        ;; Simply add-hook to `jit-lock-functions'?]
        (jit-lock-register #'font-lock-fontify-region)

        ;; don't allow other functions
        (setq-local fontification-functions '(poly-lock-function))

        (setq-local font-lock-flush-function #'poly-lock-flush)
        (setq-local font-lock-fontify-buffer-function #'poly-lock-flush)
        (setq-local font-lock-ensure-function #'poly-lock-fontify-now)

        ;; There are some more, jit-lock doesn't change those, neither do we:
        ;; font-lock-unfontify-region-function (defaults to font-lock-default-unfontify-region)
        ;; font-lock-unfontify-buffer-function (defualts to font-lock-default-unfontify-buffer)

        ;; Don't fontify eagerly (and don't abort if the buffer is large). NB:
        ;; `font-lock-flush' is not triggered if this is nil.
        (setq-local font-lock-fontified t)

        ;; Now we can finally call `font-lock-default-function' because
        ;; `font-lock-support-mode' is set to "unrecognizible" value, only core
        ;; font-lock setup happens.
        (font-lock-default-function arg)

        ;; Must happen after call to `font-lock-default-function'
        (remove-hook 'after-change-functions #'font-lock-after-change-function t)
        (remove-hook 'after-change-functions #'jit-lock-after-change t)
        (add-hook 'after-change-functions #'poly-lock-after-change nil t)

        ;; Reusing jit-lock var becuase modes populate it directly. We are using
        ;; this in `poly-lock-after-change' below. Taken from `jit-lock
        ;; initialization.
        (add-hook 'jit-lock-after-change-extend-region-functions
                  #'font-lock-extend-jit-lock-region-after-change
                  nil t))

    (remove-hook 'after-change-functions #'poly-lock-after-change t)
    (remove-hook 'fontification-functions #'poly-lock-function t))
  (current-buffer))

(defvar poly-lock-chunk-size 2500
  "Poly-lock fontifies chunks of at most this many characters at a time.")

(defun poly-lock-function (start)
  "The only function in `fontification-functions' in polymode buffers.
This is the entry point called by the display engine. START is
defined in `fontification-functions'. This function has the same
scope as `jit-lock-function'."
  (unless pm-initialization-in-progress
    (if (and poly-lock-mode (not memory-full))
        (unless (input-pending-p)
          (let ((end (min (or (text-property-any start (point-max) 'fontified t)
                              (point-max))
                          (+ start poly-lock-chunk-size))))
            (when (< start end)
              (poly-lock-fontify-now start end))))
      (with-buffer-prepared-for-poly-lock
       (put-text-property start (point-max) 'fontified t)))))

(defun poly-lock-fontify-now (&optional beg end _verbose)
  "Polymode main fontification function.
Fontifies chunk-by chunk within the region BEG END."
  (unless (or poly-lock-fontification-in-progress
              pm-initialization-in-progress)
    (setq beg (or beg (point-min))
          end (or end (point-max)))
    (let* ((font-lock-dont-widen t)
           ;; For now we fontify entire chunks at once. This simplicity is
           ;; warranted in multi-mode use cases.
           (font-lock-extend-region-functions nil)
           ;; Fontification in one buffer can trigger fontification in another
           ;; buffer. Particularly, this happens when new indirect buffers are
           ;; created and `normal-mode' triggers font-lock in those buffers. We
           ;; avoid this by dynamically binding
           ;; `poly-lock-fontification-in-progress' and un-setting
           ;; `fontification-functions' in case re-display suddenly decides to
           ;; fontify something else in other buffer. There are also font-lock
           ;; guards in pm--mode-setup.
           (poly-lock-fontification-in-progress t)
           (fontification-functions nil)
           (protect-host (or
                          (with-current-buffer (pm-base-buffer)
                            (eieio-oref pm/chunkmode 'protect-font-lock))
                          ;; HACK: Some inner modes use syntax-table text
                          ;; property. If there is, for example, a comment
                          ;; syntax somewhere in the body span, havoc is spelled
                          ;; in font-lock-fontify-syntactically-region which
                          ;; calls parse-partial-sexp. For example fortran block
                          ;; in ../poly-markdown/tests/input/markdown.md. We do
                          ;; our best and protect the host in such cases.
                          (/= (next-single-property-change beg 'syntax-table nil end)
                              end))))
      (save-restriction
        (widen)
        (save-excursion

          ;; TEMPORARY HACK: extend to the next span boundary in code blocks
          ;; (needed because re-display fontifies by small regions)
          (let ((end-span (pm-innermost-span end)))
            (if (car end-span)
                (when (< (nth 1 end-span) end)
                  (setq end (nth 2 end-span)))
              ;; in host extend to paragraphs as in poly-lock--extend-region
              (goto-char end)
              (when (search-forward "\n\n" nil t)
                (setq end (min (1- (point)) (nth 2 end-span))))))

          ;; Fontify the whole region in host first. It's ok for modes like
          ;; markdown, org and slim which understand inner mode chunks.
          (unless protect-host
            (let ((span (pm-innermost-span beg)))
              (when (or (null (pm-true-span-type span))
                        ;; in inner spans fontify only if region is bigger than the span
                        (< (nth 2 span) end))
                (with-current-buffer (pm-base-buffer)
                  (with-buffer-prepared-for-poly-lock
                   (when poly-lock-allow-fontification
                     (put-text-property beg end 'fontified nil) ; just in case
                     ;; (message "jlrf-host:%d-%d %s" beg end major-mode)
                     (condition-case-unless-debug err
                         ;; NB: Some modes fontify beyond the limits (org-mode).
                         ;; We need a reliably way to detect the actual limit of
                         ;; the fontification.
                         (save-restriction
                           (widen)
                           (jit-lock--run-functions beg end))
                       (error
                        (message "(jit-lock--run-functions %s %s) [UNPR HOST %s]: %s"
                                 beg end (current-buffer) (error-message-string err)))))
                   (put-text-property beg end 'fontified t))))))
          (pm-map-over-spans
           (lambda (span)
             (when (or (pm-true-span-type span)
                       protect-host)
               (let ((sbeg (nth 1 span))
                     (send (nth 2 span)))
                 ;; skip empty spans
                 (with-buffer-prepared-for-poly-lock
                  (when (> send sbeg)
                    (if (not (and poly-lock-allow-fontification
                                  poly-lock-mode))
                        (put-text-property sbeg send 'fontified t)
                      (let ((new-beg (max sbeg beg))
                            (new-end (min send end)))
                        (put-text-property new-beg new-end 'fontified nil)
                        ;; (message "jlrf:%d-%d %s" new-beg new-end major-mode)
                        (condition-case-unless-debug err
                            (if (eieio-oref pm/chunkmode 'protect-font-lock)
                                (pm-with-narrowed-to-span span
                                  (jit-lock--run-functions new-beg new-end))
                              (jit-lock--run-functions new-beg new-end))
                          (error
                           (message "(jit-lock--run-functions %s %s) [span %d %d %s] -> (font-lock-default-fontify-region %s %s): %s"
                                    new-beg new-end sbeg send (current-buffer) new-beg new-end
                                    (error-message-string err))))
                        ;; even if failed set to t
                        (put-text-property new-beg new-end 'fontified t)))
                    (when poly-lock-allow-background-adjustment
                      (poly-lock-adjust-span-face span)))))))
           beg end))))
    (current-buffer)))

(defun poly-lock-flush (&optional beg end)
  "Force refontification of the region BEG..END.
This function is placed in `font-lock-flush-function'."
  (unless poly-lock-fontification-in-progress
    (let ((beg (or beg (point-min)))
          (end (or end (point-max))))
      (with-buffer-prepared-for-poly-lock
       (save-restriction
         (widen)
         (pm-flush-span-cache beg end)
         (put-text-property beg end 'fontified nil))))))

(defvar jit-lock-start)
(defvar jit-lock-end)
(defun poly-lock--extend-region (beg end)
  "Our own extension function which runs first on BEG END change.
Assumes widen buffer. Sets `jit-lock-start' and `jit-lock-end'."
  ;; NB: Debug this like
  ;; (with-silent-modifications (insert "`") (poly-lock-after-change 65 66 0))

  ;; FIXME: this one extends to whole spans; not good. old span can disappear,
  ;; shrunk, extend etc

  ;; TOCHECK: Pretty surely we need not use 'no-cache here.

  ;; With differed after change, any function calling pm-innermost-span (mostly
  ;; syntax-propertize) will reset the spans, so the extension relying on
  ;; :pm-span cache will not detect the change. Use instead the especially setup
  ;; for this purpose :pm-span-old cache in poly-lock-after-change.
  (let* ((prop-name (if poly-lock-defer-after-change :pm-span-old :pm-span))
         (old-beg (or (previous-single-property-change beg prop-name)
                      (point-min)))
         (old-end (or (next-single-property-change end prop-name)
                      (point-max)))
         ;; need this here before pm-innermost-span call
         (old-beg-obj (nth 3 (get-text-property old-beg prop-name)))
         (beg-span (pm-innermost-span beg 'no-cache))
         (end-span (if (<= end (nth 2 beg-span))
                       beg-span
                     (pm-innermost-span end 'no-cache)))
         (sbeg (nth 1 beg-span))
         (send (nth 2 end-span)))

    (if (< old-beg sbeg)
        (let ((new-beg-span (pm-innermost-span old-beg)))
          (if (eq old-beg-obj (nth 3 new-beg-span)) ; old-beg == (nth 1 new-beg-span) for sure
              ;; new span appeared within an old span, don't refontify the old part (common case)
              (setq jit-lock-start (min sbeg (nth 2 new-beg-span)))
            ;; wrong span shrunk to its correct size (rare or never)
            (setq jit-lock-start old-beg)))
      ;; refontify the entire new span
      (setq jit-lock-start sbeg))

    ;; (dbg (pm-format-span beg-span))
    ;; always include head
    (when (and (eq (car beg-span) 'tail)
               (> jit-lock-start (point-min)))
      (setq jit-lock-start (nth 1 (pm-innermost-span (1- jit-lock-start)))))
    (when (and (eq (car beg-span) 'body)
               (> jit-lock-start (point-min)))
      (setq jit-lock-start (nth 1 (pm-innermost-span (1- jit-lock-start)))))

    ;; I think it's not possible to do better than this. When region is shrunk,
    ;; previous region could be incorrectly fontified even if the mode is
    ;; preserved due to wrong ppss
    (setq jit-lock-end (max send old-end))

    ;; Check if the type of following span changed (for example when
    ;; modification is in head of an auto-chunk). Do this repeatedly till no
    ;; change. [TOTHINK: Do we need similar extension backwards?]
    (let ((go-on t))
      (while (and (< jit-lock-end (point-max))
                  go-on)
        (let ((ospan (get-text-property jit-lock-end prop-name))
              (nspan (pm-innermost-span jit-lock-end 'no-cache)))
          ;; (dbg "N" (pm-format-span nspan))
          ;; (dbg "O" (pm-format-span ospan))
          ;; if spans have just been moved by buffer modification, stop
          (if ospan
              (if (and (eq (nth 3 nspan) (nth 3 ospan))
                       (= (- (nth 2 nspan) (nth 1 nspan))
                          (- (nth 2 ospan) (nth 1 ospan))))
                  (setq go-on nil)
                (setq jit-lock-end (nth 2 nspan)
                      end-span nspan))
            (setq go-on nil
                  jit-lock-end (point-max))))))

    ;; This extension is needed because some host modes (org) either don't
    ;; fontify the head correctly when tail is not there or worse, fontify
    ;; larger spans than asked for. It's mostly for unprotected hosts, but
    ;; doing it here for all cases to err on the safe side.

    ;; always include body of the head
    (when (and (eq (car end-span) 'head)
               (< jit-lock-end (point-max)))
      (setq end-span (pm-innermost-span jit-lock-end)
            jit-lock-end (nth 2 end-span)))

    ;; always include tail
    (when (and (eq (car end-span) 'body)
               (< jit-lock-end (point-max)))
      (setq jit-lock-end (nth 2 (pm-innermost-span jit-lock-end))
            end-span (pm-innermost-span jit-lock-end)))

    ;; Temporary hack for large host mode chunks - narrow to empty lines
    (when (> (* 2 poly-lock-chunk-size)
             (- jit-lock-end jit-lock-start))

      (when (eq (car beg-span) nil)
        (let ((tbeg (min beg (nth 2 beg-span))))
          (when (> (- tbeg jit-lock-start) poly-lock-chunk-size)
            (goto-char (- tbeg poly-lock-chunk-size))
            (when (search-backward "\n\n" nil t)
              (setq jit-lock-start (max jit-lock-start (1+ (point))))))))

      (when (eq (car end-span) nil)
        (let ((tend (max end (nth 1 end-span))))
          (when (> (- jit-lock-end tend) poly-lock-chunk-size)
            (goto-char (+ tend poly-lock-chunk-size))
            (when (search-forward "\n\n" nil t)
              (setq jit-lock-end (min jit-lock-end (1- (point)))))))))

    (cons jit-lock-start jit-lock-end)))

;; (defun poly-lock--jit-lock-extend-region-span (span old-len)
;;   "Call `jit-lock-after-change-extend-region-functions' protected to SPAN.
;; Extend `jit-lock-start' and `jit-lock-end' by side effect.
;; OLD-LEN is passed to the extension function."
;;   ;; FIXME: for multi-span regions this function seems to reset
;;   ;; jit-lock-start/end to spans limits
;;   (let ((beg jit-lock-start)
;;         (end jit-lock-end))
;;     (let ((sbeg (nth 1 span))
;;           (send (nth 2 span)))
;;       (when (or (> beg sbeg) (< end send))
;;         (pm-with-narrowed-to-span span
;;           (setq jit-lock-start (max beg sbeg)
;;                 jit-lock-end   (min end send))
;;           (condition-case err
;;               (progn
;;                 ;; set jit-lock-start and jit-lock-end by side effect
;;                 (run-hook-with-args 'jit-lock-after-change-extend-region-functions
;;                                     jit-lock-start jit-lock-end old-len))
;;             (error (message "(after-change-extend-region-functions %s %s %s) -> %s"
;;                             jit-lock-start jit-lock-end old-len
;;                             (error-message-string err))))
;;           ;; FIXME: this is not in the right buffer, we need to do it in the
;;           ;; original buffer.
;;           (setq jit-lock-start (min beg (max jit-lock-start sbeg))
;;                 jit-lock-end (max end (min jit-lock-end send))))
;;         (cons jit-lock-start jit-lock-end)))))

(defvar-local poly-lock--timer nil)
(defvar-local poly-lock--beg-change most-positive-fixnum)
(defvar-local poly-lock--end-change most-negative-fixnum)
(defun poly-lock--after-change-internal (buffer _old-len)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq poly-lock--timer nil)
      ;; FIXME: timers can overlap; remove this check with global timer
      (when (> poly-lock--end-change 0)
        (with-buffer-prepared-for-poly-lock
         (save-excursion
           (save-restriction
             (widen)
             (let ((beg poly-lock--beg-change)
                   (end (min (point-max) poly-lock--end-change)))
               (setq poly-lock--beg-change most-positive-fixnum
                     poly-lock--end-change most-negative-fixnum)
               (save-match-data
                 (poly-lock--extend-region beg end)
                 ;; no need for 'no-cache; poly-lock--extend-region re-computed the spans

                 ;; FIXME: currently poly-lock--extend-region extends to whole
                 ;; spans, which could get crazy for very large chunks, but
                 ;; seems to work really well with the deferred after-change
                 ;; hook. So the following jit-lock extensions are not needed
                 ;; and probably even harm.

                 ;; This extension hooks are run for major-mode's syntactic
                 ;; hacks mostly and not that much for actual extension. For
                 ;; example, markdown can syntactically propertize in this hook
                 ;; markdown-font-lock-extend-region-function. Call on the
                 ;; entire region host hooks to account for such patterns.
                 ;; (let ((hostmode (oref pm/polymode -hostmode)))
                 ;;   (unless (eieio-oref hostmode 'protect-font-lock)
                 ;;     (with-current-buffer (pm-base-buffer)
                 ;;       (run-hook-with-args 'jit-lock-after-change-extend-region-functions
                 ;;                           beg end old-len)
                 ;;       (setq beg jit-lock-start
                 ;;             end jit-lock-end)))
                 ;;   (let ((bspan (pm-innermost-span jit-lock-start)))
                 ;;     ;; FIXME: these are currently always protected and set
                 ;;     ;; jit-lock-end/start in their own buffers, not the buffer
                 ;;     ;; which invoked the after-change-hook
                 ;;     (unless (eq (nth 3 bspan) hostmode)
                 ;;       (poly-lock--jit-lock-extend-region-span bspan old-len))
                 ;;     (when (< (nth 2 bspan) jit-lock-end)
                 ;;       (let ((espan (pm-innermost-span jit-lock-end)))
                 ;;         (unless (eq (nth 3 espan) hostmode)
                 ;;           (poly-lock--jit-lock-extend-region-span espan old-len)))))
                 ;;   )

                 ;; ;; Why is this still needed? poly-lock--extend-region re-computes the spans
                 ;; (pm-flush-span-cache jit-lock-start jit-lock-end)
                 ;; (dbg (cb) jit-lock-start jit-lock-end)
                 ;; (put-text-property jit-lock-end jit-lock-end :poly-lock-refontify nil)
                 (put-text-property jit-lock-start jit-lock-end 'fontified nil))))))))))

(defun poly-lock-after-change (beg end old-len)
  "Mark changed region with `fontified' nil.
Extend the region to spans which need to be updated. BEG, END and
OLD-LEN are as in `after-change-functions'. When
`poly-lock-defer-after-change' is non-nil (the default), run fontification"
  (when (and poly-lock-mode
             pm-allow-after-change-hook
             (not memory-full))
    ;; Extension is slow but after-change functions can be called in rapid
    ;; succession (#200 with string-rectangle on which combine-change-calls is
    ;; of little help). Thus we do that in a timer.
    (when (timerp poly-lock--timer)
      ;; FIXME: Instead of local timer, make a global one iterating over
      ;; relevant buffers
      (cancel-timer poly-lock--timer))
    (if poly-lock-defer-after-change
        (progn
          (with-silent-modifications
            ;; don't re-fontify before we extend
            (put-text-property beg end 'fontified t)
            (setq poly-lock--beg-change (min beg end poly-lock--beg-change)
                  poly-lock--end-change (max beg end poly-lock--end-change))
            ;; between this call and deferred extension pm-inner-span can be
            ;; called, so we cache a few :pm-span properties around beg/end
            (poly-lock--cache-pm-span-property beg end))
          (setq-local poly-lock--timer
                      (run-at-time 0.05 nil #'poly-lock--after-change-internal
                                   (current-buffer) old-len)))
      (setq poly-lock--beg-change beg
            poly-lock--end-change end)
      (poly-lock--after-change-internal (current-buffer) old-len))))

(defun poly-lock--cache-pm-span-property (beg end)
  ;; cache one previous and 5 forward spans
  (let ((new-beg (or (previous-single-property-change beg :pm-span)
                     (point-min))))
    (put-text-property new-beg beg :pm-span-old (get-text-property new-beg :pm-span)))
  (let ((i 5))
    (while (and (< 0 i) (< end (point-max)))
      (let ((new-end (or (next-single-property-change end :pm-span)
                         (point-max))))
        (put-text-property new-end end :pm-span-old (get-text-property (1- new-end) :pm-span))
        (setq end new-end
              i (1- i))))))

(defun poly-lock--adjusted-background (prop)
  ;; if > lighten on dark backgroun. Oposite on light.
  (color-lighten-name (face-background 'default)
                      (if (eq (frame-parameter nil 'background-mode) 'light)
                          (- prop) ;; darken
                        prop)))

(declare-function pm-get-adjust-face "polymode-methods")
(defvar poly-lock--extra-span-props (when (fboundp 'set-face-extend) (list :extend t)))
(defun poly-lock-adjust-span-face (span)
  "Adjust `face' property of SPAN..
How adjustment is made is defined in :adjust-face slot of the
SPAN's chunkmode."
  (interactive "r")
  (let ((face (pm-get-adjust-face (nth 3 span) (car span))))
    (let ((face (if (numberp face)
                    (unless (= face 0)
                      (list (append (list :background (poly-lock--adjusted-background face))
                                    poly-lock--extra-span-props)))
                  face)))
      (when face
        (font-lock-append-text-property
         (nth 1 span) (nth 2 span) 'face face)))))

(provide 'poly-lock)
;;; poly-lock.el ends here
