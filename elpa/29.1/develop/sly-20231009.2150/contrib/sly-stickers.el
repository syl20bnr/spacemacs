;;; sly-stickers.el --- Live-code annotations for SLY  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, languages, lisp, tools

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
;;;
;;; There is much in this library that would merit comment. Just some points:
;;;
;;; * Stickers are just overlays that exist on the Emacs side. A lot
;;;   of the code is managing overlay nesting levels so that faces
;;;   are chosen suitably for making sticker inside stickers
;;;   visually recognizable.
;;;
;;;   The main entry-point here is the interactive command
;;;   `sly-sticker-dwim', which places and removes stickers.
;;;
;;;   Stickers are also indexed by an integer and placed in a
;;;   connection-global hash-table, `sly-stickers--stickers'.  It can
;;;   be connection-global because the same sticker with the same id
;;;   might eventually be sent, multiple times, to many
;;;   connections. It's the Slynk side that has to be able to tell
;;;   whence the stickers comes from (this is not done currently).
;;;
;;; * The gist of stickers is instrumenting top-level forms. This is
;;;   done by hooking onto `sly-compile-region-function'. Two separate
;;;   compilations are performed: one for the uninstrumented form and
;;;   another for the intrumented form. This is so that warnings and
;;;   compilations errors that are due to stickers exclusively can be
;;;   sorted out. If the second compilation fails, the stickers dont
;;;   "stick", i.e. they are not armed.
;;;
;;; * File compilation is also hooked onto via
;;;   `sly-compilation-finished-hook'. The idea here is to first
;;;   compile the whole file, then traverse any top-level forms that
;;;   contain stickers and instrument those.
;;;
;;; * On the emacs-side, the sticker overlays are very ephemeral
;;;   objects. They are not persistently saved in any way. Deleting or
;;;   modifying text inside them automatically deletes them.
;;;
;;;   The slynk side eventually must be told to let go of deleted
;;;   stickers. Before this happens these stickers are known as
;;;   zombies.  Reaping happens on almost every SLY -> Slynk call.
;;;   Killing the buffer they live in doesn't automatically delete
;;;   them, but reaping eventually happens anyway via
;;;   `sly-stickers--sticker-by-id'.
;;;
;;;   Before a zombie sticker is reaped, some code may still be
;;;   running that adds recordings to these stickers, and some of
;;;   these recordings make it to the Emacs side. The user can ignore
;;;   them in `sly-stickers-replay', being notified that a deleted
;;;   sticker is being referenced.
;;;
;;;   This need to communicate dead stickers to Slynk is only here
;;;   because using weak-hash-tables is impractical for stickers
;;;   indexed by integers. Perhaps this could be fixed if the
;;;   instrumented forms could reference sticker objects directly.
;;;
;;; * To see the results of sticker-instrumented code, there are the
;;;   interactive commands `sly-stickers-replay' and
;;;   `sly-stickers-fetch'. If "breaking stickers" is enabled, the
;;;   debugger is also invoked before a sticker is reached and after a
;;;   sticker returns (if it returns). Auxiliary data-structures like
;;;   `sly-stickers--recording' are used here.
;;;
;;; * `sly-stickers--replay-state' and `sly-stickers--replay-map' are
;;;   great big hacks just for handling the `sly-stickers-replay'
;;;   interactive loop. Should look into recursive minibuffers or
;;;   something more akin to `ediff', for example.
;;;
;;; Code:


(require 'sly)
(require 'sly-parse "lib/sly-parse")
(require 'sly-buttons "lib/sly-buttons")

(eval-when-compile
  (when (version< emacs-version "26")
      ;; Using `cl-defstruct' needs `cl' on older Emacsen. See issue
      ;; https://github.com/joaotavora/sly/issues/54
    (require 'cl)))

(require 'cl-lib)
(require 'hi-lock) ; for the faces
(require 'color)
(require 'pulse) ; pulse-momentary-highlight-overlay

(define-sly-contrib sly-stickers
  "Mark expressions in source buffers and annotate return values."
  (:authors "João Távora <joaotavora@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk/stickers)
  (:on-load (add-hook 'sly-editing-mode-hook 'sly-stickers-mode)
            (add-hook 'sly-mode-hook 'sly-stickers-shortcut-mode)
            (setq sly-compile-region-function
                  'sly-stickers-compile-region-aware-of-stickers)
            (add-hook 'sly-compilation-finished-hook
                      'sly-stickers-after-buffer-compilation t)
            (add-hook 'sly-db-extras-hooks 'sly-stickers--handle-break))
  (:on-unload (remove-hook 'sly-editing-mode-hook 'sly-stickers-mode)
              (remove-hook 'sly-mode-hook 'sly-stickers-shortcut-mode)
              (setq sly-compile-region-function 'sly-compile-region-as-string)
              (remove-hook 'sly-compilation-finished-hook
                           'sly-stickers-after-buffer-compilation)
              (remove-hook 'sly-db-extras-hooks 'sly-stickers--handle-break)))



;;;; Bookeeping for local stickers
;;;;
(defvar sly-stickers--counter 0)

(defvar sly-stickers--stickers (make-hash-table))

(defvar sly-stickers--zombie-sticker-ids nil
  "Sticker ids that might exist in Slynk but no longer in Emacs.")

(defun sly-stickers--zombies () sly-stickers--zombie-sticker-ids)

(defun sly-stickers--reset-zombies () (setq sly-stickers--zombie-sticker-ids nil))



;;;; Sticker display and UI logic
;;;;
(defgroup sly-stickers nil
  "Mark expressions in source buffers and annotate return values."
  :prefix "sly-stickers-"
  :group 'sly)

(when nil
  (cl-loop for sym in '(sly-stickers-placed-face
                        sly-stickers-armed-face
                        sly-stickers-empty-face
                        sly-stickers-recordings-face
                        sly-stickers-exited-non-locally-face)
           do
           (put sym 'face-defface-spec nil)))

(defface sly-stickers-placed-face
  '((((background dark)) (:background "light grey" :foreground "black"))
    (t (:background "light grey")))
  "Face for sticker just set")

(defface sly-stickers-armed-face
  '((t (:strike-through nil :inherit hi-blue)))
  "Face for stickers that have been armed")

(defface sly-stickers-recordings-face
  '((t (:strike-through nil :inherit hi-green)))
  "Face for stickers that have new recordings")

(defface sly-stickers-empty-face
  '((t (:strike-through nil :inherit hi-pink)))
  "Face for stickers that have no recordings.")

(defface sly-stickers-exited-non-locally-face
  '((t (:strike-through t :inherit sly-stickers-empty-face)))
  "Face for stickers that have exited non-locally.")

(defvar sly-stickers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s C-s") 'sly-stickers-dwim)
    (define-key map (kbd "C-c C-s C-d") 'sly-stickers-clear-defun-stickers)
    (define-key map (kbd "C-c C-s C-k") 'sly-stickers-clear-buffer-stickers)
    map))

(defvar sly-stickers-shortcut-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s S") 'sly-stickers-fetch)
    (define-key map (kbd "C-c C-s F") 'sly-stickers-forget)
    (define-key map (kbd "C-c C-s C-r") 'sly-stickers-replay)
    map))

(define-minor-mode sly-stickers-mode
  "Mark expression in source buffers and annotate return values.")

(define-minor-mode sly-stickers-shortcut-mode
  "Shortcuts for navigating sticker recordings.")

(defvar sly-stickers--sticker-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") 'sly-mrepl-copy-part-to-repl)
    (define-key map [down-mouse-3] 'sly-button-popup-part-menu)
    (define-key map [mouse-3] 'sly-button-popup-part-menu)
    map))

(define-button-type 'sly-stickers-sticker :supertype 'sly-part
  'sly-button-inspect 'sly-stickers--inspect-recording
  'sly-button-echo 'sly-stickers--echo-sticker
  'keymap sly-stickers--sticker-map)

(defun sly-stickers--set-tooltip (sticker &optional info)
  (let* ((help-base (button-get sticker 'sly-stickers--base-help-echo))
         (text (if info
                   (concat "[sly] Sticker:" info "\n" help-base)
                 help-base)))
    (button-put sticker 'help-echo text)
    (button-put sticker 'sly-stickers--info info)))

(defun sly-stickers--echo-sticker (sticker &rest more)
  (cl-assert (null more) "Apparently two stickers at exact same location")
  (sly-message (button-get sticker 'sly-stickers--info))
  (sly-button-flash sticker))

(defcustom sly-stickers-max-nested-stickers 4
  "The maximum expected level expected of sticker nesting.
If you nest more than this number of stickers inside other
stickers, the overlay face will be very dark, and probably
render the underlying text unreadable."
  :type :integer)

(defvar sly-stickers-color-face-attribute :background
  "Color-capable attribute of sticker faces that represents nesting.")

(gv-define-setter sly-stickers--level (level sticker)
  `(prog1
       (setf (sly-button--level ,sticker) ,level)
     (when (button-get ,sticker 'sly-stickers--base-face)
       (sly-stickers--set-face ,sticker))))

(defun sly-stickers--level (sticker) (sly-button--level sticker))

(defun sly-stickers--guess-face-color (face)
  (face-attribute-specified-or
   (face-attribute face sly-stickers-color-face-attribute nil t)
   nil))

(defun sly-stickers--set-face (sticker &optional face)
  (let* ((face (or face
                   (button-get sticker 'sly-stickers--base-face)))
         (guessed-color (sly-stickers--guess-face-color face)))
    (button-put sticker 'sly-stickers--base-face face)
    (unless guessed-color
      (sly-error "sorry, can't guess color for face %s for sticker %s"))
    (button-put sticker 'face
                `(:inherit ,face
                           ,sly-stickers-color-face-attribute
                           ,(color-darken-name
                             guessed-color
                             (* 25
                                (/ (sly-stickers--level sticker)
                                   sly-stickers-max-nested-stickers
                                   1.0)))))))

(defun sly-stickers--stickers-in (beg end)
  (sly-button--overlays-in beg end 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-at (pos)
  (sly-button--overlays-at pos 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-between (beg end)
  (sly-button--overlays-between beg end 'sly-stickers--sticker-id))
(defun sly-stickers--stickers-exactly-at (beg end)
  (sly-button--overlays-exactly-at beg end 'sly-stickers--sticker-id))


(defun sly-stickers--sticker (from to)
  "Place a new sticker from FROM to TO"
  (let* ((intersecting (sly-stickers--stickers-in from to))
         (contained (sly-stickers--stickers-between from to))
         (not-contained (cl-set-difference intersecting contained))
         (containers nil))
    (unless (cl-every #'(lambda (e)
                          (and (< (button-start e) from)
                               (> (button-end e) to)))
                      not-contained)
      (sly-error "Cannot place a sticker that partially overlaps other stickers"))
    (when (sly-stickers--stickers-exactly-at from to)
      (sly-error "There is already a sticker at those very coordinates"))
    ;; by now we know that other intersecting, non-contained stickers
    ;; are our containers.
    ;;
    (setq containers not-contained)
    (let* ((label "Brand new sticker")
           (sticker
            ;;; FIXME: We aren't using sly--make-text-button here
            ;;; because it doesn't allow overlay button s
            (make-button from to :type 'sly-stickers-sticker
                         'sly-connection (sly-current-connection)
                         'part-args (list -1 nil)
                         'part-label label
                         'sly-button-search-id (sly-button-next-search-id)
                         'modification-hooks '(sly-stickers--sticker-modified)
                         'sly-stickers-id (cl-incf sly-stickers--counter)
                         'sly-stickers--base-help-echo
                         "mouse-3: Context menu")))
      ;; choose a suitable level for ourselves and increase the
      ;; level of those contained by us
      ;;
      (setf (sly-stickers--level sticker)
            (1+ (cl-reduce #'max containers
                           :key #'sly-stickers--level
                           :initial-value -1)))
      (mapc (lambda (s) (cl-incf (sly-stickers--level s))) contained)
      ;; finally, set face
      ;;
      (sly-stickers--set-tooltip sticker label)
      (sly-stickers--set-face sticker 'sly-stickers-placed-face)
      sticker)))

(defun sly-stickers--sticker-id (sticker)
  (button-get sticker 'sly-stickers-id))

(defun sly-stickers--arm-sticker (sticker)
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (format "Sticker %d is armed" id)))
    (button-put sticker 'part-args (list id nil))
    (button-put sticker 'part-label label)
    (button-put sticker 'sly-stickers--last-known-recording nil)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-armed-face)
    (puthash id sticker sly-stickers--stickers)))

(defun sly-stickers--disarm-sticker (sticker)
  (let* ((id (sly-stickers--sticker-id sticker))
         (label (format "Sticker %d failed to stick" id)))
    (button-put sticker 'part-args (list -1 nil))
    (button-put sticker 'part-label label)
    (sly-stickers--set-tooltip sticker label)
    (sly-stickers--set-face sticker 'sly-stickers-placed-face)))

(define-button-type 'sly-stickers--recording-part :supertype 'sly-part
  'sly-button-inspect
  'sly-stickers--inspect-recording
  ;; 'sly-button-pretty-print
  ;; #'(lambda (id) ...)
  ;; 'sly-button-describe
  ;; #'(lambda (id) ...)
  ;; 'sly-button-show-source
  ;; #'(lambda (id) ...)
  )

(defun sly-stickers--recording-part (label sticker-id recording vindex
                                           &rest props)
  (apply #'sly--make-text-button
         label nil
         :type 'sly-stickers--recording-part
         'part-args (list sticker-id recording vindex)
         'part-label "Recorded value"
         props))

(cl-defun sly-stickers--describe-recording-values (recording &key
                                                             (indent 0)
                                                             (prefix "=> "))
  (cl-flet ((indent (str)
                    (concat (make-string indent ? )str))
            (prefix (str)
                    (concat prefix str)))
    (let ((descs (sly-stickers--recording-value-descriptions recording)))
      (cond ((sly-stickers--recording-exited-non-locally-p recording)
             (indent (propertize "exited non locally" 'face 'sly-action-face)))
            ((null descs)
             (indent (propertize "no values" 'face 'sly-action-face)))
            (t
             (cl-loop for (value-desc . rest) on descs
                      for vindex from 0
                      concat
                      (indent (prefix
                               (sly-stickers--recording-part
                                value-desc
                                (sly-stickers--recording-sticker-id recording)
                                recording
                                vindex)))
                      when rest
                      concat "\n"))))))

(defconst sly-stickers--newline "\n"
  "Work around bug #63, actually Emacs bug #21839.
\"25.0.50; can't use newlines in defaults in cl functions\"")

(cl-defun sly-stickers--pretty-describe-recording
    (recording &key (separator sly-stickers--newline))
  (let* ((recording-sticker-id (sly-stickers--recording-sticker-id recording))
         (sticker (gethash recording-sticker-id
                           sly-stickers--stickers))
         (nvalues (length (sly-stickers--recording-value-descriptions recording))))
    (format "%s%s:%s%s"
            (if sticker
                (format "Sticker %s on line %s of %s"
                        (sly-stickers--sticker-id sticker)
                        (with-current-buffer (overlay-buffer sticker)
                          (line-number-at-pos (overlay-start sticker)))
                        (overlay-buffer sticker))
              (format "Deleted or unknown sticker %s"
                      recording-sticker-id))
            (if (cl-plusp nvalues)
                (format " returned %s values" nvalues) "")
            separator
            (sly-stickers--describe-recording-values recording
                                                     :indent 2))))

(defun sly-stickers--populate-sticker (sticker recording)
  (let* ((id (sly-stickers--sticker-id sticker))
         (total (sly-stickers--recording-sticker-total recording)))
    (cond ((cl-plusp total)
           (button-put sticker 'part-label
                       (format "Sticker %d has %d recordings" id total))
           (unless (sly-stickers--recording-void-p recording)
             (button-put sticker 'sly-stickers--last-known-recording recording)
             (button-put sticker 'part-args (list id recording))
             (sly-stickers--set-tooltip
              sticker
              (format "Newest of %s sticker recordings:\n%s"
                      total
                      (sly-stickers--describe-recording-values recording :prefix "")))
             (sly-stickers--set-face
              sticker
              (if (sly-stickers--recording-exited-non-locally-p recording)
                  'sly-stickers-exited-non-locally-face
                'sly-stickers-recordings-face))))
          (t
           (let ((last-known-recording
                  (button-get sticker 'sly-stickers--last-known-recording)))
             (button-put sticker 'part-label
                         (format "Sticker %d has no recordings" id))
             (when last-known-recording
               (sly-stickers--set-tooltip
                sticker
                (format "No new recordings. Last known:\n%s"
                        (sly-stickers--describe-recording-values
                         last-known-recording))))
             (sly-stickers--set-tooltip sticker "No new recordings")
             (sly-stickers--set-face sticker 'sly-stickers-empty-face))))))

(defun sly-stickers--sticker-substickers (sticker)
  (let* ((retval
          (remove sticker
                  (sly-stickers--stickers-between (button-start sticker)
                                                  (button-end sticker))))
         ;; To verify an important invariant, and warn (don't crash)
         ;;
         (exactly-at
          (sly-stickers--stickers-exactly-at (button-start sticker)
                                             (button-end sticker))))
    (cond
     ((remove sticker exactly-at)
      (sly-warning "Something's fishy. More than one sticker at same position")
      (cl-set-difference retval exactly-at))
     (t
      retval))))

(defun sly-stickers--briefly-describe-sticker (sticker)
  (let ((beg (button-start sticker))
        (end (button-end sticker)))
    (if (< (- end beg) 20)
        (format "sticker around %s" (buffer-substring-no-properties beg end))
      (cl-labels ((word (point direction)
                        (apply #'buffer-substring-no-properties
                               (sort (list
                                      point
                                      (save-excursion (goto-char point)
                                                      (forward-word direction)
                                                      (point)))
                                     #'<))))
        (format "sticker from \"%s...\" to \"...%s\""
                (word beg 1)
                (word end -1))))))

(defun sly-stickers--delete (sticker)
  "Ensure that sticker is deleted."
  ;; Delete the overlay and take care of levels for contained and
  ;; containers, but note that a sticker might have no buffer anymore
  ;; if that buffer was killed, for example...
  ;;
  (when (and (overlay-buffer sticker)
             (buffer-live-p (overlay-buffer sticker)))
    (mapc (lambda (s) (cl-decf (sly-stickers--level s)))
          (sly-stickers--sticker-substickers sticker))
    (delete-overlay sticker))
  ;; We also want to deregister it from the hashtable in case it's
  ;; there (it's not there if it has never been armed)
  ;;
  (let ((id (sly-stickers--sticker-id sticker)))
    (when (gethash (sly-stickers--sticker-id sticker)
                   sly-stickers--stickers)
      (remhash id sly-stickers--stickers)
      (add-to-list 'sly-stickers--zombie-sticker-ids id))))

(defun sly-stickers--sticker-modified (sticker _after? beg end
                                               &optional _pre-change-len)
  (unless (save-excursion
            (goto-char beg)
            (skip-chars-forward "\t\n\s")
            (>= (point) end))
    (let ((inhibit-modification-hooks t))
      (sly-message "Deleting %s"
                   (sly-stickers--briefly-describe-sticker sticker))
      (sly-stickers--delete sticker))))

(defun sly-stickers-next-sticker (&optional n)
  (interactive "p")
  (sly-button-search n 'sly-stickers--sticker-id))

(defun sly-stickers-prev-sticker (&optional n)
  (interactive "p")
  (sly-button-search (- n) 'sly-stickers--sticker-id))

(put 'sly-stickers-next-sticker 'sly-button-navigation-command t)
(put 'sly-stickers-prev-sticker 'sly-button-navigation-command t)

(defun sly-stickers-clear-defun-stickers ()
  "Clear all stickers in the current top-level form."
  (interactive)
  (let* ((region (sly-region-for-defun-at-point)))
    (sly-stickers-clear-region-stickers (car region) (cadr region))))

(defun sly-stickers-clear-buffer-stickers ()
  "Clear all the stickers in the current buffer."
  (interactive)
  (sly-stickers-clear-region-stickers (point-min) (point-max)))

(defun sly-stickers-clear-region-stickers (&optional from to)
  "Clear all the stickers between FROM and TO."
  (interactive "r")
  (let* ((from (or from (region-beginning)))
         (to (or to (region-end)))
         (stickers (sly-stickers--stickers-in from to)))
    (cond (stickers
           (mapc #'sly-stickers--delete stickers)
           (sly-message "%s stickers cleared" (length stickers)))
          (t
           (sly-message "no stickers to clear")))))

(defun sly-stickers-delete-sticker-at-point (&optional point)
  "Delete the topmost sticker at point."
  (interactive "d")
  (let ((stickers (sly-stickers--stickers-at (or point (point)))))
    (cond
     (stickers
      (sly-stickers--delete (car stickers))
      (if (cdr stickers)
          (sly-message "Deleted topmost sticker (%d remain at point)"
                       (length (cdr stickers)))
        (sly-message "Deleted sticker %s"
                     (sly-stickers--briefly-describe-sticker (car stickers)))))
     (t
      (sly-user-error "No stickers at point")))))

(defun sly-stickers-maybe-add-sticker (&optional point)
  "Add of remove a sticker at POINT.
If point is currently at a sticker boundary, delete that sticker,
otherwise, add a sticker to the sexp at point."
  (interactive "d")
  (save-excursion
    (goto-char (or point (point)))
    (let* ((bounds (sly-bounds-of-sexp-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (matching (and bounds
                          (sly-stickers--stickers-exactly-at beg end))))
      (cond
       ((not bounds)
        (sly-message "Nothing here to place sticker on, apparently"))
       (matching
        (sly-stickers--delete (car matching))
        (sly-message "Deleted sticker"))
       (t
        (let ((sticker (sly-stickers--sticker beg end)))
          (sly-message "Added %s"
                       (sly-stickers--briefly-describe-sticker sticker))))))))

(defun sly-stickers-dwim (prefix)
  "Set or remove stickers at point.
Set a sticker for the current sexp at point, or delete it if it
already exists.

If the region is active set a sticker in the current region.

With interactive prefix arg PREFIX always delete stickers.

- One C-u means delete the current top-level form's stickers.
- Two C-u's means delete the current buffer's stickers"
  (interactive "p")
  (cond
   ((= prefix 4)
    (if (region-active-p)
        (sly-stickers-clear-region-stickers)
      (sly-stickers-clear-defun-stickers)))
   ((>= prefix 16)
    (sly-stickers-clear-buffer-stickers))
   ((region-active-p)
    (sly-stickers--sticker (region-beginning) (region-end))
    (deactivate-mark t))
   ((not (sly-inside-string-or-comment-p))
    (sly-stickers-maybe-add-sticker))
   (t
    (sly-message "No point placing stickers in string literals or comments"))))

(defun sly-stickers--sticker-by-id (sticker-id)
  "Return the sticker for STICKER-ID, or return NIL.
Perform some housecleaning tasks for stickers that have been
properly deleted or brutally killed with the buffer they were in."
  (let* ((sticker (gethash sticker-id sly-stickers--stickers)))
    (cond ((and sticker (overlay-buffer sticker)
                (buffer-live-p (overlay-buffer sticker)))
           sticker)
          (sticker
           ;; `sticker-id' references a sticker that hasn't been
           ;; deleted but whose overlay can't be found. One reason for
           ;; this is that the buffer it existed in was killed. So
           ;; delete it now and mark it a zombie.
           (sly-stickers--delete sticker)
           nil)
          (t
           ;; The sticker isn't in the `sly-stickers--stickers' hash
           ;; table, so it has probably already been marked zombie,
           ;; and possibly already deleted. We're probably just seeing
           ;; it because recording playback and breaking stickers may
           ;; not filtering these out by user option.
           ;;
           ;; To be on the safe side, add the id to the table anyway,
           ;; so it'll get killed on the Slynk side on the next
           ;; request.
           ;;
           (add-to-list 'sly-stickers--zombie-sticker-ids sticker-id)
           nil))))

(defvar sly-stickers--flashing-sticker nil
  "The sticker currently being flashed.")

(cl-defun sly-stickers--find-and-flash (sticker-id &key (otherwise nil))
  "Find and flash the sticker referenced by STICKER-ID.
otherwise call OTHERWISE with a single argument, a string stating
the reason why the sticker couldn't be found"
  (let ((sticker (sly-stickers--sticker-by-id sticker-id)))
    (cond (sticker
           (let ((buffer (overlay-buffer sticker)))
             (when buffer
               (with-current-buffer buffer
                 (let* ((window (display-buffer buffer t)))
                   (when window
                     (with-selected-window window
                       (push-mark nil t)
                       (goto-char (overlay-start sticker))
                       (sly-recenter (point))
                       (setq sly-stickers--flashing-sticker sticker)
                       (pulse-momentary-highlight-overlay sticker 'highlight)
                       (run-with-timer
                        2 nil
                        (lambda ()
                          (when (eq sly-stickers--flashing-sticker sticker)
                            (pulse-momentary-highlight-overlay
                             sticker 'highlight)))))))))))
          (otherwise
           (funcall otherwise "Can't find sticker (probably deleted!)")))))

;; Work around an Emacs bug, probably won't be needed in Emacs 27.1
(advice-add 'pulse-momentary-unhighlight
            :before (lambda (&rest _args)
                      (let ((o pulse-momentary-overlay))
                        (when (and o (overlay-get o 'sly-stickers-id))
                          (overlay-put o 'priority nil))))
            '((name . fix-pulse-momentary-unhighlight-bug)))


;;;; Recordings
;;;;
(cl-defstruct (sly-stickers--recording
               (:constructor sly-stickers--make-recording-1)
               (:conc-name sly-stickers--recording-)
               (:copier sly-stickers--copy-recording))
  (sticker-id nil)
  (sticker-total nil)
  (id nil)
  (value-descriptions nil)
  (exited-non-locally-p nil)
  (sly-connection nil))

(defun sly-stickers--recording-void-p (recording)
  (not (sly-stickers--recording-id recording)))

(defun sly-stickers--make-recording (description)
  "Make a `sly-stickers--recording' from DESCRIPTION.
A DESCRIPTION is how the Lisp side describes a sticker and
usually its most recent recording. If it doesn't, a recording
veryfying `sly-stickers--recording-void-p' is created."
  (cl-destructuring-bind (sticker-id sticker-total . recording-description)
      description
    (let ((recording (sly-stickers--make-recording-1
                      :sticker-id sticker-id
                      :sticker-total sticker-total
                      :sly-connection (sly-current-connection))))
      (when recording-description
        (cl-destructuring-bind (recording-id _recording-ctime
                                             value-descriptions
                                             exited-non-locally-p)
            recording-description
          (setf
           (sly-stickers--recording-id recording)
           recording-id
           (sly-stickers--recording-value-descriptions recording)
           value-descriptions
           (sly-stickers--recording-exited-non-locally-p recording)
           exited-non-locally-p)))
      recording)))


;;;; Replaying sticker recordings
;;;;
(defvar sly-stickers--replay-help nil)

(defvar sly-stickers--replay-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-flet
        ((def
          (key binding &optional desc)
          (define-key map (kbd key) binding)
          (setf
           (cl-getf sly-stickers--replay-help binding)
           (cons (cons key (car (cl-getf sly-stickers--replay-help binding)))
                 (or desc
                     (cdr (cl-getf sly-stickers--replay-help binding)))))))
      (def "n" 'sly-stickers-replay-next
           "Scan recordings forward")
      (def "SPC" 'sly-stickers-replay-next)
      (def "N" 'sly-stickers-replay-next-for-sticker
           "Scan recordings forward for this sticker")
      (def "DEL" 'sly-stickers-replay-prev
           "Scan recordings backward")
      (def "p" 'sly-stickers-replay-prev)
      (def "P" 'sly-stickers-replay-prev-for-sticker
           "Scan recordings backward for this sticker")
      (def "j" 'sly-stickers-replay-jump
           "Jump to a recording")
      (def ">" 'sly-stickers-replay-jump-to-end
           "Go to last recording")
      (def "<" 'sly-stickers-replay-jump-to-beginning
           "Go to first recording")
      (def "h" 'sly-stickers-replay-toggle-help
           "Toggle help")
      (def "v" 'sly-stickers-replay-pop-to-current-sticker
           "Pop to current sticker")
      (def "V" 'sly-stickers-replay-toggle-pop-to-stickers
           "Toggle popping to stickers")
      (def "q" 'quit-window
           "Quit")
      (def "x" 'sly-stickers-replay-toggle-ignore-sticker
           "Toggle ignoring a sticker")
      (def "z" 'sly-stickers-replay-toggle-ignore-zombies
           "Toggle ignoring deleted stickers")
      (def "R" 'sly-stickers-replay-reset-ignore-list
           "Reset ignore list")
      (def "F" 'sly-stickers-forget
           "Forget about sticker recordings")
      (def "g" 'sly-stickers-replay-refresh
           "Refresh current recording")
      map)))

(define-derived-mode sly-stickers--replay-mode fundamental-mode
  "SLY Stickers Replay" "Mode for controlling sticker replay sessions Dialog"
  (set-syntax-table lisp-mode-syntax-table)
  (read-only-mode 1)
  (sly-mode 1)
  (add-hook 'post-command-hook
            'sly-stickers--replay-postch t t))

(defun sly-stickers--replay-postch ()
  (let ((win (get-buffer-window (current-buffer))))
    (when (and win
               (window-live-p win))
      (ignore-errors
        (set-window-text-height win (line-number-at-pos (point-max)))))))

(defvar sly-stickers--replay-expanded-help nil)

(defun sly-stickers-replay-toggle-help ()
  (interactive)
  (set (make-local-variable 'sly-stickers--replay-expanded-help)
       (not sly-stickers--replay-expanded-help))
  (sly-stickers--replay-refresh-1))

(sly-def-connection-var sly-stickers--replay-data nil
  "Data structure for information related to recordings")

(defvar sly-stickers--replay-key nil
  "A symbol identifying a particular replaying session in the
  Slynk server.")

(defvar sly-stickers--replay-pop-to-stickers t)

(defun sly-stickers--replay-refresh-1 ()
  "Insert a description of the current recording into the current
buffer"
  (cl-assert (eq major-mode 'sly-stickers--replay-mode)
             nil
             "%s must be run in a stickers replay buffer"
             this-command)
  (cl-labels
      ((paragraph () (if sly-stickers--replay-expanded-help "\n\n" "\n"))
       (describe-ignored-stickers
        ()
        (let ((ignored-ids (cl-getf (sly-stickers--replay-data)
                                    :ignored-ids))
              (ignore-zombies-p (cl-getf (sly-stickers--replay-data)
                                         :ignore-zombies-p)))
          (if (or ignored-ids ignore-zombies-p)
              (format "%s%s%s"
                      (paragraph)
                      (if ignore-zombies-p
                          "Skipping recordings of deleted stickers. " "")
                      (if ignored-ids
                          (format "Skipping recordings of sticker%s %s."
                                  (if (cl-rest ignored-ids) "s" "")
                                  (concat (mapconcat #'pp-to-string
                                                     (butlast ignored-ids)
                                                     ", ")
                                          (and (cl-rest ignored-ids) " and ")
                                          (pp-to-string
                                           (car (last ignored-ids)))))
                        ""))
            "")))
       (describe-help
        ()
        (format "%s%s"
                (paragraph)
                (if sly-stickers--replay-expanded-help
                    (substitute-command-keys "\\{sly-stickers--replay-mode-map}")
                  "n => next, p => previous, x => ignore, h => help, q => quit")))
       (describe-number-of-recordings
        (new-total)
        (let* ((old-total (cl-getf (sly-stickers--replay-data) :old-total))
               (diff (and old-total (- new-total old-total))))
          (format "%s total recordings%s"
                  new-total
                  (cond ((and diff
                              (cl-plusp diff))
                         (propertize (format ", %s new in the meantime"
                                             diff)
                                     'face 'bold))
                        (t
                         "")))))
       (describe-playhead
        (recording)
        (let ((new-total (cl-getf (sly-stickers--replay-data) :total))
              (index (cl-getf (sly-stickers--replay-data) :index)))
          (cond
           ((and new-total
                 recording)
            (format "Playhead at recording %s of %s"
                    (ignore-errors (1+ index))
                    (describe-number-of-recordings new-total)))
           (new-total
            (format "Playhead detached (ignoring too many stickers?) on %s"
                    (describe-number-of-recordings new-total)))
           (recording
            (substitute-command-keys
             "Playhead confused (perhaps hit \\[sly-stickers-replay-refresh])"))
           (t
            (format
             "No recordings! Perhaps you need to run some sticker-aware code first"))))))
    (sly-refreshing ()
      (let ((rec (cl-getf (sly-stickers--replay-data) :recording)))
        (insert (describe-playhead rec) (paragraph))
        (when rec
          (insert (sly-stickers--pretty-describe-recording
                   rec
                   :separator (paragraph)))))
      (insert (describe-ignored-stickers))
      (insert (describe-help)))))

(defun sly-stickers-replay ()
  "Start interactive replaying of known sticker recordings."
  (interactive)
  (let* ((buffer-name (sly-buffer-name :stickers-replay
                                       :connection (sly-current-connection)))
         (existing-buffer (get-buffer buffer-name)))
    (let ((split-width-threshold nil)
          (split-height-threshold 0))
      (sly-with-popup-buffer (buffer-name
                              :mode 'sly-stickers--replay-mode
                              :select t)
        (setq existing-buffer standard-output)))
    (with-current-buffer existing-buffer
      (setf (cl-getf (sly-stickers--replay-data) :replay-key)
            (cl-gensym "stickers-replay-"))
      (let ((old-total (cl-getf (sly-stickers--replay-data) :total))
            (new-total (sly-eval '(slynk-stickers:total-recordings))))
        (setf (cl-getf (sly-stickers--replay-data) :old-total) old-total)
        (when (and
               old-total
               (cl-plusp old-total)
               (> new-total old-total)
               (sly-y-or-n-p
                "Looks like there are %s new recordings since last replay.\n
Forget about old ones before continuing?" (- new-total old-total)))
          (sly-stickers-forget old-total)))

      (sly-stickers-replay-refresh 0
                                   (if existing-buffer nil t)
                                   t)
      (set-window-dedicated-p nil 'soft)
      (with-current-buffer existing-buffer
        (sly-stickers--replay-postch)))))

(defun sly-stickers-replay-refresh (n command &optional interactive)
  "Refresh the current sticker replay session.
N and COMMAND are passed to the Slynk server and instruct what
recording to fetch:

If COMMAND is nil, navigate to Nth next sticker recording,
skipping ignored stickers.

If COMMAND is a number, navigate to the Nth next sticker
recording for the sticker with that numeric sticker id.

If COMMAND is any other value, jump directly to the recording
index N.

Interactively, N is 0 and and COMMAND is nil, meaning that the
playhead should stay put and the buffer should be refreshed.

Non-interactively signal an error if no recording was fetched and
INTERACTIVE is the symbol `sly-error'.

Non-interactively, set the `:recording' slot of
`sly-stickers--replay-data' to nil if no recording was fetched."
  (interactive (list 0 nil t))
  (let ((result (sly-eval
                 `(slynk-stickers:search-for-recording
                   ',(cl-getf (sly-stickers--replay-data) :replay-key)
                   ',(cl-getf (sly-stickers--replay-data) :ignored-ids)
                   ',(cl-getf (sly-stickers--replay-data) :ignore-zombies-p)
                   ',(sly-stickers--zombies)
                   ,n
                   ',command))))
    ;; presumably, Slynk cleaned up the zombies we passed it.
    ;;
    (sly-stickers--reset-zombies)
    (cond ((car result)
           (cl-destructuring-bind (total index &rest sticker-description)
               result
             (let ((rec (sly-stickers--make-recording sticker-description))
                   (old-index (cl-getf (sly-stickers--replay-data) :index)))
               (setf (cl-getf (sly-stickers--replay-data) :index) index
                     (cl-getf (sly-stickers--replay-data) :total) total
                     (cl-getf (sly-stickers--replay-data) :recording) rec)
               (if old-index
                   (if (cl-plusp n)
                       (if (> old-index index) (sly-message "Rolled over to start"))
                     (if (< old-index index) (sly-message "Rolled over to end"))))
               ;; Assert that the recording isn't void
               ;;
               (when (sly-stickers--recording-void-p rec)
                 (sly-error "Attempt to visit a void recording described by %s"
                            sticker-description))
               (when sly-stickers--replay-pop-to-stickers
                 (sly-stickers--find-and-flash
                  (sly-stickers--recording-sticker-id rec))))))
          (interactive
           ;; If we were called interactively and got an error, it's
           ;; probably because there aren't any recordings, so reset
           ;; data
           ;;
           (setf (sly-stickers--replay-data) nil)
           (when (eq interactive 'sly-error)
             (sly-error "%s for %s reported an error: %s"
                        'slynk-stickers:search-for-recording
                        n
                        (cadr result)))
           (setf (cl-getf (sly-stickers--replay-data) :recording) nil)))
    (if interactive
        (sly-stickers--replay-refresh-1)
      (cl-getf (sly-stickers--replay-data) :recording ))))

(defun sly-stickers-replay-next (n)
  "Navigate to Nth next sticker recording, skipping ignored stickers"
  (interactive "p")
  (sly-stickers-replay-refresh n nil 'sly-error))

(defun sly-stickers-replay-prev (n)
  "Navigate to Nth prev sticker recording, skipping ignored stickers"
  (interactive "p")
  (sly-stickers-replay-refresh (- n) nil 'sly-error))

(defun sly-stickers-replay--current-sticker-interactive (prompt)
  (if current-prefix-arg
      (read-number (format "[sly] %s " prompt))
    (sly-stickers--recording-sticker-id
     (cl-getf (sly-stickers--replay-data) :recording))))

(defun sly-stickers-replay-next-for-sticker (n sticker-id)
  "Navigate to Nth next sticker recording for STICKER-ID"
  (interactive (list
                (if (numberp current-prefix-arg)
                    current-prefix-arg
                  1)
                (sly-stickers-replay--current-sticker-interactive
                 "Which sticker?")))
  (sly-stickers-replay-refresh n sticker-id 'sly-error))

(defun sly-stickers-replay-prev-for-sticker (n sticker-id)
  "Navigate to Nth prev sticker recording for STICKER-ID"
  (interactive (list
                (- (if (numberp current-prefix-arg)
                       current-prefix-arg
                     1))
                (sly-stickers-replay--current-sticker-interactive
                 "Which sticker?")))
  (sly-stickers-replay-refresh n sticker-id 'sly-error))

(defun sly-stickers-replay-jump (n)
  "Fetch and jump to Nth sticker recording"
  (interactive (read-number "[sly] jump to which recording? "))
  (sly-stickers-replay-refresh n 'absolute-p 'sly-error))

(defun sly-stickers-replay-jump-to-beginning ()
  "Fetch and jump to the first sticker recording"
  (interactive)
  (sly-stickers-replay-refresh 0 'absolute-p 'sly-error))

(defun sly-stickers-replay-jump-to-end ()
  "Fetch and jump to the last sticker recording"
  (interactive)
  (sly-stickers-replay-refresh -1 'absolute-p 'sly-error))

(defun sly-stickers-replay-toggle-ignore-sticker (sticker-id)
  "Toggle ignoring recordings of sticker with STICKER-ID"
  (interactive (list
                (sly-stickers-replay--current-sticker-interactive
                 "Toggle ignoring which sticker id?")))
  (let* ((ignored (cl-getf (sly-stickers--replay-data) :ignored-ids))
         (ignored-p (memq sticker-id ignored)))
    (cond (ignored-p
           (setf (cl-getf (sly-stickers--replay-data) :ignored-ids)
                 (delq sticker-id (cdr ignored)))
           (sly-message "No longer ignoring sticker %s" sticker-id))
          (t
           (setf (cl-getf (sly-stickers--replay-data) :ignored-ids)
                 (delete-dups ; stupid but safe
                  (cons sticker-id ignored)))
           (sly-message "Now ignoring sticker %s" sticker-id)))
    (sly-stickers-replay-refresh (if ignored-p ; was ignored, now isn't
                                     0
                                   1)
                                 nil
                                 t)))

(defun sly-stickers-replay-toggle-ignore-zombies ()
  "Toggle ignoring recordings of zombie stickers."
  (interactive)
  (let ((switch
         (setf
          (cl-getf (sly-stickers--replay-data) :ignore-zombies-p)
          (not (cl-getf (sly-stickers--replay-data) :ignore-zombies-p)))))
    (if switch
        (sly-message "Now ignoring zombie stickers")
      (sly-message "No longer ignoring zombie stickers")))
  (sly-stickers-replay-refresh 0 nil t))

(defun sly-stickers-replay-pop-to-current-sticker (sticker-id)
  "Pop to sticker with STICKER-ID"
  (interactive (list
                (sly-stickers-replay--current-sticker-interactive
                 "Pop to which sticker id?")))
  (sly-stickers--find-and-flash sticker-id
                                :otherwise #'sly-error))

(defun sly-stickers-replay-toggle-pop-to-stickers ()
  "Toggle popping to stickers when replaying sticker recordings."
  (interactive)
  (set (make-local-variable 'sly-stickers--replay-pop-to-stickers)
       (not sly-stickers--replay-pop-to-stickers))
  (if sly-stickers--replay-pop-to-stickers
      (sly-message "Auto-popping to stickers ON")
    (sly-message "Auto-popping to stickers OFF")))

(defun sly-stickers-replay-reset-ignore-list ()
  "Reset the sticker ignore specs"
  (interactive)
  (setf (cl-getf (sly-stickers--replay-data) :ignored-ids) nil)
  (sly-stickers-replay-refresh 0 nil t))

(defun sly-stickers-fetch ()
  "Fetch recordings from Slynk and update stickers accordingly.
See also `sly-stickers-replay'."
  (interactive)
  (sly-eval-async `(slynk-stickers:fetch ',(sly-stickers--zombies))
    #'(lambda (result)
        (sly-stickers--reset-zombies)
        (let ((message
               (format "Fetched recordings for %s armed stickers"
                       (length result))))
          (cl-loop for sticker-description in result
                   ;; Although we are analysing sticker descriptions
                   ;; here, recordings are made to pass to
                   ;; `sly-stickers--sticker-by-id', even if they are
                   ;; are `sly-stickers--recording-void-p', which is
                   ;; the case if the sticker has never been
                   ;; traversed.
                   ;;
                   for recording =
                   (sly-stickers--make-recording sticker-description)
                   for sticker =
                   (sly-stickers--sticker-by-id
                    (sly-stickers--recording-sticker-id recording))
                   when sticker
                   do (sly-stickers--populate-sticker sticker recording))
          (sly-message message)))
    "CL_USER"))

(defun sly-stickers-forget (&optional howmany interactive)
  "Forget about sticker recordings in the Slynk side.
If HOWMANY is non-nil it must be a number stating how many
recordings to forget about. In this cases Because 0 is an index,
in the `nth' sense, the HOWMANYth recording survives."
  (interactive (list (and (numberp current-prefix-arg)
                          current-prefix-arg)
                     t))
  (when (or (not interactive)
            (sly-y-or-n-p "Really forget about sticker recordings?"))
    (sly-eval `(slynk-stickers:forget ',(sly-stickers--zombies) ,howmany))
    (sly-stickers--reset-zombies)
    (setf (cl-getf (sly-stickers--replay-data) :rec) nil
          (cl-getf (sly-stickers--replay-data) :old-total) nil)
    (when interactive
      (sly-message "Forgot all about sticker recordings."))
    (when (eq major-mode 'sly-stickers--replay-mode)
      (sly-stickers-replay-refresh 0 t t))))


;;;; Breaking stickers
(defun sly-stickers--handle-break (extra)
  (sly-dcase extra
    ((:slynk-after-sticker description)
     (let ((sticker-id (cl-first description))
           (recording (sly-stickers--make-recording description)))
       (sly-stickers--find-and-flash sticker-id
                                     :otherwise 'sly-message)
       (insert
        "\n\n"
        (sly-stickers--pretty-describe-recording recording
                                                 ))))
    ((:slynk-before-sticker sticker-id)
     (sly-stickers--find-and-flash sticker-id
                                   :otherwise 'sly-message))
    (;; don't do anything if we don't know this "extra" info
     t
     nil)))


(defun sly-stickers-toggle-break-on-stickers ()
  (interactive)
  (let ((break-p (sly-eval '(slynk-stickers:toggle-break-on-stickers))))
    (sly-message "Breaking on stickers is %s" (if break-p "ON" "OFF"))))


;;;; Functions for examining recordings
;;;;


(eval-after-load "sly-mrepl"
  `(progn
     (button-type-put 'sly-stickers-sticker
                      'sly-mrepl-copy-part-to-repl
                      'sly-stickers--copy-recording-to-repl)
     (button-type-put 'sly-stickers--recording-part
                      'sly-mrepl-copy-part-to-repl
                      'sly-stickers--copy-recording-to-repl)))


;;; shoosh byte-compiler
(declare-function sly-mrepl--save-and-copy-for-repl nil)
(cl-defun sly-stickers--copy-recording-to-repl
    (_sticker-id recording &optional (vindex 0))
  (check-recording recording)
  (sly-mrepl--save-and-copy-for-repl
   `(slynk-stickers:find-recording-or-lose
     ,(sly-stickers--recording-id recording)
     ,vindex)
   :before (format "Returning values of recording %s of sticker %s"
                   (sly-stickers--recording-id recording)
                   (sly-stickers--recording-sticker-id recording))))

(defun check-recording (recording)
  (cond ((null recording)
         (sly-error "This sticker doesn't seem to have any recordings"))
        ((not (eq (sly-stickers--recording-sly-connection recording)
                  (sly-current-connection)))
         (sly-error "Recording is for a different connection (%s)"
                    (sly-connection-name
                     (sly-stickers--recording-sly-connection recording))))))

(cl-defun sly-stickers--inspect-recording
    (_sticker-id recording &optional (vindex 0))
  (check-recording recording)
  (sly-eval-for-inspector
   `(slynk-stickers:inspect-sticker-recording
     ,(sly-stickers--recording-id recording)
     ,vindex)))

;;;; Sticker-aware compilation
;;;;

(cl-defun sly-stickers--compile-region-aware-of-stickers-1
    (start end callback &key sync fallback flash)
  "Compile from START to END considering stickers.
After compilation call CALLBACK with the stickers and the
compilation result.  If SYNC, use `sly-eval' other wise use
`sly-eval-async'.  If FALLBACK, send the uninstrumneted region as
a fallback.  If FLASH, flash the compiled region."
  (let* ((uninstrumented (buffer-substring-no-properties start end))
         (stickers (sly-stickers--stickers-between start end))
         (original-buffer (current-buffer)))
    (cond (stickers
           (when flash
             (sly-flash-region start end :face 'sly-stickers-armed-face))
           (sly-with-popup-buffer ((sly-buffer-name :stickers :hidden t)
                                   :select :hidden)
             (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
             (insert uninstrumented)
             ;; Use a second set of overlays placed just in the
             ;; pre-compilation buffer. We need this to correctly keep
             ;; track of the markers because in this buffer we are going
             ;; to change actual text
             ;;
             (cl-loop for sticker in stickers
                      for overlay =
                      (make-overlay (- (button-start sticker) (1- start))
                                    (- (button-end sticker) (1- start)))
                      do (overlay-put overlay 'sly-stickers--sticker sticker))
             (cl-loop for overlay in (overlays-in (point-min) (point-max))
                      for sticker = (overlay-get overlay 'sly-stickers--sticker)
                      do
                      (sly-stickers--arm-sticker sticker)
                      (goto-char (overlay-start overlay))
                      (insert (format "(slynk-stickers:record %d "
                                      (sly-stickers--sticker-id sticker)))
                      (goto-char (overlay-end overlay))
                      (insert ")"))
             ;; Now send both the instrumented and uninstrumented
             ;; string to the Lisp
             ;;
             (let ((instrumented (buffer-substring-no-properties (point-min)
                                                                 (point-max)))
                   (new-ids (mapcar #'sly-stickers--sticker-id stickers)))
               (with-current-buffer original-buffer
                 (let ((form `(slynk-stickers:compile-for-stickers
                               ',new-ids
                               ',(sly-stickers--zombies)
                               ,instrumented
                               ,(when fallback uninstrumented)
                               ,(buffer-name)
                               ',(sly-compilation-position start)
                               ,(if (buffer-file-name)
                                    (sly-to-lisp-filename (buffer-file-name)))
                               ',sly-compilation-policy)))
                   (cond (sync
                          (funcall callback
                                   stickers
                                   (sly-eval form))
                          (sly-stickers--reset-zombies))
                         (t (sly-eval-async form
                              (lambda (result)
                                (sly-stickers--reset-zombies)
                                (funcall callback stickers result))))))))))
          (t
           (sly-compile-region-as-string start end)))))

(defun sly-stickers-compile-region-aware-of-stickers (start end)
  "Compile region from START to END aware of stickers.
Intended to be placed in `sly-compile-region-function'"
  (sly-stickers--compile-region-aware-of-stickers-1
   start end
   (lambda (stickers result-and-stuck-p)
     (cl-destructuring-bind (result &optional stuck-p)
         result-and-stuck-p
       (unless stuck-p
         (mapc #'sly-stickers--disarm-sticker stickers))
       (sly-compilation-finished
        result
        nil
        (if stuck-p
            (format " (%d stickers armed)" (length stickers))
          " (stickers failed to stick)"))))
   :fallback t
   :flash t))

(defun sly-stickers-after-buffer-compilation (success _notes buffer loadp)
  "After compilation, compile regions with stickers.
Intented to be placed in `sly-compilation-finished-hook'"
  (when (and buffer loadp success)
    (save-restriction
      (widen)
      (let* ((all-stickers (sly-stickers--stickers-between
                            (point-min) (point-max)))
             (regions (cl-loop for sticker in all-stickers
                               for region = (sly-region-for-defun-at-point
                                             (overlay-start sticker))
                               unless (member region regions)
                               collect region into regions
                               finally (cl-return regions))))
        (when regions
          (cl-loop
           with successful
           with unsuccessful
           for region in regions
           do
           (sly-stickers--compile-region-aware-of-stickers-1
            (car region) (cadr region)
            (lambda (stickers result)
              (cond (result
                     (push (cons region stickers) successful))
                    (t
                     (mapc #'sly-stickers--disarm-sticker stickers)
                     (push (cons region stickers) unsuccessful))))
            :sync t)
           finally
           (sly-temp-message
            3 3
            "%s stickers stuck in %s regions, %s disarmed in %s regions"
            (cl-reduce #'+ successful :key (lambda (x) (length (cdr x))))
            (length successful)
            (cl-reduce #'+ unsuccessful :key (lambda (x) (length (cdr x))))
            (length unsuccessful))))))))


;;;; Menu
;;;;

(easy-menu-define sly-stickers--shortcut-menu nil
  "Placing stickers in `lisp-mode' buffers."
  (let* ((in-source-file 'sly-stickers-mode)
         (connected '(sly-connected-p)))
    `("Stickers"
      ["Add or remove sticker at point"
       sly-stickers-dwim ,in-source-file]
      ["Delete stickers from top-level form"
       sly-stickers-clear-defun-stickers ,in-source-file]
      ["Delete stickers from buffer"
       sly-stickers-clear-buffer-stickers ,in-source-file]
      "--"
      ["Start sticker recording replay"
       sly-stickers-replay ,connected]
      ["Fetch most recent recordings"
       sly-stickers-fetch ,connected]
      ["Toggle breaking on stickers"
       sly-stickers-toggle-break-on-stickers ,connected])))

(easy-menu-add-item sly-menu nil sly-stickers--shortcut-menu "Documentation")

(provide 'sly-stickers)
;;; sly-stickers.el ends here

