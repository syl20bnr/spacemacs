;;; centered-cursor-mode.el --- cursor stays vertically centered

;; Copyright (C) 2007  André Riemann

;; Author: André Riemann <andre.riemann@web.de>
;; Maintainer: André Riemann <andre.riemann@web.de>
;; Created: 2007-09-14
;; Keywords: convenience

;; URL: http://www.emacswiki.org/cgi-bin/wiki/centered-cursor-mode.el
;; Compatibility: only tested with GNU Emacs 23.0
;; Version: 0.5.2
;; Last-Updated: 2009-08-31

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Makes the cursor stay vertically in a defined position (usually
;; centered). The vertical position can be altered, see key definition
;; below.

;; To load put that in .emacs:
;;     (require 'centered-cursor-mode)
;; To activate do:
;;     M-x centered-cursor-mode
;; for buffer local or
;;     M-x global-centered-cursor-mode
;; for global minor mode.
;; Also possible: put that in .emacs
;;     (and
;;      (require 'centered-cursor-mode)
;;      (global-centered-cursor-mode +1))
;; to always have centered-cursor-mode on in all buffers.

;;; TODO:
;; - the code is a mess
;; - ccm-vpos-inverted doesn't work with ccm-vpos == 0, because first
;;   position from top is 0 and from bottom -1
;; - interactive first start isn't animated when calling global-...
;;   because it starts the modes for each buffer and interactive-p fails
;;   for that
;; - more bugs?

;;; Change Log:
;; 2009-08-31  andre-r
;;   * replaced window-body-height with window-text-height
;;     (partially visible lines are not counted in window-text-height)
;;   * bug fixed in ccm-vpos-recenter
;;     (some parentheses where wrong after the last update)
;; 2009-02-23  andre-r
;;   * some simplifications
;; 2009-02-22  andre-r
;;   * some tips from Drew Adams:
;;     - new local variable coding:utf-8
;;     - made recenter-sequence a defvar
;;     - added groups scrolling and convenience
;;     - replaced mouse-4 and mouse-5 with
;;       mouse-wheel-up-event and mouse-wheel-down-event
;;     - added scroll-bar-toolkit-scroll to ccm-ignored-commands
;;     - made ccm-ignored-commands customisable
;;   * removed a bug where it didn't work with more than one window
;;     displaying the same buffer
;;   * added function for page up and down scrolling
;;     (standard ones didn't work well with this mode)
;;   * made the animation delay customisable
;;   * made the initial vertical position customisable
;;   * made the behaviour at the end of the file customisable
;; 2008-02-02  andre-r
;;   * fixed bug that led to wrong-type-argument
;;     when opening a new buffer
;;   * some other minor stuff
;; 2007-09-24  andre-r
;;   * added global minor mode
;; 2007-09-21  andre-r
;;   * not recentering at end of buffer
;;   * defvar animate-first-start-p
;; 2007-09-14  andre-r
;;   * inital release

;; This file is *NOT* part of GNU Emacs.

;;; Code:
 
(defgroup centered-cursor nil
  "Makes the cursor stay vertically in a defined position (usually centered).
Instead the cursor the text moves around the cursor."
  :group 'scrolling
  :group 'convenience
  :link '(emacs-library-link :tag "Source Lisp File" "centered-cursor-mode.el")
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki/centered-cursor-mode.el"))

(defcustom ccm-step-size 2
  "Step size when animated recentering."
  :group 'centered-cursor
  :tag "Animation step size"
  :type 'integer)

(defcustom ccm-step-delay 0.02
  "Delay between animation steps.
If you want a different animation speed."
  :group 'centered-cursor
  :tag "Animation step delay"
  :type 'number)

(defcustom ccm-ignored-commands '(mouse-drag-region
                                  mouse-set-point
                                  widget-button-click
                                  scroll-bar-toolkit-scroll)
  "After these commands recentering is ignored.
This is to prevent unintentional jumping (especially when mouse
clicking). Following commands (except the ignored ones) will
cause an animated recentering to give a feedback and not just
jumping to the center."
  :group 'centered-cursor
  :tag "Ignored commands"
  :type '(repeat (symbol :tag "Command")))

(defcustom ccm-vpos-init '(round (window-text-height) 2)
  "This is the screen line position where the cursor initially stays."
  :group 'centered-cursor
  :tag "Vertical cursor position"
  :type '(choice (const :tag "Center" (round (window-text-height) 2))
                 (const :tag "Golden ratio" (round (* 21 (window-text-height)) 34))
                 (integer :tag "Lines from top" :value 10)))
(make-variable-buffer-local 'ccm-vpos-init)

(defcustom ccm-vpos-inverted 1
  "Inverted vertical cursor position.
Defines if the initial vertical position `ccm-vpos-init' is
measured from the bottom instead from the top."
  :group 'centered-cursor
  :tag "Inverted cursor position"
  :type '(choice (const :tag "Inverted" -1)
                 (const :tag "Not inverted" 1)))
(make-variable-buffer-local 'ccm-vpos-inverted)

(defcustom ccm-recenter-at-end-of-file nil
  "Recenter at the end of the file.
If non-nil the end of the file is recentered. If nil the end of
the file stays at the end of the window."
  :group 'centered-cursor
  :tag "Recenter at EOF"
  :type '(choice (const :tag "Don't recenter at the end of the file" nil)
                 (const :tag "Recenter at the end of the file" t)))
(make-variable-buffer-local 'ccm-recenter-end-of-file)

(defvar ccm-vpos nil
  "This is the screen line position where the cursor stays.")
(make-variable-buffer-local 'ccm-vpos)

(defvar animate-first-start-p nil
  "Whether or not to animate at first start. It is set to nil, if
centered-cursor-mode is called non-interactively.")
(make-variable-buffer-local 'animate-first-start-p)

(defvar recenter-sequence nil
  "Before animated recentering a list is generated first with positions
to successively recenter to")
(make-variable-buffer-local 'recenter-sequence)

(defvar ccm-map
  (let ((ccm-map (make-sparse-keymap)))
    (define-key ccm-map [(control meta -)]  'ccm-vpos-up)
    (define-key ccm-map [(control meta +)]  'ccm-vpos-down)
    (define-key ccm-map [(control meta =)]  'ccm-vpos-down)
    (define-key ccm-map [(control meta ?0)] 'ccm-vpos-recenter)
    (when mouse-wheel-mode
      (mapc (lambda (key)
              (define-key ccm-map key 'ccm-mwheel-scroll))
            (list (vector mouse-wheel-up-event)
                  (vector mouse-wheel-down-event)
                  (vector (list 'control mouse-wheel-up-event))
                  (vector (list 'control mouse-wheel-down-event))
                  (vector (list 'shift mouse-wheel-up-event))
                  (vector (list 'shift mouse-wheel-down-event)))))
    (define-key ccm-map [(meta v)] 'ccm-scroll-down)
    (define-key ccm-map [(control v)] 'ccm-scroll-up)
    (define-key ccm-map [prior] 'ccm-scroll-down)
    (define-key ccm-map [next] 'ccm-scroll-up)
   ccm-map)
  "Keymap used in centered-cursor-mode.")

 
(defun ccm-mwheel-scroll (event)
  "Very similar to `mwheel-scroll', but does not use `scroll-down'
and `scroll-up' but `previous-line' and `next-line', that is, the
cursor is moved and thus the text in the window is scrolled
due to `recenter'.

The customizable variable `mouse-wheel-scroll-amount' is used to
determine how much to scroll, where nil instead of a number means
the same as in mwheel-scroll, scroll by a near full screen.

This command exists, because mwheel-scroll caused strange
behaviour with automatic recentering."
;;  (interactive (list last-input-event))
  (interactive "e")
  (let* ((mods (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
         (amt (assoc mods mouse-wheel-scroll-amount)))
    ;;(message "%S" mods)
    (if amt
        (setq amt (or (cdr amt)
                      (- (window-text-height)
                         next-screen-context-lines)))
      (let ((list-elt mouse-wheel-scroll-amount))
        (while (consp (setq amt (pop list-elt))))))
    (if mouse-wheel-follow-mouse
        (select-window (posn-window (event-start event))))
    (let ((button (mwheel-event-button event)))
      (cond
       ((eq button mouse-wheel-down-event)
        (forward-line (- amt)))
        ;;(princ amt))
       ((eq button mouse-wheel-up-event)
        (forward-line amt))
         ;;(princ amt))
       (t (error "Bad binding in ccm-mwheel-scroll"))))))

(defun ccm-scroll-down (&optional arg)
  "Replaces `scroll-down' because with scroll-down
`centered-cursor-mode' sometimes doesn't reach the top of the
buffer. This version actually moves the cursor with
`previous-line'. Since with centered-cursor-mode the cursor is in
a fixed position the movement appears as page up."
  (interactive "P")
  (let ((amt (or arg (- (window-text-height)
                        next-screen-context-lines))))
    (forward-line (- amt))))

(defun ccm-scroll-up (&optional arg)
  "Replaces `scroll-up' to be consistent with `ccm-scroll-down'.
This version actually moves the cursor with `previous-line'.
Since with centered-cursor-mode the cursor is in a fixed position
the movement appears as page up."
  (interactive "P")
  (let ((amt (or arg (- (window-text-height)
                        next-screen-context-lines))))
    (forward-line amt)))

 
(defun ccm-vpos-down (arg)
  "Adjust the value of the screen line (where the cursor stays) by arg.
Negative values for arg are possible. Just the variable ccm-vpos
is set."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((new-pos (if (< ccm-vpos 0)
                     (- ccm-vpos arg)
                   (+ ccm-vpos arg)))
        ;; see pos-visible-in-window-p
        (vpos-max (if (< ccm-vpos 0)
                      -1
                    (- (window-text-height) 1)))
        (vpos-min (if (< ccm-vpos 0)
                      (- (window-text-height))
                    0)))
    (setq ccm-vpos
          (cond
           ((< new-pos vpos-min)
            vpos-min)
           ((> new-pos vpos-max)
            vpos-max)
           (t
            new-pos)))))

(defun ccm-vpos-up (arg)
  "See `ccm-vpos-down'."
  (interactive "p")
  (or arg (setq arg 1))
  (ccm-vpos-down (- arg)))

(defun ccm-vpos-recenter ()
  "Set the value of the screen line (where the cursor stays) in
the center. Just the variable ccm-vpos is set."
  (interactive)
  (if (equal (current-buffer)
             (window-buffer (selected-window)))
      (setq ccm-vpos (* (eval ccm-vpos-init)
                        ccm-vpos-inverted))))

(defun ccm-position-cursor ()
  "Do the actual recentering at the position `ccm-vpos'."
  (unless (member this-command ccm-ignored-commands)
    (unless ccm-vpos
      (ccm-vpos-recenter))
    (unless (minibufferp (current-buffer))
      (if (equal (current-buffer)
                 (window-buffer (selected-window)))
          (let* ((current-line
                  (if (< ccm-vpos 0)
                      ;; one-based, from bottom, negative
                      (- (count-lines (point)
                                      ;; window-end is sometimes < 0
                                      ;; when opening a help buffer
                                      (if (> (window-end) 0)
                                          (window-end)
                                        1)))
                    ;; zero-based, from top, positive
                    (+ (count-lines (window-start) (point))
                       ;; count-lines returns different value in column 0
                       (if (= (current-column) 0) 0 -1))))
                 (diff (- ccm-vpos current-line))
                 (step-size ccm-step-size)
                 (step-delay ccm-step-delay)
                 (vpos-inverted ccm-vpos-inverted)
                 (recenter-at-end-of-file ccm-recenter-at-end-of-file))

            (let* ((bottom-vpos (if (< ccm-vpos 0)
                                    (- ccm-vpos)
                                  (- (window-text-height) ccm-vpos)))
                   (correction (save-excursion
                                 (if (or (= (point) (point-max))
                                         (progn
                                           (goto-char (point-max))
                                           (zerop (current-column))))
                                     1 0)))
                   ;; lines from point to end of buffer
                   (bottom-lines (+ (count-lines (point) (point-max))
                                    correction)))

              ;; only animate if the point was moved rather far away
              ;; before by a mouseclick (see ccm-ignored-commands)
              ;; or if minor mode is just entered interactively
              (if (not (and (> (abs diff) 4)
                            (or (member last-command ccm-ignored-commands)
                                animate-first-start-p)))

                  (recenter (if (and (< bottom-lines bottom-vpos)
                                     (not recenter-at-end-of-file))
                                ;; if near the bottom, recenter in the
                                ;; negative screen line that equals the
                                ;; bottom buffer line, i.e. if we are in
                                ;; the second last line (-2) of the
                                ;; buffer, the cursor will be recentered
                                ;; in -2
                                (- bottom-lines)
                              ccm-vpos))

                (setq animate-first-start-p nil)
                ;; first build a list with positions to successively recenter to
                (setq recenter-sequence
                      ;; reverse: because we build the list not FROM -> TO but
                      ;; TO -> FROM because if step size in number-sequence is
                      ;; bigger than one, TO might not included, that means the
                      ;; ccm-vpos would not be reached
                      ;; cdr: don't recenter the current-line
                      (if (and (< bottom-lines bottom-vpos)
                               (not recenter-at-end-of-file))
                          ;; this one is for animation near the bottom
                          (cdr (reverse (number-sequence
                                         (- bottom-lines)
                                         (if (< ccm-vpos 0)
                                             current-line
                                           (- (- (window-text-height) current-line)))
                                         (* (/ diff (abs diff)) (- step-size)))))
                        (cdr (reverse (number-sequence
                                       ccm-vpos
                                       current-line
                                       (* (/ diff (abs diff)) (- step-size)))))))
                ;; (message "%d %d %d (%d): %S" current-line ccm-vpos bottom-lines diff recenter-sequence)
                (while recenter-sequence
                  ;; actual animation
                  (recenter (pop recenter-sequence))
                  (if (car recenter-sequence) (sit-for step-delay t))))))))))

(defun ccm-first-start (animate)
  "Called from centered-cursor-mode. Animate at first start, if
centered-cursor-mode is called interactively."
  (let ((animate-first-start-p animate))
    (ccm-vpos-recenter)
    (ccm-position-cursor)))

;;(defalias 'ccm 'centered-cursor-mode)
(define-minor-mode centered-cursor-mode
  "Makes the cursor stay vertically in a defined
position (usually centered)."
  :init-value nil
;;  :lighter nil
  :lighter " ¢"
  :keymap ccm-map
  (cond
   (centered-cursor-mode
    (ccm-first-start (interactive-p))
    (add-hook 'post-command-hook 'ccm-position-cursor t t)
    (add-hook 'window-configuration-change-hook 'ccm-vpos-recenter t t))
   (t
    (remove-hook 'post-command-hook 'ccm-position-cursor t)
    (remove-hook 'window-configuration-change-hook 'ccm-vpos-recenter t))))


(define-global-minor-mode global-centered-cursor-mode centered-cursor-mode
  centered-cursor-mode)

(provide 'centered-cursor-mode)

;;; Help:
;; (info "(elisp)Defining Minor Modes")
;; (info "(elisp)Screen Lines")
;; (info "(elisp)Hooks")
;; (info "(elisp)Customization")
;; (find-function 'mwheel-scroll)
 
;; Local Variables:
;; coding: utf-8
;; End:
 
;;; centered-cursor-mode.el ends here
