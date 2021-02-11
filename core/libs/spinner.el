;;; spinner.el --- Add spinners and progress-bars to the mode-line for ongoing operations -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Version: 1.7.3
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/Malabarba/spinner.el
;; Keywords: processes mode-line

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
;;
;; 1 Usage
;; ═══════
;;
;;   First of all, don’t forget to add `(spinner "VERSION")' to your
;;   package’s dependencies.
;;
;;
;; 1.1 Major-modes
;; ───────────────
;;
;;   1. Just call `(spinner-start)' and a spinner will be added to the
;;      mode-line.
;;   2. Call `(spinner-stop)' on the same buffer when you want to remove
;;      it.
;;
;;   The default spinner is a line drawing that rotates. You can pass an
;;   argument to `spinner-start' to specify which spinner you want. All
;;   possibilities are listed in the `spinner-types' variable, but here are
;;   a few examples for you to try:
;;
;;   • `(spinner-start 'vertical-breathing 10)'
;;   • `(spinner-start 'minibox)'
;;   • `(spinner-start 'moon)'
;;   • `(spinner-start 'triangle)'
;;
;;   You can also define your own as a vector of strings (see the examples
;;   in `spinner-types').
;;
;;
;; 1.2 Minor-modes
;; ───────────────
;;
;;   Minor-modes can create a spinner with `spinner-create' and then add it
;;   to their mode-line lighter. They can then start the spinner by setting
;;   a variable and calling `spinner-start-timer'. Finally, they can stop
;;   the spinner (and the timer) by just setting the same variable to nil.
;;
;;   Here’s an example for a minor-mode named `foo'. Assuming that
;;   `foo--lighter' is used as the mode-line lighter, the following code
;;   will add an *inactive* global spinner to the mode-line.
;;   ┌────
;;   │ (defvar foo--spinner (spinner-create 'rotating-line))
;;   │ (defconst foo--lighter
;;   │   '(" foo" (:eval (spinner-print foo--spinner))))
;;   └────
;;
;;   1. To activate the spinner, just call `(spinner-start foo--spinner)'.
;;      It will show up on the mode-line and start animating.
;;   2. To get rid of it, call `(spinner-stop foo--spinner)'. It will then
;;      disappear again.
;;
;;   Some minor-modes will need spinners to be buffer-local. To achieve
;;   that, just make the `foo--spinner' variable buffer-local and use the
;;   third argument of the `spinner-create' function. The snippet below is an
;;   example.
;;
;;   ┌────
;;   │ (defvar-local foo--spinner nil)
;;   │ (defconst foo--lighter
;;   │   '(" foo" (:eval (spinner-print foo--spinner))))
;;   │ (defun foo--start-spinner ()
;;   │   "Create and start a spinner on this buffer."
;;   │   (unless foo--spinner
;;   │     (setq foo--spinner (spinner-create 'moon t)))
;;   │   (spinner-start foo--spinner))
;;   └────
;;
;;   1. To activate the spinner, just call `(foo--start-spinner)'.
;;   2. To get rid of it, call `(spinner-stop foo--spinner)'.
;;
;;   This will use the `moon' spinner, but you can use any of the names
;;   defined in the `spinner-types' variable or even define your own.


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defconst spinner-types
  '((3-line-clock . ["┤" "┘" "┴" "└" "├" "┌" "┬" "┐"])
    (2-line-clock . ["┘" "└" "┌" "┐"])
    (flipping-line . ["_" "\\" "|" "/"])
    (rotating-line . ["-" "\\" "|" "/"])
    (progress-bar . ["[    ]" "[=   ]" "[==  ]" "[=== ]" "[====]" "[ ===]" "[  ==]" "[   =]"])
    (progress-bar-filled . ["|    |" "|█   |" "|██  |" "|███ |" "|████|" "| ███|" "|  ██|" "|   █|"])
    (vertical-breathing . ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂" "▁" " "])
    (vertical-rising . ["▁" "▄" "█" "▀" "▔"])
    (horizontal-breathing . [" " "▏" "▎" "▍" "▌" "▋" "▊" "▉" "▉" "▊" "▋" "▌" "▍" "▎" "▏"])
    (horizontal-breathing-long
     . ["  " "▎ " "▌ " "▊ " "█ " "█▎" "█▌" "█▊" "██" "█▊" "█▌" "█▎" "█ " "▊ " "▋ " "▌ " "▍ " "▎ " "▏ "])
    (horizontal-moving . ["  " "▌ " "█ " "▐▌" " █" " ▐"])
    (minibox . ["▖" "▘" "▝" "▗"])
    (triangle . ["◢" "◣" "◤" "◥"])
    (box-in-box . ["◰" "◳" "◲" "◱"])
    (box-in-circle . ["◴" "◷" "◶" "◵"])
    (half-circle . ["◐" "◓" "◑" "◒"])
    (moon . ["🌑" "🌘" "🌗" "🌖" "🌕" "🌔" "🌓" "🌒"]))
  "Predefined alist of spinners.
Each car is a symbol identifying the spinner, and each cdr is a
vector, the spinner itself.")

(defun spinner-make-progress-bar (width &optional char)
  "Return a vector of strings of the given WIDTH.
The vector is a valid spinner type and is similar to the
`progress-bar' spinner, except without the sorrounding brackets.
CHAR is the character to use for the moving bar (defaults to =)."
  (let ((whole-string (concat (make-string (1- width) ?\s)
                              (make-string 4 (or char ?=))
                              (make-string width ?\s))))
    (apply #'vector (mapcar (lambda (n) (substring whole-string n (+ n width)))
                            (number-sequence (+ width 3) 0 -1)))))

(defvar spinner-current nil
  "Spinner curently being displayed on the `mode-line-process'.")
(make-variable-buffer-local 'spinner-current)

(defconst spinner--mode-line-construct
  '(:eval (spinner-print spinner-current))
  "Construct used to display a spinner in `mode-line-process'.")
(put 'spinner--mode-line-construct 'risky-local-variable t)

(defvar spinner-frames-per-second 10
  "Default speed at which spinners spin, in frames per second.
Each spinner can override this value.")


;;; The spinner object.
(defun spinner--type-to-frames (type)
  "Return a vector of frames corresponding to TYPE.
The list of possible built-in spinner types is given by the
`spinner-types' variable, but you can also use your own (see
below).

If TYPE is nil, the frames of this spinner are given by the first
element of `spinner-types'.
If TYPE is a symbol, it specifies an element of `spinner-types'.
If TYPE is 'random, use a random element of `spinner-types'.
If TYPE is a list, it should be a list of symbols, and a random
one is chosen as the spinner type.
If TYPE is a vector, it should be a vector of strings and these
are used as the spinner's frames.  This allows you to make your
own spinner animations."
  (cond
   ((vectorp type) type)
   ((not type) (cdr (car spinner-types)))
   ((eq type 'random)
    (cdr (elt spinner-types
              (random (length spinner-types)))))
   ((listp type)
    (cdr (assq (elt type (random (length type)))
               spinner-types)))
   ((symbolp type) (cdr (assq type spinner-types)))
   (t (error "Unknown spinner type: %s" type))))

(cl-defstruct (spinner
               (:copier nil)
               (:conc-name spinner--)
               (:constructor make-spinner (&optional type buffer-local frames-per-second delay-before-start)))
  (frames (spinner--type-to-frames type))
  (counter 0)
  (fps (or frames-per-second spinner-frames-per-second))
  (timer (timer-create))
  (active-p nil)
  (buffer (when buffer-local
            (if (bufferp buffer-local)
                buffer-local
              (current-buffer))))
  (delay (or delay-before-start 0)))

;;;###autoload
(defun spinner-create (&optional type buffer-local fps delay)
  "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
curent buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all."
  (make-spinner type buffer-local fps delay))

(defun spinner-print (spinner)
  "Return a string of the current frame of SPINNER.
If SPINNER is nil, just return nil.
Designed to be used in the mode-line with:
    (:eval (spinner-print some-spinner))"
  (when (and spinner (spinner--active-p spinner))
    (let ((frame (spinner--counter spinner)))
      (when (>= frame 0)
        (elt (spinner--frames spinner) frame)))))

(defun spinner--timer-function (spinner)
  "Function called to update SPINNER.
If SPINNER is no longer active, or if its buffer has been killed,
stop the SPINNER's timer."
  (let ((buffer (spinner--buffer spinner)))
    (if (or (not (spinner--active-p spinner))
            (and buffer (not (buffer-live-p buffer))))
        (spinner-stop spinner)
      ;; Increment
      (cl-callf (lambda (x) (if (< x 0)
                           (1+ x)
                         (% (1+ x) (length (spinner--frames spinner)))))
          (spinner--counter spinner))
      ;; Update mode-line.
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (force-mode-line-update))
        (force-mode-line-update)))))

(defun spinner--start-timer (spinner)
  "Start a SPINNER's timer."
  (let ((old-timer (spinner--timer spinner)))
    (when (timerp old-timer)
      (cancel-timer old-timer))

    (setf (spinner--active-p spinner) t)

    (unless (ignore-errors (> (spinner--fps spinner) 0))
      (error "A spinner's FPS must be a positive number"))
    (setf (spinner--counter spinner) (round (- (* (or (spinner--delay spinner) 0)
                                           (spinner--fps spinner)))))
    ;; Create timer.
    (let* ((repeat (/ 1.0 (spinner--fps spinner)))
           (time (timer-next-integral-multiple-of-time (current-time) repeat))
           ;; Create the timer as a lex variable so it can cancel itself.
           (timer (spinner--timer spinner)))
      (timer-set-time timer time repeat)
      (timer-set-function timer #'spinner--timer-function (list spinner))
      (timer-activate timer)
      ;; Return a stopping function.
      (lambda () (spinner-stop spinner)))))


;;; The main functions
;;;###autoload
(defun spinner-start (&optional type-or-object fps delay)
  "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all."
  (unless (spinner-p type-or-object)
    ;; Choose type.
    (if (spinner-p spinner-current)
        (setf (spinner--frames spinner-current) (spinner--type-to-frames type-or-object))
      (setq spinner-current (make-spinner type-or-object (current-buffer) fps delay)))
    (setq type-or-object spinner-current)
    ;; Maybe add to mode-line.
    (unless (and (listp mode-line-process)
                 (memq 'spinner--mode-line-construct mode-line-process))
      (setq mode-line-process
            (list (or mode-line-process "")
                  'spinner--mode-line-construct))))

  ;; Create timer.
  (when fps (setf (spinner--fps type-or-object) fps))
  (when delay (setf (spinner--delay type-or-object) delay))
  (spinner--start-timer type-or-object))

(defun spinner-start-print (spinner)
  "Like `spinner-print', but also start SPINNER if it's not active."
  (unless (spinner--active-p spinner)
    (spinner-start spinner))
  (spinner-print spinner))

(defun spinner-stop (&optional spinner)
  "Stop SPINNER, defaulting to the current buffer's spinner.
It is always safe to call this function, even if there is no
active spinner."
  (let ((spinner (or spinner spinner-current)))
    (when (spinner-p spinner)
      (let ((timer (spinner--timer spinner)))
        (when (timerp timer)
          (cancel-timer timer)))
      (setf (spinner--active-p spinner) nil)
      (force-mode-line-update))))

(provide 'spinner)

;;; spinner.el ends here
