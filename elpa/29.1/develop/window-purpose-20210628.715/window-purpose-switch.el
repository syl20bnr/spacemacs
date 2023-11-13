;;; window-purpose-switch.el --- Purpose-aware display handling -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Bar Magal & contributors

;; Author: Bar Magal
;; Package: purpose

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'window-purpose-core)
(require 'window-purpose-utils)

(defvar purpose-action-function-ignore-buffer-names
  '("^\\*Completions\\*$"
    "^\\*Ido Completions\\*$"
    ;; `ispell' uses *Choices* buffer
    "^\\*Choices\\*$")
  "Names of buffers for which the default `display-buffer' and
`switch-to-buffer' behavior should not be overridden.  This is a list of
names.")

(defcustom purpose-display-fallback 'pop-up-window
  "Fallback action to use when `purpose--action-function' couldn't
display a buffer.
This should be either `pop-up-window' for displaying the buffer in a new
window, `pop-up-frame' for displaying the buffer in a new frame, `error'
for signalling an error, or nil for using the regular (purpose-less)
`display-buffer' behavior.
`purpose-display-fallback' can also be a display function.  In this
case, the function is called with two arguments: BUFFER and ALIST, and
should return the window that it used to display the buffer.
Any other value is treated the same as nil."
  :group 'purpose
  :type '(choice (const pop-up-window)
                 (const pop-up-frame)
                 (const error)
                 (const nil)
                 function)
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-display-buffer-functions nil
  "Hook to run after displaying a buffer with `purpose--action-function'.
This hook is called with one argument - the window used for display."
  :group 'purpose
  :type 'hook
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-select-buffer-hook nil
  "Hook to run after selecting a buffer with `purpose-select-buffer'."
  :group 'purpose
  :type 'hook
  :package-version '(window-purpose . "1.2"))

(defvar purpose--active-p nil
  "When nil, Purpose's advices and `purpose--action-function' are not
used.  This is an internal variable, don't set it yourself.")

(defvar purpose--alist nil
  "Variable used by Purpose's display functions for setting an alist for
`purpose--action-function'.  This is an internal variable, don't change
it yourself.")

(defvar purpose-action-sequences
  '((switch-to-buffer . (purpose-display-reuse-window-buffer
                         purpose-display-reuse-window-purpose
                         purpose-display-maybe-same-window
                         purpose-display-maybe-other-window
                         purpose-display-maybe-other-frame
                         purpose-display-maybe-pop-up-window
                         purpose-display-maybe-pop-up-frame))
    (prefer-same-window . (purpose-display-maybe-same-window
                           purpose-display-reuse-window-buffer
                           purpose-display-reuse-window-purpose
                           purpose-display-maybe-other-window
                           purpose-display-maybe-other-frame
                           purpose-display-maybe-pop-up-window
                           purpose-display-maybe-pop-up-frame))
    (force-same-window . (purpose-display-maybe-same-window))
    (prefer-other-window . (purpose-display-reuse-window-buffer
                            purpose-display-reuse-window-purpose
			    ;; only pops if `pop-up-frames' says so
                            purpose-display-maybe-pop-up-frame
			    ;; only pops when sensible (`split-window-sensibly')
                            purpose-display-maybe-pop-up-window
                            purpose-display-maybe-other-window
                            purpose-display-maybe-other-frame
                            purpose-display-maybe-same-window))
    (prefer-other-frame . (purpose-display-reuse-window-buffer-other-frame
                           purpose-display-reuse-window-purpose-other-frame
                           purpose-display-maybe-other-frame
                           purpose-display-maybe-pop-up-frame
                           purpose-display-maybe-other-window
                           purpose-display-maybe-pop-up-window
                           purpose-display-reuse-window-buffer
                           purpose-display-reuse-window-purpose
                           purpose-display-maybe-same-window))))

(defvar purpose-default-action-order 'prefer-other-window)

(defvar purpose-special-action-sequences nil
  "This variable makes Purpose handle some buffers differently.
`purpose-special-action-sequences' should be a list.  Each entry in the
list is a list itself, where:
1. the entry's first item (entry's car) is a condition
2. the entry's rest items (entry's cdr) are display functions
Condition is either a purpose, or a predicate function that takes 3
arguments: PURPOSE, BUFFER, ALIST.
When `purpose--action-function' tries to display a buffer, it will try
first the action sequences in `purpose-special-action-sequences' whose
condition was met.")

(defcustom purpose-display-at-top-height 8
  "Height for new windows created by `purpose-display-at-top'.
This should be either a positive integer or a percentage between 0 to 1.
If it is a positive integer, it is the number of lines in new windows
created by `purpose-display-at-top'.  If it is a percentage, the height
of new windows will be that percentage out of the frame's total height.
`purpose-display-at-top-height' can also have a value of nil.  In this
case, `purpose-display-at-top-height' is ignored."
  :group 'purpose
  :type '(choice number
                 (const nil))
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-display-at-bottom-height 8
  "Height for new windows created by `purpose-display-at-bottom'.
This should be either a positive integer or a percentage between 0 to 1.
If it is a positive integer, it is the number of lines in new windows
created by `purpose-display-at-bottom'.  If it is a percentage, the
height of new windows will be that percentage out of the frame's total
height.
`purpose-display-at-bottom-height' can also have a value of nil.  In
this case, `purpose-display-at-bottom-height' is ignored."
  :group 'purpose
  :type '(choice number
                 (const nil))
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-display-at-left-width 32
  "Width for new windows created by `purpose-display-at-left'.
This should be either a positive integer or a percentage between 0 to 1.
If it is a positive integer, it is the number of lines in new windows
created by `purpose-display-at-left'.  If it is a percentage, the width
of new windows will be that percentage out of the frame's total width.
`purpose-display-at-left-width' can also have a value of nil.  In this
case, `purpose-display-at-left-width' is ignored."
  :group 'purpose
  :type '(choice number
                 (const nil))
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-display-at-right-width 32
  "Width for new windows created by `purpose-display-at-right'.
This should be either a positive integer or a percentage between 0 to 1.
If it is a positive integer, it is the number of lines in new windows
created by `purpose-display-at-right'.  If it is a percentage, the
width of new windows will be that percentage out of the frame's total
width.
`purpose-display-at-right-width' can also have a value of nil.  In
this case, `purpose-display-at-right-width' is ignored."
  :group 'purpose
  :type '(choice number
                 (const nil))
  :package-version '(window-purpose . "1.4"))



;;; Level1 actions
;; reuse-window-buffer: display buffer in a window already displaying that buffer. frames to consider are chosen by `inhibit-same-window', `reusable-frames', `display-buffer-reuse-frames' and `pop-up-frames'.
;; reuse-window-purpose: display buffer in a window already displaying correct purpose (except buffer-dedicated windows). frames to consider are chosen the same as `reuse-window-buffer'.
;; same-window: display buffer in selected window (regardless of current purpose or buffer)
;; maybe-same-window: display buffer in selected window, if possible (not dedicated)
;; maybe-other-window: display buffer in another window in the selected frame, if possible (not dedicated)
;; maybe-other-frame: display buffer in another window in another frame, if possible (not dedicated)
;; pop-up-window: display buffer in a new window in the selected frame
;; -- how should we split the frame? should we consider other frames as well
;; maybe-pop-up-window: display buffer in a new window in the selected frame, if possible (window can be split)
;; pop-up-frame: display buffer in a new frame

(defun purpose-change-buffer (buffer window type &optional alist dedicated)
  "Display BUFFER in WINDOW, but don't select it.
BUFFER, WINDOW, TYPE, ALIST and DEDICATED have the same meaning
as in `window--display-buffer'.

DEDICATED is ignored for Emacs versions in which
`window--display-buffer' doesn't support a DEDICATED
argument (i.e. version 27)."
  (if (version< emacs-version "27")
      (window--display-buffer buffer window type alist dedicated)
    ;; optional argument `dedicated' was removed in emacs 27 development branch
    ;; (as of 2019-01-14). This is a temporary fix to be re-evaluated once emacs
    ;; 27 development reaches the pretest phase. (docstring to be re-evaluated
    ;; as well)
    (window--display-buffer buffer window type alist)))

(defun purpose-window-buffer-reusable-p (window buffer)
  "Return non-nil if WINDOW can be reused to display BUFFER.
WINDOW can be reused if it already show BUFFER."
  (eql (window-buffer window) buffer))

(defun purpose-window-purpose-reusable-p (window purpose)
  "Return non-nil of WINDOW can be reused for PURPOSE.
WINDOW can be reused if it isn't buffer-dedicated and if it already has
the purpose PURPOSE."
  (and (not (window-dedicated-p window))
       (eql purpose (purpose-window-purpose window))))

(defun purpose--reusable-frames (alist)
  "Return a list of reusable frames.
If ALIST contains a `reusable-frames' entry, its value determines which
frames to search for a reusable window:
  nil -- the selected frame
  A frame -- just that frame
  `visible' -- all visible frames
  0 -- all frames on the current terminal
  t -- all frames.

If ALIST contains no `reusable-frames' entry, search just the selected
frame if `pop-up-frames' is nil; search all frames on the current
terminal if it's non-nil."
  (let-alist alist
    (let ((reusable-frames (cond ((assoc 'reusable-frames alist)
                                  .reusable-frames)
                                 (pop-up-frames 0)
                                 (t nil))))
      (cond ((null reusable-frames)
             (list (selected-frame)))
            ((framep reusable-frames)
             (list reusable-frames))
            ((eql reusable-frames 'visible)
             (visible-frame-list))
            ((eql reusable-frames 0)
             (cl-remove-if-not
              #'(lambda (frame)
                  (eql (frame-terminal frame) (frame-terminal)))
              (frame-list)))
            ((eql reusable-frames t)
             (frame-list))
            (t
             (message "Bad value for reusable-frames in ALIST: %S"
                      reusable-frames)
             nil)))))

(defun purpose--pick-selected-or-any-window (windows)
  (car (or (memq (selected-window) windows)
	   windows)))

(defun purpose-display-reuse-window-buffer (buffer alist)
  "Return a window that is already displaying BUFFER.
Return nil if no usable window is found.

If ALIST has a non-nil `inhibit-same-window' entry, the selected window
is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame."
  (let-alist alist
    (let* ((frames (purpose--reusable-frames alist))
           (windows (purpose-flatten (mapcar #'window-list frames)))
           window)
      (setq windows (cl-delete-if-not
                     #'(lambda (window)
                         (purpose-window-buffer-reusable-p window buffer))
                     windows))
      (when .inhibit-same-window
        (setq windows (delq (selected-window) windows)))
      (setq window (purpose--pick-selected-or-any-window windows))
      (when window
        (purpose-change-buffer buffer window 'reuse alist))
      window)))

(defun purpose-display-reuse-window-purpose (buffer alist &optional purpose)
  "Display BUFFER in a window that is already used for purpose PURPOSE.
Return that window.  Return nil if no usable window is found.
Windows that are dediacted to their buffers are not eligible for reuse.

PURPOSE defaults to BUFFER's purpose.

If ALIST has a non-nil `inhibit-same-window' entry, the selected window
is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines which
frames to search for a reusable window:
  nil -- the selected frame
  A frame -- just that frame
  `visible' -- all visible frames
  0 -- all frames on the current terminal
  t -- all frames.

If ALIST contains no `reusable-frames' entry, search just the selected
frame if `pop-up-frames' is nil; search all frames on the current
terminal if it's non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the event
that a window on another frame is chosen, avoid raising that frame."
  (let-alist alist
    (let* ((frames (purpose--reusable-frames alist))
           (windows (purpose-flatten (mapcar #'window-list frames)))
           (purpose (or purpose (purpose-buffer-purpose buffer)))
           window)
      (setq windows (cl-delete-if-not
                     #'(lambda (window)
                         (purpose-window-purpose-reusable-p window purpose))
                     windows))
      (when .inhibit-same-window
        (setq windows (delq (selected-window) windows)))
      (setq window (purpose--pick-selected-or-any-window windows))
      (when window
        (purpose-change-buffer buffer window 'reuse alist))
      window)))

(defun purpose-display-reuse-window-buffer-other-frame (buffer alist)
  "Return a window that is already displaying BUFFER.
Return nil if no usable window is found.
Windows in the selected frame are not eligible for reuse, even if
`reusable-frames' says to search the selected frame.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame."
  (let* ((frames (cl-delete (selected-frame)
                            (purpose--reusable-frames alist)))
         (windows (purpose-flatten (mapcar #'window-list frames)))
         window)
    (setq windows (cl-delete-if-not
                   #'(lambda (window)
                       (purpose-window-buffer-reusable-p window buffer))
                   windows))
    (setq window (car windows))
    (when window
      (purpose-change-buffer buffer window 'reuse alist))
    window))

(defun purpose-display-reuse-window-purpose-other-frame (buffer alist &optional purpose)
  "Display BUFFER in a window that is already used for purpose PURPOSE.
Return that window.  Return nil if no usable window is found.
Windows that are dediacted to their buffers are not eligible for reuse.
Windows in the selected frame are not eligible for reuse, even if
`reusable-frames' says to search the selected frame.

PURPOSE defaults to BUFFER's purpose.

If ALIST has a non-nil `inhibit-same-window' entry, the selected window
is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines which
frames to search for a reusable window:
  nil -- the selected frame
  A frame -- just that frame
  `visible' -- all visible frames
  0 -- all frames on the current terminal
  t -- all frames.

If ALIST contains no `reusable-frames' entry, search just the selected
frame if `pop-up-frames' is nil; search all frames on the current
terminal if it's non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the event
that a window on another frame is chosen, avoid raising that frame."
  (let* ((frames (cl-delete (selected-frame)
                            (purpose--reusable-frames alist)))
         (windows (purpose-flatten (mapcar #'window-list frames)))
         (purpose (or purpose (purpose-buffer-purpose buffer)))
         window)
    (setq windows (cl-delete-if-not
                   #'(lambda (window)
                       (purpose-window-purpose-reusable-p window purpose))
                   windows))
    (setq window (car windows))
    (when window
      (purpose-change-buffer buffer window 'reuse alist))
    window))

(defun purpose-display-same-window (buffer alist)
  "Display BUFFER in selected window, no matter what.
This function ignores window dedication and any entry in ALIST."
  (purpose-change-buffer buffer (selected-window) 'reuse alist)
  (selected-window))

(defun purpose-display-maybe-same-window (buffer alist)
  "Display BUFFER in selected window, if possible.
Return selected window if BUFFER was displayed, otherwise nil.
It is not possible to display BUFFER in selected window if any of
following is true:
- selected window is dedicated to its buffer, and that buffer is not
  BUFFER itself
- selected window is dedicated to its purpose, and BUFFER has a
  different purpose
- entry `inhibit-same-window' in ALIST is non-nil"
  (let-alist alist
    (unless (or (window-dedicated-p)
                (and (purpose-window-purpose-dedicated-p)
                     (not (eql (purpose-window-purpose)
                               (purpose-buffer-purpose buffer))))
                .inhibit-same-window
                (window-minibuffer-p))
      (purpose-display-same-window buffer alist))))

(defun purpose-display--frame-usable-windows (frame buffer)
  "Return windows in FRAME that can be used to display BUFFER.
Possible windows to use match these requirements:
- window is not dediacted to its buffer
- window is not dediacted to its purpose, or BUFFER has the same purpose

FRAME defaults to the selected frame."
  (cl-remove-if-not
   #'(lambda (window)
       (and (or (not (window-dedicated-p window))
                (eql (window-buffer window) buffer))
            (or (not (purpose-window-purpose-dedicated-p window))
                (eql (purpose-window-purpose window)
                     (purpose-buffer-purpose buffer)))
            (not (window-minibuffer-p window))))
   (window-list frame)))

(defun purpose-display-maybe-other-window (buffer alist)
  "Disply BUFFER in another window in the selected frame, if possible.
Return that window. Return nil if no usable window is found.
Possible windows to use match these requirements:
- window is not dedicated to its buffer
- window is not dedicated to its purpose, or BUFFER has the same purpose"
  (let-alist alist
    (let ((windows (purpose-display--frame-usable-windows nil buffer))
          window)
      ;; (when .inhibit-same-window
      ;;   (setq windows (delete (selected-window) windows)))
      (setq windows (delete (selected-window) windows))
      (setq window (car windows))
      (when window
        (purpose-change-buffer buffer window 'reuse alist)
        window))))

(defun purpose-display-maybe-other-frame (buffer alist)
  "Display BUFFER in another window in another frame, if possible.
Return that window. Return nil if no usable window is found.
Possible windows to use match these requirements:
- window is not dedicated to its buffer
- window is no dedicated to its purpose, or BUFFER has the same purpose

This function doesn't raise the new frame."
  (let-alist alist
    (let* ((windows (purpose-flatten
                     (mapcar
                      #'(lambda (frame)
                          (purpose-display--frame-usable-windows frame buffer))
                      (remove (selected-frame) (frame-list)))))
           (window (car windows)))
      (when window
        (purpose-change-buffer buffer window 'reuse alist)
        window))))

(defun purpose-display-pop-up-window--internal (buffer alist force-split)
  "Display BUFFER in a new window.
If possible, the window is split in a sensible way.  Otherwise, if
FORCE-SPLIT is non-nil, the window is split vertically.
The window that is split is either the largest window, or the least
recently used window.  If couldn't get the largest or least recently
used window, split the selected window."
  (let* ((old-window (or (get-largest-window nil t)
                         (get-lru-window nil t)
                         (selected-window)))
         (new-window (or (split-window-sensibly old-window)
                         (and force-split
                              (split-window old-window)))))
    (when new-window
      (purpose-change-buffer buffer new-window 'window alist
			     display-buffer-mark-dedicated)
      new-window)))

(defun purpose-display-pop-up-window (buffer alist)
  "Display BUFFER in a new window.
The value of `pop-up-windows' is ignored.  If possible, the window is
split in a sensible way.  Otherwise, it is simply split vertically.
The window that is split is either the largest window, or the least
recently used window.  If couldn't get the largest or least recently
used window, split the selected window."
  (purpose-display-pop-up-window--internal buffer alist t))

(defun purpose-display-maybe-pop-up-window (buffer alist)
  "Display BUFFER in a new window, if possible.
The display is possible if `pop-up-windows' is non-nil.  The
display is done similar to `display-buffer-pop-up-window'."
  (when pop-up-windows
    (purpose-display-pop-up-window--internal buffer alist nil)))

(defun purpose-display-pop-up-frame (buffer alist)
  "Display BUFFER in a new frame.
This works by calling `pop-up-frame-function'.  If successful, return
the window used.  Otherwise return nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, avoid raising the
new frame.

Variable `pop-up-frame-alist' is an alist of frame parameters for the
new frame.  If ALIST has a non-nil `pop-up-frame-parameters' entry, its
value should be an alist of frame parameters to give the new frame.  The
values of `pop-up-frame-alist' and `pop-up-frame-parameters' are used
both.  In case of conflict, `pop-up-frame-parameters' takes precedence."
  (let-alist alist
    (let* ((pop-up-frame-alist (purpose-alist-combine .pop-up-frame-parameters
                                                      pop-up-frame-alist))
           (frame (when pop-up-frame-function
                    (with-current-buffer buffer
                      (funcall pop-up-frame-function))))
           (window (and frame (frame-selected-window frame))))
      (when window
        (purpose-change-buffer buffer window 'frame alist
			       display-buffer-mark-dedicated)))))

(defun purpose-display-maybe-pop-up-frame (buffer alist)
  "Display BUFFER in a new frame, if possible.
The display is possible if `pop-up-frames' is non-nil (and not `graphic-only'
on a text-only terminal).
The display is done with `display-buffer-pop-up-frame'."
  ;; if `pop-up-frames' is `graphic-only', check `display-graphic-p', otherwise
  ;; check that `pop-up-frames' is non-nil
  (when (if (eq pop-up-frames 'graphic-only)
            (display-graphic-p)
          pop-up-frames)
    (purpose-display-pop-up-frame buffer alist)))

(defun purpose--normalize-height (height &optional frame)
  "Convert HEIGHT into a number of lines.
HEIGHT can be either a positive integer (number of lines), a percentage
 (number of lines relative to FRAME's height) or nil.  If HEIGHT is nil,
nil is returned, otherwise return the number of lines represented by
HEIGHT.
FRAME defaults to the selected frame."
  (cond
   ;; null height, return as it is
   ((null height)
    height)
   ;; height is a percentage, convert it to number of lines
   ((and (< 0 height) (< height 1))
    (round (* height (frame-height frame))))
   ;; height is an integer, return it as it is
   ((and (integerp height) (> height 0))
    height)
   ;; wrong argument type
   (t
    (signal 'wrong-type-argument `("positive integer or percentage" ,height)))))

(defun purpose--normalize-width (width &optional frame)
  "Convert WIDTH into a number of lines.
WIDTH can be either a positive integer (number of lines), a percentage
 (number of lines relative to FRAME's width) or nil.  If WIDTH is nil,
nil is returned, otherwise return the number of lines represented by
WIDTH.
FRAME defaults to the selected frame."
  (cond
   ;; null width, return as it is
   ((null width)
    width)
   ;; width is a percentage, convert it to number of lines
   ((and (< 0 width) (< width 1))
    (round (* width (frame-width frame))))
   ;; width is an integer, return it as it is
   ((and (integerp width) (> width 0))
    width)
   ;; wrong argument type
   (t
    (signal 'wrong-type-argument `("positive integer or percentage" ,width)))))

(defun purpose-display--at (window-getter window-creator buffer alist)
  "Try to display a buffer in an existing window or in a new window.
If the window returned by WINDOW-GETTER already displays BUFFER, or has
the same purpose as BUFFER and is not buffer-dedicated, use it to
display BUFFER.  Otherwise, call WINDOW-CREATOR to create a new window,
and display BUFFER in the new WINDOW.

WINDOW-GETTER specifies which existing window to reuse for display.  It
should be a function that takes no arguments and returns a live window.
WINDOW-CREATOR specifies how to create a new window for display, if
necessary.  It should be a function that takes no arguments and returns
a live window.
BUFFER is a buffer that should be displayed.
ALIST has the same meaning as in `display-buffer'."
  (let ((window (funcall window-getter)))
    (if (and window
             (or (purpose-window-buffer-reusable-p window buffer)
                 (purpose-window-purpose-reusable-p window
                                                    (purpose-buffer-purpose
                                                     buffer))))
        ;; reuse window
        (progn
          (purpose-change-buffer buffer window 'reuse alist)
          window)
      ;; create window
      (let ((new-window (funcall window-creator)))
        (when new-window
          (purpose-change-buffer buffer new-window 'window alist)
          new-window)))))

(defun purpose-display-at-top (buffer alist &optional height)
  "Display BUFFER at the top window, create such window if necessary.
\"top window\" is a window as returned by `purpose-get-top-window'.
ALIST is for compatibility with `display-buffer' and is ignored.
HEIGHT specifies the height of the new window, if a new window needs to
be created, and can take the same values as
`purpose-display-at-top-height'.  If HEIGHT is nil, then the height of
the new window is specified by `purpose-display-at-top-height'.  If
`purpose-display-at-top-height' is also nil, then the new window will
have the default height."
  (purpose-display--at
   #'purpose-get-top-window
   #'(lambda ()
       (let* ((height (purpose--normalize-height
                       (or height
                           purpose-display-at-top-height)))
              (height (when height (- height))))
         (ignore-errors
           (split-window (frame-root-window) height 'above))))
   buffer
   alist))

(defun purpose-display-at-bottom (buffer alist &optional height)
  "Display BUFFER at the bottom window, create such window if necessary.
\"bottom window\" is a window as returned by
`purpose-get-bottom-window'.
ALIST is for compatibility with `display-buffer' and is ignored.
HEIGHT specifies the height of the new window, if a new window needs to
be created, and can take the same values as
`purpose-display-at-bottom-height'.  If HEIGHT is nil, then the height
of the new window is specified by `purpose-display-at-bottom-height'.
If `purpose-display-at-bottom-height' is also nil, then the new window
will have the default height."
  (purpose-display--at
   #'purpose-get-bottom-window
   #'(lambda ()
       (let* ((height (purpose--normalize-height
                       (or height
                           purpose-display-at-bottom-height)))
              (height (when height (- height))))
         (ignore-errors
           (split-window (frame-root-window) height 'below))))
   buffer
   alist))

(defun purpose-display-at-left (buffer alist &optional width)
  "Display BUFFER at the left window, create such window if necessary.
\"left window\" is a window as returned by `purpose-get-left-window'.
ALIST is for compatibility with `display-buffer' and is ignored.
WIDTH specifies the width of the new window, if a new window needs to
be created, and can take the same values as
`purpose-display-at-left-width'.  If WIDTH is nil, then the width of the
new window is specified by `purpose-display-at-left-width'.  If
`purpose-display-at-left-width' is also nil, then the new window will
have the default width."
  (purpose-display--at
   #'purpose-get-left-window
   #'(lambda ()
       (let* ((width (purpose--normalize-width
                      (or width
                          purpose-display-at-left-width)))
              (width (when width (- width))))
         (ignore-errors
           (split-window (frame-root-window) width 'left))))
   buffer
   alist))

(defun purpose-display-at-right (buffer alist &optional width)
  "Display BUFFER at the right window, create such window if necessary.
\"right window\" is a window as returned by `purpose-get-right-window'.
ALIST is for compatibility with `display-buffer' and is ignored.
WIDTH specifies the width of the new window, if a new window needs to be
created, and can take the same values as
`purpose-display-at-right-width'.  If WIDTH is nil, then the width of
the new window is specified by `purpose-display-at-right-width'.  If
`purpose-display-at-right-width' is also nil, then the new window will
have the default width."
  (purpose-display--at
   #'purpose-get-right-window
   #'(lambda ()
       (let* ((width (purpose--normalize-width
                      (or width
                          purpose-display-at-right-width)))
              (width (when width (- width))))
         (ignore-errors
           (split-window (frame-root-window) width 'right))))
   buffer
   alist))



;;; Level2 actions

(defun purpose--use-action-function-p (buffer alist)
  "Determine whether `purpose--action-function' should run or not."
  (and
   purpose--active-p
   (not (cdr (assoc 'inhibit-purpose alist)))
   (let ((buffer-name (buffer-name buffer)))
     (cl-loop for ignored-regexp
              in purpose-action-function-ignore-buffer-names
              never (string-match-p ignored-regexp buffer-name)))))

(defun purpose--special-action-sequence (buffer alist)
  "Return special action sequences to use for display BUFFER.
This function loops over list `purpose-special-action-sequences' and for
each entry in the list:
- check if the entry's condition (entry's car):
  a. is equal to BUFFER's purpose, or
  b. is a function that returns non-nil when called with these 3
     arguments: buffer's purpose, BUFFER, ALIST.
- if so, append the entry's action sequence to the result
The function returns a list of display functions that
`purpose--action-function' should use before trying the regular action
sequence."
  (let ((purpose (purpose-buffer-purpose buffer)))
    (purpose-flatten
     (cl-loop
      for (condition . action-sequence) in purpose-special-action-sequences
      when (or (eql purpose condition)
               (and (functionp condition)
                    (funcall condition purpose buffer alist)))
      collect action-sequence))))

;; Purpose action function (integration with `display-buffer')
(defun purpose--action-function (buffer alist)
  "Action function to use for overriding default display-buffer
behavior.
This function should be used by setting
`display-buffer-overriding-action' to (purpose--action-function . nil).
If ALIST is nil, it is ignored and `purpose--alist' is used instead."
  (setq alist (purpose-alist-combine alist purpose--alist))
  (purpose-message "Purpose display: Buffer: %S; Alist: %S" buffer alist)
  (when (purpose--use-action-function-p buffer alist)
    (let-alist alist
      (let* ((user-action-sequence .user-action-sequence)
             (special-action-sequence (purpose--special-action-sequence buffer
                                                                        alist))
             (normal-action-sequence (purpose-alist-get
                                      (or .action-order
                                          purpose-default-action-order)
                                      purpose-action-sequences))
             (action-sequence (append user-action-sequence
                                      special-action-sequence
                                      normal-action-sequence))
             (new-window
              ;; call all display actions in action-sequence until one of them
              ;; succeeds, and return the window used for display (action's
              ;; return value)
              (cl-do ((action-sequence action-sequence (cdr action-sequence))
                      (window nil
                              (progn
                                (purpose-message "trying: %S"
                                                 (car action-sequence))
                                (funcall (car action-sequence) buffer alist))))
                  ((or (null action-sequence) window) window))))
        (let ((window (if new-window
                          new-window
                        (cond
                         ((eql purpose-display-fallback 'pop-up-frame)
                          (purpose-message
                           "trying fallback: purpose-display-pop-up-frame")
                          (purpose-display-pop-up-frame buffer alist))

                         ((eql purpose-display-fallback 'pop-up-window)
                          (purpose-message
                           "trying fallback: purpose-display-pop-up-window")
                          (purpose-display-pop-up-window buffer alist))

                         ((or (eql purpose-display-fallback 'error)
                              (eql .action-order 'force-same-window))
                          (error "No window available"))

                         ((functionp purpose-display-fallback)
                          (purpose-message
                           "trying custom fallback %s" purpose-display-fallback)
                          (funcall purpose-display-fallback buffer alist))

                         (t
                          (purpose-message
                           "falling back to regular display-buffer")
                          nil)))))
          ;; window can be a non-nil non-window object. e.g.
          ;; `display-buffer-no-window' returns 'fail (`display-buffer' treats
          ;; 'fail specially)
          (when (windowp window)
            (prog1 window
              (run-hook-with-args 'purpose-display-buffer-functions
                                  window))))))))

(defun purpose-select-buffer (buffer-or-name &optional action-order norecord)
  "Display buffer BUFFER-OR-NAME in window and then select that window.
ACTION-ORDER is used as the `action-order' entry in
`purpose--action-function''s alist.
This function runs hook `purpose-select-buffer-hook' when its done."
  (let* ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
         (purpose--alist (purpose-alist-set 'action-order
                                            action-order
                                            purpose--alist))
         (old-frame (selected-frame))
         (new-window (display-buffer buffer-or-name))
         (new-frame (window-frame new-window)))
    (when new-window
      ;; If we chose another frame, make sure it gets input focus. - taken from
      ;; `pop-to-buffer''s code
      (unless (eq new-frame old-frame)
        (select-frame-set-input-focus new-frame norecord))
      (select-window new-window norecord))
    (run-hooks 'purpose-select-buffer-hook)
    buffer))


;;; Level3 actions

;;;###autoload
(defun purpose-switch-buffer (buffer-or-name
                              &optional norecord force-same-window)
  "Select buffer BUFFER-OR-NAME, preferably in the selected window.
If FORCE-SAME-WINDOW is non-nil, don't select a different window if the
currently selected window is not available.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  ;; `display-buffer' should call `purpose--action-function', and
  ;; `purpose--action-function' should try to switch buffer in current window,
  ;; and if that's impossible - display buffer in another window.
  (purpose-select-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                         (if force-same-window
                             'force-same-window
                           'switch-to-buffer)
                         norecord))

;;;###autoload
(defun purpose-switch-buffer-other-window (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME in another window.
Never selects the currently selected window.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (let ((pop-up-windows t)
        (purpose--alist (purpose-alist-set 'inhibit-same-window
                                           t
                                           purpose--alist)))
    (purpose-select-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                           'prefer-other-window
                           norecord)))

;;;###autoload
(defun purpose-switch-buffer-other-frame (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME, preferably in another frame.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (let ((pop-up-frames t)
        (purpose--alist (purpose-alist-set 'inhibit-same-window
                                           t
                                           purpose--alist)))
    (purpose-select-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                           'prefer-other-frame
                           norecord)))

;;;###autoload
(defun purpose-pop-buffer (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME, preferably in another window.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (purpose-select-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                         'prefer-other-window
                         norecord))

;;;###autoload
(defun purpose-pop-buffer-same-window (buffer-or-name &optional norecord)
  "Select buffer BUFFER-OR-NAME, preferably in the selected window.
If BUFFER-OR-NAME is nil, select the buffer returned by `other-buffer'."
  (interactive (list (read-buffer-to-switch "[PU] Switch to buffer: ")))
  (purpose-select-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                         'prefer-same-window
                         norecord))



;;; Overrides (advices)

(defun purpose-display--action-to-order (action)
  "Return appropriate `action-order' value for ACTION."
  (unless (listp action)            ; non-nil, non-list
    'prefer-other-window))

(defun purpose-display--action-to-sequence (action)
  "Return appropriate action sequence for ACTION.
If ACTION is not t, it should be a list whose car is a function or a
list of functions, as described in `display-buffer'.  In such case,
treat the funcion(s) as an action sequence."
  (when (listp action)
    (let ((fn (car action)))
      (cond
       ((functionp fn) (list fn))
       ((listp fn) fn)
       (t (error "Unrecognized display action '%s'" action))))))

(defun purpose-display-buffer-advice
    (oldfun buffer-or-name &optional action frame)
  "Update `purpose--alist' when calling `display-buffer'."
  (let ((action-order (purpose-display--action-to-order action))
        (user-action-sequence (purpose-display--action-to-sequence action))
        (purpose--alist purpose--alist))
    (when action-order
      (setq purpose--alist
            (purpose-alist-set 'action-order action-order purpose--alist)))
    (when user-action-sequence
      (setq purpose--alist (purpose-alist-set 'user-action-sequence
                                              user-action-sequence
                                              purpose--alist)))
    (funcall oldfun buffer-or-name action frame)))

(defun purpose-switch-to-buffer-advice
    (oldfun buffer-or-name &optional norecord force-same-window)
  "Advice for overriding `switch-to-buffer' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-switch-buffer', otherwise call `switch-to-buffer'."
  (purpose-message "switch-to-buffer advice")
  ;; check the full `purpose--use-action-function-p' here, because
  ;; if purpose shouldn't be used for some reason (such as
  ;; `purpose-action-function-ignore-buffer-names'), then we want
  ;; to fallback to `switch-to-buffer', instead of
  ;; `display-buffer'
  (if (purpose--use-action-function-p (window-normalize-buffer-to-switch-to
                                       buffer-or-name)
                                      nil)
      (purpose-switch-buffer buffer-or-name
                             norecord
                             ;; when `switch-to-buffer' is called
                             ;; interactively force-same-window is non-nil,
                             ;; but want it to be nil, so we check
                             ;; `called-interactively-p' as well
                             (and force-same-window
                                  (not (called-interactively-p 'interactive))
                                  ;; `ivy--switch-buffer-action' replicates the
                                  ;; interactive behavior, so handle the same as
                                  ;; an interactive call
                                  (not (member 'ivy--switch-buffer-action
                                               (purpose--function-stack)))))
    (funcall oldfun buffer-or-name norecord force-same-window)))

(defun purpose-switch-to-buffer-other-window-advice
    (oldfun buffer-or-name &optional norecord)
  "Advice for overriding `switch-to-buffer-other-window' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-switch-buffer-other-window', otherwise call
`switch-to-buffer-other-window'."
  (purpose-message "switch-to-buffer-other-window advice")
  (if purpose--active-p
      (purpose-switch-buffer-other-window buffer-or-name norecord)
    (funcall oldfun buffer-or-name norecord)))

(defun purpose-switch-to-buffer-other-frame-advice
    (oldfun buffer-or-name &optional norecord)
  "Advice for overriding `switch-to-buffer-other-frame' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-switch-buffer-other-frame', otherwise call
`switch-to-buffer-other-frame'."
  (purpose-message "switch-to-buffer-other-frame advice")
  (if purpose--active-p
      (purpose-switch-buffer-other-frame buffer-or-name norecord)
    (funcall oldfun buffer-or-name norecord)))

(defun purpose-pop-to-buffer-advice
    (oldfun buffer-or-name &optional action norecord)
  "Advice for overriding `pop-to-buffer' conditionally.
If Purpose is active (`purpose--active-p' is non-nil) and ACTION is nil,
call `purpose-pop-buffer', otherwise call `pop-to-buffer'."
  (purpose-message "pop-to-buffer advice")
  (if (and purpose--active-p
           (not action))
      (purpose-pop-buffer buffer-or-name norecord)
    (funcall oldfun buffer-or-name action norecord)))

(defun purpose-pop-to-buffer-same-window-advice
    (oldfun buffer-or-name &optional norecord)
  "Advice for overriding `pop-to-buffer-same-window' conditionally.
If Purpose is active (`purpose--active-p' is non-nil), call
`purpose-pop-buffer-same-window', otherwise call
`pop-to-buffer-same-window'."
  (purpose-message "pop-to-buffer-same-window advice")
  (if purpose--active-p
      (purpose-pop-buffer-same-window buffer-or-name norecord)
    (funcall oldfun buffer-or-name norecord)))

;; anti-override:

(defmacro without-purpose (&rest body)
  "Make Purpose inactive while executing BODY.
This works internally by temporarily setting `purpose--active-p'."
  (declare (indent defun) (debug body))
  `(let ((purpose--active-p nil))
     ,@body))

(defmacro without-purpose-command (command)
  "Create a command that runs COMMAND with purpose inactive.
This works internally by using `without-purpose' and
`call-interactively'."
  (declare (indent defun) (debug function-form))
  `(lambda ()
     (interactive)
     (without-purpose
       (call-interactively ,command))))



;;; Advanced switching functions

(defun purpose-read-buffers-with-purpose (purpose)
  "Prompt the user for a buffer with purpose PURPOSE."
  (completing-read
   "[PU] Buffer: "
   (mapcar #'buffer-name
           (delq (current-buffer)
                 (purpose-buffers-with-purpose purpose)))))

;;;###autoload
(defun purpose-switch-buffer-with-purpose (&optional purpose)
  "Prompt the user and switch to a buffer with purpose PURPOSE.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose."
  (interactive)
  (purpose-switch-buffer
   (purpose-read-buffers-with-purpose
    (or purpose (purpose-buffer-purpose (current-buffer))))))

;;;###autoload
(defun purpose-switch-buffer-with-some-purpose (purpose)
  "Like `purpose-switch-buffer-with-purpose', but first choose a PURPOSE."
  (interactive
   (list (purpose-read-purpose "Purpose: "
                               (cl-delete-if-not #'purpose-buffers-with-purpose
                                                 (purpose-get-all-purposes))
                               t)))
  (purpose-switch-buffer-with-purpose purpose))

;;;###autoload
(defun purpose-switch-buffer-with-purpose-other-window (&optional purpose)
  "Prompt the user and switch to a buffer with purpose PURPOSE.
The buffer is display in another window.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose."
  (interactive)
  (purpose-switch-buffer-other-window
   (purpose-read-buffers-with-purpose
    (or purpose (purpose-buffer-purpose (current-buffer))))))

;;;###autoload
(defun purpose-switch-buffer-with-purpose-other-frame (&optional purpose)
  "Prompt the user and switch to a buffer with purpose PURPOSE.
The buffer is display in another frame.
If called interactively, or with PURPOSE nil, PURPOSE defaults to the
current buffer's purpose."
  (interactive)
  (purpose-switch-buffer-other-frame
   (purpose-read-buffers-with-purpose
    (or purpose (purpose-buffer-purpose (current-buffer))))))


(defmacro purpose-generate-display-and-dedicate (display-fn &rest extra-args)
  "Generate lambda to display a buffer and purpose-dedicate its window.
The generated lambda takes two arguments - BUFFER and ALIST.  It
tries to display BUFFER, and if successful it purpose-dedicates the
window used for display.

DISPLAY-FN is the function used for displaying the buffer.  EXTRA-ARGS
is additional arguments to pass to DISPLAY-FN, and should be a list.
The display is done by calling DISPLAY-FN with arguments BUFFER, ALIST
and EXTRA-ARGS, like so:
  (apply display-fn buffer alist extra-args).

Example of how this macro might be used:
  (defalias 'display-at-bottom-and-dedicate
            (purpose-generate-display-and-dedicate
             'purpose-display-at-bottom))
Another example:
  (add-to-list purpose-special-action-sequences
               `(terminal ,(purpose-generate-display-and-dedicate
                            purpose-display-at-bottom 6)))"
  (declare (indent defun) (debug (function-form &rest sexp)))
  `(lambda (buffer alist)
     (let ((window (apply ,display-fn buffer alist (list,@extra-args))))
       (when window
         (purpose-set-window-purpose-dedicated-p window t))
       window)))

(defmacro purpose-generate-display-and-do (display-fn do-fn)
  "Generate a lambda to display a buffer and execute additional actions.
The generated lambda receives two arguments - buffer and alist - and can
be used as a display function.
The buffer is displayed by calling DISPLAY-FN with arguments two
arguments - buffer and alist.
If the display is successful, DO-FN is called with one argument - the
window that was used for displaying the buffer.
The lambda returns the window used for display, or nil if display was
unsuccessful.

Possible usage:
  (defalias 'display-at-left-and-do-stuff
            (purpose-generate-display-and-do
              'purpose-display-at-left
              (lambda (window) (message \"Let's do stuff!!\"))))"
  (declare (indent defun) (debug (function-form function-form)))
  `(lambda (buffer alist)
     (let ((window (funcall ,display-fn buffer alist)))
       (when window
         (funcall ,do-fn window))
       window)))



;;; change `purpose-special-action-sequences' temporarily

(defmacro purpose-with-temp-display-actions (actions &rest body)
  "Override `purpose-special-action-sequences' temporarily.
Set ACTIONS as `purpose-special-action-sequences' while BODY is executed.
`purpose-special-action-sequences' is restored after BODY is executed."
  (declare (indent 1) (debug (sexp body)))
  `(let ((purpose-special-action-sequences ,actions))
     ,@body))

(defmacro purpose-with-temp-display-action (action &rest body)
  "Override `purpose-special-action-sequences' temporarily.
Shortcut for using `purpose-with-temp-display-actions' with only one action.
ACTION should be an entry suitable for `purpose-special-action-sequences'.
BODY has the same meaning as in `purpose-with-temp-display-actions'."
  (declare (indent 1) (debug (sexp body)))
  `(purpose-with-temp-display-actions (list ,action) ,@body))

(defmacro purpose-with-additional-display-actions (actions &rest body)
  "Add to `purpose-special-action-sequences' temporarily.
ACTIONS is a list of actions that are added to
`purpose-special-action-sequences' while BODY is executed.
`purpose-special-action-sequences' is restored after BODY is executed."
  (declare (indent 1) (debug (sexp body)))
  `(let ((purpose-special-action-sequences
          (append ,actions purpose-special-action-sequences)))
     ,@body))

(defmacro purpose-with-additional-display-action (action &rest body)
  "Add to `purpose-special-action-sequences' temporarily.
Shortcut for using `purpose-with-additional-display-actions' with only one
action.
ACTION should be an entry suitable for `purpose-special-action-sequences'.
BODY has the same meaning as in `purpose-with-additional-display-actions'."
  (declare (indent 1) (debug (sexp body)))
  `(purpose-with-additional-display-actions (list ,action) ,@body))



(provide 'window-purpose-switch)
;;; window-purpose-switch.el ends here
