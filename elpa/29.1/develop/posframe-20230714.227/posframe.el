;;; posframe.el --- Pop a posframe (just a frame) at point    -*- lexical-binding:t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 1.4.2
;; Keywords: convenience, tooltip
;; Package-Requires: ((emacs "26.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; * Posframe README                                :README:

;; Posframe can pop up a frame at point, this *posframe* is a
;; child-frame connected to its root window's buffer.

;; The main advantages are:
;; 1. It is fast enough for daily usage :-)
;; 2. It works well with CJK languages.

;; More info please see: README.org

;;; Code:
;; * posframe's code                         :CODE:
(require 'cl-lib)

(defgroup posframe nil
  "Pop a posframe (just a frame) at point."
  :group 'lisp
  :prefix "posframe-")

(defcustom posframe-inhibit-double-buffering nil
  "Set the posframe's frame-parameter: inhibit-double-buffering."
  :group 'posframe
  :type 'boolean)

(defcustom posframe-mouse-banish-function #'posframe-mouse-banish-default
  "The function used to banish mouse.

Function `posframe-mouse-banish-default' will work well in most
case, but suggest use function `posframe-mouse-banish-simple' or
custom function for EXWM users."
  :type 'function)

(defvar-local posframe--frame nil
  "Record posframe's frame.")

(defvar-local posframe--last-posframe-pixel-position nil
  "Record the last pixel position of posframe's frame.")

(defvar-local posframe--last-posframe-size nil
  "Record the last size of posframe's frame.")

(defvar-local posframe--last-posframe-displayed-size nil
  "Record the last displayed size of posframe's frame.")

(defvar-local posframe--last-parent-frame-size nil
  "Record the last size of posframe's parent-frame.")

(defvar-local posframe--last-poshandler-info nil
  "Record the last poshandler info.")

(defvar-local posframe--last-font-height-info nil
  "Record the last font height info.")

(defvar-local posframe--last-args nil
  "Record the last arguments of `posframe--create-posframe'.

If these args have changed, posframe will recreate its
frame.")

(defvar-local posframe--timeout-timer nil
  "Record the timer to deal with timeout argument of `posframe-show'.")

(defvar-local posframe--refresh-timer nil
  "Record the timer to deal with refresh argument of `posframe-show'.")

(defvar-local posframe--initialized-p nil
  "Record initialize status of `posframe-show'.")

(defvar-local posframe--accept-focus nil
  "Record accept focus status of `posframe-show'.")

(defvar posframe-hidehandler-timer nil
  "Timer used by hidehandler function.")

;; Avoid compilation warnings on Emacs < 27.
(defvar x-gtk-resize-child-frames)

(defvar posframe-gtk-resize-child-frames
  (when (and
         (> emacs-major-version 26)
         (string-match-p "GTK3" system-configuration-features)
         (let ((value (or (getenv "XDG_CURRENT_DESKTOP") (getenv "DESKTOP_SESSION"))))
           (and (stringp value)
                ;; It can be "ubuntu:GNOME".
                (string-match-p "GNOME" value))))
    ;; Not future-proof, but we can use it now.
    'resize-mode)
  "Value to bind `x-gtk-resize-child-frames' to.

The value `resize-mode' only has effect on new child frames, so
if you change it, call `posframe-delete-all' for it to take
effect.")

;;;###autoload
(defun posframe-workable-p ()
  "Test posframe workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

;;;###autoload
(cl-defun posframe-show (buffer-or-name
                         &key
                         string
                         position
                         poshandler
                         poshandler-extra-info
                         width
                         height
                         max-width
                         max-height
                         min-width
                         min-height
                         x-pixel-offset
                         y-pixel-offset
                         left-fringe
                         right-fringe
                         border-width
                         border-color
                         internal-border-width
                         internal-border-color
                         font
                         cursor
                         window-point
                         foreground-color
                         background-color
                         respect-header-line
                         respect-mode-line
                         initialize
                         no-properties
                         keep-ratio
                         lines-truncate
                         override-parameters
                         timeout
                         refresh
                         accept-focus
                         hidehandler
                         refposhandler
                         &allow-other-keys)
  "Pop up a posframe to show STRING at POSITION.

 (1) POSITION

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

 (2) POSHANDLER

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-start xxx
   :parent-window-end xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :mouse-x xxx
   ;mouse-y xxx
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.

The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-top-left-or-right-other-corner'
6.  `posframe-poshandler-frame-bottom-center'
7.  `posframe-poshandler-frame-bottom-left-corner'
8.  `posframe-poshandler-frame-bottom-right-corner'
9.  `posframe-poshandler-window-center'
10.  `posframe-poshandler-window-top-center'
11. `posframe-poshandler-window-top-left-corner'
12. `posframe-poshandler-window-top-right-corner'
13. `posframe-poshandler-window-bottom-center'
14. `posframe-poshandler-window-bottom-left-corner'
15. `posframe-poshandler-window-bottom-right-corner'
16. `posframe-poshandler-point-top-left-corner'
17. `posframe-poshandler-point-bottom-left-corner'
18. `posframe-poshandler-point-bottom-left-corner-upward'
19. `posframe-poshandler-point-window-center'
20. `posframe-poshandler-point-frame-center'

 (3) POSHANDLER-EXTRA-INFO

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: `info', it will *OVERRIDE* the
exist key in `info'.

 (4) BUFFER-OR-NAME

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

buffer name can prefix with space, for example \" *mybuffer*\", so
the buffer name will hide for ibuffer and `list-buffers'.

 (5) NO-PROPERTIES

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

 (6) HEIGHT, MAX-HEIGHT, MIN-HEIGHT, WIDTH, MAX-WIDTH and MIN-WIDTH

These arguments are specified in the canonical character width
and height of posframe, more details can be found in docstring of
function `fit-frame-to-buffer',

 (7) LEFT-FRINGE and RIGHT-FRINGE

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

 (8) BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and BORDER-COLOR, but do not suggest to use for the
reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

 (9) FONT, FOREGROUND-COLOR and BACKGROUND-COLOR

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

 (10) CURSOR and WINDOW-POINT

By default, cursor is not showed in posframe, user can let cursor
showed with this argument help by set its value to a `cursor-type'.

When cursor need to be showed in posframe, user may need to set
WINDOW-POINT to the point of BUFFER, which can let cursor showed
at this point.

 (11) RESPECT-HEADER-LINE and RESPECT-MODE-LINE

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

 (12) INITIALIZE

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

 (13) LINES-TRUNCATE

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

 (14) OVERRIDE-PARAMETERS

OVERRIDE-PARAMETERS is very powful, *all* the valid frame parameters
used by posframe's frame can be overridden by it.

NOTE: some `posframe-show' arguments are not frame parameters, so they
can not be overrided by this argument.

 (15) TIMEOUT

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

 (15) REFRESH

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

 (17) ACCEPT-FOCUS

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

 (18) HIDEHANDLER

HIDEHANDLER is a function, when it return t, posframe will be
hide, this function has a plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'

 (19) REFPOSHANDLER

REFPOSHANDLER is a function, a reference position (most is
top-left of current frame) will be returned when call this
function.

when it is nil or it return nil, child-frame feature will be used
and reference position will be deal with in Emacs.

The user case I know at the moment is let ivy-posframe work well
in EXWM environment (let posframe show on the other application
window).

         DO NOT USE UNLESS NECESSARY!!!

An example parent frame poshandler function is:

1. `posframe-refposhandler-xwininfo'

 (19) Others

You can use `posframe-delete-all' to delete all posframes."
  (let* ((position (or position (point)))
         (max-width (if (numberp max-width)
                        (min max-width (frame-width))
                      (frame-width)))
         (max-height (if (numberp max-height)
                         (min max-height (frame-height))
                       (frame-height)))
         (min-width (min (or min-width 1) max-width))
         (min-height (min (or min-height 1) max-height))
         (width (when width
                  (min (max width min-width) max-width)))
         (height (when height
                   (min (max height min-height) max-height)))
         (x-pixel-offset (or x-pixel-offset 0))
         (y-pixel-offset (or y-pixel-offset 0))
         (window-point (or window-point 0))
         ;;-----------------------------------------------------
         (buffer (get-buffer-create buffer-or-name))
         (parent-window (selected-window))
         (parent-window-start (window-start parent-window))
         (parent-window-end (window-end parent-window))
         (parent-window-top (window-pixel-top parent-window))
         (parent-window-left (window-pixel-left parent-window))
         (parent-window-width (window-pixel-width parent-window))
         (parent-window-height (window-pixel-height parent-window))
         (parent-frame (window-frame parent-window))
         (parent-frame-width (frame-pixel-width parent-frame))
         (parent-frame-height (frame-pixel-height parent-frame))
         (ref-position
          (when (functionp refposhandler)
            (ignore-errors
              (funcall refposhandler parent-frame))))
         (font-width (default-font-width))
         (font-height (with-current-buffer (window-buffer parent-window)
                        (posframe--get-font-height position)))
         (mode-line-height (window-mode-line-height))
         (minibuffer-height (window-pixel-height (minibuffer-window)))
         (header-line-height (window-header-line-height parent-window))
         (tab-line-height (if (functionp 'window-tab-line-height)
                              (window-tab-line-height)
                            0))
         (mouse-position (cdr (mouse-pixel-position)))
         (frame-resize-pixelwise t)
         posframe)

    (with-current-buffer buffer

      ;; Initialize
      (unless posframe--initialized-p
        (let ((func initialize))
          (when (functionp func)
            (funcall func)
            (setq posframe--initialized-p t))))

      ;; Create posframe
      (setq posframe
            (posframe--create-posframe
             buffer
             :position position
             :font font
             :cursor cursor
             :parent-frame
             (unless ref-position
               parent-frame)
             :left-fringe left-fringe
             :right-fringe right-fringe
             :border-width border-width
             :border-color border-color
             :internal-border-width internal-border-width
             :internal-border-color internal-border-color
             :foreground-color foreground-color
             :background-color background-color
             :keep-ratio keep-ratio
             :lines-truncate lines-truncate
             :respect-header-line respect-header-line
             :respect-mode-line respect-mode-line
             :override-parameters override-parameters
             :accept-focus accept-focus))

      ;; Insert string into the posframe buffer
      (posframe--insert-string string no-properties)

      (let ((size-info
             (list :posframe posframe
                   :width width
                   :height height
                   :max-width max-width
                   :max-height max-height
                   :min-width min-width
                   :min-height min-height)))
        ;; Set posframe's size
        (posframe--set-frame-size size-info)
        ;; Re-adjust posframe's size when buffer's content has changed.
        (posframe--run-refresh-timer refresh size-info))

      ;; Get new position of posframe.
      (setq position
            (posframe-run-poshandler
             ;; All poshandlers will get info from this plist.
             `(,@poshandler-extra-info
               ,@(list :position position
                       :poshandler poshandler
                       :font-height font-height
                       :font-width font-width
                       :posframe posframe
                       :posframe-width (frame-pixel-width posframe)
                       :posframe-height (frame-pixel-height posframe)
                       :posframe-buffer buffer
                       :parent-frame parent-frame
                       :parent-frame-width parent-frame-width
                       :parent-frame-height parent-frame-height
                       :ref-position ref-position
                       :parent-window parent-window
                       :parent-window-start parent-window-start
                       :parent-window-end parent-window-end
                       :parent-window-top parent-window-top
                       :parent-window-left parent-window-left
                       :parent-window-width parent-window-width
                       :parent-window-height parent-window-height
                       :mouse-x (car mouse-position)
                       :mouse-y (cdr mouse-position)
                       :mode-line-height mode-line-height
                       :minibuffer-height minibuffer-height
                       :header-line-height header-line-height
                       :tab-line-height tab-line-height
                       :x-pixel-offset x-pixel-offset
                       :y-pixel-offset y-pixel-offset))))

      ;; Move posframe
      (posframe--set-frame-position
       posframe position parent-frame-width parent-frame-height)

      ;; Delay hide posframe when timeout is a number.
      (posframe--run-timeout-timer posframe timeout)

      ;; Make sure not hide buffer's content for scroll down.
      (let ((window (frame-root-window posframe--frame)))
        (when (window-live-p window)
          (set-window-point window window-point)))

      ;; Hide posframe when switch buffer
      (let* ((parent-buffer (window-buffer parent-window))
             (parent-buffer-name (buffer-name parent-buffer)))
        (set-frame-parameter posframe--frame 'posframe-hidehandler hidehandler)
        (set-frame-parameter posframe--frame 'posframe-parent-buffer
                             (cons parent-buffer-name parent-buffer)))

      ;; Mouse banish
      (funcall
       posframe-mouse-banish-function
       (list :parent-frame parent-frame
             :mouse-x (when (car mouse-position)
                        (+ (or (car ref-position) 0)
                           (car mouse-position)))
             :mouse-y (when (cdr mouse-position)
                        (+ (or (cdr ref-position) 0)
                           (cdr mouse-position)))
             :posframe-x
             (if (>= (car position) 0)
                 (car position)
               (- (frame-pixel-width parent-frame)
                  (frame-pixel-width posframe)))
             :posframe-y
             (if (>= (cdr position) 0)
                 (cdr position)
               (- (frame-pixel-height parent-frame)
                  (frame-pixel-height posframe)))
             :posframe-width (frame-pixel-width posframe)
             :posframe-height (frame-pixel-height posframe)
             :parent-frame-width parent-frame-width
             :parent-frame-height parent-frame-height))

      ;; Return posframe
      posframe)))

(defun posframe--get-font-height (position)
  "Get the font's height at POSITION."
  (if (eq position (car posframe--last-font-height-info))
      (cdr posframe--last-font-height-info)
    (let* ((font (when (and (integerp position)
                            (not (= position 1)))
                   (font-at (if (>= position (point-max))
                                (- (point-max) 1)
                              position))))
           (height (when (integerp position)
                     (if (or (= position 1) (not (fontp font)))
                         (default-line-height)
                       (aref (font-info font) 3)))))
      (setq posframe--last-font-height-info
            (cons position height))
      height)))

(cl-defun posframe--create-posframe (buffer-or-name
                                     &key
                                     position
                                     parent-frame
                                     foreground-color
                                     background-color
                                     left-fringe
                                     right-fringe
                                     border-width
                                     border-color
                                     internal-border-width
                                     internal-border-color
                                     font
                                     cursor
                                     keep-ratio
                                     lines-truncate
                                     override-parameters
                                     respect-header-line
                                     respect-mode-line
                                     accept-focus)
  "Create and return a posframe child frame.
This posframe's buffer is BUFFER-OR-NAME.

The below optional arguments are similar to `posframe-show''s:
PARENT-FRAME, FOREGROUND-COLOR, BACKGROUND-COLOR, LEFT-FRINGE,
RIGHT-FRINGE, BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH,
INTERNAL-BORDER-COLOR, FONT, KEEP-RATIO, LINES-TRUNCATE,
OVERRIDE-PARAMETERS, RESPECT-HEADER-LINE, RESPECT-MODE-LINE,
ACCEPT-FOCUS."
  (let ((left-fringe (or left-fringe 0))
        (right-fringe (or right-fringe 0))
        ;; See emacs.git:  Add distinct controls for child frames' borders (Bug#45620)
        ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc
        (border-width (or border-width internal-border-width 0))
        (border-color (or border-color internal-border-color))
        (buffer (get-buffer-create buffer-or-name))
        (after-make-frame-functions nil)
        (x-gtk-resize-child-frames posframe-gtk-resize-child-frames)
        (args (list "args"
                    foreground-color
                    background-color
                    right-fringe
                    left-fringe
                    border-width
                    border-color
                    internal-border-width
                    internal-border-color
                    font
                    keep-ratio
                    override-parameters
                    respect-header-line
                    respect-mode-line
                    accept-focus)))
    (with-current-buffer buffer
      ;; Many variables take effect after call `set-window-buffer'
      (setq-local display-line-numbers nil)
      (setq-local frame-title-format "")
      (setq-local left-margin-width nil)
      (setq-local right-margin-width nil)
      (setq-local left-fringe-width nil)
      (setq-local right-fringe-width nil)
      (setq-local fringes-outside-margins 0)
      (setq-local fringe-indicator-alist nil)
      ;; Need to use `lines-truncate' as our keyword variable instead
      ;; of `truncate-lines' so we don't shadow the variable that we
      ;; are trying to set.
      (setq-local truncate-lines lines-truncate)
      (setq-local show-trailing-whitespace nil)
      (setq-local posframe--accept-focus accept-focus)
      (unless respect-mode-line
        (setq-local mode-line-format nil))
      (unless respect-header-line
        (setq-local header-line-format nil))

      (if cursor
          (progn
            (setq-local cursor-type cursor)
            (setq-local cursor-in-non-selected-windows cursor))
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil))

      ;; Find existing posframe: buffer-local variables used by
      ;; posframe can be cleaned by other packages, so we should find
      ;; existing posframe first if possible.
      (unless (or posframe--frame posframe--last-args)
        (setq-local posframe--frame
                    (posframe--find-existing-posframe buffer args))
        (setq-local posframe--last-args args))

      ;; Create child-frame
      (unless (and posframe--frame
                   (frame-live-p posframe--frame)
                   ;; For speed reason, posframe will reuse
                   ;; existing frame at possible, but when
                   ;; user change args, recreating frame
                   ;; is needed.
                   (equal posframe--last-args args))
        (posframe-delete-frame buffer)
        (setq-local posframe--last-args args)
        (setq-local posframe--last-posframe-pixel-position nil)
        (setq-local posframe--last-posframe-size nil)
        (setq-local posframe--frame
                    (make-frame
                     `(,@override-parameters
                       ,(when foreground-color
                          (cons 'foreground-color foreground-color))
                       ,(when background-color
                          (cons 'background-color background-color))
                       (title . "posframe")
                       (parent-frame . ,parent-frame)
                       (keep-ratio ,keep-ratio)
                       (posframe-buffer . ,(cons (buffer-name buffer)
                                                 buffer))
                       (fullscreen . nil)
                       (no-accept-focus . ,(not accept-focus))
                       (min-width  . 0)
                       (min-height . 0)
                       (border-width . 0)
                       (internal-border-width . ,border-width)
                       (child-frame-border-width . ,border-width)
                       (vertical-scroll-bars . nil)
                       (horizontal-scroll-bars . nil)
                       (left-fringe . ,left-fringe)
                       (right-fringe . ,right-fringe)
                       (menu-bar-lines . 0)
                       (tool-bar-lines . 0)
                       (tab-bar-lines . 0)
                       (line-spacing . 0)
                       (unsplittable . t)
                       (no-other-frame . t)
                       (undecorated . t)
                       (visibility . nil)
                       (cursor-type . nil)
                       (minibuffer . nil)
                       (left . ,(if (consp position) (car position) 0))
                       (top . ,(if (consp position) (cdr position) 0))
                       (width . 1)
                       (height . 1)
                       (no-special-glyphs . t)
                       (skip-taskbar . t)
                       (inhibit-double-buffering . ,posframe-inhibit-double-buffering)
                       ;; Do not save child-frame when use desktop.el
                       (desktop-dont-save . t))))
        (set-frame-parameter posframe--frame 'last-args args)
        (set-frame-parameter
         posframe--frame 'font
         (or font (face-attribute 'default :font parent-frame)))
        (when border-color
          (if parent-frame
	      (set-face-background
               (if (facep 'child-frame-border)
                   'child-frame-border
                 'internal-border)
               border-color posframe--frame)
            ;; NOTE: when use refposhander feature, parent-frame will be
            ;; nil, we should use internal-border instead.
            (set-face-background
             'internal-border
             border-color posframe--frame))
          ;; HACK: Set face background after border color, otherwise the
          ;; border is not updated (BUG!).
          (when (version< emacs-version "28.0")
            (set-frame-parameter
             posframe--frame 'background-color
             (or background-color (face-attribute 'default :background)))))
        (let ((posframe-window (frame-root-window posframe--frame)))
          ;; This method is more stable than 'setq mode/header-line-format nil'
          (unless respect-mode-line
            (set-window-parameter posframe-window 'mode-line-format 'none))
          (unless respect-header-line
            (set-window-parameter posframe-window 'header-line-format 'none))
          (set-window-buffer posframe-window buffer)
          ;; When the buffer of posframe is killed, the child-frame of
          ;; this posframe will be deleted too.
          (set-window-dedicated-p posframe-window t)))

      ;; Remove tab-bar always.
      ;; NOTE: if we do not test the value of frame parameter
      ;; 'tab-bar-lines before set it, posframe will flicker when
      ;; scroll.
      (unless (equal (frame-parameter posframe--frame 'tab-bar-lines) 0)
        (set-frame-parameter posframe--frame 'tab-bar-lines 0))
      (when (version< "27.0" emacs-version)
        (setq-local tab-line-format nil))

      ;; If user set 'parent-frame to nil after run posframe-show.
      ;; for cache reason, next call to posframe-show will be affected.
      ;; so we should force set parent-frame again in this place.
      (set-frame-parameter posframe--frame 'parent-frame parent-frame)

      posframe--frame)))

(defun posframe--find-existing-posframe (buffer &optional last-args)
  "Find existing posframe with BUFFER and LAST-ARGS."
  (let ((posframe
         (cl-find-if
          (lambda (frame)
            (let* ((buffer-info (frame-parameter frame 'posframe-buffer))
                   (buffer-equal-p
                    (or (equal (buffer-name buffer) (car buffer-info))
                        (equal buffer (cdr buffer-info)))))
              (if last-args
                  (and buffer-equal-p
                       (equal last-args (frame-parameter frame 'last-args)))
                buffer-equal-p)))
          (frame-list))))
    (when posframe
      (set-frame-parameter posframe 'existing-posframe t))
    posframe))

(defun posframe-delete-frame (buffer-or-name)
  "Delete posframe pertaining to BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (let* ((buffer (get-buffer buffer-or-name))
         (posframe (when buffer
                     (posframe--find-existing-posframe buffer)))
         ;; NOTE: `delete-frame' runs ‘delete-frame-functions’ before
         ;; actually deleting the frame, unless the frame is a
         ;; tooltip, posframe is a child-frame, but its function like
         ;; a tooltip.
         (delete-frame-functions nil))
    (when posframe
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (timer '(posframe--refresh-timer
                           posframe--timeout-timer))
            (when (timerp timer)
              (cancel-timer timer)))))
      (delete-frame posframe))))

(defun posframe--insert-string (string no-properties)
  "Insert STRING to current buffer.
If NO-PROPERTIES is non-nil, all properties of STRING
will be removed."
  (when (and string (stringp string))
    (remove-text-properties
     0 (length string) '(read-only t) string)
    (let ((str (if no-properties
                   (substring-no-properties string)
                 string)))
      (erase-buffer)
      (insert str))))

(defun posframe--set-frame-size (size-info)
  "Set POSFRAME's size based on SIZE-INFO."
  (let ((posframe (plist-get size-info :posframe))
        (width (plist-get size-info :width))
        (height (plist-get size-info :height))
        (max-width (plist-get size-info :max-width))
        (max-height (plist-get size-info :max-height))
        (min-width (plist-get size-info :min-width))
        (min-height (plist-get size-info :min-height)))
    (when height (set-frame-height posframe height))
    (when width (set-frame-width posframe width))
    (unless (and height width)
      (posframe--fit-frame-to-buffer
       posframe max-height min-height max-width min-width
       (cond (width 'vertically)
             (height 'horizontally))))
    (setq-local posframe--last-posframe-size size-info)))

(defun posframe--fit-frame-to-buffer (posframe max-height min-height max-width min-width only)
  "POSFRAME version of function `fit-frame-to-buffer'.
Arguments HEIGHT, MAX-HEIGHT, MIN-HEIGHT, WIDTH, MAX-WIDTH,
MIN-WIDTH and ONLY are similar function `fit-frame-to-buffer''s."
  ;; This only has effect if the user set the latter var to `hide'.
  (let ((x-gtk-resize-child-frames posframe-gtk-resize-child-frames))
    ;; More info: Don't skip empty lines when fitting mini frame to buffer (Bug#44080)
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=e0de9f3295b4c46cb7198ec0b9634809d7b7a36d
    (if (functionp 'fit-frame-to-buffer-1)
        (fit-frame-to-buffer-1
         posframe max-height min-height max-width min-width only nil nil)
      (fit-frame-to-buffer
       posframe max-height min-height max-width min-width only))))

(defun posframe--run-refresh-timer (repeat size-info)
  "Refresh POSFRAME every REPEAT seconds.

It will set POSFRAME's size by SIZE-INFO."
  (let ((posframe (plist-get size-info :posframe))
        (width (plist-get size-info :width))
        (height (plist-get size-info :height)))
    (when (and (numberp repeat) (> repeat 0))
      (unless (and width height)
        (when (timerp posframe--refresh-timer)
          (cancel-timer posframe--refresh-timer))
        (setq-local posframe--refresh-timer
                    (run-with-timer
                     nil repeat
                     (lambda (size-info)
                       (let ((frame-resize-pixelwise t))
                         (when (and posframe (frame-live-p posframe))
                           (posframe--set-frame-size size-info))))
                     size-info))))))

;; Posframe's position handler
(defun posframe-run-poshandler (info)
  "Run posframe's position handler.

the structure of INFO can be found in docstring
of `posframe-show'."
  (if (equal info posframe--last-poshandler-info)
      posframe--last-posframe-pixel-position
    (setq posframe--last-poshandler-info info)
    (let* ((ref-position (plist-get info :ref-position))
           (poshandler (posframe--get-valid-poshandler info))
           (position (funcall poshandler info)))
      (if (not ref-position)
          position
        (posframe--calculate-new-position
         info position ref-position)))))

(defun posframe--get-valid-poshandler (info)
  "Get valid poshandler function with the help of INFO."
  (or (plist-get info :poshandler)
      (let ((position (plist-get info :position)))
        (cond ((integerp position)
               #'posframe-poshandler-point-bottom-left-corner)
              ((and (consp position)
                    (integerp (car position))
                    (integerp (cdr position)))
               #'posframe-poshandler-absolute-x-y)
              (t (error "Posframe: have no valid poshandler"))))))

(defun posframe--calculate-new-position (info position ref-position)
  "Calculate new position according to INFO, POSITION and REF-POSITION."
  (let* ((parent-frame-width (plist-get info :parent-frame-width))
         (parent-frame-height (plist-get info :parent-frame-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (ref-x (or (car ref-position) 0))
         (ref-y (or (cdr ref-position) 0))
         (x (car position))
         (y (cdr position)))
    (when (< x 0)
      (setq x (- (+ x parent-frame-width) posframe-width)))
    (when (< y 0)
      (setq y (- (+ y parent-frame-height) posframe-height)))
    (cons (+ ref-x x)
          (+ ref-y y))))

(defun posframe--set-frame-position (posframe position
                                              parent-frame-width
                                              parent-frame-height)
  "Move POSFRAME to POSITION.
This need PARENT-FRAME-WIDTH and PARENT-FRAME-HEIGHT"
  (unless (and (equal position posframe--last-posframe-pixel-position)
               ;; When working frame's size change, re-posit
               ;; the posframe.
               (equal posframe--last-parent-frame-size
                      (cons parent-frame-width parent-frame-height))
               (equal posframe--last-posframe-displayed-size
                      (cons (frame-pixel-width posframe)
                            (frame-pixel-height posframe))))
    (set-frame-position posframe (car position) (cdr position))
    (setq-local posframe--last-posframe-pixel-position position)
    (setq-local posframe--last-parent-frame-size
                (cons parent-frame-width parent-frame-height))
    (setq-local posframe--last-posframe-displayed-size
                (cons (frame-pixel-width posframe)
                      (frame-pixel-height posframe))))
  (posframe--make-frame-visible posframe))

(defun posframe--make-frame-visible (posframe)
  "Let POSFRAME visible and redraw it when needed."
  (unless (frame-visible-p posframe)
    (make-frame-visible posframe)
    (when (posframe--posframe-need-redraw-p posframe)
      (redraw-frame posframe))))

(defun posframe--posframe-need-redraw-p (posframe)
  "Test POSFRAME need to redraw or not."
  ;; When posframe is found by `posframe--find-existing-posframe',
  ;; it need to redraw, more info:
  ;; 1. https://github.com/tumashu/ivy-posframe/pull/30
  ;; 2. https://github.com/tumashu/posframe/pull/118
  (frame-parameter posframe 'existing-posframe))

(defun posframe--run-timeout-timer (posframe secs)
  "Hide POSFRAME after a delay of SECS seconds."
  (when (and (numberp secs) (> secs 0))
    (when (timerp posframe--timeout-timer)
      (cancel-timer posframe--timeout-timer))
    (setq-local posframe--timeout-timer
                (run-with-timer
                 secs nil #'posframe--make-frame-invisible posframe))))

(defun posframe--make-frame-invisible (frame)
  "`make-frame-invisible' replacement to hide FRAME safely."
  (when (and (frame-live-p frame)
             (frame-visible-p frame))
    (make-frame-invisible frame)))

(defun posframe-mouse-banish-simple (info)
  "Banish mouse to (0, 0) of posframe base on INFO."
  (let ((parent-frame (plist-get info :parent-frame))
        (x (plist-get info :posframe-x))
        (y (plist-get info :posframe-y))
        (w (plist-get info :posframe-width))
        (h (plist-get info :posframe-height))
        (p-w (plist-get info :parent-frame-width))
        (p-h (plist-get info :parent-frame-height)))
    (set-mouse-pixel-position
     parent-frame
     (if (= x 0)
         (min p-w (+ w 5))
       (max 0 (- x 5)))
     (if (= y 0)
         (min p-h (+ h 10))
       (max 0 (- y 10))))))

(defun posframe-mouse-banish-default (info)
  "Banish mouse base on INFO.

FIXME: This is a hacky fix for the mouse focus problem, which like:
https://github.com/tumashu/posframe/issues/4#issuecomment-357514918"
  (let* ((parent-frame (plist-get info :parent-frame))
         (m-x (plist-get info :mouse-x))
         (m-y (plist-get info :mouse-y))
         (x (plist-get info :posframe-x))
         (y (plist-get info :posframe-y))
         (w (plist-get info :posframe-width))
         (h (plist-get info :posframe-height))
         (p-w (plist-get info :parent-frame-width))
         (p-h (plist-get info :parent-frame-height)))
    (when (and m-x m-y
               (>= m-x x)
               (<= m-x (+ x w))
               (>= m-y y)
               (<= m-y (+ y h)))
      (set-mouse-pixel-position
       parent-frame
       (if (= x 0)
           (min p-w (+ w 5))
         (max 0 (- x 5)))
       (if (= y 0)
           (min p-h (+ h 10))
         (max 0 (- y 10)))))))

(defun posframe-refresh (buffer-or-name)
  "Refresh posframe pertaining to BUFFER-OR-NAME.

For example:

   (defvar buf \" *test*\")
   (posframe-show buf)

   (with-current-buffer buf
     (erase-buffer)
     (insert \"ffffffffffffff\")
     (posframe-refresh buf))

User can use posframe-show's :refresh argument,
to do similar job:

   (defvar buf \" *test*\")
   (posframe-show buf :refresh 0.25)

   (with-current-buffer buf
     (erase-buffer)
     (insert \"ffffffffffffff\"))"
  (dolist (frame (frame-list))
    (let ((buffer-info (frame-parameter frame 'posframe-buffer))
          (frame-resize-pixelwise t))
      (when (or (equal buffer-or-name (car buffer-info))
                (equal buffer-or-name (cdr buffer-info)))
        (with-current-buffer buffer-or-name
          (posframe--set-frame-size posframe--last-posframe-size))))))

;;;###autoload
(defun posframe-hide-all ()
  "Hide all posframe frames."
  (interactive)
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (posframe--make-frame-invisible frame))))

(defun posframe-hide (buffer-or-name)
  "Hide posframe pertaining to BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a buffer name."
  ;; Make sure buffer-list-update-hook is nil when posframe-hide is
  ;; called, otherwise:
  ;;   (add-hook 'buffer-list-update-hook  #'posframe-hide)
  ;; will lead to infinite recursion.
  (when buffer-or-name
    (let ((buffer-list-update-hook nil))
      (dolist (frame (frame-list))
        (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
          (when (or (equal buffer-or-name (car buffer-info))
                    (equal buffer-or-name (cdr buffer-info)))
            (posframe--make-frame-invisible frame)))))))

(defun posframe-hidehandler-daemon ()
  "Run posframe hidehandler daemon."
  (when (timerp posframe-hidehandler-timer)
    (cancel-timer posframe-hidehandler-timer))
  (setq posframe-hidehandler-timer
        (run-with-idle-timer 0.5 t #'posframe-hidehandler-daemon-function)))

(defun posframe-hidehandler-daemon-function ()
  "Posframe hidehandler daemon function."
  (ignore-errors
    (dolist (frame (frame-list))
      (let ((hidehandler (frame-parameter frame 'posframe-hidehandler))
            (buffer (frame-parameter frame 'posframe-buffer))
            (parent-buffer (frame-parameter frame 'posframe-parent-buffer)))
        (when (and hidehandler
                   (funcall hidehandler
                            (list
                             :posframe-buffer buffer
                             :posframe-parent-buffer parent-buffer)))
          (posframe--make-frame-invisible frame))))))

(posframe-hidehandler-daemon)

(defun posframe-hidehandler-when-buffer-switch (info)
  "Posframe hidehandler function.

This function let posframe hide when user switch buffer.
Note: This function is called in `post-command-hook'.
Argument INFO ."
  (let ((parent-buffer (cdr (plist-get info :posframe-parent-buffer))))
    (and (buffer-live-p parent-buffer)
         (not (equal parent-buffer (current-buffer))))))

;;;###autoload
(defun posframe-delete-all ()
  "Delete all posframe frames and buffers."
  (interactive)
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'posframe-buffer)
      (let ((delete-frame-functions nil))
        (delete-frame frame))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when posframe--frame
        (posframe--kill-buffer buffer)))))

(defun posframe--kill-buffer (buffer-or-name)
  "Kill posframe's buffer: BUFFER-OR-NAME.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (when (buffer-live-p (get-buffer buffer-or-name))
    (kill-buffer buffer-or-name)))

(defun posframe-delete (buffer-or-name)
  "Delete posframe pertaining to BUFFER-OR-NAME and kill the buffer.
BUFFER-OR-NAME can be a buffer or a buffer name.

This function is not commonly used, for delete and recreate
posframe is very very slowly, `posframe-hide' is more useful."
  (posframe-delete-frame buffer-or-name)
  (posframe--kill-buffer buffer-or-name))

(defun posframe-funcall (buffer-or-name function &rest arguments)
  "Select posframe of BUFFER-OR-NAME and call FUNCTION with ARGUMENTS.
BUFFER-OR-NAME can be a buffer or a buffer name."
  (when (functionp function)
    (when (get-buffer buffer-or-name)
      (with-current-buffer buffer-or-name
        (when (framep posframe--frame)
          (with-selected-frame posframe--frame
            (apply function arguments)))))))

(defun posframe-poshandler-absolute-x-y (info)
  "Posframe's position handler.

This poshandler function deal with (integer . integer) style
position.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let ((position (plist-get info :position))
        (x-pixel-offset (plist-get info :x-pixel-offset))
        (y-pixel-offset (plist-get info :y-pixel-offset)))
    (cons (+ (car position) x-pixel-offset)
          (+ (cdr position) y-pixel-offset))))

(defun posframe-poshandler-point-1 (info &optional font-height upward)
  "The internal function used to deal with point-poshandler.
Argument INFO .

Optional arguments: FONT-HEIGHT and UPWARD."
  (let* ((x-pixel-offset (plist-get info :x-pixel-offset))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (window (plist-get info :parent-window))
         (xmax (plist-get info :parent-frame-width))
         (ymax (plist-get info :parent-frame-height))
         (position-info (plist-get info :position))
         (position-info
          (if (integerp position-info)
              (posn-at-point position-info window)
            position-info))
         (header-line-height (plist-get info :header-line-height))
         (tab-line-height (plist-get info :tab-line-height))
         (x (+ (car (window-inside-pixel-edges window))
               (- (or (car (posn-x-y position-info)) 0)
                  (or (car (posn-object-x-y position-info)) 0))
               x-pixel-offset))
         (y-top (+ (cadr (window-pixel-edges window))
                   tab-line-height
                   header-line-height
                   (- (or (cdr (posn-x-y position-info)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y position-info)) 0))
                   y-pixel-offset))
         (font-height (or font-height (plist-get info :font-height)))
         (y-bottom (+ y-top font-height)))
    (cons (max 0 (min x (- xmax (or posframe-width 0))))
          (max 0 (if (if upward
                         (> (- y-bottom (or posframe-height 0)) 0)
                       (> (+ y-bottom (or posframe-height 0)) ymax))
                     (- y-top (or posframe-height 0))
                   y-bottom)))))

(defun posframe-poshandler-point-bottom-left-corner (info)
  "Posframe's position handler.

This poshandler function let top left corner of posframe align to
bottom left corner of point.

The structure of INFO can be found in docstring of
`posframe-show'."
  (posframe-poshandler-point-1 info))

(defun posframe-poshandler-point-window-center (info)
  "Posframe's position handler.

This poshandler function let center of posframe vertical align to
center of window and top edge of posframe horizontal align to
buttom edge of current point.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let ((x (car (posframe-poshandler-window-top-center info)))
        (y (cdr (posframe-poshandler-point-bottom-left-corner info))))
    (cons x y)))

(defun posframe-poshandler-point-frame-center (info)
  "Posframe's position handler.

This poshandler function let center of posframe vertical align to
center of frame and top edge of posframe horizontal align to
buttom edge of current point.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let ((x (car (posframe-poshandler-frame-top-center info)))
        (y (cdr (posframe-poshandler-point-bottom-left-corner info))))
    (cons x y)))

(defun posframe-poshandler-point-bottom-left-corner-upward (info)
  "Posframe's position handler.

This poshandler function let bottom left corner of posframe align
to bottom left corner of point.

The structure of INFO can be found in docstring of
`posframe-show'."
  (posframe-poshandler-point-1 info nil t))

(defun posframe-poshandler-point-top-left-corner (info)
  "Posframe's position handler.

This poshandler function let top left corner of posframe align to
top left corner of point.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let ((font-height 0))
    (posframe-poshandler-point-1 info font-height)))

(defun posframe-poshandler-frame-center (info)
  "Posframe's position handler.

This poshandler function let center of posframe align to center
of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (/ (- (plist-get info :parent-frame-height)
              (plist-get info :posframe-height))
           2)))

(defun posframe-poshandler-frame-top-center (info)
  "Posframe's position handler.

This poshandler function let top edge center of posframe align
to top edge center of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        0))

(defun posframe-poshandler-frame-top-left-corner (_info)
  "Posframe's position handler.

This poshandler function let top left corner of posframe align to
top left corner of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  '(0 . 0))

(defun posframe-poshandler-frame-top-right-corner (_info)
  "Posframe's position handler.

This poshandler function let top right corner of posframe align to
top right corner of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  '(-1 . 0))

(defun posframe-poshandler-frame-top-left-or-right-other-corner (info)
  "Posframe's position handler.

This poshandler function let posframe align to top left or top right corner of frame,
based on whether current window is relatively at left or right in the current frame.
If window is at left, place posframe on right, and vice versa.
(This is calculated by whether current window center is left or right to frame center)

The structure of INFO can be found in docstring of
`posframe-show'."
  (let ((window-left (plist-get info :parent-window-left))
        (window-width (plist-get info :parent-window-width))
        (frame-width (plist-get info :parent-frame-width)))
    ;; when equal, put posframe on right because content in window tend to be on left
    (if (<= (+ window-left (/ window-width 2))
            (/ frame-width 2))
        '(-1 . 0)
      '(0 . 0))))

(defun posframe-poshandler-frame-bottom-left-corner (info)
  "Posframe's position handler.

This poshandler function let bottom left corner of posframe align
to buttom left corner of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  (cons 0 (- 0
             (plist-get info :mode-line-height)
             (plist-get info :minibuffer-height))))

(defun posframe-poshandler-frame-bottom-right-corner (info)
  "Posframe's position handler.

This poshandler function let bottom right corner of posframe
align to buttom right corner of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  (cons -1 (- 0
              (plist-get info :mode-line-height)
              (plist-get info :minibuffer-height))))

(defun posframe-poshandler-frame-bottom-center (info)
  "Posframe's position handler.

This poshandler function let bottom edge center of posframe align
to buttom edge center of frame.

The structure of INFO can be found in docstring of
`posframe-show'."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (- (plist-get info :parent-frame-height)
           (plist-get info :posframe-height)
           (plist-get info :mode-line-height)
           (plist-get info :minibuffer-height))))

(defun posframe-poshandler-window-center (info)
  "Posframe's position handler.

This poshandler function let center of posframe align to center
of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height)))
    (cons (max 0 (+ window-left (/ (- window-width posframe-width) 2)))
          (max 0 (+ window-top (/ (- window-height posframe-height) 2))))))

(defun posframe-poshandler-window-top-left-corner (info)
  "Posframe's position handler.

This poshandler function let top left corner of posframe align to
top left corner of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top)))
    (cons window-left
          window-top)))

(defun posframe-poshandler-window-top-right-corner (info)
  "Posframe's position handler.

This poshandler function let top right corner of posframe align to
top left right of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          window-top)))

(defun posframe-poshandler-window-top-center (info)
  "Posframe's position handler.

This poshandler function let top edge center of posframe align to
top edge center of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width)))
    (cons (max 0 (+ window-left (/ (- window-width posframe-width) 2)))
          window-top)))

(defun posframe-poshandler-window-bottom-left-corner (info)
  "Posframe's position handler.

This poshandler function let bottom left corner of posframe align to
bottom left corner of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-height (plist-get info :parent-window-height))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons window-left
          (+ window-top window-height
             (- 0 mode-line-height posframe-height)))))

(defun posframe-poshandler-window-bottom-right-corner (info)
  "Posframe's position handler.

This poshandler function let bottom right corner of posframe
align to bottom right corner of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons (+ window-left window-width
             (- 0 posframe-width))
          (+ window-top window-height
             (- 0 mode-line-height posframe-height)))))

(defun posframe-poshandler-window-bottom-center (info)
  "Posframe's position handler.

This poshandler function let bottom edge center of posframe align
to bottom edge center of window.

The structure of INFO can be found in docstring of
`posframe-show'."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (mode-line-height (plist-get info :mode-line-height)))
    (cons (max 0 (+ window-left (/ (- window-width posframe-width) 2)))
          (+ window-top window-height
             (- 0 mode-line-height posframe-height)))))

(defun posframe-refposhandler-xwininfo (&optional frame)
  "Parent FRAME poshander function.
Get the position of parent frame (current frame) with the help of
xwininfo."
  (when (executable-find "xwininfo")
    (with-temp-buffer
      (let ((case-fold-search nil))
        (call-process "xwininfo" nil t nil
                      "-display" (frame-parameter frame 'display)
                      "-id"  (frame-parameter frame 'window-id))
        (goto-char (point-min))
        (search-forward "Absolute upper-left")
        (let ((x (string-to-number
                  (buffer-substring-no-properties
                   (search-forward "X: ")
                   (line-end-position))))
              (y (string-to-number
                  (buffer-substring-no-properties
                   (search-forward "Y: ")
                   (line-end-position)))))
          (cons x y))))))

(if (version< emacs-version "27.1")
    (with-no-warnings
      (add-hook 'focus-in-hook #'posframe--redirect-posframe-focus))
  (add-function :after after-focus-change-function #'posframe--redirect-posframe-focus))

(defun posframe--redirect-posframe-focus ()
  "Redirect focus from the posframe to the parent frame.
This prevents the posframe from catching keyboard input if the
window manager selects it."
  (when (and (eq (selected-frame) posframe--frame)
             ;; Do not redirect focus when posframe can accept focus.
             ;; See posframe-show's accept-focus argument.
             (not posframe--accept-focus))
    (redirect-frame-focus posframe--frame (frame-parent))))

(provide 'posframe)

;;; posframe.el ends here
