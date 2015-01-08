;;; frame-fns.el --- Non-interactive frame and window functions.
;;
;; Filename: frame-fns.el
;; Description: Non-interactive frame and window functions.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2015, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 16:15:50 1996
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 10:45:03 2015 (-0800)
;;           By: dradams
;;     Update #: 227
;; URL: http://www.emacswiki.org/frame-fns.el
;; Doc URL: http://emacswiki.org/FrameModes
;; Keywords: internal, extensions, local, frames
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Non-interactive frame and window functions.
;;
;;  Main new functions defined here:
;;
;;    `1-window-frames-on', `distance', `flash-ding',
;;    `frame-geom-spec-cons', `frame-geom-value-cons',
;;    `frame-geom-spec-numeric', `frame-geom-value-numeric',
;;    `frames-on', `get-a-frame', `get-frame-name',
;;    `multi-window-frames-on', `read-frame', `window-coords'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dadams
;;     Removed autoload cookies from non-interactive functions.
;; 2010/01/12 dadams
;;     1-window-frames-on, multi-window-frames-on:
;;       save-excursion + set-buffer -> with-current-buffer.
;; 2008/04/05 dadams
;;     get-a-frame: Define without using member-if.
;; 2005/10/31 dadams
;;     read-frame: Swapped default and init values in call to completing-read.
;; 2004/11/26 dadams
;;     Added frame-geom-spec-numeric and frame-geom-value-numeric.
;; 2004/03/19 dadams
;;     read-frame: 1) if default is a frame, use its name,
;;                 2) use frame-name-history, not minibuffer-history,
;;                    and use make-frame-names-alist, not frame-alist,
;;                    in completing-read
;; 1996/02/14 dadams
;;     Added: window-coords, distance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (when (< emacs-major-version 21) (require 'cl)))
 ;; dolist, push
(require 'avoid nil t) ;; mouse-avoidance-point-position

;;;;;;;;;;;;;;;;;;;;;;;

(defun window-coords (&optional position)
  "Return window coordinates of buffer POSITION (default: point).
If POSITION is nil, (point) is used."
  (unless (fboundp 'mouse-avoidance-point-position) (require 'avoid))
  (cdr (mouse-avoidance-point-position)))

(defun distance (pt1 pt2)
  "Distance as the crow flies between PT1 and PT2.
PT1 and PT2 are each a cons of the form (X . Y)."
  (let ((xdiff  (abs (- (car pt1) (car pt2))))
        (ydiff  (abs (- (cdr pt1) (cdr pt2)))))
    (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))

(defun frame-geom-value-numeric (type value &optional frame)
  "Return equivalent geometry value for FRAME in numeric terms.
A geometry value equivalent to VALUE for FRAME is returned,
where the value is numeric, not a consp.
TYPE is the car of the original geometry spec (TYPE . VALUE).
   It is `top' or `left', depending on which edge VALUE is related to.
VALUE is the cdr of a frame geometry spec: (left/top . VALUE).
If VALUE is a consp, then it is converted to a numeric value, perhaps
   relative to the opposite frame edge from that in the original spec.
FRAME defaults to the selected frame.

Examples (measures in pixels) -
 Assuming display height/width=1024, frame height/width=600:
 300 inside display edge:                   300  =>  300
                                        (+  300) =>  300
 300 inside opposite display edge:      (-  300) => -300
                                           -300  => -300
 300 beyond display edge
  (= 724 inside opposite display edge): (+ -300) => -724
 300 beyond display edge
  (= 724 inside opposite display edge): (- -300) =>  724

In the last two examples, the returned value is relative to the
opposite frame edge from the edge indicated in the input spec."
  (if (consp value)
      (if (natnump (cadr value))
          ;; e.g. (+ 300) or (- 300) => 300 or -300
          (funcall (car value) (cadr value))
        ;; e.g. (+ -300) or (- -300)
        (let ((oppval  (- (if (eq 'left type)
                              (x-display-pixel-width)
                            (x-display-pixel-height))
                          (cadr value)
                          (if (eq 'left type)
                              (frame-pixel-width frame)
                            (frame-pixel-height frame)))))
          (if (eq '+ (car value))
              (- oppval)                ; e.g. (+ -300) => -724
            oppval)))                   ; e.g. (- -300) =>  724
    ;; e.g. 300 or -300
    value))

(defun frame-geom-spec-numeric (spec &optional frame)
  "Return equivalent geometry specification for FRAME in numeric terms.
A geometry specification equivalent to SPEC for FRAME is returned,
where the value is numeric, not a consp.
SPEC is a frame geometry spec: (left . VALUE) or (top . VALUE).
If VALUE is a consp, then it is converted to a numeric value, perhaps
   relative to the opposite frame edge from that in the original SPEC.
FRAME defaults to the selected frame.

Examples (measures in pixels) -
 Assuming display height=1024, frame height=600:
 top 300 below display top:               (top .  300) => (top .  300)
                                          (top +  300) => (top .  300)
 bottom 300 above display bottom:         (top -  300) => (top . -300)
                                          (top . -300) => (top . -300)
 top 300 above display top
  (= bottom 724 above display bottom):    (top + -300) => (top . -724)
 bottom 300 below display bottom
  (= top 724 below display top):          (top - -300) => (top .  724)

In the last two examples, the returned value is relative to the
opposite frame edge from the edge indicated in the input SPEC."
  (cons (car spec) (frame-geom-value-numeric (car spec) (cdr spec))))

(defun frame-geom-value-cons (type value &optional frame)
  "Return equivalent geometry value for FRAME as a cons with car `+'.
A geometry value equivalent to VALUE for FRAME is returned,
where the value is a cons with car `+', not numeric.
TYPE is the car of the original geometry spec (TYPE . VALUE).
   It is `top' or `left', depending on which edge VALUE is related to.
VALUE is the cdr of a frame geometry spec: (left/top . VALUE).
If VALUE is a number, then it is converted to a cons value, perhaps
   relative to the opposite frame edge from that in the original spec.
FRAME defaults to the selected frame.

Examples (measures in pixels) -
 Assuming display height/width=1024, frame height/width=600:
 300 inside display edge:                   300  => (+  300)
                                        (+  300) => (+  300)
 300 inside opposite display edge:      (-  300) => (+  124)
                                           -300  => (+  124)
 300 beyond display edge
  (= 724 inside opposite display edge): (+ -300) => (+ -300)
 300 beyond display edge
  (= 724 inside opposite display edge): (- -300) => (+  724)

In the 3rd, 4th, and 6th examples, the returned value is relative to
the opposite frame edge from the edge indicated in the input spec."
  (cond ((and (consp value)  (eq '+ (car value))) ; e.g. (+ 300), (+ -300)
         value)
        ((natnump value) (list '+ value)) ; e.g. 300 => (+ 300)
        (t                              ; e.g. -300, (- 300), (- -300)
         (list '+ (- (if (eq 'left type) ; => (+ 124), (+ 124), (+ 724)
                         (x-display-pixel-width)
                       (x-display-pixel-height))
                     (if (integerp value) (- value) (cadr value))
                     (if (eq 'left type)
                         (frame-pixel-width frame)
                       (frame-pixel-height frame)))))))

(defun frame-geom-spec-cons (spec &optional frame)
  "Return equivalent geometry spec for FRAME as a cons with car `+'.
A geometry specification equivalent to SPEC for FRAME is returned,
where the value is a cons with car `+', not numeric.
SPEC is a frame geometry spec: (left . VALUE) or (top . VALUE).
If VALUE is a number, then it is converted to a cons value, perhaps
   relative to the opposite frame edge from that in the original spec.
FRAME defaults to the selected frame.

Examples (measures in pixels) -
 Assuming display height=1024, frame height=600:
 top 300 below display top:               (top .  300) => (top +  300)
                                          (top +  300) => (top +  300)
 bottom 300 above display bottom:         (top -  300) => (top +  124)
                                          (top . -300) => (top +  124)
 top 300 above display top
  (= bottom 724 above display bottom):    (top + -300) => (top + -300)
 bottom 300 below display bottom
  (= top 724 below display top):          (top - -300) => (top +  724)

In the 3rd, 4th, and 6th examples, the returned value is relative to
the opposite frame edge from the edge indicated in the input spec."
  (cons (car spec) (frame-geom-value-cons (car spec) (cdr spec))))

(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame  (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(defun get-a-frame (frame)
  "Return a frame, if any, named FRAME (a frame or a string).
If none, return nil.
If FRAME is a frame, it is returned."
  (cond ((framep frame) frame)
        ((stringp frame)
         (catch 'get-a-frame-found
           (dolist (fr (frame-list))
             (when (string= frame (get-frame-name fr))
               (throw 'get-a-frame-found fr)))
           nil))
        (t (error
            "Function `get-frame-name': Arg neither a string nor a frame: `%s'"
            frame))))

(defun read-frame (prompt &optional default existing)
  "Read the name of a frame, and return it as a string.
Prompts with 1st arg, PROMPT (a string).

The default frame is named by the optional 2nd arg, DEFAULT, if a
string or a frame, or by the `selected-frame', if nil.

Non-nil optional 3rd arg, EXISTING, means to allow only names of
existing frames."
  (setq default  (if (framep default)
                     (get-frame-name default)
                   (or default  (get-frame-name))))
  (unless (stringp default)
    (error "Function `read-frame': DEFAULT arg is neither a frame nor a string"))
  (completing-read prompt (make-frame-names-alist)
                   ;; To limit to live frames:
                   ;; (function (lambda (fn+f)(frame-live-p (cdr fn+f))))
                   ;; `frame-name-history' is defined in `frame.el'.
                   nil existing nil '(frame-name-history . 2) default))

(defun frames-on (buffer &optional frame)
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

(defun 1-window-frames-on (buffer)
  "List of all visible 1-window frames showing BUFFER."
  (setq buffer  (get-buffer buffer))
  (when buffer                          ; Do nothing if BUFFER is not a buffer.
    (let ((frs  ()))
      (with-current-buffer buffer
        (when (buffer-live-p buffer)    ; Do nothing if dead buffer.
          ;; $$$$$$ Is it better to search through frames-on or windows-on?
          (dolist (fr  (frames-on buffer))
            (save-window-excursion (select-frame fr)
                                   (when (one-window-p t fr) (push fr frs))))))
      frs)))

(defun multi-window-frames-on (buffer)
  "List of all visible multi-window frames showing BUFFER."
  (setq buffer  (get-buffer buffer))
  (when buffer                          ; Do nothing if BUFFER is not a buffer.
    (let ((frs  ()))
      (with-current-buffer buffer
        (when (buffer-live-p buffer)    ; Do nothing if dead buffer.
          ;; $$$$$$ Is it better to search through frames-on or windows-on?
          (dolist (fr  (frames-on buffer))
            (save-window-excursion (select-frame fr)
                                   (unless (one-window-p t fr)
                                     (push fr frs))))))
      frs)))

(defun flash-ding (&optional do-not-terminate frame)
  "Ring bell (`ding'), after flashing FRAME (default: current), if relevant.
Terminates any keyboard macro executing, unless arg DO-NOT-TERMINATE non-nil."
  (save-window-excursion
    (when frame (select-frame frame))
    (let ((visible-bell  t)) (ding 'DO-NOT-TERMINATE))) ; Flash.
  (let ((visible-bell  nil)) (ding 'DO-NOT-TERMINATE))) ; Bell.

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'frame-fns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame-fns.el ends here
