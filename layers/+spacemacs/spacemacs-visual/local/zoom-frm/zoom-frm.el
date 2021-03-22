;;; zoom-frm.el --- Commands to zoom frame font size.
;;
;; Filename: zoom-frm.el
;; Description: Commands to zoom frame font size.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2005-2019, Drew Adams, all rights reserved.
;; Created: Fri Jan 07 10:24:35 2005
;; Version: 0
;; Package-Requires: ((frame-fns "0") (frame-cmds "0"))
;; Last-Updated: Tue Nov 19 13:15:54 2019 (-0800)
;;           By: dradams
;;     Update #: 359
;; URL: https://www.emacswiki.org/emacs/download/zoom-frm.el
;; Doc URL: https://emacswiki.org/emacs/SetFonts
;; Keywords: frames, extensions, convenience
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `backquote', `bytecomp', `cconv', `cl-lib',
;;   `frame-cmds', `frame-fns', `macroexp', `misc-fns', `strings',
;;   `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Commands to zoom into and out of text.  They zoom a frame or a
;;  buffer, so that the text appears larger or smaller.
;;
;;  Commands `zoom-in', `zoom-out', and `zoom-in/out' do both kinds of
;;  zooming.  They can behave like command `text-scale-adjust',
;;  zooming a buffer wherever it is displayed, or they can zoom an
;;  entire single frame (all of its windows).  Hit `C-u' at any time
;;  while using these commands to toggle between buffer and frame
;;  zooming.
;;
;;  Because it is a more general replacement for `text-scale-adjust',
;;  I suggest you bind `zoom-in/out' to the keys bound by default to
;;  `text-scale-adjust': `C-x C-+', `C-x C-=', `C-x C--', and `C-x
;;  C-0'.
;;
;;  It is also handy to use a mouse button or wheel for zooming, hence
;;  the mouse binding suggestions.  For example, binding `zoom-in' and
;;  `zoom-out' to mouse wheel events gives you the zooming effect you
;;  are perhaps used to in a Web browser.
;;
;;  User option `zoom-frame/buffer' determines which kind of zooming
;;  (frame or buffer) is used by default.  You can customize this
;;  option, but (in Emacs 23 or later) you can also toggle it just by
;;  providing a prefix arg (`C-u') to `zoom-in/out', `zoom-in', or
;;  `zoom-out'.
;;
;;  Note about saving changes made dynamically using the commands
;;  defined here:
;;
;;    Some of the commands defined here change frame properties.  You
;;    can save any changes you have made, by using Customize.  To
;;    visit a Customize buffer of all unsaved changes you have made,
;;    use command `customize-customized'.
;;
;;    Frame parameter changes, such as font size, can be saved for
;;    future use by all frames or all frames of a certain kind.  For
;;    that, you must change the frame parameters of the correponding
;;    frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame.  Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames.  These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'.  The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined in library
;;    `frame-cmds.el'.
;;
;;    Example: Suppose you change the font size of a frame and want to
;;    make that the default font size for new frames in the future.
;;    You will need to update the value of variable
;;    `default-frame-alist' to use the `font' parameter setting of the
;;    changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'.  Those commands are
;;    defined in library `frame-cmds.el'.
;;
;;
;;  Commands defined here:
;;
;;    `toggle-zoom-frame', `zoom-all-frames-in',
;;    `zoom-all-frames-out', `zoom-frm-in', `zoom-frm-out',
;;    `zoom-frm-unzoom', `zoom-in', `zoom-in/out' (Emacs 23+),
;;    `zoom-out'.
;;
;;
;;  User options (variables) defined here:
;;
;;    `frame-zoom-font-difference', `zoom-frame/buffer' (Emacs 23+).
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'zoom-frm)
;;
;;  Suggested key bindings:
;;
;;    Emacs 23 and later:
;;
;;    (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
;;    (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
;;    (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
;;    (define-key ctl-x-map [(control ?0)] 'zoom-in/out)
;;
;;    Any Emacs version:
;;
;;    (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
;;                        (vector (list 'control
;;                                      mouse-wheel-down-event))
;;                      [C-mouse-wheel])    ; Emacs 20, 21
;;                    'zoom-in)
;;    (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
;;                        (vector (list 'control 'meta
;;                                      mouse-wheel-down-event))
;;                      [C-M-mouse-wheel])  ; Emacs 20, 21
;;                    'zoom-all-frames-in)
;;    (when (boundp 'mouse-wheel-up-event) ; Emacs 22+
;;      (global-set-key (vector (list 'control 
;;                                    mouse-wheel-up-event))
;;                      'zoom-out))
;;      (global-set-key (vector (list 'control 'meta
;;                                    mouse-wheel-up-event))
;;                      'zoom-all-frames-out))
;;
;;    (global-set-key [S-mouse-1]    'zoom-in)
;;    (global-set-key [C-S-mouse-1]  'zoom-out)
;;    ;; Get rid of `mouse-set-font' or `mouse-appearance-menu':
;;    (global-set-key [S-down-mouse-1] nil)
;;
;;
;;  Some of the commands are not autoloaded by default, because this
;;  library works with old as well as recent Emacs releases.  The
;;  commands that are not autoloaded are not usable in older releases.
;;  You can autoload such commands yourself.  For example, if you use
;;  Emacs 23 or later, you can add this to your init file, to autoload
;;  `zoom-in/out':
;;
;;  (autoload 'zoom-in/out "zoom-frm"
;;            "Zoom current frame or buffer in or out" t)
;;
;;  The first two of the mouse bindings mean that in Emacs 22 or later
;;  you can hold the Control key and rotate the mouse wheel to zoom in
;;  and out, just as you might do in a Web browser.
;;
;;  (In Emacs 20 and 21, Control plus mouse wheeling zooms in, but to
;;  zoom out you need to use `C--' before wheeling with Control.  This
;;  is because Emacs 20 and 21 do not have separate events for the
;;  mouse wheel directions, and it is the prefix arg, not the wheel
;;  direction, that determines the effect.)
;;
;;
;;  See also these files for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window.  Uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `doremi-frm.el'    - Incrementally adjust frame properties
;;                          using arrow keys and/or mouse wheel.
;;
;;     `frame-cmds.el'    - Miscellaneous frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2019/11/19 dadams
;;     zoom-all-frames-(in|out):
;;       Use zoom-frm-(in|out): zoom each relative to its current font.  Visible only.  Autoload.
;; 2015/11/01 dadams
;;     Require cl.el at compile time, for macro case.
;; 2015/01/10 dadams
;;     zoom-in, zoom-out: Added message about new zoom type.
;;     zoom-in/out: Corrected msg: C- modifier was missing.  Reminder at end of doc string.
;; 2013/12/31 dadams
;;     zoom-in/out: Use set-transient-map, if defined.
;; 2013/09//29 dadams
;;     zoom-in/out: Only for Emacs 24.3+ (needs set-temporary-overlay-map).
;; 2013/09/13 dadams
;;     Added: zoom-all-frames-in, zoom-all-frames-out.
;; 2013/04/21 dadams
;;     Added: zoom-in/out.
;; 2011/01/04 dadams
;;     Added autoload cookies for defgroup and defcustom.
;; 2010/07/06 dadams
;;     zoom-(in|out): Put doc strings before interactive spec.  Thx to Yidong Chong.
;; 2009/06/11 dadams
;;     Added buffer zooming, for Emacs 23.
;;       Added zoom-(in|out), group zoom, zoom-frame/buffer.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     frame-zoom-font-difference: Changed :group to Frame-Commands. Added :link.
;; 2005/01/18 dadams
;;     Changed default value of frame-zoom-font-difference.
;;     Added Note on saving changes.
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

(eval-when-compile (require 'cl)) ;; case

(require 'frame-cmds) ;; enlarge-font


(defvar zoom-frame/buffer) ;; Defined here for Emacs 22+.

;;;;;;;;;;;;;;;;;;;;;;;;


;;; USER OPTIONS (VARIABLES) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defgroup zoom nil
  "Zoom a frame or buffer."
  :group 'frames :group 'Frame-Commands ; Defined in `frame-cmds.el'.
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
zoom-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/zoom-frm.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/SetFonts#ChangingFontSize")
  :link '(emacs-commentary-link :tag "Commentary" "zoom-frm"))

;;;###autoload
(defcustom frame-zoom-font-difference 1
  "*Number of points to change the frame font size when zooming.
This applies to commands `zoom-in/out', `zoom-in', `zoom-out',
`zoom-frm-in', and `zoom-frm-out' when zooming a frame.

The absolute value of the value must be less than the current font
size for the frame, because the new font size cannot be less than one
point."
  :type 'integer :group 'zoom)

(when (> emacs-major-version 22)
  (defcustom zoom-frame/buffer 'frame
    "*What to zoom: current frame or current buffer.
See command `zoom-in/out', `zoom-in', or `zoom-out'."
    :type '(choice (const :tag "Zoom frame"  frame) (const :tag "Zoom buffer" buffer))
    :group 'zoom))


;;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (> emacs-major-version 22) (defalias 'zoom-in 'zoom-frm-in))
;;;###autoload
(defun zoom-frm-in (&optional frame flip)
  "Zoom FRAME in by `frame-zoom-font-difference', making text larger.
If `frame-zoom-font-difference' is negative, make text smaller.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text smaller.
This is equal but opposite to `zoom-frm-out'."
  (interactive (list (selected-frame) current-prefix-arg))
  (setq frame  (or frame  (selected-frame)))
  (let ((zoom-factor  (frame-parameter frame 'zoomed))
        (increment    (if flip (- frame-zoom-font-difference) frame-zoom-font-difference)))
    (unless zoom-factor (setq zoom-factor  0))
    (setq zoom-factor  (+ zoom-factor increment))
    (enlarge-font increment frame)
    (modify-frame-parameters frame (list (cons 'zoomed zoom-factor)))))

(unless (> emacs-major-version 22) (defalias 'zoom-out 'zoom-frm-out))
;;;###autoload
(defun zoom-frm-out (&optional frame flip)
  "Zoom FRAME out by `frame-zoom-font-difference', making text smaller.
If `frame-zoom-font-difference' is negative, make text larger.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, then make text larger.
This is equal but opposite to `zoom-frm-in'."
  (interactive (list (selected-frame) current-prefix-arg))
  (setq frame  (or frame  (selected-frame)))
  (let ((frame-zoom-font-difference  (- frame-zoom-font-difference)))
    (zoom-frm-in frame flip)))

;;;###autoload
(defun zoom-frm-unzoom (&optional frame)
  "Cancel zoom of FRAME."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (let ((zoom-factor  (frame-parameter frame 'zoomed)))
    (if (not zoom-factor)
        (error "Frame is not zoomed")
      (enlarge-font (- zoom-factor) frame)
      (modify-frame-parameters frame '((zoomed))))))

;;;###autoload
(defun toggle-zoom-frame (&optional frame)
  "Alternately zoom/unzoom FRAME by `frame-zoom-font-difference'."
  (interactive)
  (setq frame  (or frame  (selected-frame)))
  (if (frame-parameter frame 'zoomed) (zoom-frm-unzoom frame) (zoom-frm-in frame)))

(when (> emacs-major-version 22)
  (defun zoom-in (arg)
    "Zoom current frame or buffer in.
With a prefix arg, toggle between zooming frame and zooming buffer.
Frame zooming uses command `zoom-frm-in'.
Buffer zooming uses command `text-scale-increase'."
    (interactive "P")
    (when arg
      (setq zoom-frame/buffer  (if (eq zoom-frame/buffer 'frame) 'buffer 'frame))
      (message "%s zooming from now on" (upcase (symbol-name zoom-frame/buffer)))
      (sit-for 1))
    (if (eq zoom-frame/buffer 'frame)
        (zoom-frm-in)
      (with-current-buffer
          (if (string-match "mouse" (format "%S" (event-basic-type
                                                  last-command-event)))
              (window-buffer (posn-window (event-start last-command-event)))
            (current-buffer))
        (text-scale-increase 1))))

  (defun zoom-out (arg)
    "Zoom current frame or buffer out.
With a prefix arg, toggle between zooming frame and zooming buffer.
Frame zooming uses command `zoom-frm-out'.
Buffer zooming uses command `text-scale-decrease'."
    (interactive "P")
    (when arg
      (setq zoom-frame/buffer  (if (eq zoom-frame/buffer 'frame) 'buffer 'frame))
      (message "%s zooming from now on" (upcase (symbol-name zoom-frame/buffer)))
      (sit-for 1))
    (if (eq zoom-frame/buffer 'frame)
        (zoom-frm-out)
      (with-current-buffer
          (if (string-match "mouse" (format "%S" (event-basic-type
                                                  last-command-event)))
              (window-buffer (posn-window (event-start last-command-event)))
            (current-buffer))
        (text-scale-decrease 1))))

  (when (or (fboundp 'set-transient-map) ; Emacs 24.4+
            (fboundp 'set-temporary-overlay-map)) ; Emacs 24.3
            
    (defun zoom-in/out (arg)
      "Zoom current frame or buffer in or out.
A prefix arg determines the behavior, as follows:
 none       : Use 1 as the zoom amount.
 plain `C-u': Toggle between zooming frame and zooming buffer.
 0          : Unzoom: reset size to the default.
 other      : Use the numeric value as the zoom amount.

Similar to the behavior of command `text-scale-adjust', you can
continue to use any of the keys `+', `-', `0', and `C-u' repeatedly.
The zoom amount from the initial key sequence is used each time.

Example: `C-3 C-x C-- C-- C-- C--' zooms out 4 times with a zoom
amount of 3 each time.

The zoom amount for frame zooming is a point-size increment/decrement.
The zoom amount for buffer zooming is a number of text-scaling steps.

Frame zooming uses command `zoom-frm-in'.
Buffer zooming uses command `text-scale-increase'.

User option `zoom-frame/buffer' determines the default zoom type:
frame or buffer.  If the option value is `buffer' and you never use
plain `C-u' with this command then it acts like `text-scale-adjust'.

Remember that you can also use `C-u' when you are done zooming."
      (interactive "P")
      (when (or (equal arg '(4))  (eq ?\025 last-command-event)) ; `C-u'
        (setq zoom-frame/buffer  (if (eq zoom-frame/buffer 'frame) 'buffer 'frame)
              arg                1)
        (message "%s zooming from now on" (upcase (symbol-name zoom-frame/buffer)))
        (sit-for 1))    
      (let* ((ev               last-command-event)
             (echo-keystrokes  nil)
             (base             (event-basic-type ev))
             (step             (if (or (equal arg '(4))  (eq ?\025 last-command-event)) ; C-u
                                   'C-U-WAS-USED
                                 (setq arg  (prefix-numeric-value arg))
                                 (case base
                                   ((?+ ?=) arg)
                                   (?-      (- arg))
                                   (?0      0)
                                   (t       arg)))))
        (message (if (eq step 0)
                     "Reset to default size.  Use C-x C-+/C-- to zoom in/out"
                   "Use C-x C-+/C-- to zoom in/out, C-0 to reset (unzoom)"))
        (unless (eq step 'C-U-WAS-USED)
          (if (eq zoom-frame/buffer 'frame)
              (if (eq step 0)
                  (zoom-frm-unzoom)
                (let ((frame-zoom-font-difference  step)) (zoom-frm-in)))
            (with-current-buffer
                (if (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
                    (window-buffer (posn-window (event-start last-command-event)))
                  (current-buffer))
              (text-scale-increase step))))
        (let ((fun  (if (fboundp 'set-transient-map)
                        #'set-transient-map
                      #'set-temporary-overlay-map)))
          (funcall fun
                   (let ((map  (make-sparse-keymap)))
                     (dolist (mods  '(() (control)))
                       (dolist (key  '(?- ?+ ?= ?0)) ; The `=' key is often unshifted `+' key.
                         (define-key map (vector (append mods (list key)))
                           `(lambda () (interactive) (zoom-in/out ',arg)))))
                     (define-key map "\C-u" `(lambda () (interactive) (zoom-in/out ',arg)))
                     map)))))))

;;;###autoload
(defun zoom-all-frames-in (&optional flip)
  "Zoom all visible frames in, making text larger.
Zoom by `frame-zoom-font-difference' points.

If `frame-zoom-font-difference' is negative, make text smaller.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, make text smaller.
This is equal but opposite to `zoom-all-frames-out'."
  (interactive "P")
  (dolist (fr  (visible-frame-list))
    (zoom-frm-in fr flip)))

;;;###autoload
(defun zoom-all-frames-out (&optional flip)
  "Zoom all frames out, making text smaller.
Zoom by `frame-zoom-font-difference' points.

If `frame-zoom-font-difference' is negative, make text larger.
With prefix argument FLIP, reverse the direction:
if `frame-zoom-font-difference' is positive, make text larger.
This is equal but opposite to `zoom-all-frames-in'."
  (interactive "P")
  (dolist (fr  (visible-frame-list))
    (zoom-frm-out fr flip)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zoom-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zoom-frm.el ends here
