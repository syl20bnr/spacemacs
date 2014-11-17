;;; smooth-scrolling.el --- Make emacs scroll smoothly
;;
;; Copywrite (c) 2007-2013 Adam Spiers
;;
;; Filename: smooth-scrolling.el
;; Description: Make emacs scroll smoothly
;; Author: Adam Spiers <emacs-ss@adamspiers.org>, Jeremy Bondeson <jbondeson@gmail.com>
;; Maintainer: Adam Spiers <emacs-ss@adamspiers.org>
;; URL: http://github.com/aspiers/smooth-scrolling/
;; Version: 20131219.2039
;; X-Original-Version: 1.0.4
;; Keywords: convenience
;; GitHub: http://github.com/aspiers/smooth-scrolling/

;; This file is not part of GNU Emacs

;;; License:
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;;; Commentary:

;; Make emacs scroll smoothly, keeping the point away from the top and
;; bottom of the current buffer's window in order to keep lines of
;; context around the point visible as much as possible, whilst
;; avoiding sudden scroll jumps which are visually confusing.
;;
;; This is a nice alternative to all the native scroll-* custom
;; variables, which unfortunately cannot provide this functionality
;; perfectly.  `scroll-margin' comes closest, but has some bugs
;; (e.g. with handling of mouse clicks).  See
;;
;;   http://www.emacswiki.org/cgi-bin/wiki/SmoothScrolling
;;
;; for the gory details.
;;

;;; Notes:

;; This only affects the behaviour of the `next-line' and
;; `previous-line' functions, usually bound to the cursor keys and
;; C-n/C-p, and repeated isearches (`isearch-repeat').  Other methods
;; of moving the point will behave as normal according to the standard
;; custom variables.
;;
;; Prefix arguments to `next-line' and `previous-line' are honoured.
;; The minimum number of lines are scrolled in order to keep the
;; point outside the margin.
;;
;; There is one case where moving the point in this fashion may cause
;; a jump: if the point is placed inside one of the margins by another
;; method (e.g. left mouse click, or M-x goto-line) and then moved in
;; the normal way, the advice code will scroll the minimum number of
;; lines in order to keep the point outside the margin.  This jump may
;; cause some slight confusion at first, but hopefully it is justified
;; by the benefit of automatically ensuring `smooth-scroll-margin'
;; lines of context are visible around the point as often as possible.

;;; TODO:
;;
;; - Maybe add option to avoid scroll jumps when point is within
;;   margin.
;;
;; - Minimize the number of autoloads in the file.  Currently
;;   everything is marked as such.

;;; Acknowledgements:

;; Thanks to Mark Hulme-Jones and consolers on #emacs for helping
;; debug issues with line-wrapping.

;;; Change Log:
;; 19 Dec 2013 -- v1.0.4
;;      * Disabled scrolling while a keyboard macro is executing in
;;        order to prevent a premature termination of the macro by
;;        the mode throwing an error such as "End of Buffer"
;; 02 Jun 2013 -- v1.0.3
;;      * Fixed Issue #3 where bounds checking was not being performed
;;        prior to calls to 'count-lines' and 'count-screen-lines'
;;        functions.
;; 14 Apr 2013 -- v1.0.2
;;      * Adam Spiers GitHub account now houses the canonical
;;        repository.
;; 06 Dec 2011 -- v1.0.1
;;	* Altered structure to conform to package.el standards.
;;	* Restructured code to group settings changes
;;	* Set "redisplay-dont-pause" to true.
;; ?? ??? 2007 -- v1.0.0
;;      * Original version from Adam Spiers

;;; Code:

;;;_ + defcustoms

;;;###autoload
(defcustom smooth-scroll-margin 10
  "Number of lines of visible margin at the top and bottom of a window.
If the point is within these margins, then scrolling will occur
smoothly for `previous-line' at the top of the window, and for
`next-line' at the bottom.

This is very similar in its goal to `scroll-margin'.  However, it
is implemented by activating `smooth-scroll-down' and
`smooth-scroll-up' advise via `defadvice' for `previous-line' and
`next-line' respectively.  As a result it avoids problems
afflicting `scroll-margin', such as a sudden jump and unexpected
highlighting of a region when the mouse is clicked in the margin.

Scrolling only occurs when the point is closer to the window
boundary it is heading for (top or bottom) than the middle of the
window.  This is to intelligently handle the case where the
margins cover the whole buffer (e.g. `smooth-scroll-margin' set
to 5 and `window-height' returning 10 or less).

See also `smooth-scroll-strict-margins'."
  :type  'integer
  :group 'windows)

;;;###autoload
(defcustom smooth-scroll-strict-margins t
  "If true, the advice code supporting `smooth-scroll-margin'
will use `count-screen-lines' to determine the number of
*visible* lines between the point and the window top/bottom,
rather than `count-lines' which obtains the number of actual
newlines.  This is because there might be extra newlines hidden
by a mode such as folding-mode, outline-mode, org-mode etc., or
fewer due to very long lines being displayed wrapped when
`truncate-lines' is nil.

However, using `count-screen-lines' can supposedly cause
performance issues in buffers with extremely long lines.  Setting
`cache-long-line-scans' may be able to address this;
alternatively you can set this variable to nil so that the advice
code uses `count-lines', and put up with the fact that sometimes
the point will be allowed to stray into the margin."
  :type  'boolean
  :group 'windows)
;;;_ + helper functions
;;;###autoload
(defun smooth-scroll-lines-from-window-top ()
  "Work out, using the function indicated by
`smooth-scroll-strict-margins', what the current screen line is,
relative to the top of the window.  Counting starts with 1 referring
to the top line in the window."
  (interactive)
  (cond ((>= (window-start) (point))
         ;; In this case, count-screen-lines would return 0, or
         ;; error, so we override.
         1)
        (smooth-scroll-strict-margins
         (count-screen-lines (window-start) (point) 'count-final-newline))
        (t
         (count-lines (window-start) (point)))))

;;;###autoload
(defun smooth-scroll-lines-from-window-bottom ()
  "Work out, using the function indicated by
`smooth-scroll-strict-margins', how many screen lines there are
between the point and the bottom of the window.  Counting starts
with 1 referring to the bottom line in the window."
  (interactive)
  (cond ((<= (window-end) (point))
         ;; In this case, count-screen-lines would return 0, or
         ;; error, so we override.
         1)
        (smooth-scroll-strict-margins
         (count-screen-lines (point) (window-end)))
        (t
         (count-lines (point) (window-end)))))
;;;_ + after advice

;;;###autoload
(defun smooth-scroll-down ()
  "Scroll down smoothly if cursor is within `smooth-scroll-margin'
lines of the top of the window."
  (and
   ;; Only scroll down if there is buffer above the start of the window.
   (> (line-number-at-pos (window-start)) 1)
   (let ((lines-from-window-top
          (smooth-scroll-lines-from-window-top)))
     (and
      ;; [GitHub Issue #5] Keyboard macros execute in interactive mode so
      ;; we need to be careful not to do anything.
      (not executing-kbd-macro)
      ;; Only scroll down if we're within the top margin
      (<= lines-from-window-top smooth-scroll-margin)
      ;; Only scroll down if we're in the top half of the window
      (<= lines-from-window-top
          ;; N.B. `window-height' includes modeline, so if it returned 21,
          ;; that would mean exactly 10 lines in the top half and 10 in
          ;; the bottom.  22 (or any even number) means there's one in the
          ;; middle.  In both cases the following expression will
          ;; yield 10:
          (/ (1- (window-height)) 2))
      (save-excursion
        (scroll-down
              (1+ (- smooth-scroll-margin lines-from-window-top))))))))

;;;###autoload
(defun smooth-scroll-up ()
  "Scroll up smoothly if cursor is within `smooth-scroll-margin'
lines of the bottom of the window."
  (and
   ;; [GitHub Issue #5] Keyboard macros execute in interactive mode so
   ;; we need to be careful not to do anything.
   (not executing-kbd-macro)
   ;; Only scroll up if there is buffer below the end of the window.
   (< (window-end) (buffer-end 1))
   (let ((lines-from-window-bottom
          (smooth-scroll-lines-from-window-bottom)))
     (and
      ;; Only scroll up if we're within the bottom margin
      (<= lines-from-window-bottom smooth-scroll-margin)
      ;; Only scroll up if we're in the bottom half of the window.
      (<= lines-from-window-bottom
          ;; See above notes on `window-height'.
          (/ (1- (window-height)) 2))
      (save-excursion
        (scroll-up
         (1+ (- smooth-scroll-margin lines-from-window-bottom))))))))

;;;###autoload
(defadvice previous-line (after smooth-scroll-down
                            (&optional arg try-vscroll)
                            activate)
  (smooth-scroll-down))

;;;###autoload
(defadvice next-line (after smooth-scroll-up
                            (&optional arg try-vscroll)
                            activate)
  (smooth-scroll-up))

;;;###autoload
(defadvice isearch-repeat (after isearch-smooth-scroll
                                 (direction)
                                 activate)
  (if (eq direction 'forward)
      (smooth-scroll-up)
    (smooth-scroll-down)))

;;;###autoload
(progn
  (setq scroll-margin 0)
  (setq redisplay-dont-pause t))

;;;_ + provide
(provide 'smooth-scrolling)
;;; smooth-scrolling.el ends here
