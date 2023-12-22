;;; popup.el --- Visual Popup User Interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2015  Tomohiro Matsuyama
;; Copyright (c) 2020-2024 Jen-Chieh Shen

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/auto-complete/popup-el
;; Keywords: lisp
;; Version: 0.5.9
;; Package-Requires: ((emacs "24.3"))

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

;; popup.el is a visual popup user interface library for Emacs.  This
;; provides a basic API and common UI widgets such as popup tooltips
;; and popup menus.
;; See README.markdown for more information.

;;; Code:

(require 'cl-lib)
(require 'mule)

(defconst popup-version "0.5.9")



;;; Utilities

(defun popup-calculate-max-width (max-width)
  "Determines whether the width with MAX-WIDTH desired is character or window
proportion based, And return the result."
  (cl-typecase max-width
    (integer max-width)
    (float (* (ceiling (/ (round (* max-width (window-width))) 10.0)) 10))))

(defvar popup-use-optimized-column-computation t
  "Use the optimized column computation routine.
If there is a problem, please set it nil.")

(defmacro popup-aif (test then &rest else)
  "Anaphoric if."
  (declare (indent 2))
  `(let ((it ,test))
     (if it ,then ,@else)))

(defmacro popup-awhen (test &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))

(defun popup-x-to-string (x)
  "Convert any object to string efficiently.
This is faster than `prin1-to-string' in many cases."
  (cl-typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun popup-substring-by-width (string width)
  "Return a cons cell of substring and remaining string by
splitting with WIDTH."
  ;; Expand tabs into 4 spaces
  (setq string (replace-regexp-in-string "\t" "    " string))
  (cl-loop with len = (length string)
           with w = 0
           for l from 0
           for c in (append string nil)
           while (<= (cl-incf w (char-width c)) width)
           finally return
           (if (< l len)
               (cons (substring string 0 l) (substring string l))
             (list string))))

(defun popup-fill-string (string &optional width max-width justify squeeze)
  "Split STRING into fixed width strings and return a cons cell
like \(WIDTH . ROWS). Here, the car WIDTH indicates the actual
maxim width of ROWS.

The argument WIDTH specifies the width of filling each
paragraph. WIDTH nil means don't perform any justification and
word wrap. Note that this function doesn't add any padding
characters at the end of each row.

MAX-WIDTH, if WIDTH is nil, specifies the maximum number of
columns.

The optional fourth argument JUSTIFY specifies which kind of
justification to do: `full', `left', `right', `center', or
`none' (equivalent to nil).  A value of t means handle each
paragraph as specified by its text properties.

SQUEEZE nil means leave whitespaces other than line breaks
untouched."
  (if (eq width 0)
      (error "Can't fill string with 0 width"))
  (if width
      (setq max-width width))
  (with-temp-buffer
    (let ((tab-width 4)
          (fill-column width)
          (left-margin 0)
          (kinsoku-limit 1)
          indent-tabs-mode
          row rows)
      (insert string)
      (untabify (point-min) (point-max))
      (if width
          (fill-region (point-min) (point-max) justify (not squeeze)))
      (goto-char (point-min))
      (setq width 0)
      (while (prog2
                 (let ((line (buffer-substring
                              (point) (progn (end-of-line) (point)))))
                   (if max-width
                       (while (progn
                                (setq row (truncate-string-to-width line max-width)
                                      width (max width (string-width row)))
                                (push row rows)
                                (if (not (= (length row) (length line)))
                                    (setq line (substring line (length row))))))
                     (setq width (max width (string-width line)))
                     (push line rows)))
                 (< (point) (point-max))
               (beginning-of-line 2)))
      (cons width (nreverse rows)))))

(defmacro popup-save-buffer-state (&rest body)
  (declare (indent 0))
  `(save-excursion
     (let ((buffer-undo-list t)
           (inhibit-read-only t)
           (modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p modified)))))

(defun popup-vertical-motion (column direction)
  "A portable version of `vertical-motion'."
  (when (bound-and-true-p display-line-numbers-mode)
    (setq column (- column (line-number-display-width 'columns))))
  (if (>= emacs-major-version 23)
      (vertical-motion (cons column direction))
    (vertical-motion direction)
    (move-to-column (+ (current-column) column))))

(defun popup-last-line-of-buffer-p ()
  "Return non-nil if the cursor is at the last line of the
buffer."
  (save-excursion (end-of-line) (/= (forward-line) 0)))

(defun popup-lookup-key-by-event (function event)
  (or (funcall function (vector event))
      (if (symbolp event)
          (popup-aif (get event 'event-symbol-element-mask)
              (funcall function
                       (vector (logior (or (get (car it) 'ascii-character)
                                           0)
                                       (cadr it))))))))



;;; Core

(defgroup popup nil
  "Visual Popup User Interface"
  :group 'lisp
  :prefix "popup-")

(defface popup-face
  '((t (:inherit default :background "lightgray" :foreground "black")))
  "Face for popup."
  :group 'popup)

(defface popup-summary-face
  '((t (:inherit popup-face :foreground "dimgray")))
  "Face for popup summary."
  :group 'popup)

(defface popup-scroll-bar-foreground-face
  '((t (:background "black")))
  "Foreground face for scroll-bar."
  :group 'popup)

(defface popup-scroll-bar-background-face
  '((t (:background "gray")))
  "Background face for scroll-bar."
  :group 'popup)

(defvar popup-instances nil
  "Popup instances.")

(defvar popup-scroll-bar-foreground-char
  (propertize " " 'face 'popup-scroll-bar-foreground-face)
  "Foreground character for scroll-bar.")

(defvar popup-scroll-bar-background-char
  (propertize " " 'face 'popup-scroll-bar-background-face)
  "Background character for scroll-bar.")

(cl-defstruct popup
  point row column width height min-height direction overlays keymap
  parent depth
  face mouse-face selection-face summary-face
  margin-left margin-right margin-left-cancel scroll-bar symbol
  cursor offset scroll-top current-height list newlines
  pattern original-list invis-overlays)

(defun popup-item-propertize (item &rest properties)
  "Same as `propertize' except that this avoids overriding
existed value with `nil' property."
  (cl-loop for (k v) on properties by 'cddr
           if v append (list k v) into props
           finally return
           (apply 'propertize
                  (popup-x-to-string item)
                  props)))

(defun popup-item-property (item property)
  "Same as `get-text-property' except that this returns nil if
ITEM is not string."
  (if (stringp item)
      (get-text-property 0 property item)))

(defun popup-replace-displayable (str &optional rep)
  "Replace non-displayable character from STR.

Optional argument REP is the replacement string of
non-displayable character."
  (let ((rep (or rep ""))
        (results (list)))
    (dolist (string (split-string str ""))
      (let* ((char (string-to-char string))
             (string (if (char-displayable-p char)
                         string
                       rep)))
        (push string results)))
    (string-join (reverse results))))

(cl-defun popup-make-item (name
                           &key
                           value
                           face
                           mouse-face
                           selection-face
                           sublist
                           document
                           symbol
                           summary)
  "Utility function to make popup item. See also
`popup-item-propertize'."
  (popup-item-propertize name
                         'value value
                         'popup-face face
                         'popup-mouse-face mouse-face
                         'selection-face selection-face
                         'document document
                         'symbol symbol
                         'summary summary
                         'sublist sublist))

(defsubst popup-item-value (item)               (popup-item-property item 'value))
(defsubst popup-item-value-or-self (item)       (or (popup-item-value item) item))
(defsubst popup-item-face (item)                (popup-item-property item 'popup-face))
(defsubst popup-item-mouse-face (item)          (popup-item-property item 'popup-mouse-face))
(defsubst popup-item-selection-face (item)      (popup-item-property item 'selection-face))
(defsubst popup-item-document (item)            (popup-item-property item 'document))
(defsubst popup-item-summary (item)             (popup-item-property item 'summary))
(defsubst popup-item-symbol (item)              (popup-item-property item 'symbol))
(defsubst popup-item-sublist (item)             (popup-item-property item 'sublist))

(defun popup-item-documentation (item)
  (let ((doc (popup-item-document item)))
    (if (functionp doc)
        (setq doc (funcall doc (popup-item-value-or-self item))))
    doc))

(defun popup-item-show-help-1 (item)
  (let ((doc (popup-item-documentation item)))
    (when doc
      (with-current-buffer (get-buffer-create " *Popup Help*")
        (erase-buffer)
        (insert doc)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      t)))

(defun popup-item-show-help-with-event-loop (item)
  (save-window-excursion
    (when (popup-item-show-help-1 item)
      (cl-loop do (clear-this-command-keys)
               for key = (read-key-sequence-vector nil)
               do
               (cl-case (key-binding key)
                 (scroll-other-window
                  (scroll-other-window))
                 (scroll-other-window-down
                  (scroll-other-window-down nil))
                 (otherwise
                  (setq unread-command-events (append key unread-command-events))
                  (cl-return)))))))

(defun popup-item-show-help (item &optional persist)
  "Display the documentation of ITEM with `display-buffer'. If
PERSIST is nil, the documentation buffer will be closed
automatically, meaning interal event loop ensures the buffer to
be closed. Otherwise, the buffer will be just displayed as
usual."
  (when item
    (if (not persist)
        (popup-item-show-help-with-event-loop item)
      (popup-item-show-help-1 item))))

(defun popup-set-list (popup list)
  (popup-set-filtered-list popup list)
  (setf (popup-pattern popup) nil)
  (setf (popup-original-list popup) list))

(defun popup-set-filtered-list (popup list)
  (let ((offset
         (if (> (popup-direction popup) 0)
             0
           (max (- (popup-height popup) (length list)) 0))))
    (setf (popup-list popup) list
          (popup-offset popup) offset)))

(defun popup-selected-item (popup)
  (nth (popup-cursor popup) (popup-list popup)))

(defun popup-selected-line (popup)
  (- (popup-cursor popup) (popup-scroll-top popup)))

(defun popup-line-overlay (popup line)
  (aref (popup-overlays popup) line))

(defun popup-selected-line-overlay (popup)
  (popup-line-overlay popup (popup-selected-line popup)))

(defun popup-hide-line (popup line)
  (let ((overlay (popup-line-overlay popup line)))
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'after-string nil)))

(defun popup-line-hidden-p (popup line)
  (let ((overlay (popup-line-overlay popup line)))
    (and (eq (overlay-get overlay 'display) nil)
         (eq (overlay-get overlay 'after-string) nil))))

(cl-defun popup-set-line-item (popup
                               line
                               &key
                               item
                               face
                               mouse-face
                               margin-left
                               margin-right
                               scroll-bar-char
                               symbol
                               summary
                               summary-face
                               keymap)
  (let* ((overlay (popup-line-overlay popup line))
         (content (popup-create-line-string popup (popup-x-to-string item)
                                            :margin-left margin-left
                                            :margin-right margin-right
                                            :symbol symbol
                                            :summary summary
                                            :summary-face summary-face))
         (start 0)
         (prefix (overlay-get overlay 'prefix))
         (postfix (overlay-get overlay 'postfix))
         end)
    (put-text-property 0 (length content) 'popup-item item content)
    (put-text-property 0 (length content) 'keymap keymap content)
    ;; Overlap face properties
    (when (get-text-property start 'face content)
      (setq start (next-single-property-change start 'face content)))
    (while (and start (setq end (next-single-property-change start 'face content)))
      (put-text-property start end 'face face content)
      (setq start (next-single-property-change end 'face content)))
    (when start
      (put-text-property start (length content) 'face face content))
    (when mouse-face
      (put-text-property 0 (length content) 'mouse-face mouse-face content))
    (let ((prop (if (overlay-get overlay 'dangle)
                    'after-string
                  'display)))
      (overlay-put overlay
                   prop
                   (concat prefix
                           content
                           scroll-bar-char
                           postfix)))))

(cl-defun popup-create-line-string (popup
                                    string
                                    &key
                                    margin-left
                                    margin-right
                                    symbol
                                    summary
                                    summary-face)
  (let* ((popup-width (popup-width popup))
         (summary-width (string-width summary))
         (content-width (max
                         (min popup-width (string-width string))
                         (- popup-width
                            (if (> summary-width 0)
                                (+ summary-width 2)
                              0))))
         (string (car (popup-substring-by-width string content-width)))
         (string-width (string-width string))
         (spacing (max (- popup-width string-width summary-width)
                       (if (> popup-width string-width) 1 0)))
         (truncated-summary
          (car (popup-substring-by-width
                summary (max (- popup-width string-width spacing) 0)))))
    (when summary-face
      (put-text-property 0 (length truncated-summary)
                         'face summary-face truncated-summary))
    (concat margin-left
            string
            (make-string spacing ? )
            truncated-summary
            symbol
            margin-right)))

(defun popup-live-p (popup)
  "Return non-nil if POPUP is alive."
  (and popup (popup-overlays popup) t))

(defun popup-child-point (popup &optional offset)
  (overlay-end
   (popup-line-overlay
    popup
    (or offset
        (popup-selected-line popup)))))

(defun popup-calculate-direction (height row)
  "Return a proper direction when displaying a popup on this
window. HEIGHT is the a height of the popup, and ROW is a line
number at the point."
  (let* ((remaining-rows (- (max 1 (- (window-text-height)
                                      (if mode-line-format 1 0)
                                      (if header-line-format 1 0)))
                            (count-lines (window-start) (point))))
         (enough-space-above (> row height))
         (enough-space-below (<= height remaining-rows)))
    (if (and enough-space-above
             (not enough-space-below))
        -1
      1)))

(cl-defun popup-create (point
                        width
                        height
                        &key
                        min-height
                        max-width
                        around
                        (face 'popup-face)
                        mouse-face
                        (selection-face face)
                        (summary-face 'popup-summary-face)
                        scroll-bar
                        margin-left
                        margin-right
                        symbol
                        parent
                        parent-offset
                        keymap)
  "Create a popup instance at POINT with WIDTH and HEIGHT.

MIN-HEIGHT is a minimal height of the popup. The default value is
0.

MAX-WIDTH is the maximum width of the popup. The default value is
nil (no limit). If a floating point, the value refers to the ratio of
the window. If an integer, limit is in characters.

If AROUND is non-nil, the popup will be displayed around the
point but not at the point.

FACE is a background face of the popup. The default value is POPUP-FACE.

SELECTION-FACE is a foreground (selection) face of the popup The
default value is POPUP-FACE.

If SCROLL-BAR is non-nil, the popup will have a scroll bar at the
right.

If MARGIN-LEFT is non-nil, the popup will have a margin at the
left.

If MARGIN-RIGHT is non-nil, the popup will have a margin at the
right.

SYMBOL is a single character which indicates a kind of the item.

PARENT is a parent popup instance. If PARENT is omitted, the
popup will be a root instance.

PARENT-OFFSET is a row offset from the parent popup.

KEYMAP is a keymap that will be put on the popup contents."
  (or margin-left (setq margin-left 0))
  (or margin-right (setq margin-right 0))
  (unless point
    (setq point
          (if parent (popup-child-point parent parent-offset) (point))))
  (when max-width
    (setq width (min width (popup-calculate-max-width max-width))))
  (save-excursion
    (goto-char point)
    (let* ((col-row (posn-col-row (posn-at-point)))
           (row (cdr col-row))
           (column (car col-row))
           (overlays (make-vector height nil))
           (popup-width (+ width
                           (if scroll-bar 1 0)
                           margin-left
                           margin-right
                           (if symbol 2 0)))
           margin-left-cancel
           (window (selected-window))
           (window-start (window-start))
           (window-hscroll (window-hscroll))
           (window-width (window-width))
           (right (+ column popup-width))
           (overflow (and (> right window-width)
                          (>= right popup-width)))
           (foldable (and (null parent)
                          (>= column popup-width)))
           (direction (or
                       ;; Currently the direction of cascade popup won't be changed
                       (and parent (popup-direction parent))

                       ;; Calculate direction
                       (popup-calculate-direction height row)))
           (depth (if parent (1+ (popup-depth parent)) 0))
           (newlines (max 0 (+ (- height (count-lines point (point-max))) (if around 1 0))))
           invis-overlays
           current-column)
      ;; Case: no newlines at the end of the buffer
      (when (> newlines 0)
        (popup-save-buffer-state
          (goto-char (point-max))
          (insert (make-string newlines ?\n))))

      ;; Case: the popup overflows
      (if overflow
          (if foldable
              (progn
                (cl-decf column (- popup-width margin-left margin-right))
                (unless around (move-to-column column)))
            (when (not truncate-lines)
              ;; Truncate.
              (let ((d (1+ (- popup-width (- window-width column)))))
                (cl-decf popup-width d)
                (cl-decf width d)))
            (cl-decf column margin-left))
        (cl-decf column margin-left))

      ;; Case: no space at the left
      (when (and (null parent)
                 (< column 0))
        ;; Cancel margin left
        (setq column 0)
        (cl-decf popup-width margin-left)
        (setq margin-left-cancel t))

      (dotimes (i height)
        (let (overlay begin w (dangle t) (prefix "") (postfix ""))
          (when around
            (popup-vertical-motion column direction))
          (cl-loop for ov in (overlays-in (save-excursion
                                            (beginning-of-visual-line)
                                            (point))
                                          (save-excursion
                                            (end-of-visual-line)
                                            (point)))
                   when (and (not (overlay-get ov 'popup))
                             (not (overlay-get ov 'popup-item))
                             (or (overlay-get ov 'invisible)
                                 (overlay-get ov 'display)))
                   do (progn
                        (push (list ov (overlay-get ov 'display)) invis-overlays)
                        (overlay-put ov 'display "")))
          (setq around t)
          (setq current-column (car (posn-col-row (posn-at-point))))

          (when (< current-column column)
            ;; Extend short buffer lines by popup prefix (line of spaces)
            (setq prefix (make-string
                          (+ (if (= current-column 0)
                                 (- window-hscroll current-column)
                               0)
                             (- column current-column))
                          ? )))

          (setq begin (point))
          (setq w (+ popup-width (length prefix)))
          (while (and (not (eolp)) (> w 0))
            (setq dangle nil)
            (cl-decf w (char-width (char-after)))
            (forward-char))
          (if (< w 0)
              (setq postfix (make-string (- w) ? )))

          (setq overlay (make-overlay begin (point)))
          (overlay-put overlay 'popup t)
          (overlay-put overlay 'window window)
          (overlay-put overlay 'dangle dangle)
          (overlay-put overlay 'prefix prefix)
          (overlay-put overlay 'postfix postfix)
          (overlay-put overlay 'width width)
          (aset overlays
                (if (> direction 0) i (- height i 1))
                overlay)))
      (cl-loop for p from (- 10000 (* depth 1000))
               for overlay in (nreverse (append overlays nil))
               do (overlay-put overlay 'priority p))
      (let ((it (make-popup :point point
                            :row row
                            :column column
                            :width width
                            :height height
                            :min-height min-height
                            :direction direction
                            :parent parent
                            :depth depth
                            :face face
                            :mouse-face mouse-face
                            :selection-face selection-face
                            :summary-face summary-face
                            :margin-left margin-left
                            :margin-right margin-right
                            :margin-left-cancel margin-left-cancel
                            :scroll-bar scroll-bar
                            :symbol symbol
                            :cursor 0
                            :offset 0
                            :scroll-top 0
                            :current-height 0
                            :list nil
                            :newlines newlines
                            :overlays overlays
                            :invis-overlays invis-overlays
                            :keymap keymap)))
        (push it popup-instances)
        it))))

(defun popup-delete (popup)
  "Delete POPUP instance."
  (when (popup-live-p popup)
    (popup-hide popup)
    (mapc 'delete-overlay (popup-overlays popup))
    (setf (popup-overlays popup) nil)
    (setq popup-instances (delq popup popup-instances))
    ;; Restore newlines state
    (let ((newlines (popup-newlines popup)))
      (when (> newlines 0)
        (popup-save-buffer-state
          (goto-char (point-max))
          (dotimes (i newlines)
            (if (and (char-before)
                     (= (char-before) ?\n))
                (delete-char -1)))))))
  nil)

(defun popup-draw (popup)
  "Draw POPUP."
  (cl-loop for (ov olddisplay) in (popup-invis-overlays popup)
           do (overlay-put ov 'display ""))

  (cl-loop with height = (popup-height popup)
           with min-height = (popup-min-height popup)
           with popup-face = (popup-face popup)
           with mouse-face = (popup-mouse-face popup)
           with selection-face = (popup-selection-face popup)
           with summary-face-0 = (popup-summary-face popup)
           with list = (popup-list popup)
           with length = (length list)
           with thum-size = (max (/ (* height height) (max length 1)) 1)
           with page-size = (/ (+ 0.0 (max length 1)) height)
           with scroll-bar = (popup-scroll-bar popup)
           with margin-left = (make-string (if (popup-margin-left-cancel popup) 0 (popup-margin-left popup)) ? )
           with margin-right = (make-string (popup-margin-right popup) ? )
           with symbol = (popup-symbol popup)
           with cursor = (popup-cursor popup)
           with scroll-top = (popup-scroll-top popup)
           with offset = (popup-offset popup)
           with keymap = (popup-keymap popup)
           for o from offset
           for i from scroll-top
           while (< o height)
           for item in (nthcdr scroll-top list)
           for page-index = (* thum-size (/ o thum-size))
           for face = (if (= i cursor)
                          (or (popup-item-selection-face item) selection-face)
                        (or (popup-item-face item) popup-face))
           for summary-face = (unless (= i cursor) summary-face-0)
           for empty-char = (propertize " " 'face face)
           for scroll-bar-char = (if scroll-bar
                                     (cond
                                      ((and (not (eq scroll-bar :always))
                                            (<= page-size 1))
                                       empty-char)
                                      ((and (> page-size 1)
                                            (>= cursor (* page-index page-size))
                                            (< cursor (* (+ page-index thum-size) page-size)))
                                       popup-scroll-bar-foreground-char)
                                      (t
                                       popup-scroll-bar-background-char))
                                   "")
           for sym = (if symbol
                         (concat " " (or (popup-item-symbol item) " "))
                       "")
           for summary = (or (popup-item-summary item) "")

           do
           ;; Show line and set item to the line
           (popup-set-line-item popup o
                                :item item
                                :face face
                                :mouse-face mouse-face
                                :margin-left margin-left
                                :margin-right margin-right
                                :scroll-bar-char scroll-bar-char
                                :symbol sym
                                :summary summary
                                :summary-face summary-face
                                :keymap keymap)

           finally
           ;; Remember current height
           (setf (popup-current-height popup) (- o offset))

           ;; Hide remaining lines
           (let ((scroll-bar-char (if scroll-bar (propertize " " 'face popup-face) ""))
                 (symbol (if symbol " " "")))
             (if (> (popup-direction popup) 0)
                 (progn
                   (when min-height
                     (while (< o min-height)
                       (popup-set-line-item popup o
                                            :item ""
                                            :face popup-face
                                            :margin-left margin-left
                                            :margin-right margin-right
                                            :scroll-bar-char scroll-bar-char
                                            :symbol symbol
                                            :summary "")
                       (cl-incf o)))
                   (while (< o height)
                     (popup-hide-line popup o)
                     (cl-incf o)))
               (cl-loop with h = (if min-height (- height min-height) offset)
                        for o from 0 below offset
                        if (< o h)
                        do (popup-hide-line popup o)
                        if (>= o h)
                        do (popup-set-line-item popup o
                                                :item ""
                                                :face popup-face
                                                :margin-left margin-left
                                                :margin-right margin-right
                                                :scroll-bar-char scroll-bar-char
                                                :symbol symbol
                                                :summary ""))))))

(defun popup-hide (popup)
  "Hide POPUP."
  (cl-loop for (ov olddisplay) in (popup-invis-overlays popup)
           do (overlay-put ov 'display olddisplay))
  (dotimes (i (popup-height popup))
    (popup-hide-line popup i)))

(defun popup-hidden-p (popup)
  "Return non-nil if POPUP is hidden."
  (let ((hidden t))
    (when (popup-live-p popup)
      (dotimes (i (popup-height popup))
        (unless (popup-line-hidden-p popup i)
          (setq hidden nil))))
    hidden))

(defun popup-jump (popup cursor)
  "Jump to a position specified by CURSOR of POPUP and draw."
  (let ((scroll-top (popup-scroll-top popup)))
    ;; Do not change page as much as possible.
    (unless (and (<= scroll-top cursor)
                 (< cursor (+ scroll-top (popup-height popup))))
      (setf (popup-scroll-top popup) cursor))
    (setf (popup-cursor popup) cursor)
    (popup-draw popup)))

(defun popup-select (popup i)
  "Select the item at I of POPUP and draw."
  (setq i (+ i (popup-offset popup)))
  (when (and (<= 0 i) (< i (popup-height popup)))
    (setf (popup-cursor popup) i)
    (popup-draw popup)
    t))

(defun popup-next (popup)
  "Select the next item of POPUP and draw."
  (let ((height (popup-height popup))
        (cursor (1+ (popup-cursor popup)))
        (scroll-top (popup-scroll-top popup))
        (length (length (popup-list popup))))
    (cond
     ((>= cursor length)
      ;; Back to first page
      (setq cursor 0
            scroll-top 0))
     ((= cursor (+ scroll-top height))
      ;; Go to next page
      (setq scroll-top (min (1+ scroll-top) (max (- length height) 0)))))
    (setf (popup-cursor popup) cursor
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))

(defun popup-previous (popup)
  "Select the previous item of POPUP and draw."
  (let ((height (popup-height popup))
        (cursor (1- (popup-cursor popup)))
        (scroll-top (popup-scroll-top popup))
        (length (length (popup-list popup))))
    (cond
     ((< cursor 0)
      ;; Go to last page
      (setq cursor (1- length)
            scroll-top (max (- length height) 0)))
     ((= cursor (1- scroll-top))
      ;; Go to previous page
      (cl-decf scroll-top)))
    (setf (popup-cursor popup) cursor
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))

(defun popup-page-next (popup)
  "Select next item of POPUP per `popup-height' range.
Pages down through POPUP."
  (dotimes (counter (1- (popup-height popup)))
    (popup-next popup)))

(defun popup-page-previous (popup)
  "Select previous item of POPUP per `popup-height' range.
Pages up through POPUP."
  (dotimes (counter (1- (popup-height popup)))
    (popup-previous popup)))

(defun popup-scroll-down (popup &optional n)
  "Scroll down N of POPUP and draw."
  (let ((scroll-top (min (+ (popup-scroll-top popup) (or n 1))
                         (- (length (popup-list popup)) (popup-height popup)))))
    (setf (popup-cursor popup) scroll-top
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))

(defun popup-scroll-up (popup &optional n)
  "Scroll up N of POPUP and draw."
  (let ((scroll-top (max (- (popup-scroll-top popup) (or n 1))
                         0)))
    (setf (popup-cursor popup) scroll-top
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))



;;; Popup Incremental Search

(defface popup-isearch-match
  '((t (:inherit default :background "sky blue")))
  "Popup isearch match face."
  :group 'popup)

(defvar popup-isearch-cursor-color "blue")

(defvar popup-isearch-keymap
  (let ((map (make-sparse-keymap)))
    ;;(define-key map "\r"        'popup-isearch-done)
    (define-key map "\C-g"      'popup-isearch-cancel)
    (define-key map "\C-b"      'popup-isearch-close)
    (define-key map [left]      'popup-isearch-close)
    (define-key map "\C-h"      'popup-isearch-delete)
    (define-key map (kbd "DEL") 'popup-isearch-delete)
    (define-key map (kbd "C-y") 'popup-isearch-yank)
    map))

(defvar popup-menu-show-quick-help-function 'popup-menu-show-quick-help
  "Function used for showing quick help by `popup-menu*'.")

(defcustom popup-isearch-regexp-builder-function #'regexp-quote
  "Function used to construct a regexp from a pattern. You may for instance
  provide a function that replaces spaces by '.+' if you like helm or ivy style
  of completion."
  :type 'function)

(defsubst popup-isearch-char-p (char)
  (and (integerp char)
       (<= 32 char)
       (<= char 126)))

(defun popup-isearch-filter-list (pattern list)
  (cl-loop with regexp = (funcall popup-isearch-regexp-builder-function pattern)
           for item in list
           do
           (unless (stringp item)
             (setq item (popup-item-propertize (popup-x-to-string item)
                                               'value item)))
           if (string-match regexp item)
           collect
           (let ((beg (match-beginning 0))
                 (end (match-end 0)))
             (alter-text-property 0 (length item) 'face
                                  (lambda (prop)
                                    (unless (eq prop 'popup-isearch-match)
                                      prop))
                                  item)
             (put-text-property beg end
                                'face 'popup-isearch-match
                                item)
             item)))

(defun popup-isearch-prompt (popup pattern)
  (format "Pattern: %s" (if (= (length (popup-list popup)) 0)
                            (propertize pattern 'face 'isearch-fail)
                          pattern)))

(defun popup-isearch-update (popup filter pattern &optional callback)
  (setf (popup-cursor popup) 0
        (popup-scroll-top popup) 0
        (popup-pattern popup) pattern)
  (let ((list (funcall filter pattern (popup-original-list popup))))
    (popup-set-filtered-list popup list)
    (if callback
        (funcall callback list)))
  (popup-draw popup))

(cl-defun popup-isearch (popup
                         &key
                         (filter 'popup-isearch-filter-list)
                         (cursor-color popup-isearch-cursor-color)
                         (keymap popup-isearch-keymap)
                         callback
                         help-delay)
  "Start isearch on POPUP. This function is synchronized, meaning
event loop waits for quiting of isearch.

FILTER is function with two argumenst to perform popup items filtering.

CURSOR-COLOR is a cursor color during isearch. The default value
is `popup-isearch-cursor-color'.

KEYMAP is a keymap which is used when processing events during
event loop. The default value is `popup-isearch-keymap'.

CALLBACK is a function taking one argument. `popup-isearch' calls
CALLBACK, if specified, after isearch finished or isearch
canceled. The arguments is whole filtered list of items.

HELP-DELAY is a delay of displaying helps."
  (let ((list (popup-original-list popup))
        (pattern (or (popup-pattern popup) ""))
        (old-cursor-color (frame-parameter (selected-frame) 'cursor-color))
        prompt key binding)
    (unwind-protect
        (cl-block nil
          (if cursor-color
              (set-cursor-color cursor-color))
          (while t
            (setq prompt (popup-isearch-prompt popup pattern))
            (setq key (popup-menu-read-key-sequence keymap prompt help-delay))
            (if (null key)
                (unless (funcall popup-menu-show-quick-help-function popup nil :prompt prompt)
                  (clear-this-command-keys)
                  (push (read-event prompt) unread-command-events))
              (setq binding (lookup-key keymap key))
              (cond
               ((and (stringp key)
                     (popup-isearch-char-p (aref key 0)))
                (setq pattern (concat pattern key)))
               ((eq binding 'popup-isearch-done)
                (cl-return nil))
               ((eq binding 'popup-isearch-cancel)
                (popup-isearch-update popup filter "" callback)
                (cl-return t))
               ((eq binding 'popup-isearch-close)
                (popup-isearch-update popup filter "" callback)
                (setq unread-command-events
                      (append (listify-key-sequence key) unread-command-events))
                (cl-return nil))
               ((eq binding 'popup-isearch-delete)
                (if (> (length pattern) 0)
                    (setq pattern (substring pattern 0 (1- (length pattern))))))
               ((eq binding 'popup-isearch-yank)
                (popup-isearch-update popup filter (car kill-ring) callback)
                (cl-return nil))
               (t
                (setq unread-command-events
                      (append (listify-key-sequence key) unread-command-events))
                (cl-return nil)))
              (popup-isearch-update popup filter pattern callback))))
      (if old-cursor-color
          (set-cursor-color old-cursor-color)))))



;;; Popup Tip

(defface popup-tip-face
  '((t (:background "khaki1" :foreground "black")))
  "Face for popup tip."
  :group 'popup)

(defvar popup-tip-max-width 80)

(cl-defun popup-tip (string
                     &key
                     point
                     (around t)
                     width
                     (height 15)
                     min-height
                     max-width
                     truncate
                     margin
                     margin-left
                     margin-right
                     scroll-bar
                     parent
                     parent-offset
                     nowait
                     nostrip
                     prompt
                     face
                     &allow-other-keys
                     &aux tip lines)
  "Show a tooltip of STRING at POINT. This function is
synchronized unless NOWAIT specified. Almost all arguments are
the same as in `popup-create', except for TRUNCATE, NOWAIT, and
PROMPT.

If TRUNCATE is non-nil, the tooltip can be truncated.

If NOWAIT is non-nil, this function immediately returns the
tooltip instance without entering event loop.

If `NOSTRIP` is non-nil, `STRING` properties are not stripped.

PROMPT is a prompt string when reading events during event loop.

If FACE is non-nil, it will be used instead of face `popup-tip-face'."
  (if (bufferp string)
      (setq string (with-current-buffer string (buffer-string))))

  (unless nostrip
    ;; TODO strip text (mainly face) properties
    (setq string (substring-no-properties string)))

  (setq string (popup-replace-displayable string))

  (and (eq margin t) (setq margin 1))
  (or margin-left (setq margin-left margin))
  (or margin-right (setq margin-right margin))

  (let ((it (popup-fill-string string width popup-tip-max-width)))
    (setq width (car it)
          lines (cdr it)))

  (setq tip (popup-create point width height
                          :min-height min-height
                          :max-width max-width
                          :around around
                          :margin-left margin-left
                          :margin-right margin-right
                          :scroll-bar scroll-bar
                          :face (or face 'popup-tip-face)
                          :parent parent
                          :parent-offset parent-offset))

  (unwind-protect
      (when (> (popup-width tip) 0)                   ; not to be corrupted
        (when (and (not (eq width (popup-width tip))) ; truncated
                   (not truncate))
          ;; Refill once again to lines be fitted to popup width
          (setq width (popup-width tip))
          (setq lines (cdr (popup-fill-string string width width))))

        (popup-set-list tip lines)
        (popup-draw tip)
        (if nowait
            tip
          (clear-this-command-keys)
          (push (read-event prompt) unread-command-events)
          t))
    (unless nowait
      (popup-delete tip))))



;;; Popup Menu

(defface popup-menu-face
  '((t (:inherit popup-face)))
  "Face for popup menu."
  :group 'popup)

(defface popup-menu-mouse-face
  '((t (:background "blue" :foreground "white")))
  "Face for popup menu."
  :group 'popup)

(defface popup-menu-selection-face
  '((t (:inherit default :background "steelblue" :foreground "white")))
  "Face for popup menu selection."
  :group 'popup)

(defface popup-menu-summary-face
  '((t (:inherit popup-summary-face)))
  "Face for popup summary."
  :group 'popup)

(defvar popup-menu-show-tip-function 'popup-tip
  "Function used for showing tooltip by `popup-menu-show-quick-help'.")

(defun popup-menu-show-help (menu &optional persist item)
  (popup-item-show-help (or item (popup-selected-item menu)) persist))

(defun popup-menu-documentation (menu &optional item)
  (popup-item-documentation (or item (popup-selected-item menu))))

(defun popup-menu-show-quick-help (menu &optional item &rest args)
  (let* ((point (plist-get args :point))
         (height (or (plist-get args :height) (popup-height menu)))
         (min-height (min height (popup-current-height menu)))
         (around nil)
         (parent-offset (popup-offset menu))
         (doc (popup-menu-documentation menu item)))
    (when (stringp doc)
      (if (popup-hidden-p menu)
          (setq around t
                menu nil
                parent-offset nil)
        (setq point nil))
      (let ((popup-use-optimized-column-computation nil)) ; To avoid wrong positioning
        (apply popup-menu-show-tip-function
               doc
               :point point
               :height height
               :min-height min-height
               :around around
               :parent menu
               :parent-offset parent-offset
               args)))))

(defun popup-menu-item-of-mouse-event (event)
  (when (and (consp event)
             (memq (cl-first event) '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5)))
    (let* ((position (cl-second event))
           (object (elt position 4)))
      (when (consp object)
        (get-text-property (cdr object) 'popup-item (car object))))))

(defun popup-menu-read-key-sequence (keymap &optional prompt timeout)
  (catch 'timeout
    (let ((timer (and timeout
                      (run-with-timer timeout nil
                                      (lambda ()
                                        (if (zerop (length (this-command-keys)))
                                            (throw 'timeout nil))))))
          (old-global-map (current-global-map))
          (temp-global-map (make-sparse-keymap))
          (overriding-terminal-local-map (make-sparse-keymap)))
      (substitute-key-definition 'keyboard-quit 'keyboard-quit
                                 temp-global-map old-global-map)
      (define-key temp-global-map [menu-bar] (lookup-key old-global-map [menu-bar]))
      (define-key temp-global-map [tool-bar] (lookup-key old-global-map [tool-bar]))
      (set-keymap-parent overriding-terminal-local-map keymap)
      (if (current-local-map)
          (define-key overriding-terminal-local-map [menu-bar]
            (lookup-key (current-local-map) [menu-bar])))
      (unwind-protect
          (progn
            (use-global-map temp-global-map)
            (clear-this-command-keys)
            (with-temp-message prompt
              (read-key-sequence nil)))
        (use-global-map old-global-map)
        (if timer (cancel-timer timer))))))

(defun popup-menu-fallback (event default))

(cl-defun popup-menu-event-loop (menu
                                 keymap
                                 fallback
                                 &key
                                 prompt
                                 help-delay
                                 isearch
                                 isearch-filter
                                 isearch-cursor-color
                                 isearch-keymap
                                 isearch-callback
                                 &aux key binding)
  (cl-block nil
    (while (popup-live-p menu)
      (and isearch
           (popup-isearch menu
                          :filter isearch-filter
                          :cursor-color isearch-cursor-color
                          :keymap isearch-keymap
                          :callback isearch-callback
                          :help-delay help-delay)
           (keyboard-quit))
      (setq key (popup-menu-read-key-sequence keymap prompt help-delay))
      (setq binding (and key (lookup-key keymap key)))
      (cond
       ((or (null key) (zerop (length key)))
        (unless (funcall popup-menu-show-quick-help-function menu nil :prompt prompt)
          (clear-this-command-keys)
          (push (read-event prompt) unread-command-events)))
       ((eq (lookup-key (current-global-map) key) 'keyboard-quit)
        (keyboard-quit)
        (cl-return))
       ((eq binding 'popup-close)
        (if (popup-parent menu)
            (cl-return)))
       ((memq binding '(popup-select popup-open))
        (let* ((item (or (popup-menu-item-of-mouse-event (elt key 0))
                         (popup-selected-item menu)))
               (index (cl-position item (popup-list menu)))
               (sublist (popup-item-sublist item)))
          (unless index (cl-return))
          (if sublist
              (popup-aif (let (popup-use-optimized-column-computation)
                           (popup-cascade-menu sublist
                                               :around nil
                                               :margin-left (popup-margin-left menu)
                                               :margin-right (popup-margin-right menu)
                                               :scroll-bar (popup-scroll-bar menu)
                                               :parent menu
                                               :parent-offset index
                                               :help-delay help-delay
                                               :isearch isearch
                                               :isearch-filter isearch-filter
                                               :isearch-cursor-color isearch-cursor-color
                                               :isearch-keymap isearch-keymap
                                               :isearch-callback isearch-callback))
                  (and it (cl-return it)))
            (if (eq binding 'popup-select)
                (cl-return (popup-item-value-or-self item))))))
       ((eq binding 'popup-next)
        (popup-next menu))
       ((eq binding 'popup-previous)
        (popup-previous menu))
       ((eq binding 'popup-page-next)
        (popup-page-next menu))
       ((eq binding 'popup-page-previous)
        (popup-page-previous menu))
       ((eq binding 'popup-help)
        (popup-menu-show-help menu))
       ((eq binding 'popup-isearch)
        (popup-isearch menu
                       :filter isearch-filter
                       :cursor-color isearch-cursor-color
                       :keymap isearch-keymap
                       :callback isearch-callback
                       :help-delay help-delay))
       ((commandp binding)
        (call-interactively binding))
       (t
        (funcall fallback key (key-binding key)))))))

(defun popup-preferred-width (list)
  "Return the preferred width to show LIST beautifully."
  (cl-loop with tab-width = 4
           for item in list
           for summary = (popup-item-summary item)
           maximize (string-width (popup-x-to-string item)) into width
           if (stringp summary)
           maximize (+ (string-width summary) 2) into summary-width
           finally return
           (let ((total (+ (or width 0) (or summary-width 0))))
             (* (ceiling (/ total 10.0)) 10))))

(defvar popup-menu-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r"        'popup-select)
    (define-key map "\C-f"      'popup-open)
    (define-key map [right]     'popup-open)
    (define-key map "\C-b"      'popup-close)
    (define-key map [left]      'popup-close)

    (define-key map "\C-n"      'popup-next)
    (define-key map [down]      'popup-next)
    (define-key map "\C-p"      'popup-previous)
    (define-key map [up]        'popup-previous)

    (define-key map [next]      'popup-page-next)
    (define-key map [prior]     'popup-page-previous)

    (define-key map [f1]        'popup-help)
    (define-key map (kbd "\C-?") 'popup-help)

    (define-key map "\C-s"      'popup-isearch)

    (define-key map [mouse-1]   'popup-select)
    (define-key map [mouse-4]   'popup-previous)
    (define-key map [mouse-5]   'popup-next)
    map))

(cl-defun popup-menu* (list
                       &key
                       point
                       (around t)
                       (width (popup-preferred-width list))
                       (height 15)
                       max-width
                       margin
                       margin-left
                       margin-right
                       scroll-bar
                       symbol
                       parent
                       parent-offset
                       cursor
                       (keymap popup-menu-keymap)
                       (fallback 'popup-menu-fallback)
                       help-delay
                       nowait
                       prompt
                       isearch
                       (isearch-filter 'popup-isearch-filter-list)
                       (isearch-cursor-color popup-isearch-cursor-color)
                       (isearch-keymap popup-isearch-keymap)
                       isearch-callback
                       initial-index
                       &allow-other-keys
                       &aux menu event)
  "Show a popup menu of LIST at POINT. This function returns a
value of the selected item. Almost all arguments are the same as in
`popup-create', except for KEYMAP, FALLBACK, HELP-DELAY, PROMPT,
ISEARCH, ISEARCH-FILTER, ISEARCH-CURSOR-COLOR, ISEARCH-KEYMAP, and
ISEARCH-CALLBACK.

If KEYMAP is a keymap which is used when processing events during
event loop.

If FALLBACK is a function taking two arguments; a key and a
command. FALLBACK is called when no special operation is found on
the key. The default value is `popup-menu-fallback', which does
nothing.

HELP-DELAY is a delay of displaying helps.

If NOWAIT is non-nil, this function immediately returns the menu
instance without entering event loop.

PROMPT is a prompt string when reading events during event loop.

If ISEARCH is non-nil, do isearch as soon as displaying the popup
menu.

ISEARCH-FILTER is a filtering function taking two arguments:
search pattern and list of items. Returns a list of matching items.

ISEARCH-CURSOR-COLOR is a cursor color during isearch. The
default value is `popup-isearch-cursor-color'.

ISEARCH-KEYMAP is a keymap which is used when processing events
during event loop. The default value is `popup-isearch-keymap'.

ISEARCH-CALLBACK is a function taking one argument.  `popup-menu'
calls ISEARCH-CALLBACK, if specified, after isearch finished or
isearch canceled. The arguments is whole filtered list of items.

If `INITIAL-INDEX' is non-nil, this is an initial index value for
`popup-select'. Only positive integer is valid."
  (and (eq margin t) (setq margin 1))
  (or margin-left (setq margin-left margin))
  (or margin-right (setq margin-right margin))
  (if (and scroll-bar
           (integerp margin-right)
           (> margin-right 0))
      ;; Make scroll-bar space as margin-right
      (cl-decf margin-right))
  (setq menu (popup-create point width height
                           :max-width max-width
                           :around around
                           :face 'popup-menu-face
                           :mouse-face 'popup-menu-mouse-face
                           :selection-face 'popup-menu-selection-face
                           :summary-face 'popup-menu-summary-face
                           :margin-left margin-left
                           :margin-right margin-right
                           :scroll-bar scroll-bar
                           :symbol symbol
                           :parent parent
                           :parent-offset parent-offset))
  (unwind-protect
      (progn
        (popup-set-list menu list)
        (if cursor
            (popup-jump menu cursor)
          (popup-draw menu))
        (when initial-index
          (dotimes (_i (min (- (length list) 1) initial-index))
            (popup-next menu)))
        (if nowait
            menu
          (popup-menu-event-loop menu keymap fallback
                                 :prompt prompt
                                 :help-delay help-delay
                                 :isearch isearch
                                 :isearch-filter isearch-filter
                                 :isearch-cursor-color isearch-cursor-color
                                 :isearch-keymap isearch-keymap
                                 :isearch-callback isearch-callback)))
    (unless nowait
      (popup-delete menu))))

(defun popup-cascade-menu (list &rest args)
  "Same as `popup-menu' except that an element of LIST can be
also a sub-menu if the element is a cons cell formed (ITEM
. SUBLIST) where ITEM is an usual item and SUBLIST is a list of
the sub menu."
  (apply 'popup-menu*
         (mapcar (lambda (item)
                   (if (consp item)
                       (popup-make-item (car item)
                                        :sublist (cdr item)
                                        :symbol ">")
                     item))
                 list)
         :symbol t
         args))

(provide 'popup)
;;; popup.el ends here
