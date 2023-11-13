;;; winum.el --- Navigate windows and frames using numbers.
;;
;; Copyright (c) 2006-2015 Nikolaj Schumacher
;; Copyright (c) 2016 Thomas Chauvot de Beauchêne
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
;; Version: 2.2.0
;; Keywords: convenience, frames, windows, multi-screen
;; URL: http://github.com/deb0ch/winum.el
;; Created: 2016
;; Compatibility: GNU Emacs 24.x
;; Package-requires: ((cl-lib "0.5") (dash "2.13.0"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Window numbers for Emacs: Navigate your windows and frames using numbers.
;;
;; This package is an extended and actively maintained version of the
;; https://github.com/nschum/window-numbering.el package by Nikolaj Schumacher,
;; with some ideas and code taken from https://github.com/abo-abo/ace-window.
;;
;; This version brings, among other things, support for number sets across multiple
;; frames, giving the user a smoother experience of multi-screen Emacs.
;;
;;; Code:
;;
;; FIXME: The mode-line's window number is not always up to date in all frames.
;;

(eval-when-compile (require 'cl-lib))
(require 'dash)

;; Configuration variables -----------------------------------------------------

(defgroup winum nil
  "Navigate and manage windows using numbers."
  :group 'convenience)

(defcustom winum-scope 'global
  "Frames affected by a number set."
  :group 'winum
  :type  '(choice
           (const :tag "frame local" frame-local)
           (const :tag "visible frames" visible)
           (const :tag "global" global)))

(defcustom winum-reverse-frame-list nil
  "If t, order frames by reverse order of creation.
Has effect only when `winum-scope' is not 'frame-local."
  :group 'winum
  :type  'boolean)

(defcustom winum-auto-assign-0-to-minibuffer t
  "If non-nil, `winum-mode' assigns 0 to the minibuffer when active."
  :group 'winum
  :type  'boolean)

(defcustom winum-assign-func nil
  "Function called for each window by `winum-mode'.
This is called before automatic assignment begins.  The function should
return a number to have it assigned to the current-window, nil otherwise.

This function along with `winum-auto-assign-0-to-minibuffer' are the only
ways to have 0 assigned to a window.

Example: always assign *Calculator* the number 9 and *NeoTree* the number 0:

  (defun my-winum-assign-func ()
    (cond
     ((equal (buffer-name) \"*Calculator*\")
      9)
     ((string-match-p (buffer-name) \".*\\*NeoTree\\*.*\")
      0)
     (t
      nil)))

  (setq winum-assign-func 'my-winum-assign-func)"
  :group 'winum
  :type  'function)

(make-obsolete-variable 'winum-assign-func 'winum-assign-functions "2.0.0")

(defcustom winum-assign-functions nil
  "List of functions called for each window by `winum-mode'.

These functions allow for deterministic assignment of numbers to windows. Each
function is called for every window. A function should return the number to be
assigned to a window or nil. The *first* function to output a number for
a given window will determine this window's number.

If the list is empty or if every functions returns nil for a given window winum
will proceed to automatic number assignment.

Since this list is meant to allow custom window assignment for *mutiple*
packages at once it should never be directly set, only added to and removed
from.

These functions, along with `winum-auto-assign-0-to-minibuffer', are the only
way to have 0 assigned to a window.

Example: always assign *Calculator* the number 9, *Flycheck-errors* the number 8
and *NeoTree* the number 0:

  (defun winum-assign-9-to-calculator-8-to-flycheck-errors ()
    (cond
     ((equal (buffer-name) \"*Calculator*\") 9)
     ((equal (buffer-name) \"*Flycheck errors*\") 8)))

  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) \".*\\*NeoTree\\*.*\") 10))

  (add-to-list
    'winum-assign-functions #'winum-assign-9-to-calculator-8-to-flycheck-errors)
  (add-to-list
    'winum-assign-functions #'winum-assign-0-to-neotree)"
  :group 'winum
  :type  'list)

(defcustom winum-auto-setup-mode-line t
  "When nil, `winum-mode' will not display window numbers in the mode-line.
You might want this to be nil if you use a package that already manages window
numbers in the mode-line."
  :group 'winum
  :type  'boolean)

(defcustom winum-mode-line-position 1
  "The position in the mode-line `winum-mode' displays the number."
  :group 'winum
  :type  'integer)

(defcustom winum-format " %s "
  "Format string defining how the window number looks like in the mode-line.
This string is passed to the `format' function along with the
result of `winum-get-number-string'."
  :group 'winum
  :type  'string)

(defcustom winum-ignored-buffers '(" *which-key*")
  "List of buffers to ignore when assigning numbers."
  :group 'winum
  :type  '(repeat string))

(defcustom winum-ignored-buffers-regexp '()
  "List of regexps for buffer names to ignore when assigning numbers.
See Info node `(emacs) Regexps' or Info node `(elisp) Regular Expressions'"
  :group 'winum
  :type '(repeat string)
  :risky t)

(defface winum-face '()
  "Face used for the number in the mode-line."
  :group 'winum)

(defvar winum-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "`") 'winum-select-window-by-number)
    (define-key map (kbd "²") 'winum-select-window-by-number)
    (define-key map (kbd "0") 'winum-select-window-0-or-10)
    (define-key map (kbd "1") 'winum-select-window-1)
    (define-key map (kbd "2") 'winum-select-window-2)
    (define-key map (kbd "3") 'winum-select-window-3)
    (define-key map (kbd "4") 'winum-select-window-4)
    (define-key map (kbd "5") 'winum-select-window-5)
    (define-key map (kbd "6") 'winum-select-window-6)
    (define-key map (kbd "7") 'winum-select-window-7)
    (define-key map (kbd "8") 'winum-select-window-8)
    (define-key map (kbd "9") 'winum-select-window-9)
    map)
  "Keymap to be used under the prefix provided by `winum-keymap-prefix'.")

(defvar winum-keymap (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-x w") winum-base-map)
                       map)
  "Keymap used for `winum-mode'.")

;; Internal variables ----------------------------------------------------------

(defvar winum--max-frames 16
  "Maximum number of frames that can be numbered.")

(defvar winum--window-count nil
  "Current count of windows to be numbered.")

(defvar winum--remaining nil
  "A list of window numbers to assign.")

(defvar winum--window-vector nil
  "Vector of windows indexed by their number.
Used internally by winum to get a window provided a number.")

(defvar winum--numbers-table nil
  "Hash table of numbers indexed by their window.
Used internally by winum to get a number provided a window.")

(defvar winum--frames-table nil
  "Table linking windows to numbers and numbers to windows for each frame.

Used only when `winum-scope' is 'frame-local to keep track of
separate window numbers sets in every frame.

It is a hash table using Emacs frames as keys and cons of the form
\(`winum--window-vector' . `winum--numbers-table')
as values.

To get a window given a number, use the `car' of a value.
To get a number given a window, use the `cdr' of a value.

Such a structure allows for per-frame bidirectional fast access.")

(defvar winum--mode-line-segment
  '(:eval (format winum-format (winum-get-number-string)))
  "What is pushed into `mode-line-format' when setting it up automatically.")

(defvar winum--last-used-scope winum-scope
  "Tracks the last used `winum-scope'.
Needed to detect scope changes at runtime.")

;; Interactive functions -------------------------------------------------------

;;;###autoload
(define-minor-mode winum-mode
  "A minor mode that allows for managing windows based on window numbers."
  nil
  nil
  winum-keymap
  :global t
  (if winum-mode
      (winum--init)
    (winum--deinit)))

;;;###autoload
(defun winum-select-window-0-or-10 (&optional arg)
  "Jump to window 0 if assigned or 10 if exists.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((n (if (winum-get-window-by-number 0)
               (if arg '- 0)
             (if arg -10 10))))
    (winum-select-window-by-number n)))

;;;###autoload
(defun winum-select-window-0 (&optional arg)
  "Jump to window 0.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg '- 0)))

;;;###autoload
(defun winum-select-window-1 (&optional arg)
  "Jump to window 1.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -1 1)))

;;;###autoload
(defun winum-select-window-2 (&optional arg)
  "Jump to window 2.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -2 2)))

;;;###autoload
(defun winum-select-window-3 (&optional arg)
  "Jump to window 3.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -3 3)))

;;;###autoload
(defun winum-select-window-4 (&optional arg)
  "Jump to window 4.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -4 4)))

;;;###autoload
(defun winum-select-window-5 (&optional arg)
  "Jump to window 5.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -5 5)))

;;;###autoload
(defun winum-select-window-6 (&optional arg)
  "Jump to window 6.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -6 6)))

;;;###autoload
(defun winum-select-window-7 (&optional arg)
  "Jump to window 7.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -7 7)))

;;;###autoload
(defun winum-select-window-8 (&optional arg)
  "Jump to window 8.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -8 8)))

;;;###autoload
(defun winum-select-window-9 (&optional arg)
  "Jump to window 9.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -9 9)))

;;;###autoload
(defun winum-select-window-by-number (&optional arg)
  "Select or delete window which number is specified by ARG.
If the number is negative, delete the window instead of selecting it.
There are several ways to provide the number:
- if called from elisp with an argument, use it.
- if called interactively with a numeric prefix argument, use it.
- if prefix argument is the negative argument, delete window 0.
- if prefix argument is the default prefix argument, delete current window.
- if called interactively and no valid argument is provided, read from
  minibuffer."
  (interactive "P")
  (let* ((n (cond
             ((integerp arg) arg)
             ((eq arg '-) 0) ; the negative argument
             (arg (winum-get-number))
             ((called-interactively-p 'any)
              (let ((user-input-str (read-from-minibuffer "Window: ")))
                (if (not (string-match-p "[+-]?[0-9]+\.*" user-input-str))
                    (winum-get-number)
                  (string-to-number user-input-str))))
             (t (winum-get-number))))
         (w (winum-get-window-by-number (abs n)))
         (delete (and arg
                      (or (not (integerp arg))
                          (> 0 n)))))
    (if w
        (if delete
            (delete-window w)
          (winum--switch-to-window w))
      (error "No window numbered %d" n))))

;; Public API ------------------------------------------------------------------

;;;###autoload
(defun winum-set-keymap-prefix (prefix)
  "Set key bindings prefix for `winum-keymap' based on `winum-base-map'.
This function overrides the value of `winum-keymap', so you
should call it before customization of `winum-keymap' and/or
after customization of `winum-base-map'.
PREFIX must be a key sequence, like the ones returned by `kbd'."
  (setq winum-keymap (when prefix (let ((map (make-sparse-keymap)))
                                    (define-key map prefix winum-base-map)
                                    map)))
  (setcdr (assoc 'winum-mode minor-mode-map-alist)
          winum-keymap))

;;;###autoload
(defun winum-get-window-by-number (n)
  "Return window numbered N if exists, nil otherwise."
  (let ((window-vector (winum--get-window-vector)))
    (when (and (>= n 0) (< n (length window-vector)))
      (aref window-vector n))))

;;;###autoload
(defun winum-get-number-string (&optional window)
  "Get the current or specified window's current number as a propertized string.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let* ((n (winum-get-number window))
         (s (if (numberp n)
                (int-to-string n)
              "")))
    (propertize s 'face 'winum-face)))

;;;###autoload
(defun winum-get-number (&optional window)
  "Get the current or specified window's current number.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let ((w (or window (selected-window))))
    (gethash w (winum--get-numbers-table))))

;; Internal functions ----------------------------------------------------------

(defun winum--init ()
  "Initialize winum-mode."
  (setq winum--window-count (length (winum--window-list)))
  (if (eq winum-scope 'frame-local)
      (setq winum--frames-table (make-hash-table :size winum--max-frames))
    (setq winum--numbers-table (make-hash-table :size winum--window-count)))
  (when winum-auto-setup-mode-line
    (winum--install-mode-line))
  (add-hook 'minibuffer-setup-hook 'winum--update)
  (add-hook 'window-configuration-change-hook 'winum--update)
  (dolist (frame (frame-list))
    (select-frame frame)
    (winum--update)))

(defun winum--deinit ()
  "Actions performed when turning off winum-mode."
  (when winum-auto-setup-mode-line
    (winum--clear-mode-line))
  (remove-hook 'minibuffer-setup-hook 'winum--update)
  (remove-hook 'window-configuration-change-hook 'winum--update)
  (setq winum--frames-table nil))

(defun winum--install-mode-line (&optional position)
  "Install the window number from `winum-mode' to the mode-line.
POSITION: position in the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        res)
    (dotimes (i (min (or position winum-mode-line-position 1)
                     (length mode-line)))
      (push (pop mode-line) res))
    (unless (equal (car mode-line) winum--mode-line-segment)
      (push winum--mode-line-segment res))
    (while mode-line
      (push (pop mode-line) res))
    (let ((nres (nreverse res)))
      (setq mode-line-format nres)
      (setq-default mode-line-format nres)))
  (force-mode-line-update t))

(defun winum--clear-mode-line ()
  "Remove the window number of `winum-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        res)
    (while mode-line
      (let ((item (pop mode-line)))
        (unless (equal item winum--mode-line-segment)
        (push item res))))
    (let ((nres (nreverse res)))
      (setq mode-line-format nres)
      (setq-default mode-line-format nres)))
  (force-mode-line-update t))

(defun winum--update ()
  "Update window numbers."
  (let ((windows (winum--window-list)))
    (setq winum--window-count (length windows)
          winum--remaining (winum--available-numbers))
    (winum--set-window-vector (make-vector (1+ winum--window-count) nil))
    (clrhash (winum--get-numbers-table))
    (when winum-assign-functions
      (-each windows #'winum--try-to-find-custom-number))
    (when (and winum-auto-assign-0-to-minibuffer
               (active-minibuffer-window)
               (not (winum-get-window-by-number 0)))
      (winum--assign (active-minibuffer-window) 0))
    (dolist (w windows)
      (winum--assign w))))

(defun winum--try-to-find-custom-number (window)
  "Try to find and assign a custom number for WINDOW.
Do so by trying every function in `winum-assign-functions' and assign the
*first* non nil integer.
When multiple functions assign a number to a window log a warning and use the
first number anyway."
  (with-selected-window window
    (with-current-buffer (window-buffer window)
      (let* ((nums (->> winum-assign-functions
                        (--map (cons it (funcall it)))
                        (--remove (null (cdr it)))))
             (num (-> nums (cl-first) (cdr))))
        (when (> (length nums) 1)
          (message "Winum conflict - window %s was assigned a number by multiple custom assign functions: '%s'"
                   window (--map (format "%s -> %s" (car it) (cdr it)) nums)))
        (when (integerp num) (winum--assign window num))))))

(defun winum--assign (window &optional number)
  "Assign to window WINDOW the number NUMBER.
If NUMBER is not specified, determine it first based on `winum--remaining'.
Returns the assigned number, or nil on error."
  (if number
      (progn
        (winum--maybe-expand-window-vector number)
        (if (aref (winum--get-window-vector) number)
            (progn
              (message "Number %s already assigned to %s, can't assign to %s"
                       number (aref (winum--get-window-vector) number) window)
              nil)
          (setf (aref (winum--get-window-vector) number) window)
          (puthash window number (winum--get-numbers-table))
          (setq winum--remaining (delq number winum--remaining))
          number))
    ;; else determine number and assign
    (when winum--remaining
      (unless (gethash window (winum--get-numbers-table))
        (let ((number (car winum--remaining)))
          (winum--assign window number))))))

(defun winum--maybe-expand-window-vector (number)
  "Expand `winum--window-vector' if NUMBER is bigger than its size.
The size of `winum--window-vector' is normally based on the number of live
windows, however a higher number can be reserved by the user-defined
`winum-assign-func'."
  (let* ((window-vector (winum--get-window-vector))
         (window-vector-length (length window-vector)))
    (when (> number window-vector-length)
      (winum--set-window-vector
       (vconcat window-vector
                (make-vector (1+ (- number window-vector-length)) nil))))))

(defun winum--window-list ()
  "Return a list of interesting windows."
  (cl-remove-if
   #'winum--ignore-window-p
   (cl-case winum-scope
     (global
      (cl-mapcan 'winum--list-windows-in-frame
                 (if winum-reverse-frame-list
                     (frame-list)
                   (nreverse (frame-list)))))
     (visible
      (cl-mapcan 'winum--list-windows-in-frame
                 (if winum-reverse-frame-list
                     (visible-frame-list)
                   (nreverse (visible-frame-list)))))
     (frame-local
      (winum--list-windows-in-frame))
     (t
      (error "Invalid `winum-scope': %S" winum-scope)))))

(defun winum--ignore-window-p (window)
  "Non-nil if WINDOW should be ignored for numbering."
  (let ((f (window-frame window)))
    (or (not (and (frame-live-p f)
                  (frame-visible-p f)))
        (string= "initial_terminal" (terminal-name f))
        (member (buffer-name (window-buffer window)) winum-ignored-buffers)
        (cl-some
         (lambda (regex) (string-match regex (buffer-name (window-buffer window))))
         winum-ignored-buffers-regexp))))

(defun winum--list-windows-in-frame (&optional f)
  "List windows in frame F using natural Emacs ordering."
  (window-list f 0 (frame-first-window f)))

(defun winum--set-window-vector (window-vector)
  "Set WINDOW-VECTOR according to the current `winum-scope'."
  (winum--check-for-scope-change)
  (if (eq winum-scope 'frame-local)
      (puthash (selected-frame)
               (cons window-vector
                     (make-hash-table :size winum--window-count))
               winum--frames-table)
    (setq winum--window-vector window-vector)))

(defun winum--get-window-vector ()
  "Return the window vector used to get a window given a number.
This vector is not stored the same way depending on the value of `winum-scope'."
  (winum--check-for-scope-change)
  (if (eq winum-scope 'frame-local)
      (car (gethash (selected-frame) winum--frames-table))
    winum--window-vector))

(defun winum--get-numbers-table ()
  "Return the numbers hashtable used to get a number given a window.
This hashtable is not stored the same way depending on the value of
`winum-scope'"
  (winum--check-for-scope-change)
  (winum--check-frames-table)
  (if (eq winum-scope 'frame-local)
      (cdr (gethash (selected-frame) winum--frames-table))
    winum--numbers-table))

(defun winum--check-frames-table ()
  "Make sure `winum--frames-table' exists and is correctly equipped.
Verifies 2 things (when `winum-scope' is frame local):
 * When `winum-scope' is frame-local for the first time it may be necessary to
   instantiate `winum--frames-table'.
 * A table entry for the current frame must be made when the frame has just
   been created."
  (when (eq winum-scope 'frame-local)
    (unless winum--frames-table
      (setq winum--frames-table (make-hash-table :size winum--max-frames)))
    (unless (gethash (selected-frame) winum--frames-table)
      (winum--update))))

(defun winum--available-numbers ()
  "Return a list of numbers from 1 to `winum--window-count'.
0 is is not part of the list as its assignment is either manual
using the `winum-assign-func', or using `winum-auto-assign-0-to-minibuffer'."
  (let ((numbers))
    (dotimes (i winum--window-count)
      (push (1+ i) numbers))
    (nreverse numbers)))

(defun winum--switch-to-window (window)
  "Switch to the window WINDOW and switch input focus if on a different frame."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))

(defun winum--check-for-scope-change ()
  "Check whether the `winum-scope' has been changed.
If a change is detected run `winum--init' to reinitialize all
internal data structures according to the new scope."
  (unless (eq winum-scope winum--last-used-scope)
    (setq winum--last-used-scope winum-scope)
    (winum--init)))

(defun winum--remove-deleted-frame-from-frames-table (frame)
  "Remove FRAME from `winum--frames-table' after it was deleted."
  (when winum--frames-table
    (remhash frame winum--frames-table)))

(add-hook 'delete-frame-functions #'winum--remove-deleted-frame-from-frames-table)

(push "^No window numbered .$"     debug-ignored-errors)
(push "^Got a dead window .$"      debug-ignored-errors)
(push "^Invalid `winum-scope': .$" debug-ignored-errors)

(provide 'winum)

;;; winum.el ends here
