;;; popwin.el --- Popup Window Manager

;; Copyright (C) 2011-2015  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: convenience
;; Version: 1.0.2
;; URL: https://github.com/emacsorphanage/popwin
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

;; Popwin makes you free from the hell of annoying buffers such like
;; *Help*, *Completions*, *compilation*, and etc.
;;
;; To use popwin, just add the following code into your .emacs:
;;
;;     (require 'popwin)
;;     (popwin-mode 1)
;;
;; Then try to show some buffer, for example *Help* or
;; *Completeions*.  Unlike standard behavior, their buffers may be
;; shown in a popup window at the bottom of the frame.  And you can
;; close the popup window seamlessly by typing C-g or selecting other
;; windows.
;;
;; `popwin:display-buffer' displays special buffers in a popup window
;; and displays normal buffers as unsual.  Special buffers are
;; specified in `popwin:special-display-config', which tells popwin
;; how to display such buffers.  See docstring of
;; `popwin:special-display-config' for more information.
;;
;; The default width/height/position of popup window can be changed by
;; setting `popwin:popup-window-width', `popwin:popup-window-height',
;; and `popwin:popup-window-position'.  You can also change the
;; behavior for a specific buffer.  See docstring of
;; `popwin:special-display-config'.
;;
;; If you want to use some useful commands such like
;; `popwin:popup-buffer' and `popwin:find-file' easily.  You may bind
;; `popwin:keymap' to `C-z', for example, like:
;;
;;     (global-set-key (kbd "C-z") popwin:keymap)
;;
;; See also `popwin:keymap' documentation.
;;
;; Enjoy!

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst popwin:version "1.0.1")



;;; Utility

(defun popwin:listify (object)
  "Return a singleton list of OBJECT if OBJECT is an atom, otherwise OBJECT itself."
  (if (atom object) (list object) object))

(defun popwin:subsitute-in-tree (map tree)
  "Not documented (MAP) (TREE)."
  (if (consp tree)
      (cons (popwin:subsitute-in-tree map (car tree))
            (popwin:subsitute-in-tree map (cdr tree)))
    (or (cdr (assq tree map)) tree)))

(defun popwin:get-buffer (buffer-or-name &optional if-not-found)
  "Return a buffer named BUFFER-OR-NAME or BUFFER-OR-NAME itself \
if BUFFER-OR-NAME is a buffer.  If BUFFER-OR-NAME is a string and
such a buffer named BUFFER-OR-NAME not found, a new buffer will
be returned when IF-NOT-FOUND is :create, or an error reported
when IF-NOT-FOUND is :error.  The default of value of IF-NOT-FOUND
is :error."
  (cl-ecase (or if-not-found :error)
    (:create
     (get-buffer-create buffer-or-name))
    (:error
     (or (get-buffer buffer-or-name)
         (error "No buffer named %s" buffer-or-name)))))

(defun popwin:switch-to-buffer (buffer-or-name &optional norecord)
  "Call `switch-to-buffer' forcing BUFFER-OR-NAME be displayed in the \
selected window.  NORECORD is the same as `switch-to-buffer' NORECORD."
  (with-no-warnings
    (if (>= emacs-major-version 24)
        (switch-to-buffer buffer-or-name norecord t)
      (switch-to-buffer buffer-or-name norecord))))

(defun popwin:select-window (window &optional norecord)
  "Call `select-window' (WINDOW) with saving the current buffer.
NORECORD is the same as `switch-to-buffer' NORECORD."
  (save-current-buffer
    (select-window window norecord)))

(defun popwin:buried-buffer-p (buffer)
  "Return t if BUFFER might be thought of as a buried buffer."
  (eq (car (last (buffer-list))) buffer))

(defun popwin:window-point (window)
  "Return `window-point' of WINDOW.
If WINDOW is currently selected, then return buffer-point instead."
  (if (eq (selected-window) window)
      (with-current-buffer (window-buffer window)
        (point))
    (window-point window)))

(defun popwin:window-deletable-p (window)
  "Return t if WINDOW is deletable, meaning that WINDOW is alive \
and not a minibuffer's window, plus there is two or more windows."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (one-window-p))))

(defmacro popwin:save-selected-window (&rest body)
  "Evaluate BODY saving the selected window."
  `(with-selected-window (selected-window) ,@body))

(defun popwin:minibuffer-window-selected-p ()
  "Return t if minibuffer window is selected."
  (minibuffer-window-active-p (selected-window)))

(defun popwin:last-selected-window ()
  "Return currently selected window or lastly selected window if \
minibuffer window is selected."
  (if (popwin:minibuffer-window-selected-p)
      (minibuffer-selected-window)
    (selected-window)))



;;; Common

(defvar popwin:debug nil)

(defvar popwin:dummy-buffer nil)

(defun popwin:dummy-buffer ()
  "Not documented."
  (if (buffer-live-p popwin:dummy-buffer)
      popwin:dummy-buffer
    (setq popwin:dummy-buffer (get-buffer-create " *popwin-dummy*"))))

(defun popwin:kill-dummy-buffer ()
  "Not documented."
  (when (buffer-live-p popwin:dummy-buffer)
    (kill-buffer popwin:dummy-buffer))
  (setq popwin:dummy-buffer nil))

(defun popwin:window-trailing-edge-adjustable-p (window)
  "Return t if a trailing edge of WINDOW is adjustable."
  (let ((next-window (next-window window)))
    (and (not (eq next-window (frame-first-window)))
         (not (eq (window-buffer next-window)
                  (popwin:dummy-buffer))))))

(cl-defun popwin:adjust-window-edges (window
                                      edges
                                      &optional
                                      (hfactor 1)
                                      (vfactor 1))
  "Adjust edges of WINDOW to EDGES accoring to horizontal factor
HFACTOR, and vertical factor VFACTOR."
  (when (popwin:window-trailing-edge-adjustable-p window)
    (cl-destructuring-bind ((left top right bottom)
                            (cur-left cur-top cur-right cur-bottom))
        (list edges (window-edges window))
      (let ((hdelta (floor (- (* (- right left) hfactor) (- cur-right cur-left))))
            (vdelta (floor (- (* (- bottom top) vfactor) (- cur-bottom cur-top)))))
        (ignore-errors
          (adjust-window-trailing-edge window hdelta t))
        (ignore-errors
          (adjust-window-trailing-edge window vdelta nil))))))

(defun popwin:window-config-tree-1 (node)
  "Not documented (NODE)."
  (if (windowp node)
      (list 'window
            node
            (window-buffer node)
            (popwin:window-point node)
            (window-start node)
            (window-edges node)
            (eq (selected-window) node)
            (window-dedicated-p node))
    (cl-destructuring-bind (dir edges . windows) node
      (append (list dir edges)
              (cl-loop for window in windows
                       unless (or (and (windowp window)
                                       (window-parameter window 'window-side))
                                  (not (windowp window)))
                       collect (popwin:window-config-tree-1 window))))))

(defun popwin:window-config-tree ()
  "Return `window-tree' with replacing window values in the tree \
with persistent representations."
  (cl-destructuring-bind (root mini)
      (window-tree)
    (list (popwin:window-config-tree-1 root) mini)))

(defun popwin:replicate-window-config (window node hfactor vfactor)
  "Replicate NODE of window configuration on WINDOW with \
horizontal factor HFACTOR, and vertical factor VFACTOR.  The
return value is a association list of mapping from old-window to
new-window."
  (if (eq (car node) 'window)
      (cl-destructuring-bind (old-win buffer point start edges selected dedicated)
          (cdr node)
        (set-window-dedicated-p window nil)
        (popwin:adjust-window-edges window edges hfactor vfactor)
        (with-selected-window window
          (popwin:switch-to-buffer buffer t))
        (when selected
          (popwin:select-window window))
        (set-window-point window point)
        (set-window-start window start t)
        (when dedicated
          (set-window-dedicated-p window t))
        `((,old-win . ,window)))
    (cl-destructuring-bind (dir edges . windows) node
      (cl-loop while windows
               for sub-node = (pop windows)
               for win = window then next-win
               for next-win = (and windows (split-window win nil (not dir)))
               append (popwin:replicate-window-config win sub-node hfactor vfactor)))))

(defun popwin:restore-window-outline (node outline)
  "Restore window outline accoding to the structures of NODE \
which is a node of `window-tree' and OUTLINE which is a node of
`popwin:window-config-tree'."
  (cond
   ((and (windowp node)
         (eq (car outline) 'window))
    ;; same window
    (cl-destructuring-bind (old-win buffer point start edges selected dedicated)
        (cdr outline)
      (popwin:adjust-window-edges node edges)
      (when (and (eq (window-buffer node) buffer)
                 (eq (popwin:window-point node) point))
        (set-window-start node start))))
   ((or (windowp node)
        (not (eq (car node) (car outline))))
    ;; different structure
    ;; nothing to do
    )
   (t
    (let ((child-nodes (cddr node))
          (child-outlines (cddr outline)))
      (when (eq (length child-nodes) (length child-outlines))
        ;; same structure
        (cl-loop for child-node in child-nodes
                 for child-outline in child-outlines
                 do (popwin:restore-window-outline child-node child-outline)))))))

(defun popwin:position-horizontal-p (position)
  "Return t if POSITION is hozirontal."
  (and (memq position '(left :left right :right)) t))

(defun popwin:position-vertical-p (position)
  "Return t if POSITION is vertical."
  (and (memq position '(top :top bottom :bottom)) t))

(defun popwin:create-popup-window-1 (window size position)
  "Create a new window with SIZE at POSITION of WINDOW.
The return value is a list of a master window and the popup window."
  (let ((width (window-width window))
        (height (window-height window)))
    (cl-ecase position
      ((left :left)
       (list (split-window window size t)
             window))
      ((top :top)
       (list (split-window window size nil)
             window))
      ((right :right)
       (list window
             (split-window window (- width size) t)))
      ((bottom :bottom)
       (list window
             (split-window window (- height size) nil))))))

(cl-defun popwin:create-popup-window (&optional (size 15) (position 'bottom) (adjust t))
  "Create a popup window with SIZE on the frame.  If SIZE
is integer, the size of the popup window will be SIZE. If SIZE is
float, the size of popup window will be a multiplier of SIZE and
frame-size. can be an integer and a float. If ADJUST is t, all of
windows will be adjusted to fit the frame. POSITION must be one
of (left top right bottom). The return value is a pair of a
master window and the popup window. To close the popup window
properly, get `current-window-configuration' before calling this
function, and call `set-window-configuration' with the
window-configuration."
  (let* ((root (car (popwin:window-config-tree)))
         (root-win (popwin:last-selected-window))
         (hfactor 1)
         (vfactor 1))
    (popwin:save-selected-window
     (delete-other-windows root-win))
    (let ((root-width (window-width root-win))
          (root-height (window-height root-win)))
      (when adjust
        (if (floatp size)
            (if (popwin:position-horizontal-p position)
                (setq hfactor (- 1.0 size)
                      size (round (* root-width size)))
              (setq vfactor (- 1.0 size)
                    size (round (* root-height size))))
          (if (popwin:position-horizontal-p position)
              (setq hfactor (/ (float (- root-width size)) root-width))
            (setq vfactor (/ (float (- root-height size)) root-height)))))
      (cl-destructuring-bind (master-win popup-win)
          (popwin:create-popup-window-1 root-win size position)
        ;; Mark popup-win being a popup window.
        (with-selected-window popup-win
          (popwin:switch-to-buffer (popwin:dummy-buffer) t))
        (let ((win-map (popwin:replicate-window-config master-win root hfactor vfactor)))
          (list master-win popup-win win-map))))))



;;; Common User Interface

(defgroup popwin nil
  "Popup Window Manager."
  :group 'convenience
  :prefix "popwin:")

(defcustom popwin:popup-window-position 'bottom
  "Default popup window position.
This must be one of (left top right bottom)."
  :type 'symbol
  :group 'popwin)

(defcustom popwin:popup-window-width 30
  "Default popup window width.
If `popwin:popup-window-position' is top or bottom, this configuration
will be ignored.  If this variable is float, the popup window width will
be a multiplier of the value and frame-size."
  :type 'number
  :group 'popwin)

(defcustom popwin:popup-window-height 15
  "Default popup window height.
If `popwin:popup-window-position' is left or right, this configuration
will be ignored.  If this variable is float, the popup window height will
be a multiplier of the value and frame-size."
  :type 'number
  :group 'popwin)

(defcustom popwin:reuse-window 'current
  "Non-nil means `popwin:display-buffer' will not popup the visible buffer.
The value is same as a second argument of `get-buffer-window', except `current'
means the selected frame."
  :type 'symbol
  :group 'popwin)

(defcustom popwin:adjust-other-windows t
  "Non-nil means all of other windows will be adjusted to fit the \
frame when a popup window is shown."
  :type 'boolean
  :group 'popwin)

(defvar popwin:context-stack nil)

(defvar popwin:popup-window nil
  "Main popup window instance.")

(defvar popwin:popup-buffer nil
  "Buffer of currently shown in the popup window.")

(defvar popwin:popup-last-config nil
  "Arguments to `popwin:popup-buffer' of last call.")

;; Deprecated
(defvar popwin:master-window nil
  "Master window of a popup window.")

(defvar popwin:focus-window nil
  "Focused window which is used to check whether or not to close the popup window.")

(defvar popwin:selected-window nil
  "Last selected window when the popup window is shown.")

(defvar popwin:popup-window-dedicated-p nil
  "Non-nil means the popup window is dedicated to the original popup buffer.")

(defvar popwin:popup-window-stuck-p nil
  "Non-nil means the popup window has been stuck.")

(defvar popwin:window-outline nil
  "Original window outline which is obtained by `popwin:window-config-tree'.")

(defvar popwin:window-map nil
  "Mapping from old windows to new windows.")

(defvar popwin:window-config nil
  "An original window configuration for restoreing.")

(defvar popwin:close-popup-window-timer nil
  "Timer of closing the popup window.")

(defvar popwin:close-popup-window-timer-interval 0.05
  "Interval of `popwin:close-popup-window-timer'.")

(defvar popwin:before-popup-hook nil)

(defvar popwin:after-popup-hook nil)

(cl-symbol-macrolet ((context-vars '(popwin:popup-window
                                     popwin:popup-buffer
                                     popwin:master-window
                                     popwin:focus-window
                                     popwin:selected-window
                                     popwin:popup-window-dedicated-p
                                     popwin:popup-window-stuck-p
                                     popwin:window-outline
                                     popwin:window-map)))
  (defun popwin:valid-context-p (context)
    (window-live-p (plist-get context 'popwin:popup-window)))

  (defun popwin:current-context ()
    (cl-loop for var in context-vars
             collect var
             collect (symbol-value var)))

  (defun popwin:use-context (context)
    (cl-loop for var = (pop context)
             for val = (pop context)
             while var
             do (set var val)))

  (defun popwin:push-context ()
    (push (popwin:current-context) popwin:context-stack))

  (defun popwin:pop-context ()
    (popwin:use-context (pop popwin:context-stack)))

  (cl-defun popwin:find-context-for-buffer (buffer &key valid-only)
    (cl-loop with stack = popwin:context-stack
             for context = (pop stack)
             while context
             if (and (eq buffer (plist-get context 'popwin:popup-buffer))
                     (or (not valid-only)
                         (popwin:valid-context-p context)))
             return (list context stack))))

(defun popwin:popup-window-live-p ()
  "Return t if `popwin:popup-window' is alive."
  (window-live-p popwin:popup-window))

(cl-defun popwin:update-window-reference (symbol
                                          &key
                                          (map popwin:window-map)
                                          safe
                                          recursive)
  (unless (and safe (not (boundp symbol)))
    (let ((value (symbol-value symbol)))
      (set symbol
           (if recursive
               (popwin:subsitute-in-tree map value)
             (or (cdr (assq value map)) value))))))

(defun popwin:start-close-popup-window-timer ()
  "Not documented."
  (or popwin:close-popup-window-timer
      (setq popwin:close-popup-window-timer
            (run-with-idle-timer popwin:close-popup-window-timer-interval
                                 popwin:close-popup-window-timer-interval
                                 'popwin:close-popup-window-timer))))

(defun popwin:stop-close-popup-window-timer ()
  "Not documented."
  (when popwin:close-popup-window-timer
    (cancel-timer popwin:close-popup-window-timer)
    (setq popwin:close-popup-window-timer nil)))

(defun popwin:close-popup-window-timer ()
  "Not documented."
  (condition-case var
      (popwin:close-popup-window-if-necessary)
    (error
     (message "popwin:close-popup-window-timer: error: %s" var)
     (when popwin:debug (backtrace)))))

(defun popwin:close-popup-window (&optional keep-selected)
  "Close the popup window and restore to the previous window configuration.
If KEEP-SELECTED is non-nil, the lastly selected window will not be selected."
  (interactive)
  (when popwin:popup-window
    (unwind-protect
        (progn
          (when (popwin:window-deletable-p popwin:popup-window)
            (delete-window popwin:popup-window))
          (popwin:restore-window-outline (car (window-tree)) popwin:window-outline)
          ;; Call `redisplay' here so `window-start' could be set
          ;; prior to the point change of the master buffer.
          (redisplay)
          (when (and (not keep-selected)
                     (window-live-p popwin:selected-window))
            (select-window popwin:selected-window)))
      (popwin:pop-context)
      ;; Cleanup if no context left.
      (when (null popwin:context-stack)
        (popwin:kill-dummy-buffer)
        (popwin:stop-close-popup-window-timer)))))

(defun popwin:close-popup-window-if-necessary ()
  "Close the popup window if necessary.
The all situations where the popup window will be closed are followings:

* `C-g' has been pressed.
* The popup buffer has been killed.
* The popup buffer has been buried.
* The popup buffer has been changed if the popup window is
  dedicated to the buffer.
* Another window has been selected."
  (when popwin:popup-window
    (let* ((window (selected-window))
           (window-point (popwin:window-point window))
           (window-buffer (window-buffer window))
           (minibuf-window-p (window-minibuffer-p window))
           (reading-from-minibuf
            (and minibuf-window-p
                 (minibuffer-prompt)
                 t))
           (quit-requested
            (and (eq last-command 'keyboard-quit)
                 (eq last-command-event ?\C-g)))
           (other-window-selected
            (and (not (eq window popwin:focus-window))
                 (not (eq window popwin:popup-window))))
           (orig-this-command this-command)
           (popup-buffer-alive
            (buffer-live-p popwin:popup-buffer))
           (popup-buffer-buried
            (popwin:buried-buffer-p popwin:popup-buffer))
           (popup-buffer-changed-despite-of-dedicated
            (and popwin:popup-window-dedicated-p
                 (not popwin:popup-window-stuck-p)
                 (or (not other-window-selected)
                     (not reading-from-minibuf))
                 (buffer-live-p window-buffer)
                 (not (eq popwin:popup-buffer window-buffer))))
           (popup-window-alive (popwin:popup-window-live-p)))
      (when (or quit-requested
                (not popup-buffer-alive)
                popup-buffer-buried
                popup-buffer-changed-despite-of-dedicated
                (not popup-window-alive)
                (and other-window-selected
                     (not minibuf-window-p)
                     (not popwin:popup-window-stuck-p)))
        (when popwin:debug
          (message (concat "popwin: CLOSE:\n"
                           "  quit-requested = %s\n"
                           "  popup-buffer-alive = %s\n"
                           "  popup-buffer-buried = %s\n"
                           "  popup-buffer-changed-despite-of-dedicated = %s\n"
                           "  popup-window-alive = %s\n"
                           "  (selected-window) = %s\n"
                           "  popwin:focus-window = %s\n"
                           "  popwin:popup-window = %s\n"
                           "  other-window-selected = %s\n"
                           "  minibuf-window-p = %s\n"
                           "  popwin:popup-window-stuck-p = %s")
                   quit-requested
                   popup-buffer-alive
                   popup-buffer-buried
                   popup-buffer-changed-despite-of-dedicated
                   popup-window-alive
                   window
                   popwin:focus-window
                   popwin:popup-window
                   other-window-selected
                   minibuf-window-p
                   popwin:popup-window-stuck-p))
        (when (and quit-requested
                   (null orig-this-command))
          (setq this-command 'popwin:close-popup-window)
          (run-hooks 'pre-command-hook))
        (cond
         ((and quit-requested
               (null orig-this-command)
               popwin:window-config)
          (set-window-configuration popwin:window-config)
          (setq popwin:window-config nil))
         (reading-from-minibuf
          (popwin:close-popup-window)
          (select-window (minibuffer-window)))
         (t
          (popwin:close-popup-window
           (and other-window-selected
                (and popup-buffer-alive
                     (not popup-buffer-buried))))
          (when popup-buffer-changed-despite-of-dedicated
            (popwin:switch-to-buffer window-buffer)
            (goto-char window-point))))
        (when (and quit-requested
                   (null orig-this-command))
          (run-hooks 'post-command-hook)
          (setq last-command 'popwin:close-popup-window))))))

;;;###autoload
(cl-defun popwin:popup-buffer (buffer
                               &key
                               (width popwin:popup-window-width)
                               (height popwin:popup-window-height)
                               (position popwin:popup-window-position)
                               noselect
                               dedicated
                               stick
                               tail)
  "Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be stuck. If TAIL is
non-nil, the popup window will show the last contents. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER."
  (interactive "BPopup buffer:\n")
  (setq buffer (get-buffer buffer))
  (popwin:push-context)
  (run-hooks 'popwin:before-popup-hook)
  (cl-multiple-value-bind (context context-stack)
      (popwin:find-context-for-buffer buffer :valid-only t)
    (if context
        (progn
          (popwin:use-context context)
          (setq popwin:context-stack context-stack))
      (let ((win-outline (car (popwin:window-config-tree))))
        (cl-destructuring-bind (master-win popup-win win-map)
            (let ((size (if (popwin:position-horizontal-p position) width height))
                  (adjust popwin:adjust-other-windows))
              (popwin:create-popup-window size position adjust))
          (setq popwin:popup-window popup-win
                popwin:master-window master-win
                popwin:window-outline win-outline
                popwin:window-map win-map
                popwin:window-config nil
                popwin:selected-window (selected-window)))
        (popwin:update-window-reference 'popwin:context-stack :recursive t)
        (popwin:start-close-popup-window-timer))
      (with-selected-window popwin:popup-window
        (popwin:switch-to-buffer buffer)
        (when tail
          (set-window-point popwin:popup-window (point-max))
          (recenter -2)))
      (setq popwin:popup-buffer buffer
            popwin:popup-last-config (list buffer
                                           :width width :height height :position position
                                           :noselect noselect :dedicated dedicated
                                           :stick stick :tail tail)
            popwin:popup-window-dedicated-p dedicated
            popwin:popup-window-stuck-p stick)))
  (if noselect
      (setq popwin:focus-window popwin:selected-window)
    (setq popwin:focus-window popwin:popup-window)
    (select-window popwin:popup-window))
  (run-hooks 'popwin:after-popup-hook)
  popwin:popup-window)

(defun popwin:popup-last-buffer (&optional noselect)
  "Show the last popup buffer with the same configuration.
If NOSELECT is non-nil, the popup window will not be selected."
  (interactive "P")
  (if popwin:popup-last-config
      (if noselect
          (cl-destructuring-bind (buffer . keyargs) popwin:popup-last-config
            (apply 'popwin:popup-buffer buffer :noselect t keyargs))
        (apply 'popwin:popup-buffer popwin:popup-last-config))
    (error "No popup buffer ever")))
(defalias 'popwin:display-last-buffer 'popwin:popup-last-buffer)

(defun popwin:select-popup-window ()
  "Select the currently shown popup window."
  (interactive)
  (if (popwin:popup-window-live-p)
      (select-window popwin:popup-window)
    (error "No popup window displayed")))

(defun popwin:stick-popup-window ()
  "Stick the currently shown popup window.
The popup window can be closed by `popwin:close-popup-window'."
  (interactive)
  (if (popwin:popup-window-live-p)
      (progn
        (setq popwin:popup-window-stuck-p t)
        (message "Popup window stuck"))
    (error "No popup window displayed")))



;;; Special Display

(defmacro popwin:without-special-displaying (&rest body)
  "Evaluate BODY without special displaying."
  (if (boundp 'display-buffer-alist)
      `(with-no-warnings
         (let ((display-buffer-function nil)
               (display-buffer-alist
                (remove '(popwin:display-buffer-condition
                          popwin:display-buffer-action)
                        display-buffer-alist)))
           ,@body))
    `(with-no-warnings (let ((display-buffer-function nil)) ,@body))))

(defcustom popwin:special-display-config
  '(;; Emacs
    ("*Miniedit Help*" :noselect t)
    help-mode
    (completion-list-mode :noselect t)
    (compilation-mode :noselect t)
    (grep-mode :noselect t)
    (occur-mode :noselect t)
    ("*Pp Macroexpand Output*" :noselect t)
    "*Shell Command Output*"
    ;; VC
    "*vc-diff*"
    "*vc-change-log*"
    ;; Undo-Tree
    (" *undo-tree*" :width 60 :position right)
    ;; Anything
    ("^\\*anything.*\\*$" :regexp t)
    ;; SLIME
    "*slime-apropos*"
    "*slime-macroexpansion*"
    "*slime-description*"
    ("*slime-compilation*" :noselect t)
    "*slime-xref*"
    (sldb-mode :stick t)
    slime-repl-mode
    slime-connection-list-mode)
  "Configuration of special displaying buffer for `popwin:display-buffer' and \
`popwin:special-display-popup-window'.  The value is a list of
CONFIG as a form of (PATTERN . KEYWORDS) where PATTERN is a
pattern of specifying buffer and KEYWORDS is a list of a pair of
key and value.  PATTERN is in general a buffer name, a symbol
specifying `major-mode' of buffer, or a predicate function which
takes one argument: the buffer.  If CONFIG is a string or a
symbol, PATTERN will be CONFIG and KEYWORDS will be
empty.  Available keywords are following:

  regexp: If the value is non-nil, PATTERN will be used as regexp
    to matching buffer.

  width, height: Specify width or height of the popup window.  If
    no size specified, `popwin:popup-window-width' or
    `popwin:popup-window-height' will be used.  See also position
    keyword.

  position: The value must be one of (left top right bottom).  The
    popup window will shown at the position of the frame.  If no
    position specified, `popwin:popup-window-position' will be
    used.

  noselect: If the value is non-nil, the popup window will not be
    selected when it is shown.

  dedicated: If the value is non-nil, the popup window will be
    dedicated to the original popup buffer.  In this case, when
    another buffer is selected in the popup window, the popup
    window will be closed immedicately and the selected buffer
    will be shown on the previously selected window.

  stick: If the value is non-nil, the popup window will be stuck
    when it is shown.

  tail: If the value is non-nil, the popup window will show the
    last contents.

Examples: With '(\"*scratch*\" :height 30 :position top),
*scratch* buffer will be shown at the top of the frame with
height 30. With '(dired-mode :width 80 :position left), dired
buffers will be shown at the left of the frame with width 80."
  :type '(repeat
          (cons :tag "Config"
                (choice :tag "Pattern"
                        (string :tag "Buffer Name")
                        (symbol :tag "Major Mode"))
                (plist :tag "Keywords"
                       :value (:regexp nil) ; BUG? need default value
                       :options
                       ((:regexp (boolean :tag "On/Off"))
                        (:width (choice :tag "Width"
                                        (integer :tag "Width")
                                        (float :tag "Width (%)")))
                        (:height (choice :tag "Height"
                                         (integer :tag "Height")
                                         (float :tag "Height (%)")))
                        (:position (choice :tag "Position"
                                           (const :tag "Bottom" bottom)
                                           (const :tag "Top" top)
                                           (const :tag "Left" left)
                                           (const :tag "Right" right)))
                        (:noselect (boolean :tag "On/Off"))
                        (:dedicated (boolean :tag "On/Off"))
                        (:stick (boolean :tag "On/Off"))
                        (:tail (boolean :tag "On/Off"))))))
  :get (lambda (symbol)
         (mapcar (lambda (element)
                   (if (consp element)
                       element
                     (list element)))
                 (default-value symbol)))
  :group 'popwin)

(defun popwin:apply-display-buffer (function buffer &optional not-this-window)
  "Call FUNCTION on BUFFER without special displaying."
  (popwin:without-special-displaying
   (let ((same-window
          (or (same-window-p (buffer-name buffer))
              (and (>= emacs-major-version 24)
                   (boundp 'action)
                   (consp action)
                   (eq (car action) 'display-buffer-same-window)))))
     ;; Close the popup window here so that the popup window won't to
     ;; be splitted.
     (when (and (eq (selected-window) popwin:popup-window)
                (not same-window))
       (popwin:close-popup-window)))
   (if (and (>= emacs-major-version 24)
            (boundp 'action)
            (boundp 'frame))
       ;; Use variables ACTION and FRAME which are formal parameters
       ;; of DISPLAY-BUFFER.
       ;;
       ;; TODO: use display-buffer-alist instead of
       ;; display-buffer-function.
       (funcall function buffer action frame)
     (funcall function buffer not-this-window))))

(defun popwin:original-display-buffer (buffer &optional not-this-window)
  "Call `display-buffer' on BUFFER without special displaying."
  (popwin:apply-display-buffer 'display-buffer buffer not-this-window))

(defun popwin:original-pop-to-buffer (buffer &optional not-this-window)
  "Call `pop-to-buffer' on BUFFER without special displaying."
  (popwin:apply-display-buffer 'pop-to-buffer buffer not-this-window))

(defun popwin:original-display-last-buffer ()
  "Call `display-buffer' for the last popup buffer without special displaying."
  (interactive)
  (if popwin:popup-last-config
      (popwin:original-display-buffer (car popwin:popup-last-config))
    (error "No popup buffer ever")))

(defun popwin:switch-to-last-buffer ()
  "Switch to the last popup buffer."
  (interactive)
  (if popwin:popup-last-config
      (popwin:apply-display-buffer
       (lambda (buffer &rest ignore) (switch-to-buffer buffer))
       (car popwin:popup-last-config))
    (error "No popup buffer ever")))

(defun popwin:original-pop-to-last-buffer ()
  "Call `pop-to-buffer' for the last popup buffer without special displaying."
  (interactive)
  (if popwin:popup-last-config
      (popwin:original-pop-to-buffer (car popwin:popup-last-config))
    (error "No popup buffer ever")))

(defun popwin:reuse-window-p (buffer-or-name not-this-window)
  "Return t if a window showing BUFFER-OR-NAME exists and should be used displaying the buffer."
  (and popwin:reuse-window
       (let ((window (get-buffer-window buffer-or-name
                                        (if (eq popwin:reuse-window 'current)
                                            nil
                                          popwin:reuse-window))))
         (and (not (null window))
              (not (eq window (if not-this-window (selected-window))))))))

(cl-defun popwin:match-config (buffer)
  (when (stringp buffer) (setq buffer (get-buffer buffer)))
  (cl-loop with name = (buffer-name buffer)
           with mode = (buffer-local-value 'major-mode buffer)
           for config in popwin:special-display-config
           for (pattern . keywords) = (popwin:listify config)
           if (cond ((eq pattern t) t)
                    ((and (stringp pattern) (plist-get keywords :regexp))
                     (string-match pattern name))
                    ((stringp pattern)
                     (string= pattern name))
                    ((symbolp pattern)
                     (eq pattern mode))
                    ((functionp pattern)
                     (funcall pattern buffer))
                    (t (error "Invalid pattern: %s" pattern)))
           return (cons pattern keywords)))

(cl-defun popwin:display-buffer-1 (buffer-or-name
                                   &key
                                   default-config-keywords
                                   (if-buffer-not-found :create)
                                   if-config-not-found)
  "Display BUFFER-OR-NAME, if possible, in a popup
window. Otherwise call IF-CONFIG-NOT-FOUND with BUFFER-OR-NAME if
the value is a function. If IF-CONFIG-NOT-FOUND is nil,
`popwin:popup-buffer' will be called. IF-BUFFER-NOT-FOUND
indicates what happens when there is no such buffers. If the
value is :create, create a new buffer named BUFFER-OR-NAME. If
the value is :error, report an error. The default value
is :create. DEFAULT-CONFIG-KEYWORDS is a property list which
specifies default values of the config."
  (let* ((buffer (popwin:get-buffer buffer-or-name if-buffer-not-found))
         (pattern-and-keywords (popwin:match-config buffer)))
    (unless pattern-and-keywords
      (if if-config-not-found
          (cl-return-from popwin:display-buffer-1
            (funcall if-config-not-found buffer))
        (setq pattern-and-keywords '(t))))
    (cl-destructuring-bind (&key regexp width height position noselect dedicated stick tail)
        (append (cdr pattern-and-keywords) default-config-keywords)
      (popwin:popup-buffer buffer
                           :width (or width popwin:popup-window-width)
                           :height (or height popwin:popup-window-height)
                           :position (or position popwin:popup-window-position)
                           :noselect (or (popwin:minibuffer-window-selected-p) noselect)
                           :dedicated dedicated
                           :stick stick
                           :tail tail))))

;;;###autoload
(defun popwin:display-buffer (buffer-or-name &optional not-this-window)
  "Display BUFFER-OR-NAME, if possible, in a popup window, or as usual.
This function can be used as a value of
`display-buffer-function'."
  (interactive "BDisplay buffer:\n")
  (if (popwin:reuse-window-p buffer-or-name not-this-window)
      ;; Call `display-buffer' for reuse.
      (popwin:original-display-buffer buffer-or-name not-this-window)
    (popwin:display-buffer-1
     buffer-or-name
     :if-config-not-found
     (unless (with-no-warnings
               ;; FIXME: emacs bug?
               (called-interactively-p))
       (lambda (buffer)
         (popwin:original-display-buffer buffer not-this-window))))))

(defun popwin:special-display-popup-window (buffer &rest ignore)
  "Obsolete (BUFFER) (IGNORE)."
  (popwin:display-buffer-1 buffer))

(cl-defun popwin:pop-to-buffer-1 (buffer
                                  &key
                                  default-config-keywords
                                  other-window
                                  norecord)
  (popwin:display-buffer-1 buffer
                           :default-config-keywords default-config-keywords
                           :if-config-not-found
                           (lambda (buffer)
                             (pop-to-buffer buffer other-window norecord))))

;;;###autoload
(defun popwin:pop-to-buffer (buffer &optional other-window norecord)
  "Same as `pop-to-buffer' except that this function will use \
`popwin:display-buffer-1' instead of `display-buffer'.  BUFFER,
OTHER-WINDOW amd NORECORD are the same arguments."
  (interactive (list (read-buffer "Pop to buffer: " (other-buffer))
                     (if current-prefix-arg t)))
  (popwin:pop-to-buffer-1 buffer
                          :other-window other-window
                          :norecord norecord))



;;; Universal Display

(defcustom popwin:universal-display-config '(t)
  "Same as `popwin:special-display-config' except that this will \
be used for `popwin:universal-display'."
  :type 'list
  :group 'popwin)

;;;###autoload
(defun popwin:universal-display ()
  "Call the following command interactively with letting \
`popwin:special-display-config' be `popwin:universal-display-config'.
This will be useful when displaying buffers in popup windows temporarily."
  (interactive)
  (let ((command (key-binding (read-key-sequence "" t)))
        (popwin:special-display-config popwin:universal-display-config))
    (call-interactively command)))



;;; Extensions

;;;###autoload
(defun popwin:one-window ()
  "Delete other window than the popup window. C-g restores the original \
window configuration."
  (interactive)
  (setq popwin:window-config (current-window-configuration))
  (delete-other-windows))

;;;###autoload
(defun popwin:popup-buffer-tail (&rest same-as-popwin:popup-buffer)
  "Same as `popwin:popup-buffer' except that the buffer will be \
`recenter'ed at the bottom."
  (interactive "bPopup buffer:\n")
  (cl-destructuring-bind (buffer . keyargs) same-as-popwin:popup-buffer
    (apply 'popwin:popup-buffer buffer :tail t keyargs)))

;;;###autoload
(defun popwin:find-file (filename &optional wildcards)
  "Edit file FILENAME with popup window by `popwin:popup-buffer'."
  (interactive
   (find-file-read-args "Find file in popup window: "
                        (when (fboundp 'confirm-nonexistent-file-or-buffer)
                          (confirm-nonexistent-file-or-buffer))))
  (popwin:popup-buffer (find-file-noselect filename wildcards)))

;;;###autoload
(defun popwin:find-file-tail (file &optional wildcard)
  "Edit file FILENAME with popup window by `popwin:popup-buffer-tail'."
  (interactive
   (find-file-read-args "Find file in popup window: "
                        (when (fboundp 'confirm-nonexistent-file-or-buffer)
                          (confirm-nonexistent-file-or-buffer))))
  (popwin:popup-buffer-tail (find-file-noselect file wildcard)))

;;;###autoload
(defun popwin:messages ()
  "Display *Messages* buffer in a popup window."
  (interactive)
  (popwin:popup-buffer-tail "*Messages*"))



;;; Minor Mode

(defun popwin:display-buffer-condition (buffer action)
  "Not documented (BUFFER) (ACTION)."
  (and (popwin:match-config buffer) t))

(defun popwin:display-buffer-action (buffer alist)
  "Not documented (BUFFER) (ALIST)."
  (let ((not-this-window (plist-get 'inhibit-same-window alist)))
    (popwin:display-buffer buffer not-this-window)))

;;;###autoload
(define-minor-mode popwin-mode
  "Minor mode for `popwin-mode'."
  :init-value nil
  :global t
  (if (boundp 'display-buffer-alist)
      (let ((pair '(popwin:display-buffer-condition popwin:display-buffer-action)))
        (if popwin-mode
            (push pair display-buffer-alist)
          (setq display-buffer-alist (delete pair display-buffer-alist))))
    (with-no-warnings
      (unless (or (null display-buffer-function)
                  (eq display-buffer-function 'popwin:display-buffer))
        (warn "Overwriting display-buffer-function variable to enable/disable popwin-mode"))
      (setq display-buffer-function (if popwin-mode 'popwin:display-buffer nil)))))



;;; Keymaps

(defvar popwin:keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "b"    'popwin:popup-buffer)
    (define-key map "l"    'popwin:popup-last-buffer)
    (define-key map "o"    'popwin:display-buffer)
    (define-key map "\C-b" 'popwin:switch-to-last-buffer)
    (define-key map "\C-p" 'popwin:original-pop-to-last-buffer)
    (define-key map "\C-o" 'popwin:original-display-last-buffer)
    (define-key map " "    'popwin:select-popup-window)
    (define-key map "s"    'popwin:stick-popup-window)
    (define-key map "0"    'popwin:close-popup-window)
    (define-key map "f"    'popwin:find-file)
    (define-key map "\C-f" 'popwin:find-file)
    (define-key map "e"    'popwin:messages)
    (define-key map "\C-u" 'popwin:universal-display)
    (define-key map "1"    'popwin:one-window)

    map)
  "Default keymap for popwin commands.  Use like:
\(global-set-key (kbd \"C-z\") popwin:keymap\)

Keymap:

| Key    | Command                               |
|--------+---------------------------------------|
| b      | popwin:popup-buffer                   |
| l      | popwin:popup-last-buffer              |
| o      | popwin:display-buffer                 |
| C-b    | popwin:switch-to-last-buffer          |
| C-p    | popwin:original-pop-to-last-buffer    |
| C-o    | popwin:original-display-last-buffer   |
| SPC    | popwin:select-popup-window            |
| s      | popwin:stick-popup-window             |
| 0      | popwin:close-popup-window             |
| f, C-f | popwin:find-file                      |
| e      | popwin:messages                       |
| C-u    | popwin:universal-display              |
| 1      | popwin:one-window                     |")

(provide 'popwin)
;;; popwin.el ends here
