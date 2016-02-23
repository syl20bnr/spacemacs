;;; funcs.el --- Eyebrowse Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'dash)

;; Eyebrowse uses window-state objects (as returned by `window-state-get') to
;; store window configurations, so here are some utility functions to help us
;; analyse window-states.
;; it might make more sense to move these functions to a more general place

(defun spacemacs/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
       (memq (car object) '(leaf vc hc))))

(defun spacemacs/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state object.
The returned value is the representation of a buffer in a window-state
object."
  (cdr (assq 'buffer window)))

(defun spacemacs/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (spacemacs/window-state-get-buffer window)))

(defun spacemacs/window-state-walk-windows-1 (window fn)
  "Helper function for `spacemacs/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
         (-filter #'spacemacs/window-state-window-p window))
        (bare-window
         ;; if WINDOW contains more than one window, take only the first window
         (--take-while (not (spacemacs/window-state-window-p it))
                       window)))
    (--each child-windows
      (spacemacs/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun spacemacs/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute.
STATE is a window-state object."
  (let (result)
    (spacemacs/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun spacemacs/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object.
The returned windows are not actual window objects. They are windows as
represented in window-state objects."
  (spacemacs/window-state-walk-windows state #'identity))

(defun spacemacs/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (spacemacs/window-state-walk-windows state #'spacemacs/window-state-get-buffer-name)))

(defun spacemacs/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (spacemacs/window-state-get-buffer-names state))))
