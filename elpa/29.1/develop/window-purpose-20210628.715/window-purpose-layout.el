;;; window-purpose-layout.el --- Save and load window layout -*- lexical-binding: t -*-

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
;; This file contains function for saving and loading the entire window
;; layout and frame layout.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'window-purpose-core)
(eval-when-compile (require 'subr-x))

(defconst purpose--built-in-layouts-dir
  (when load-file-name
    (concat (file-name-directory load-file-name) "layouts/"))
  "Location of built-in layouts shipped with Purpose.")

(defcustom purpose-use-built-in-layouts t
  "If nil, don't use layouts from `purpose--built-in-layouts-dir'."
  :group 'purpose
  :type 'boolean
  :package-version '(window-purpose . "1.6"))

(defcustom purpose-default-layout-file
  (concat user-emacs-directory ".purpose-layout")
  "Default file for saving/loading purpose layout."
  :group 'purpose
  :type 'file
  :package-version '(window-purpose . "1.2"))

(defcustom purpose-layout-dirs
  (list (locate-user-emacs-file "layouts/"))
  "List of directories containing layout files."
  :group 'purpose
  :type '(repeat file)
  :package-version '(window-purpose . "1.5"))

(defcustom purpose-get-extra-window-params-functions nil
  "If non-nil, this variable should be a list of functions.
This variable is used by `purpose-window-params'.  See
`purpose-window-params' for more details."
  :group 'purpose
  :type 'hook
  :package-version '(window-purpose . "1.4"))

(defcustom purpose-set-window-properties-functions nil
  "Hook to run after calling `purpose-set-window-properties'.
Use this to set additional properties for windows as they are created,
when `purpose-set-window-layout' or `purpose-load-window-layout-file' is called.  Each
function in `purpose-set-window-properties-functions' is called with two
arguments: PROPERTIES and WINDOW.  PROPERTIES is the window's property
list as saved in the used layout, and WINDOW is the new window.  If
WINDOW is nil, your function should act on the selected window
instead."
  :group 'purpose
  :type 'hook
  :package-version '(window-purpose . "1.2"))

(defvar purpose-recent-window-layouts (make-ring 50)
  "Most recently used window layouts.
This variable stores recent layouts used by `purpose-set-window-layout'.")

(defvar purpose-recent-frame-layouts (make-ring 50)
  "Most recently used frame layouts.
This variable stores recent layouts used by `purpose-set-frame-layout'.")



;;; window-params low-level functions
(defun purpose--window-edges-to-percentage (&optional window)
  "Convert a `window-edges' list from integers to percentages.
The percentages represent the WINDOW's edges relative to its frame's
size."
  (cl-multiple-value-bind (left top right bottom) (window-edges window)
    (let ((frame-width (frame-width (window-frame window)))
          (frame-height (frame-height (window-frame window))))
      (list (/ left frame-width 1.0)
            (/ top frame-height 1.0)
            (/ right frame-width 1.0)
            (/ bottom frame-height 1.0)))))

(defun purpose--window-width-to-percentage (&optional window)
  "Return a percentage of WINDOW's width to its frame's width.
WINDOW defaults to the selected window."
  (/ (window-total-width window) (frame-width (window-frame window)) 1.0))

(defun purpose--window-height-to-percentage (&optional window)
  "Return a percentage of WINDOW's height to its frame's height.
WINDOW defaults to the selected window."
  (/ (window-total-height window) (frame-height (window-frame window)) 1.0))

(defun purpose--window-percentage-to-width (percentage &optional window)
  "Return a window width as an integer.
The width is the PERCENTAGE of WINDOW's frame's width."
  (round (* percentage (frame-width (window-frame window)))))

(defun purpose--window-percentage-to-height (percentage &optional window)
  "Return a window height as an integer.
The height is the PERCENTAGE of WINDOW's frame's height."
  (round (* percentage (frame-height (window-frame window)))))



;;; window-params high-level functions
(defun purpose-window-params-p (obj)
  "Return non-nil if OBJ is a window-params plist.
A window-params plist is a plist that is given by
`purpose-window-params'."
  (and (listp obj)
       (plist-member obj :purpose)))

(defun purpose-window-params (&optional window)
  "Return a plist containing the window parameters that are relevant for
Purpose plugin.
These parameters are :purpose, :purpose-dedicated, :width, :height and
:edges.
:purpose is the window's purpose.
:purpose-dedicated corresponds to WINDOW's window parameter of the same
name.
:width is the width of the window as a percentage of the frame's width.
:height is the height of the window as a percentage of the frame's
height.
:edges is also given in percentages.

WINDOW defaults to the selected window.

If the variable `purpose-get-extra-window-params-functions' is non-nil,
it should be a list of functions, where each function receives a window
as an optional argument and returns a plist.  Each plist is concatenated
into the plist that `purpose-window-params' returns.  The plists returned
by `purpose-get-extra-window-params-functions' shouldn't contain any of
the keys described above (:purpose, :purpose-dedicated, :width, :height,
:edges).  If any of them does contain any of these keys, the behavior is
not defined."
  (let ((buffer (window-buffer window)))
    (apply #'append
           (list :purpose (purpose-buffer-purpose buffer)
                 :purpose-dedicated (purpose-window-purpose-dedicated-p window)
                 :width (purpose--window-width-to-percentage window)
                 :height (purpose--window-height-to-percentage window)
                 :edges (purpose--window-edges-to-percentage window))
           (mapcar #'(lambda (fn)
                       (funcall fn window))
                   purpose-get-extra-window-params-functions))))

(defun purpose-set-window-properties (properties &optional window)
  "Set the buffer and window-parameters of window WINDOW, according to
property list PROPERTIES.
This function runs `purpose-set-window-properties-functions' when it
finishes."
  (purpose--set-window-buffer (plist-get properties :purpose) window)
  (purpose-set-window-purpose-dedicated-p window
                                          (plist-get properties
                                                     :purpose-dedicated))
  (run-hook-with-args 'purpose-set-window-properties-functions
                      properties window))



;;; Low level functions for changing the layout
(defun purpose--split-window (tree window)
  "Split window WINDOW to the amount of child windows it contains.
TREE is a window tree (see `window-tree' for what is a window tree).
WINDOW should be a live window, and defaults to the selected one.

This function is mainly intended to be used by
`purpose-restore-windows-1'."
  (append (list window)
          ;; Starting from 2nd sub-tree, since N sub-trees require N-1 splits.
          ;; Reversing, because the selected window doesn't change, so the
          ;; first split window is actually the farthest away, and so matches
          ;; the last sub-tree.
          (nreverse
           (cl-loop for _sub-tree in (cl-cdddr tree)
                    with direction = (not (car tree))
                    collect (split-window window -5 direction)))))

(defun purpose--set-size (width height &optional window)
  "Set the size of window WINDOW to width WIDTH and height HEIGHT.
WINDOW must be a live window and defaults to the selected one."
  (unless (one-window-p)
    (let ((width-delta (- width (window-total-width window)))
          (height-delta (- height (window-total-height window))))
      (window-resize window
                     (window-resizable window width-delta t nil)
                     t nil)
      (window-resize window
                     (window-resizable window height-delta nil nil)
                     nil nil))))

(defun purpose--set-size-percentage (width-percentage
                                     height-percentage
                                     &optional window)
  (purpose--set-size
   (purpose--window-percentage-to-width width-percentage window)
   (purpose--window-percentage-to-height height-percentage window)
   window))

(defun purpose--tree-width-from-edges (tree)
  "Get TREE's total width."
  (let ((edges (nth 1 tree)))
    (- (nth 2 edges) (car edges))))

(defun purpose--tree-height-from-edges (tree)
  "Get TREE's total height."
  (let ((edges (nth 1 tree)))
    (- (nth 3 edges) (nth 1 edges))))



;;; Helpers for finding layouts by name
(defun purpose--directory-files (suffix directory)
  "Get a list of all filenames that end in SUFFIX in DIRECTORY.
The base filenames without the suffix are returned."
  (cl-loop for filename in (directory-files directory nil nil t)
           if (string-suffix-p suffix filename)
           collect (string-remove-suffix suffix filename)))

(defun purpose--file-with-suffix (name suffix directory)
  "Get file with basename NAME and suffix SUFFIX in DIRECTORY.
The full path of the file returned.  Return nil if no file can be found.
NAME is the name of the file without the suffix or the directory.
SUFFIX should include a \".\" (if needed), it will not be added
automatically.
DIRECTORY must be the name of an existing directory."
  (cl-loop for filename in (directory-files directory t)
           if (string= (file-name-nondirectory filename)
                       (format "%s%s" name suffix))
           return filename))

(defun purpose-normalize-layout-directories (&optional layout-dirs include-built-in-p)
  "Return a list of layout directories.
LAYOUT-DIRS is a list of directory names, and defaults to
`purpose-layout-dirs'.
If INCLUDE-BUILT-IN-P is non-nil, include
`purpose--built-in-layouts-dir' in the result."
  (if include-built-in-p
      (append (or layout-dirs purpose-layout-dirs)
              (list purpose--built-in-layouts-dir))
    (or layout-dirs purpose-layout-dirs)))

(defun purpose-all-window-layouts (&optional layout-dirs include-built-in-p)
  "Get a sorted list of all window layouts in LAYOUT-DIRS.
LAYOUT-DIRS is a list of directory names, and defaults to
`purpose-layout-dirs'.
If INCLUDE-BUILT-IN-P is non-nil, also search layouts in
`purpose--built-in-layouts-dir'."
  (sort (delete-dups
         (purpose-flatten
          (cl-loop for dir in (purpose-normalize-layout-directories
                               layout-dirs include-built-in-p)
                   if (and (file-readable-p dir)
                           (file-directory-p dir))
                   collect (purpose--directory-files ".window-layout" dir))))
        #'string-lessp))

(defun purpose-find-window-layout (name &optional layout-dirs)
  "Get the full path of a window layout.
NAME is the name of the window layout.
LAYOUT-DIRS is a list of directories to search for the layout file, and
defaults to `purpose-layout-dirs'.  Any non-existent or unreadable
directory is ignored. If `purpose-use-built-in-layouts' is non-nil and
the layout can't be found in LAYOUT-DIRS, then search also in
`purpose--built-in-layouts-dir'.
If there are severeal layouts with the same name, the first that is found
is returned.  The directories are searched in the same order that they
appear in LAYOUT-DIRS."
  (cl-loop for dir in (purpose-normalize-layout-directories
                       layout-dirs purpose-use-built-in-layouts)
           for filename = (and (file-readable-p dir)
                               (file-directory-p dir)
                               (purpose--file-with-suffix name ".window-layout" dir))
           if filename return filename))

(defun purpose-all-frame-layouts (&optional layout-dirs include-built-in-p)
  "Get a sorted list of all frame layouts in LAYOUT-DIRS.
LAYOUT-DIRS is a list of directory names, and defaults to
`purpose-layout-dirs'.
If INCLUDE-BUILT-IN-P is non-nil, also search layouts in
`purpose--built-in-layouts-dir'."
  (sort (delete-dups
         (purpose-flatten
          (cl-loop for dir in (purpose-normalize-layout-directories
                               layout-dirs include-built-in-p)
                   if (and (file-readable-p dir)
                           (file-directory-p dir))
                   collect (purpose--directory-files ".frame-layout" dir))))
        #'string-lessp))

(defun purpose-find-frame-layout (name &optional layout-dirs)
  "Get the full path of a frame layout.
NAME is the name of the frame layout.
LAYOUT-DIRS is a list of directories to search for the layout file, and
defaults to `purpose-layout-dirs'.  Any non-existent or unreadable
directory is ignored. If `purpose-use-built-in-layouts' is non-nil and
the layout can't be found in LAYOUT-DIRS, then search also in
`purpose--built-in-layouts-dir'.
If there are severeal layouts with the same name, the first that is found
is returned.  The directories are searched in the same order that they
appear in LAYOUT-DIRS."
  (cl-loop for dir in (purpose-normalize-layout-directories
                       layout-dirs purpose-use-built-in-layouts)
           for filename = (and (file-readable-p dir)
                               (file-directory-p dir)
                               (purpose--file-with-suffix name ".frame-layout" dir))
           if filename return filename))



;;; Recursive helpers for setting/getting the layout
(defun purpose--get-window-layout-1 (window-tree)
  "Helper function for `purpose-get-window-layout'."
  (if (windowp window-tree)
      (purpose-window-params window-tree)
    (append (list (cl-first window-tree)
                  (cl-second window-tree))
            (mapcar #'purpose--get-window-layout-1 (cddr window-tree)))))

(defun purpose--set-window-layout-1 (tree window ref-width ref-height)
  "Helper function for `purpose-set-window-layout'.
REF-WIDTH and REF-HEIGHT are the sizes of the root window, as
saved in the top-level tree (not the actual sizes of the selected
frame)."
  (if (purpose-window-params-p tree)
      (progn
        ;; set size and properties of leaf window
        (purpose--set-size-percentage (plist-get tree :width)
                                      (plist-get tree :height)
                                      window)
        (purpose-set-window-properties tree window))

    ;; set size of parent window
    (let ((width-percentage (/ (purpose--tree-width-from-edges tree)
                               ref-width 1.0))
          (height-percentage (/ (purpose--tree-height-from-edges tree)
                                ref-height 1.0)))
      (purpose--set-size-percentage width-percentage height-percentage window))

    ;; split parent window and recurse into children
    (let ((windows (purpose--split-window tree window)))
      (cl-loop for sub-tree in (cddr tree)
               for window in windows
               do (purpose--set-window-layout-1 sub-tree window ref-width ref-height)))))



;;; High level functions for setting/getting the layout (UI/API)

;;; get/set/load/save window layout

(defun purpose-get-window-layout (&optional frame)
  "Get window layout of FRAME.
FRAME defaults to the selected frame."
  (purpose--get-window-layout-1 (car (window-tree frame))))

(defun purpose-set-window-layout (layout &optional frame norecord)
  "Set LAYOUT as FRAME's window layout.
FRAME defaults to the selected frame.
LAYOUT must be a layout as returned by `purpose-get-window-layout'.
Unless NORECORD is non-nil, this function sets LAYOUT as the value of
`purpose-recent-window-layouts'.
This function doesn't change the selected frame (uses
`with-selected-frame' internally)."
  (with-selected-frame (or frame (selected-frame))
    (delete-other-windows)
    ;; ensure that the remaining window is not dedicated, otherwise we won't be
    ;; able to change its buffer, and an error will be signaled (issue #38)
    (set-window-dedicated-p nil nil)
    ;; 1. split
    ;; 2. let each window splits itself/set its size

    ;; 1. if windowp, set size+buffer
    ;; 2. split window, recurse for each window
    (if (purpose-window-params-p layout)
        (purpose-set-window-properties layout)
      (purpose--set-window-layout-1 layout (selected-window)
                                    (purpose--tree-width-from-edges layout)
                                    (purpose--tree-height-from-edges layout)))
    ;; if purpose has more than 1 window, try to show different buffers in those
    ;; windows
    (let ((purposes (delete-dups (mapcar #'purpose-window-purpose
                                         (window-list frame)))))
      (dolist (purpose purposes)
        (cl-mapcar #'set-window-buffer
                   (purpose-windows-with-purpose purpose frame)
                   ;; TODO: implement function to take only first N buffers with a
                   ;; certain purpose, instead of getting all them (more efficient)
                   (purpose-buffers-with-purpose purpose))))
    (unless norecord
      (ring-insert purpose-recent-window-layouts layout))))

;;;###autoload
(defun purpose-save-window-layout-file (&optional filename)
  "Save window layout of current frame to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name
          "[PU] Save window layout to file: "
          (file-name-directory purpose-default-layout-file)
          nil nil
          (file-name-nondirectory purpose-default-layout-file))))
  (with-temp-file (or filename purpose-default-layout-file)
    ;; "%S" - print as S-expression - this allows us to read the value with
    ;; `read' later in `purpose-load-window-layout-file'
    (insert (format "%S" (purpose-get-window-layout)))))

;;;###autoload
(defun purpose-load-window-layout-file (&optional filename)
  "Load window layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name
          "[PU] Load window layout from file: "
          (file-name-directory purpose-default-layout-file)
          nil nil
          (file-name-nondirectory purpose-default-layout-file))))
  (purpose-set-window-layout
   (with-temp-buffer
     (insert-file-contents (or filename purpose-default-layout-file))
     (read (point-marker)))))

;;;###autoload
(defun purpose-save-window-layout (name directory)
  "Save a window layout.
NAME is the name to give the window layout.
DIRECTORY is the directory in which to save the layout."
  (interactive
   (let ((layout-dirs (purpose-normalize-layout-directories nil nil)))
     (if (null layout-dirs)
         (user-error (concat "No directory is set for user layouts. "
                             "Please add a directory to `purpose-layouts-dir'"))
       (list (read-string "[PU] save layout name: ")
             (completing-read
              "[PU] save to directory: "
              layout-dirs nil t)))))
  (let ((layout-file (concat (file-name-as-directory directory)
                             name ".window-layout")))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (purpose-save-window-layout-file layout-file)))

;;;###autoload
(defun purpose-load-window-layout (&optional name layout-dirs)
  "Load a window layout.
NAME is the name of a window layout.  If NAME is not given, prompt the
user for a name.
LAYOUT-DIRS is a list of directories to search for the layout file, and
defaults to `purpose-layout-dirs'.  If `purpose-use-built-in-layouts',
then `purpose--built-in-layouts-dir' is also searched.  See
`purpose-find-window-layout' for more details.

To load a window layout from a specific file, use
`purpose-load-window-layout-file'."
  (interactive)
  (let ((name (or name (completing-read
                        "[PU] Load window layout:"
                        (purpose-all-window-layouts layout-dirs
                                                    purpose-use-built-in-layouts)
                        nil t))))
    ;; layout with NAME is guaranteed to exist because of non-nil REQUIRE-MATCH
    ;; argument to (completing-read).
    (purpose-load-window-layout-file (purpose-find-window-layout name layout-dirs))))

(defun purpose-reset-window-layout ()
  "Load most recent window layout from `purpose-reset-window-layouts'.
If there is no recent layout, do nothing."
  (interactive)
  (unless (ring-empty-p purpose-recent-window-layouts)
    (purpose-load-recent-window-layout 0)))

(defun purpose-load-recent-window-layout (index)
  "Load window layout from `purpose-recent-window-layouts'.
Use INDEX=0 for most recent."
  (purpose-set-window-layout (ring-ref purpose-recent-window-layouts index)
                             nil
                             (zerop index)))

;;; get/set/load/save frame layout

(defun purpose-get-frame-layout ()
  "Return Emacs' frame layout.
The frame layout is a list of all live frames' window layouts. Each
window-layout is a window-layout as returned by
`purpose-get-window-layout'."
  (mapcar #'purpose-get-window-layout (frame-list)))

(defun purpose-set-frame-layout (layout &optional norecord)
  "Set LAYOUT as Emacs' frame layout.
LAYOUT must be a layout as returned by `purpose-get-frame-layout'.
Unless NORECORD is non-nil, this function adds LAYOUT to
`purpose-recent-frame-layouts'.
This function deletes all existing frames and creates frames as
specified by LAYOUT."
  (delete-other-frames)
  (purpose-set-window-layout (car layout) nil t)
  (dolist (window-layout (cdr layout))
    (purpose-set-window-layout window-layout (make-frame) t))
  (unless norecord
    (ring-insert purpose-recent-frame-layouts layout)))

;;;###autoload
(defun purpose-save-frame-layout-file (&optional filename)
  "Save frame layout of Emacs to file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name
          "[PU] Save frame layout to file: "
          (file-name-directory purpose-default-layout-file)
          nil nil
          (file-name-nondirectory purpose-default-layout-file))))
  (with-temp-file (or filename purpose-default-layout-file)
    ;; "%S" - print as S-expression - this allows us to read the value with
    ;; `read' later in `purpose-load-window-layout-file'
    (insert (format "%S" (purpose-get-frame-layout)))))

;;;###autoload
(defun purpose-load-frame-layout-file (&optional filename)
  "Load frame layout from file FILENAME.
If FILENAME is nil, use `purpose-default-layout-file' instead."
  (interactive
   (list (read-file-name
          "[PU] Load frame layout from file: "
          (file-name-directory purpose-default-layout-file)
          nil nil
          (file-name-nondirectory purpose-default-layout-file))))
  (purpose-set-frame-layout
   (with-temp-buffer
     (insert-file-contents (or filename purpose-default-layout-file))
     (read (point-marker)))))

;;;###autoload
(defun purpose-save-frame-layout (name directory)
  "Save a frame layout.
NAME is the name to give the frame layout.
DIRECTORY is the directory in which to save the layout."
  (interactive
   (let ((layout-dirs (purpose-normalize-layout-directories nil nil)))
     (if (null layout-dirs)
         (user-error (concat "No directory is set for user layouts. "
                             "Please add a directory to `purpose-layouts-dir'"))
       (list (read-string "[PU] save layout name: ")
             (completing-read
              "[PU] save to directory: "
              layout-dirs nil t)))))
  (let ((layout-file (concat (file-name-as-directory directory)
                             name ".frame-layout")))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (purpose-save-frame-layout-file layout-file)))

;;;###autoload
(defun purpose-load-frame-layout (&optional name layout-dirs)
  "Load a frame layout.
NAME is the name of a frame layout.  If NAME is not given, prompt the
user for a name.
LAYOUT-DIRS is a list of directories to search for the layout file, and
defaults to `purpose-layout-dirs'.  If `purpose-use-built-in-layouts',
then `purpose--built-in-layouts-dir' is also searched.  See
`purpose-find-frame-layout' for more details.

To load a frame layout from a specific file, use
`purpose-load-frame-layout-file'."
  (interactive)
  (let ((name (or name (completing-read
                        "[PU] Load frame layout:"
                        (purpose-all-frame-layouts layout-dirs
                                                   purpose-use-built-in-layouts)
                        nil t))))
    ;; layout with NAME is guaranteed to exist because of non-nil REQUIRE-MATCH
    ;; argument to (completing-read).
    (purpose-load-frame-layout-file (purpose-find-frame-layout name layout-dirs))))

(defun purpose-reset-frame-layout ()
  "Load most recent frame layout from `purpose-reset-frame-layouts'.
If there is no recent layout, do nothing."
  (interactive)
  (unless (ring-empty-p purpose-recent-frame-layouts)
    (purpose-load-recent-frame-layout 0)))

(defun purpose-load-recent-frame-layout (index)
  "Load frame layout from `purpose-recent-frame-layouts'.
Use INDEX=0 for most recent."
  (purpose-set-frame-layout (ring-ref purpose-recent-frame-layouts 0)
                            (zerop index)))



;;; Other

;;;###autoload
(defun purpose-delete-non-dedicated-windows ()
  "Delete all windows that aren't dedicated to their purpose or buffer."
  (interactive)
  (mapc #'(lambda (window)
            (when (and (window-deletable-p window)
                       (not (window-dedicated-p window))
                       (not (purpose-window-purpose-dedicated-p window)))
              (delete-window window)))
        (window-list)))

;;;###autoload
(defun purpose-set-window-purpose (purpose &optional dont-dedicate)
  "Set window's purpose to PURPOSE, and dedicate it.
With prefix argument (DONT-DEDICATE is non-nil), don't dedicate the
window.  If DONT-DEDICATE is non-nil, and the current window is
dedicated, un-dedicate the window.
Changing the window's purpose is done by displaying a buffer of
the right purpose in it, or creating a dummy buffer."
  (interactive
   (list (purpose-read-purpose "Purpose: " nil 'confirm)
         current-prefix-arg))
  (purpose--set-window-buffer purpose)
  (purpose-set-window-purpose-dedicated-p nil (not dont-dedicate)))

(defun purpose--delete-window-at (window-getter &optional frame)
  "Delete window returned by WINDOW-GETTER.
WINDOW-GETTER should be a function that takes one argument - FRAME."
  (let ((window (funcall window-getter frame)))
    (if window
        (delete-window window)
      (user-error "Couldn't find window."))))

(defun purpose-delete-window-at-top (&optional frame)
  "Delete window at top.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-top-window frame))

(defun purpose-delete-window-at-bottom (&optional frame)
  "Delete window at bottom.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-bottom-window frame))

(defun purpose-delete-window-at-left (&optional frame)
  "Delete window at left.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-left-window frame))

(defun purpose-delete-window-at-right (&optional frame)
  "Delete window at right.
FRAME defaults to the selected frame."
  (interactive)
  (purpose--delete-window-at #'purpose-get-right-window frame))

(provide 'window-purpose-layout)
;;; window-purpose-layout.el ends here
