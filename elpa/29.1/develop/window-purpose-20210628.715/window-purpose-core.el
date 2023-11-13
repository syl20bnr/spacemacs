;;; window-purpose-core.el --- Core functions for Purpose -*- lexical-binding: t -*-

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
;; This file contains core functions to be used by other parts of
;; package Purpose.

;;; Code:

(require 'window-purpose-configuration)
(eval-when-compile (require 'subr-x))

(defgroup purpose nil
  "purpose-mode configuration"
  :group 'windows
  :prefix "purpose-"
  :package-version '(window-purpose . "1.2"))

(defcustom default-purpose 'general
  "The default purpose for buffers which didn't get another purpose."
  :group 'purpose
  :type 'symbol
  :package-version '(window-purpose . "1.2"))

(defcustom default-file-purpose 'edit
  "The default purpose for buffers visiting a file which didn't get a purpose."
  :group 'purpose
  :type 'symbol
  :package-version '(window-purpose . "1.6.1"))

;;; utilities

(defun purpose--buffer-major-mode (buffer-or-name)
  "Return the major mode of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun purpose--dummy-buffer-name (purpose)
  "Create the name for a dummy buffer with purpose PURPOSE.
The name created is \"*pu-dummy-PURPOSE-*\".  e.g. for purpose 'edit,
the name is \"*pu-dummy-edit-*\"."
  (concat "*pu-dummy-" (symbol-name purpose) "*"))

(defun purpose--dummy-buffer-purpose (buffer-or-name)
  "Get buffer's purpose for dummy buffers.
A dummy buffer is a buffer with a name that starts with \"*pu-dummy-\"
and ends with \"*\".  For example, the buffer \"*pu-dummy-edit*\" is a
dummy buffer with the purpose 'edit."
  (let ((name (if (stringp buffer-or-name)
                  buffer-or-name
                (buffer-name buffer-or-name))))
    (when (and (string-prefix-p "*pu-dummy-" name)
               (string= "*" (substring name -1)))
      ;; 10 = (length "*pu-dummy-")
      (intern (substring name 10 -1)))))

;;; simple purpose-finding operations for `purpose-buffer-purpose'
(defun purpose--buffer-purpose-mode (buffer-or-name mode-conf)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by its
mode and MODE-CONF.
MODE-CONF is a hash table mapping modes to purposes."
  (when (get-buffer buffer-or-name)     ; check if buffer exists
    (let* ((major-mode (purpose--buffer-major-mode buffer-or-name))
           (derived-modes (purpose--iter-hash #'(lambda (mode _purpose) mode)
                                              mode-conf))
           (derived-mode (apply #'derived-mode-p derived-modes)))
      (when derived-mode
        (gethash derived-mode mode-conf)))))

(defun purpose--buffer-purpose-name (buffer-or-name name-conf)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by its
exact name and NAME-CONF.
NAME-CONF is a hash table mapping names to purposes."
  (gethash (if (stringp buffer-or-name)
               buffer-or-name
             (buffer-name buffer-or-name))
           name-conf))

(defun purpose--buffer-purpose-name-regexp-1 (buffer-or-name regexp purpose)
  "Return purpose PURPOSE if buffer BUFFER-OR-NAME's name matches
regexp REGEXP."
  (when (string-match-p regexp (or (and (bufferp buffer-or-name)
                                        (buffer-name buffer-or-name))
                                   buffer-or-name))
    purpose))

(defun purpose--buffer-purpose-name-regexp (buffer-or-name regexp-conf)
  "Return the purpose of buffer BUFFER-OR-NAME, as determined by the
regexps matched by its name.
REGEXP-CONF is a hash table mapping name regexps to purposes."
  (catch 'found
    (maphash
     #'(lambda (regexp purpose)
         (when (purpose--buffer-purpose-name-regexp-1 buffer-or-name
                                                      regexp
                                                      purpose)
           (throw 'found purpose)))
     regexp-conf)))

(defun purpose-buffer-purpose (buffer-or-name)
  "Get the purpose of buffer BUFFER-OR-NAME.
The purpose is determined by consulting these functions in this order:
1. `purpose--dummy-buffer-purpose'
2. `purpose--buffer-purpose-name' with the user configuration
3. `purpose--buffer-purpose-name-regexp' with the user configuration
4. `purpose--buffer-purpose-mode' with the user configuration
5. `purpose--buffer-purpose-name' with the extended configuration
6. `purpose--buffer-purpose-name-regexp' with the extended configuration
7. `purpose--buffer-purpose-mode' with the extended configuration
And if `purpose-use-default-configuration' is non-nil, consult also:
8. `purpose--buffer-purpose-name' with the default configuration
9. `purpose--buffer-purpose-name-regexp' with the default configuration
10. `purpose--buffer-purpose-mode' with the default configuration

If no purpose was determined, return `default-purpose'."
  (or
   ;; check dummy buffer
   (purpose--dummy-buffer-purpose buffer-or-name)

   ;; check user config
   (purpose--buffer-purpose-name buffer-or-name purpose--user-name-purposes)
   (purpose--buffer-purpose-name-regexp buffer-or-name
                                        purpose--user-regexp-purposes)
   (purpose--buffer-purpose-mode buffer-or-name purpose--user-mode-purposes)

   ;; check extensions' config
   (purpose--buffer-purpose-name buffer-or-name
                                 purpose--extended-name-purposes)
   (purpose--buffer-purpose-name-regexp buffer-or-name
                                        purpose--extended-regexp-purposes)
   (purpose--buffer-purpose-mode buffer-or-name
                                 purpose--extended-mode-purposes)

   ;; check default config
   (and
    purpose-use-default-configuration
    (or
     (purpose--buffer-purpose-name buffer-or-name
                                   purpose--default-name-purposes)
     (purpose--buffer-purpose-name-regexp buffer-or-name
                                          purpose--default-regexp-purposes)
     (purpose--buffer-purpose-mode buffer-or-name
                                   purpose--default-mode-purposes)))

   ;; if the buffer is visiting a file, fallback to 'edit purpose
   (and (buffer-file-name (get-buffer buffer-or-name))
        default-file-purpose)
   ;; fallback to default purpose
   default-purpose))

(defun purpose-buffers-with-purpose (purpose)
  "Return a list of all existing buffers with purpose PURPOSE."
  (cl-delete-if-not #'(lambda (buffer)
                        (and (eql purpose (purpose-buffer-purpose buffer))
                             (not (minibufferp buffer))))
                    (buffer-list)))

(defun purpose-window-purpose (&optional window)
  "Get the purpose of window WINDOW.
The window's purpose is determined by its buffer's purpose.
WINDOW defaults to the selected window."
  (purpose-buffer-purpose (window-buffer window)))

(defun purpose-windows-with-purpose (purpose &optional frame)
  "Return a list of all live windows with purpose PURPOSE in FRAME.
FRAME defaults to the selected frame."
  (cl-remove-if-not #'(lambda (window)
                        (eql purpose (purpose-window-purpose window)))
                    (window-list frame)))

(defun purpose-get-all-purposes ()
  "Return a list of all known purposes."
  (delete-dups
   (append (list default-purpose)
           (purpose-flatten
            (mapcar #'hash-table-values
                    (append (when purpose-use-default-configuration
                              (list purpose--default-name-purposes
                                    purpose--default-mode-purposes
                                    purpose--default-regexp-purposes))
                            (list purpose--extended-name-purposes
                                  purpose--extended-mode-purposes
                                  purpose--extended-regexp-purposes
                                  purpose--user-mode-purposes
                                  purpose--user-name-purposes
                                  purpose--user-regexp-purposes)))))))

(defun purpose-read-purpose (prompt &optional purposes require-match initial-output)
  "Read a purpose from the user.
PROMPT is the prompt to show the user.
PURPOSES is the available purposes the user can choose from, and
defaults to all defined purposes.
REQUIRE-MATCH and INITIAL-OUTPUT have the same meaning as in
`completing-read'."
  (let ((purpose-strings (mapcar #'symbol-name
                                 (or purposes (purpose-get-all-purposes)))))
    (intern (completing-read
             prompt
             purpose-strings
             nil
             require-match
             initial-output))))


;;; purpose-aware buffer low-level functions
(defun purpose--get-buffer-create (purpose)
  "Get the first buffer with purpose PURPOSE.
If there is no such buffer, create a dummy buffer with purpose
PURPOSE."
  (or (car (purpose-buffers-with-purpose purpose))
      (get-buffer-create (purpose--dummy-buffer-name purpose))))

(defun purpose--set-window-buffer (purpose &optional window)
  "Make WINDOW display first buffer with purpose PURPOSE.
WINDOW must be a live window and defaults to the selected one.
If there is no buffer with purpose PURPOSE, create a dummy buffer with
purpose PURPOSE."
  (set-window-buffer window (purpose--get-buffer-create purpose)))



;;; window purpose dedication
(defun purpose-set-window-purpose-dedicated-p (window flag)
  "Set window parameter 'purpose-dedicated of window WINDOW to value
FLAG.
WINDOW defaults to the selected window."
  (set-window-parameter window 'purpose-dedicated flag))

(defun purpose-window-purpose-dedicated-p (&optional window)
  "Return non-nil if window WINDOW is dedicated to its purpose.
The result is determined by window parameter 'purpose-dedicated.
WINDOW defaults to the selected window."
  (window-parameter window 'purpose-dedicated))

(defun purpose-toggle-window-purpose-dedicated (&optional window)
  "Toggle window WINDOW's dedication to its purpose on or off.
WINDOW defaults to the selected window."
  (interactive)
  (let ((flag (not (purpose-window-purpose-dedicated-p window))))
    (purpose-set-window-purpose-dedicated-p window flag)
    (if flag
        (message "Window purpose is now dedicated")
      (message "Window purpose is not dedicated anymore"))
    (force-mode-line-update)
    flag))

;; not really purpose-related, but helpful for the user
;;;###autoload
(defun purpose-toggle-window-buffer-dedicated (&optional window)
  "Toggle window WINDOW's dedication to its current buffer on or off.
WINDOW defaults to the selected window."
  (interactive)
  (let* ((flag (not (window-dedicated-p window))))
    (set-window-dedicated-p window flag)
    (if flag
        (message "Window buffer is now dedicated")
      (message "Window buffer is not dedicated anymore"))
    (force-mode-line-update)
    flag))



;;; special window locations
(defun purpose-get-top-window (&optional frame)
  "Get FRAME's top window.
The top window is a window that takes up all the of the frame's width
and has no window above it.  If there is no top window, return nil."
  (let (top-window)
    (walk-window-tree #'(lambda (window)
                          (unless (or (window-in-direction 'left window)
                                      (window-in-direction 'right window)
                                      (window-in-direction 'above window)
                                      (not (window-in-direction 'below window)))
                            (setq top-window window)))
                      frame)
    top-window))

(defun purpose-get-bottom-window (&optional frame)
  "Get FRAME's bottom window.
The bottom window is a window that takes up all the of the frame's width
and has no window below it.  If there is no bottom window, return nil."
  (let (bottom-window)
    (walk-window-tree #'(lambda (window)
                          (unless (or (window-in-direction 'left window)
                                      (window-in-direction 'right window)
                                      (window-in-direction 'below window)
                                      (not (window-in-direction 'above window)))
                            (setq bottom-window window)))
                      frame)
    bottom-window))

(defun purpose-get-left-window (&optional frame)
  "Get FRAME's left window.
The left window is a window that takes up all the of the frame's
height and has no window to its left.  If there is no left window,
return nil."
  (let (left-window)
    (walk-window-tree #'(lambda (window)
                          (unless (or (window-in-direction 'above window)
                                      (window-in-direction 'below window)
                                      (window-in-direction 'left window)
                                      (not (window-in-direction 'right window)))
                            (setq left-window window)))
                      frame)
    left-window))

(defun purpose-get-right-window (&optional frame)
  "Get FRAME's right window.
The right window is a window that takes up all the of the frame's
height and has no window to its right.  If there is no right window,
return nil."
  (let (right-window)
    (walk-window-tree #'(lambda (window)
                          (unless (or (window-in-direction 'above window)
                                      (window-in-direction 'below window)
                                      (window-in-direction 'right window)
                                      (not (window-in-direction 'left window)))
                            (setq right-window window)))
                      frame)
    right-window))

(provide 'window-purpose-core)
;;; window-purpose-core.el ends here
