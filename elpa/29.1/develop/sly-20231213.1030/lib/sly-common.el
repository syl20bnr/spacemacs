;;; sly-common.el --- common utils for SLY and its contribs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: 

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

;; Common utilities for SLY and its contribs

;;; Code:
(require 'cl-lib)

(defun sly--call-refreshing (buffer
                              overlay
                              dont-erase
                              recover-point-p
                              flash-p
                              fn)
  (with-current-buffer buffer
    (let ((inhibit-point-motion-hooks t)
          (inhibit-read-only t)
          (saved (point)))
      (save-restriction
        (when overlay
          (narrow-to-region (overlay-start overlay)
                            (overlay-end overlay)))
        (unwind-protect
            (if dont-erase
                (goto-char (point-max))
              (delete-region (point-min) (point-max)))
          (funcall fn)
          (when recover-point-p
            (goto-char saved)))
        (when flash-p
          (sly-flash-region (point-min) (point-max)))))
    buffer))

(cl-defmacro sly-refreshing ((&key
                              overlay
                              dont-erase
                              (recover-point-p t)
                              flash-p
                              buffer)
                             &rest body)
  "Delete a buffer region and run BODY which presumably refreshes it.
Region is OVERLAY or the whole buffer.
Recover point position if RECOVER-POINT-P.
Flash the resulting region if FLASH-P"
  (declare (indent 1)
           (debug (sexp &rest form)))
  `(sly--call-refreshing ,(or buffer
                              `(current-buffer))
                         ,overlay
                         ,dont-erase
                         ,recover-point-p
                         ,flash-p
                         #'(lambda () ,@body)))


(provide 'sly-common)
;;; sly-common.el ends here
