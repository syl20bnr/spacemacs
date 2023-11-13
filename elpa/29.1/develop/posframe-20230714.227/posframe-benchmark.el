;;; posframe-benchmark.el --- Benchmark tool for posframe    -*- lexical-binding:t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/posframe
;; Version: 1.0.3
;; Keywords: convenience, tooltip
;; Package-Requires: ((emacs "26"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'posframe)

(defvar posframe-benchmark-alist
  (let ((str (with-temp-buffer
               (insert-file-contents (locate-library "posframe.el"))
               (buffer-string))))
    `((font-at
       (font-at (point-min)))
      (redraw-display
       (redraw-display))
      (redraw-frame
       (redraw-frame (window-frame)))
      (remove-text-properties
       (let ((string ,str))
         (remove-text-properties
          0 (length string) '(read-only t)
          string)))
      (mouse-position
       (mouse-position))
      (default-font-width
       (default-font-width))
      (posframe--get-font-height
       (posframe--get-font-height (point-min)))
      (frame-parameter
       (frame-parameter (window-frame) 'no-accept-focus))
      (set-mouse-position
       (set-mouse-position (window-frame) 0 0))
      (posn-at-point
       (posn-at-point))
      (posn-x-y
       (posn-x-y (posn-at-point)))
      (posn-object-x-y
       (posn-object-x-y (posn-at-point)))
      (set-frame-parameter
       (set-frame-parameter (window-frame) 'test 1))
      (raise-frame
       (raise-frame (window-frame))))))

;;;###autoload
(defun posframe-benchmark ()
  "Benchmark tool for posframe."
  (interactive)
  (let ((n 1000))
    (message "\n* Posframe Benchmark")
    (dolist (x posframe-benchmark-alist)
      (message "\n** Benchmark `%S' %s times ..." (car x) n)
      (benchmark n (car (cdr x))))
    (message "\n* Finished.")))


(provide 'posframe-benchmark)

;;; posframe.el ends here
