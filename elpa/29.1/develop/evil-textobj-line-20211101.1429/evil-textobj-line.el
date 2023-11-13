;;; evil-textobj-line.el --- Line text object for Evil

;; Copyright (C) 2015 Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Package-Requires: ((evil "1.0.0"))

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

;; This package implements a line text object for Evil.

;;; Code:

(require 'evil)

(defgroup evil-textobj-line nil
  "Line text object for Evil."
  :group 'evil)

(defcustom evil-textobj-line-i-key "l"
  "Keys for evil-inner-line."
  :type 'string
  :group 'evil-textobj-line)

(defcustom evil-textobj-line-a-key "l"
  "Keys for evil-a-line."
  :type 'string
  :group 'evil-textobj-line)

(defun evil-line-range (count beg end type &optional inclusive)
  (if inclusive
      (evil-range (line-beginning-position) (line-end-position))
    (let ((start (save-excursion
                   (back-to-indentation)
                   (point)))
          (end (save-excursion
                 (goto-char (line-end-position))
                 (skip-syntax-backward " " (line-beginning-position))
                 (point))))
      (evil-range start end))))

(evil-define-text-object evil-a-line (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-line-range count beg end type t))
(evil-define-text-object evil-inner-line (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-line-range count beg end type))

(define-key evil-outer-text-objects-map evil-textobj-line-a-key 'evil-a-line)
(define-key evil-inner-text-objects-map evil-textobj-line-i-key 'evil-inner-line)

(provide 'evil-textobj-line)

;;; evil-textobj-line.el ends here
