;;; window-purpose-utils.el --- Utilities -*- lexical-binding: t -*-

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
;; Utilities used in the Purpose package

;;; Code:

(require 'cl-lib)

(defcustom purpose-message-on-p nil
  "If non-nil, `purpose-message' will produce a message.
Toggling this on will cause Purpose to produce some debug messages."
  :group 'purpose
  :type 'boolean
  :package-version '(window-purpose . "1.2"))

(defun purpose-message (format-string &rest args)
  "Produce a message if `purpose-message-on-p' is non-nil.
The message is produced with the `message' function.  In any case,
return the formatted string. FORMAT-STRING and ARGS are passed to
`message' or `format' as is."
  (if purpose-message-on-p
      (apply #'message format-string args)
    (apply #'format format-string args)))

;; define our (limited) version of alist-get
(defun purpose-alist-get (key alist &optional default _remove)
  "Get KEY's value in ALIST.
If no such key, return DEFAULT.
When setting KEY's value, if the new value is equal to DEFAULT and
REMOVE is non-nil, then delete the KEY instead."
  (let ((entry (assq key alist)))
    (if entry
        (cdr entry)
      default)))

(defun purpose-alist-set (key value alist)
  "Set VALUE to be the value associated to KEY in ALIST.
This doesn't change the original alist, but returns a modified copy."
  (cons (cons key value)
        (purpose-alist-del key alist)))

(defun purpose-alist-del (key alist)
  "Delete KEY from ALIST.
This doesn't change the original alist, but returns a modified copy."
  ;; we could use any value instead of 0, as long as we used it instead
  ;; of 0 in both places
  (cl-remove-if #'(lambda (entry)
                    (eq key (car entry)))
                alist))

(defun purpose-flatten (seq)
  "Turn a list of lists (SEQ) to one concatenated list."
  (apply #'append seq))

(defun purpose-alist-combine (&rest alists)
  "Combine ALISTS into one alist.
If several alists have the same key, the entry from the first alist with
that key is used.  Example:
  (purpose-alist-combine '((a . 1) (b . 2)) '((a . 3) (c . 4)))
  => '((a . 1) (b . 2) (c . 4))"
  ;; (purpose-flatten alists)
  (let ((result nil))
    (dolist (alist alists)
      (dolist (element alist)
        (unless (assoc (car element) result)
          (setq result (purpose-alist-set (car element)
                                          (cdr element)
                                          result)))))
    result))

(defun purpose-plist-values (plist)
  "Return only the values of PLIST, as a list.
PLIST is a property list.
Example:
 (plist-values '(:foo 1 :bar 2)) -> (1 2)"
  (cl-loop for i from 0
           for item in plist
           when (cl-oddp i) collect item))



(defun purpose--iter-hash (function table)
  "Like `maphash', but return a list the results of calling FUNCTION
for each entry in hash-table TABLE."
  (let (results)
    (maphash #'(lambda (key value)
                 (push (funcall function key value) results))
             table)
    results))

;; taken from http://emacs.stackexchange.com/a/7405/6533, credit to Jordon Biondo
(defun purpose--call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-remove-if-not 'car frames)))

;; taken from http://emacs.stackexchange.com/a/7405/6533, credit to Jordon Biondo
(defun purpose--function-stack ()
  "Like `purpose--call-stack' but is a list of only the function names."
  (butlast (mapcar 'cl-second (purpose--call-stack))))

(provide 'window-purpose-utils)
;;; window-purpose-utils.el ends here
