;;; spacemacs-ht.el --- hash table util  -*- lexical-binding: t; -*-
;; Copyright (C) 2013 Wilfred Hughes - original implementation
;; Copyright (C) 2021 Sylvain Benner & Contributors - fork
;;
;; Author: Thanh Vuong
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
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

;;; Commentary:  This is a fork of ht.el by Wilfred Hughes
;;  https://github.com/Wilfred/ht.el/blob/master/ht.el
;;
;;; Commentary:
;; Spacemacs only uses 6 out 29 functions provded by the original ht.el
;; but ht.el also requires dash.el
;; To avoid extra dependency we define those 6 ht functions and add the
;; spacemacs- prefix to avoid any conflict.

(define-inline spacemacs-ht-get (table key &optional default)
  "Look up KEY in TABLE, and return the matching value.
If KEY isn't present, return DEFAULT (nil if not specified)."
  (declare (side-effect-free t))
  (inline-quote
   (gethash ,key ,table ,default)))

(define-inline spacemacs-ht-contains? (table key)
  "Return 't if TABLE contains KEY."
  (declare (side-effect-free t))
  (inline-quote
   (let ((not-found-symbol (make-symbol "spacemacs-ht--not-found")))
     (not (eq (spacemacs-ht-get ,table ,key not-found-symbol) not-found-symbol)))))

(defun spacemacs-ht-map (function table)
  "Apply FUNCTION to each key-value pair of TABLE, and make a list of the results.
FUNCTION is called with two arguments, KEY and VALUE."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall function key value) results))
     table)
    results))

(defun spacemacs-ht-keys (table)
  "Return a list of all the keys in TABLE."
  (declare (side-effect-free t))
  (spacemacs-ht-map (lambda (key _value) key) table))

(define-inline spacemacs-ht-set! (table key value)
  "Associate KEY in TABLE with VALUE."
  (inline-quote
   (prog1 nil
     (puthash ,key ,value ,table))))

(define-inline spacemacs-ht-create (&optional test)
  "Create an empty hash table.

TEST indicates the function used to compare the hash
keys.  Default is `equal'.  It can be `eq', `eql', `equal' or a
user-supplied test created via `define-hash-table-test'."
  (declare (side-effect-free t))
  (inline-quote (make-hash-table :test (or ,test 'equal))))

(provide 'spacemacs-ht)
