;;; gsexp.el --- GraphQl as S-expressions  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library encodes S-expressions as GraphQl documents.  Currently
;; it is rather incomplete and only intended for use by `ghub-graphql'
;; but I might fill the gaps and add documentation eventually.  I am
;; not in a hurry though.

;;; Code:

(require 'cl-lib)

(defvar gsexp-one-variable-per-line nil)

(defun gsexp-encode (document)
  (gsexp--pp (gsexp--encode-field document)))

(defun gsexp--encode-field (field)
  (cond
   ((stringp field)
    field)
   ((symbolp field)
    (symbol-name field))
   ((listp (car field))
    (concat (format "%s: " (caar field))
            (gsexp--encode-field (cons (car (cdar field))
                                       (cdr field)))))
   ((concat
     (pcase (pop field)
       (`[] "")
       ((and (pred symbolp) op) (symbol-name op))
       (`[,op] (symbol-name op))
       (`[,op ,name] (format "%s %s" op name)))
     (and (vectorp (car field))
          (format " (%s%s)"
                  (if gsexp-one-variable-per-line "\n" "")
                  (mapconcat #'gsexp--encode-argument
                             (append (pop field) nil)
                             (if gsexp-one-variable-per-line ",\n" ","))))
     (and field
          (format " {\n%s\n}"
                  (mapconcat #'gsexp--encode-field field "\n")))))))

(cl-defun gsexp--encode-argument ((argument value))
  (format "%s: %s" argument (gsexp--encode-value value)))

(defun gsexp--encode-value (value)
  (cond
   ((numberp value)
    (number-to-string value))
   ((symbolp value) ; including variables, enums, booleans and null
    (symbol-name value))
   ((stringp value)
    (prin1-to-string value))
   ((vectorp value)
    (format "(%s)" (mapconcat #'gsexp--encode-value value "")))
   ((listp value)
    (format "{%s}" (mapconcat
                    (pcase-lambda (`(,name ,value))
                      (format "%s: %s" name (gsexp--encode-value value)))
                    value ", ")))
   ((error "Invalid field value: %S" value))))

(defun gsexp--pp (string)
  (with-temp-buffer
    (save-excursion
      (insert string))
    (while (< (point) (point-max))
      (unless (and (bolp) (eolp))
        (save-excursion
          (let ((level (car (syntax-ppss (line-beginning-position)))))
            (when (looking-at "\\s-*\\s)")
              (cl-decf level))
            (indent-line-to (* 2 level)))))
      (forward-line 1))
    (buffer-string)))

;;; _
(provide 'gsexp)
;;; gsexp.el ends here
