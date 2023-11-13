;; taxy-package-report.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>n

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by Manuel Uberti's blog post:
;; <https://www.manueluberti.eu//emacs/2021/09/01/package-report/>.

;;; Code:

(require 'cl-lib)
(require 'package)

(require 'taxy-magit-section)

(defun taxy-package-report ()
  "List installed packages by archive in a `magit-section' buffer."
  (interactive)
  (cl-labels ((package-archive
               (package) (if-let* ((package-struct (car (alist-get (car package) package-archive-contents)))
                                   (archive (package-desc-archive package-struct)))
                             archive
                           "no archive"))
              (format-package
               (package) (symbol-name (car package))))
    (let ((taxy (make-taxy-magit-section
                 :name "Packages by archive"
                 :take (lambda (item taxy)
                         (taxy-take-keyed (list #'package-archive) item taxy))
                 :make (lambda (&rest args)
                         (apply #'make-taxy-magit-section :format-fn #'format-package :indent 0 args))
                 :format-fn #'format-package
                 :indent 0)))
      (taxy-magit-section-pp (taxy-fill package-alist taxy)))))

;;; taxy-package-report.el ends here
