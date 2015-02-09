;;; solarized-theme-utils.el --- Utilities for solarized theme development

;; Copyright (C) 2012 Thomas Frössman

;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs

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
;;
;; Development utilities, these are not needed for normal theme usage
;;

;;;; Code:
(require 'cl)
(require 'solarized)

(defun solarized-import-faces (&optional regexp already-defined)
  "Imports current effective face definitions by regular expression
in the format of solarized-theme.el."
  (interactive (list (read-regexp "List faces matching regexp")))
  (let*
      ((all-faces (zerop (length regexp)))
       (faces
        (delq nil
              (mapcar (lambda (face)
                        (let ((s (symbol-name face)))
                          (when (or all-faces (string-match regexp s))
                            face)))
                      (sort (face-list) #'string-lessp)))))
    (mapc (lambda(face)
            (when (or (not (get face 'theme-face)) already-defined)
              (insert (format
                       "`(%s ((,class %s)))%s
"
                       face
                       (let (result)
                         (dolist (entry face-attribute-name-alist result)
                           (let* ((attribute (car entry))
                                  (value (face-attribute face attribute)))
                             (unless (eq value 'unspecified)
                               (setq result
                                     (nconc (list attribute
                                                  (cond
                                                   ((member* attribute
                                                             '(":background"
                                                               ":foreground")
                                                             :test 'string=)
                                                    (format "\"%s\"" value))
                                                   (t value))) result))))))
                       (if (get face 'theme-face)
                           (format " ;; Already set by current theme!")
                         "")))))
          faces)))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; indent-tabs-mode: nil
;; End:
(provide 'solarized-theme-utils)
;;; solarized-theme-utils.el ends here
