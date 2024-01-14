;;; evil-development.el --- Useful features for Evil developers -*- lexical-binding: t -*-

;; Author: Justin Burkett <justin at burkett dot cc>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; Teach imenu about evil macros

(with-eval-after-load 'lisp-mode
  (when (boundp 'lisp-imenu-generic-expression)
    (dolist (macro '("interactive-code"
                     "type"
                     "text-object"
                     "motion"
                     "command"
                     "operator"))
      (let ((macro-name (format "evil-%s" macro)))
        (unless (assoc macro-name lisp-imenu-generic-expression)
          (push (list
                 macro-name
                 (format "^\\s-*(evil-define-%s\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
                         macro)
                 1)
                lisp-imenu-generic-expression))))))

(provide 'evil-development)

;;; evil-development.el ends here
