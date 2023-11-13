;;; smartparens-scala.el --- Additional configuration for Scala based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018, 2021 Greg Nwosu

;; Author: Greg Nwosu <greg.nwosu@gmail.com>
;; Maintainer: Greg Nwosu <greg.nwosu@gmail.com>
;; Created: 8 July 2015
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Scala based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-scala)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;; If you have good ideas about what should be added please file an
;; issue on the github tracker.
;;
;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

;; Scala has no sexp suffices.  This fixes slurping
;; import scala.mutable{|} ListBuffer, Set ---the comma should not travel with the closing
;; paren
(--each '(scala-mode inferior-scala-mode)
  (add-to-list 'sp-sexp-suffix (list it 'regexp "")))

(defun sp-scala-wrap-with-indented-newlines (_1 action _2)
  "Post handler for the wrap ACTION, putting the region on indented newlines."
  (when (eq action 'wrap)
    (indent-for-tab-command)
    (sp-get sp-last-wrapped-region
      (let ((beg :beg-in)
            (end :end-in))
        (save-excursion
          (goto-char end)
          (newline-and-indent))
        (save-excursion
          (goto-char beg)
          (newline))
        (indent-region beg end)))))

(sp-local-pair 'scala-mode "(" nil
               :post-handlers '(("||\n[i]" "RET")
                                ("| " "SPC")))

(sp-local-pair 'scala-mode "{" nil
               :post-handlers '(("||\n[i]" "RET")
                                ("| " "SPC")
                                sp-scala-wrap-with-indented-newlines))

(sp-local-pair 'scala-mode "\"\"\"" "\"\"\"")

(provide 'smartparens-scala)
;;; smartparens-scala.el ends here
