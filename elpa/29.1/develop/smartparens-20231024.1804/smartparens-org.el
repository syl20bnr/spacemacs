;;; smartparens-org.el --- Configuration for Org mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 15th January 2017
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Org based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-org)
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
(require 'smartparens-text)

(defun sp--org-skip-asterisk (_ms mb me)
  "Non-nil if the asterisk is part of the outline marker."
  (save-excursion
    (goto-char mb)
    (beginning-of-line)
    (let ((skip-distance (skip-chars-forward "*")))
      (if (= skip-distance 1)
          (not (memq (syntax-class (syntax-after (point))) '(2 3)))
        (<= me (point))))))

(defun sp-org-point-after-left-square-bracket-p (id action _context)
  "Return t if point is after a left square bracket, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "\\[" (regexp-quote id)))))

(sp-with-modes 'org-mode
  (sp-local-pair "*" "*"
                 :unless '(sp-point-after-word-p sp-point-at-bol-p)
                 :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p))
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p sp-org-point-after-left-square-bracket-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»"))

(provide 'smartparens-org)
;;; smartparens-org.el ends here
