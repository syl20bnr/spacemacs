;;; smartparens-markdown.el --- Additional configuration for Markdown based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 11th May 2017
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

;; This file provides some additional configuration for Markdown based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-markdown)
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
(eval-when-compile
  (defvar markdown-gfm-use-electric-backquote))


(defun sp-gfm-electric-backquote-p (_id action _context)
  "Do not insert ```...``` pair if that would be handled by `markdown-electric-backquote'."
  (and (eq action 'insert)
       markdown-gfm-use-electric-backquote
       (sp--looking-back-p "^```")))

(defun sp--gfm-point-after-word-p (id action _context)
  "Return t if point is after a word, nil otherwise.
This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (sp--looking-back-p (concat "\\(\\sw\\)" (regexp-quote id)))))

(defun sp--gfm-skip-asterisk (_ms mb _me)
  "Non-nil if we should ignore this asterisk as a delimiter."
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

(sp-with-modes '(markdown-mode gfm-mode)
  (sp-local-pair "*" "*"
                 :unless '(sp--gfm-point-after-word-p sp-point-at-bol-p)
                 :post-handlers '(("[d1]" "SPC"))
                 :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "**" "**")
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p)))

(sp-with-modes 'markdown-mode
  (sp-local-pair "```" "```"))

(sp-with-modes 'gfm-mode
  (sp-local-pair "`" "`" :unless '(:add sp-gfm-electric-backquote-p))
  (sp-local-pair "```" "```" :unless '(:add sp-gfm-electric-backquote-p)))

(provide 'smartparens-markdown)
;;; smartparens-markdown.el ends here
