;;; smartparens-lua.el --- Additional configuration for Lua based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014, 2016-2018 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 3 August 2013
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

;; This file provides some additional configuration for Lua based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-lua)
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

(defun sp-lua-post-keyword-insert (id action _context)
  "ID, ACTION, CONTEXT."
  (cond
   ((eq action 'insert)
    (cond
     ((member id '("while" "for"))
      (insert " do")
      (save-excursion (newline-and-indent))
      (backward-char 3))
     ((equal id "if")
      (insert " then")
      (save-excursion (newline-and-indent))
      (backward-char 5))
     ((equal id "function")
      (save-excursion (newline-and-indent))
      (insert " "))))))

;; all the pairs are expanded only if followed by "SPC" event.  This
;; will reduce false positives like 'dIFficult' to trigger.
(sp-with-modes '(lua-mode)
  (sp-local-pair "if" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-lua-post-keyword-insert))
  (sp-local-pair "function" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-lua-post-keyword-insert))
  (sp-local-pair "for" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-lua-post-keyword-insert))
  (sp-local-pair "while" "end"
                 :when '(("SPC"))
                 :unless '(sp-in-comment-p sp-in-string-p)
                 :post-handlers '(sp-lua-post-keyword-insert))
  )

(provide 'smartparens-lua)

;;; smartparens-lua.el ends here
