;;; smartparens-latex.el --- Additional configuration for (La)TeX based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 14 Feb 2013
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

;; This file provides some additional configuration for (La)TeX based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-latex)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defun sp-latex-insert-spaces-inside-pair (_id action _context)
  "ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (insert "  ")
    (backward-char 1))
  (when (and (eq action 'wrap)
             (save-excursion
               (goto-char (sp-get sp-last-wrapped-region :beg-in))
               (not (sp--looking-back-p "[[{(]"))))
    (save-excursion
      (goto-char (sp-get sp-last-wrapped-region :end-in))
      (insert " ")
      (goto-char (sp-get sp-last-wrapped-region :beg-in))
      (insert " "))))

(defun sp-latex-skip-match-apostrophe (ms _mb me)
  "MS, MB, ME."
  (when (equal ms "'")
    (save-excursion
      (goto-char me)
      (looking-at-p "\\sw"))))

(defun sp-latex-skip-double-quote (_id action _context)
  "ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (when (looking-at-p "''''")
      (delete-char -2)
      (delete-char 2)
      (forward-char 2))))

(defun sp-latex-point-after-backslash (id action _context)
  "Return t if point follows a backslash, nil otherwise.
This predicate is only tested on \"insert\" action.
ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (let ((trigger (sp-get-pair id :trigger)))
      (looking-back (concat "\\\\" (regexp-quote (if trigger trigger id))) nil))))

(add-to-list 'sp-navigate-skip-match
             '((tex-mode plain-tex-mode latex-mode) . sp--backslash-skip-match))

(sp-with-modes '(
                 tex-mode
                 plain-tex-mode
                 latex-mode
                 LaTeX-mode
                 )
  (sp-local-pair "`" "'"
                 :actions '(:rem autoskip)
                 :skip-match 'sp-latex-skip-match-apostrophe
                 :unless '(sp-latex-point-after-backslash sp-in-math-p))
  ;; math modes, yay.  The :actions are provided automatically if
  ;; these pairs do not have global definitions.
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]"
                 :unless '(sp-latex-point-after-backslash))

  ;; disable useless pairs.
  (sp-local-pair "\\\\(" nil :actions nil)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "\\\"" nil :actions nil)

  ;; quote should insert ``'' instead of double quotes.  If we ever
  ;; need to insert ", C-q is our friend.
  (sp-local-pair "``" "''"
                 :trigger "\""
                 :unless '(sp-latex-point-after-backslash sp-in-math-p)
                 :post-handlers '(sp-latex-skip-double-quote))

  ;; add the prefix function sticking to {} pair
  (sp-local-pair "{" nil :prefix "\\\\\\(\\sw\\|\\s_\\)*")

  ;; do not add more space when slurping
  (sp-local-pair "{" "}")
  (sp-local-pair "(" ")")
  (sp-local-pair "[" "]")

  ;; pairs for big brackets.  Needs more research on what pairs are
  ;; useful to add here.  Post suggestions if you know some.
  (sp-local-pair "\\left(" "\\right)"
                 :trigger "\\l("
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left[" "\\right]"
                 :trigger "\\l["
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left\\{" "\\right\\}"
                 :trigger "\\l{"
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\left|" "\\right|"
                 :trigger "\\l|"
                 :when '(sp-in-math-p)
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\bigl(" "\\bigr)"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\biggl(" "\\biggr)"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Bigl(" "\\Bigr)"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Biggl(" "\\Biggr)"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\bigl[" "\\bigr]"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\biggl[" "\\biggr]"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Bigl[" "\\Bigr]"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Biggl[" "\\Biggr]"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\bigl\\{" "\\bigr\\}"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\biggl\\{" "\\biggr\\}"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Bigl\\{" "\\Bigr\\}"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\Biggl\\{" "\\Biggr\\}"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\lfloor" "\\rfloor"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\lceil" "\\rceil"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair "\\langle" "\\rangle"
                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair  "\\lVert" "\\rVert"
          :when '(sp-in-math-p)
          :trigger "\\lVert"
          :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair  "\\lvert" "\\rvert"
          :when '(sp-in-math-p)
          :trigger "\\lvert"
          :post-handlers '(sp-latex-insert-spaces-inside-pair))

  ;; some common wrappings
  (sp-local-tag "\"" "``" "''" :actions '(wrap))
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
  (sp-local-tag "bi" "\\begin{itemize}" "\\end{itemize}")
  (sp-local-tag "be" "\\begin{enumerate}" "\\end{enumerate}"))

(provide 'smartparens-latex)

;;; smartparens-latex.el ends here
