;;; evil-matchit-fortran.el --- fortran plugin of evil-matchit

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;; Code:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

(defvar evilmi-fortran-extract-keyword-howtos
  '(("^[ \t]*\\([a-zA-Z _]+\\)[(:].*$" 1) ;; "if (...)"
    ("^[ \t]*\\([a-zA-Z]+\\) *$" 1) ;; "end"
    ("^[ \t]*\\(do\\) .*$" 1) ;; "do i=1,10"
    ("^[ \t]*\\([a-zA-Z]+ [a-zA-Z0-9_]+\\).*$" 1) ;; "end program"
    ))

;; Fortran (http://www.fortran.org) syntax
(defvar evilmi-fortran-match-tags
  '((("if") ("else if" "else") ("end" "end *if"))
    (("do" "while") () ("end *do" "until" "continue"))
    (("where") ("elsewhere") ("end *where") "MONOGAMY")
    (("select *case") ("case" "case default") ("end *select") "MONOGAMY")
    (("forall") () ("end *forall.*") "MONOGAMY")
    (("associate .*") () ("end *associate.*") "MONOGAMY")
    (("enum") () ("end *enum.*") "MONOGAMY")
    (("interface") () ("end *interface.*") "MONOGAMY")
    (("subroutine .*") () ("end *subroutine" "end"))
    (("function .*") () ("end *function" "end"))
    (("module .*") () ("end *module.*") "MONOGAMY")
    (("program .*") () ("end *program.*") "MONOGAMY")
    ))

;;;###autoload
(defun evilmi-fortran-get-tag ()
  (evilmi-sdk-get-tag evilmi-fortran-match-tags evilmi-fortran-extract-keyword-howtos))

;;;###autoload
(defun evilmi-fortran-jump (info num)
  (evilmi-sdk-jump info num evilmi-fortran-match-tags evilmi-fortran-extract-keyword-howtos))

(provide 'evil-matchit-fortran)
;;; evil-matchit-fortran.el ends here
